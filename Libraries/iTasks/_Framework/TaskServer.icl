implementation module iTasks._Framework.TaskServer

import StdFile, StdBool, StdInt, StdClass, StdList, StdMisc, StdArray, StdTuple, StdOrdList
import Data.Maybe, Data.Functor, Data.Func, Data.Error, System.Time, Text, Data.Tuple
from StdFunc import seq
from Data.Map import :: Map (..)
import qualified System.Process as Process
from System.Process import :: ProcessIO (..), :: ReadPipe, :: WritePipe
import System.CommandLine
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified iTasks._Framework.SDS as SDS
import TCPChannelClass, TCPChannels, TCPEvent, TCPStringChannels, TCPDef, tcp

import iTasks._Framework.Engine, iTasks._Framework.IWorld, iTasks._Framework.TaskEval, iTasks._Framework.TaskStore
import iTasks._Framework.IWorld
import iTasks._Framework.Task
import iTasks._Framework.TaskEval
from iTasks._Framework.TaskStore import queueRefresh
import iTasks.API.Common.SDSCombinators

//Helper type that holds the mainloop instances during a select call
//in these mainloop instances the unique listeners and read channels
//have been temporarily removed.
:: *IOTaskInstanceDuringSelect
    = ListenerInstanceDS !ListenerInstanceOpts
    | ConnectionInstanceDS !ConnectionInstanceOpts !*TCP_SChannel
    | ExternalProcessInstanceDS !ExternalProcessInstanceOpts !ProcessHandle !ProcessIO
    | BackgroundInstanceDS !BackgroundInstanceOpts !BackgroundTask

serve :: ![TaskWrapper] ![(!Int,!ConnectionTask)] ![BackgroundTask] (*IWorld -> (!Maybe Timeout,!*IWorld)) *IWorld -> *IWorld
serve its cts bts determineTimeout iworld
    = loop determineTimeout (init its cts bts iworld)

init :: ![TaskWrapper] ![(!Int,!ConnectionTask)] ![BackgroundTask] !*IWorld -> *IWorld
init its cts bts iworld
	// Check if it the initial tasks have been added already
	# iworld = createInitialInstances its iworld	
 	// All persistent task instances should receive a reset event to continue their work
    # iworld=:{IWorld|ioTasks,world} = queueAll iworld
	# (listeners,world) = connectAll cts world
    # ioStates = 'DM'.fromList [(TaskId 0 0, IOActive 'DM'.newMap)]
    = {iworld & ioTasks = {done=[],todo=listeners ++ map (BackgroundInstance {bgInstId=0}) bts}, ioStates = ioStates,  world = world}
where
	createInitialInstances :: [TaskWrapper] !*IWorld -> *IWorld
	createInitialInstances its iworld
		# (mbNextNo,iworld) = read nextInstanceNo iworld
		| (mbNextNo =: (Ok 1)) = createAll its iworld //This way we check if it is the initial run of the program
                               = iworld

	createAll :: [TaskWrapper] !*IWorld -> *IWorld
	createAll [] iworld = iworld
	createAll [TaskWrapper task:ts] iworld
		= case createTaskInstance task iworld of
			(Ok _,iworld) = createAll ts iworld
			(Error (_,e),iworld) = abort e

	queueAll :: !*IWorld -> *IWorld
	queueAll iworld
		# (mbIndex,iworld) = read (sdsFocus defaultValue filteredInstanceIndex) iworld
		= case mbIndex of
			Ok index    = queueRefresh [(instanceNo,"Persistent first refresh") \\ (instanceNo,_,_,_)<- index]  iworld
			_           = iworld

	connectAll :: ![(!Int,!ConnectionTask)] !*World -> *(![*IOTaskInstance],!*World)
	connectAll [] world = ([],world)
	connectAll [(port,ct):cts] world
		# (l,world) = connect port ct world
		# (ls,world) = connectAll cts world 
		= ([l:ls],world)

	connect :: !Int !ConnectionTask !*World -> *(!*IOTaskInstance,!*World)
	connect port ct world
    	# (success, mbListener, world) = openTCP_Listener port world
    	| not success = abort ("Error: port "+++ toString port +++ " already in use.\n")
    	# opts = {ListenerInstanceOpts|taskId=TaskId 0 0, nextConnectionId=0, port=port, connectionTask=ct, removeOnClose = True}
		= (ListenerInstance opts (fromJust mbListener),world)

loop :: !(*IWorld -> (!Maybe Timeout,!*IWorld)) !*IWorld -> *IWorld
loop determineTimeout iworld
    # (mbTimeout,iworld=:{IWorld|ioTasks={todo},world}) = determineTimeout iworld
    //Check which mainloop tasks have data available
    # (todo,chList,world) = select mbTimeout todo world
    //Process the select result
    # iworld =:{shutdown,ioTasks={done}} = process 0 chList {iworld & ioTasks = {done=[],todo=todo}, world = world}
    //Move everything from the done list  back to the todo list
    # iworld = {iworld & ioTasks={todo = reverse done,done=[]}}
    //Everything needs to be re-evaluated
	= case shutdown of
    	(Just exitCode) = halt exitCode iworld
        _               = loop determineTimeout iworld

select :: (Maybe Timeout) *[IOTaskInstance] *World -> (!*[IOTaskInstance],![(Int,SelectResult)],!*World)
select mbTimeout mlInstances world
    # (empty,listeners,rChannels,mlInstances) = toSelectSet mlInstances
	| empty //selectChannel_MT aborts if it is called with an empty list, so we must make sure that never happens
    	# (mlInstances, chList) = fromSelectSet listeners rChannels mlInstances []
    	= (mlInstances, chList, world)
	| otherwise
    	# (chList,(TCP_Pair (TCP_Listeners listeners) (TCP_RChannels rChannels)),_,world)	
     	   = selectChannel_MT mbTimeout (TCP_Pair (TCP_Listeners listeners) (TCP_RChannels rChannels)) TCP_Void world
    	# (mlInstances, chList)
           = fromSelectSet listeners rChannels mlInstances chList
    	= (mlInstances, chList, world)

toSelectSet :: !*[IOTaskInstance] -> *(!Bool,!*[*TCP_Listener],!*[*TCP_RChannel],!*[*IOTaskInstanceDuringSelect])
toSelectSet [] = (True,[],[],[])
toSelectSet [i:is]
    # (e,ls,rs,is) = toSelectSet is
    = case i of
        ListenerInstance opts l = (False,[l:ls],rs,[ListenerInstanceDS opts:is])
        ConnectionInstance opts {rChannel,sChannel} = (False,ls,[rChannel:rs],[ConnectionInstanceDS opts sChannel:is])
        ExternalProcessInstance opts pHandle pIO = (e, ls, rs, [ExternalProcessInstanceDS opts pHandle pIO : is])
        BackgroundInstance opts bt = (e,ls,rs,[BackgroundInstanceDS opts bt:is])

/* Restore the list of main loop instances.
    In the same pass also update the indices in the select result to match the
    correct indices of the main loop instance list.
*/
fromSelectSet :: !*[*TCP_Listener] !*[*TCP_RChannel] !*[*IOTaskInstanceDuringSelect] ![(!Int,!SelectResult)] -> *(![*IOTaskInstance],![(!Int,!SelectResult)])
fromSelectSet ls rs is chList
    # (numListeners,ls) = ulength ls
    # sortedChList      = sortBy (\(x,_) (y,_) -> (x < y)) chList //The single-pass algorithm expects a sorted select result
    = fromSelectSet` 0 numListeners 0 0 ls rs sortedChList is
where
    fromSelectSet` i numListeners numSeenListeners numSeenReceivers ls rs _ [] = ([],[])
    //Listeners
    fromSelectSet` i numListeners numSeenListeners numSeenReceivers [l:ls] rs [] [ListenerInstanceDS opts:is]
        # (is,_) = fromSelectSet` (i+1) numListeners (numSeenListeners+1) numSeenReceivers ls rs [] is
        = ([ListenerInstance opts l:is],[])
    fromSelectSet` i numListeners numSeenListeners numSeenReceivers [l:ls] rs [(c,what):ch] [ListenerInstanceDS opts:is]
        | c == numSeenListeners //Check select result
            # (is,ch) = fromSelectSet` (i+1) numListeners (numSeenListeners+1) numSeenReceivers ls rs ch is
            = ([ListenerInstance opts l:is],[(i,what):ch])
        | otherwise 
            # (is,ch) = fromSelectSet` (i+1) numListeners (numSeenListeners+1) numSeenReceivers ls rs [(c,what):ch] is
            = ([ListenerInstance opts l:is],ch)
    //Receivers
    fromSelectSet` i numListeners numSeenListeners numSeenReceivers ls [rChannel:rs] [] [ConnectionInstanceDS opts sChannel:is]
        # (is,ch) = fromSelectSet` (i+1) numListeners numSeenListeners (numSeenReceivers+1) ls rs [] is
        = ([ConnectionInstance opts {rChannel=rChannel,sChannel=sChannel}:is],[])
    fromSelectSet` i numListeners numSeenListeners numSeenReceivers ls [rChannel:rs] [(c,what):ch] [ConnectionInstanceDS opts sChannel:is]
        | c == numListeners + numSeenReceivers
            # (is,ch) = fromSelectSet` (i+1) numListeners numSeenListeners (numSeenReceivers+1) ls rs ch is
            = ([ConnectionInstance opts {rChannel=rChannel,sChannel=sChannel}:is],[(i,what):ch])
        | otherwise
            # (is,ch) = fromSelectSet` (i+1) numListeners numSeenListeners (numSeenReceivers+1) ls rs [(c,what):ch] is
            = ([ConnectionInstance opts {rChannel=rChannel,sChannel=sChannel}:is],ch)
    //External process task
    fromSelectSet` i numListeners numSeenListeners numSeenReceivers ls rs ch [ExternalProcessInstanceDS opts pHandle pIO : is]
        # (is,ch) = fromSelectSet` (i+1) numListeners numSeenListeners numSeenReceivers ls rs ch is
        = ([ExternalProcessInstance opts pHandle pIO:is],ch)
    //Background tasks
    fromSelectSet` i numListeners numSeenListeners numSeenReceivers ls rs ch [BackgroundInstanceDS opts bt:is]
        # (is,ch) = fromSelectSet` (i+1) numListeners numSeenListeners numSeenReceivers ls rs ch is
        = ([BackgroundInstance opts bt:is],ch)

    ulength [] = (0,[])
    ulength [x:xs]
        # (n,xs) = ulength xs
        = (n + 1,[x:xs])

//TODO: Use share notification to trigger task re-evaluation based on io events
process :: !Int [(!Int,!SelectResult)] !*IWorld -> *IWorld
process i chList iworld=:{ioTasks={done,todo=[]}} = iworld
process i chList iworld=:{ioTasks={done,todo=[ListenerInstance lopts listener:todo]},ioStates,world}
    # (TaskId instanceNo _) = lopts.ListenerInstanceOpts.taskId
    = case 'DM'.get lopts.ListenerInstanceOpts.taskId ioStates of
        //Active listener:
        Just (IOActive conStates)
            # (mbSelect,chList) = checkSelect i chList
            | mbSelect =:(Just _)
     	        # (tReport, mbNewConn, listener, world)   = receive_MT (Just 0) listener world
                | tReport == TR_Success
                    # (ip,{rChannel,sChannel}) = fromJust mbNewConn
                    # (ConnectionTask handlers sds) = lopts.ListenerInstanceOpts.connectionTask
                    # (mbr,iworld) = 'SDS'.read sds {iworld & ioTasks={done=done,todo=todo},world=world}
                    | mbr =:(Error _)
                        # iworld=:{ioTasks={done,todo},world} = if (instanceNo > 0) (queueRefresh [(instanceNo,"IO Exception for instance "<+++instanceNo)] iworld) iworld
                        # ioStates = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOException (snd (fromError mbr))) ioStates
 	                    # world = closeRChannel listener world
                        = process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}
                    # (mbConState,mbw,out,close,iworld) = handlers.ConnectionHandlersIWorld.onConnect (toString ip) (fromOk mbr) iworld
                    # iworld = if (instanceNo > 0) (queueRefresh [(instanceNo,"New TCP connection for instance "<+++instanceNo)] iworld) iworld
                    # (mbSdsErr, iworld=:{ioTasks={done,todo},world}) = writeShareIfNeeded sds mbw iworld
                    | mbConState =:(Error _)
                        # ioStates = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOException (fromError mbConState)) ioStates
                        = process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}, ioStates = ioStates, world=world}
                    | isError mbSdsErr
                        # ioStates = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOException (snd (fromError mbSdsErr))) ioStates
                        = process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}, ioStates = ioStates, world=world}
                    # conStates = 'DM'.put lopts.ListenerInstanceOpts.nextConnectionId (fromOk mbConState,close) conStates
                    # (sChannel,world) = case out of
                        []          = (sChannel,world)
                        data        = foldl (\(s,w) d -> send (toByteSeq d) s w) (sChannel,world) data
                    | close
                    //Close the connection immediately
                        # world = closeRChannel rChannel world
                        # world = closeChannel sChannel world
                        //Remove the connection state if configured in the connection listener options
                        # conStates = if lopts.ListenerInstanceOpts.removeOnClose
                            ('DM'.del lopts.ListenerInstanceOpts.nextConnectionId conStates)
                            conStates
                        # ioStates  = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOActive conStates) ioStates
                        = process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}, ioStates = ioStates, world=world}
                    | otherwise 
                    //Persist the connection
                        # copts = {ConnectionInstanceOpts|taskId = lopts.ListenerInstanceOpts.taskId
                                  ,connectionId = lopts.ListenerInstanceOpts.nextConnectionId
                                  ,remoteHost = ip, connectionTask = lopts.ListenerInstanceOpts.connectionTask
                                  ,removeOnClose = lopts.ListenerInstanceOpts.removeOnClose}
                        # todo = todo ++ [ConnectionInstance copts {rChannel=rChannel,sChannel=sChannel}]
                        # lopts = {lopts & nextConnectionId = lopts.nextConnectionId + 1}
                        # ioStates  = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOActive conStates) ioStates
                        = process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}, ioStates = ioStates, world=world}
                //We did not accept properly accept a connection
                | otherwise
                    = process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}, world=world}
            //Nothing to do
            | otherwise
                = process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}, world=world}
        //Destroyed listener:
        Just (IODestroyed conStates)
 	        # world = closeRChannel listener world
            //If there are no connections belonging to this listener we can clean up, if there are the last connection will cleanup
            # ioStates = if ('DM'.mapSize conStates == 0) ('DM'.del lopts.ListenerInstanceOpts.taskId ioStates) ioStates
            = process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}
        //There was an exception or the state has already been removed
        _
 	        # world = closeRChannel listener world
            = process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}

process i chList iworld=:{ioTasks={done, todo=[ConnectionInstance opts duplexChannel:todo]}}
    # iworld = {iworld & ioTasks = {done = done, todo = todo}} 
    # iworld = processIOTask
        i chList opts.ConnectionInstanceOpts.taskId opts.ConnectionInstanceOpts.connectionId
        opts.ConnectionInstanceOpts.removeOnClose sds tcpConnectionIOOps
        (\_ -> handlers.ConnectionHandlersIWorld.onDisconnect) handlers.ConnectionHandlersIWorld.onData
        handlers.ConnectionHandlersIWorld.onShareChange handlers.ConnectionHandlersIWorld.onTick (ConnectionInstance opts) duplexChannel iworld
    = process (i+1) chList iworld
where
    (ConnectionTask handlers sds) = opts.ConnectionInstanceOpts.connectionTask

process i chList iworld=:{ioTasks={done, todo=[ExternalProcessInstance opts pHandle pIO:todo]}}
    # iworld = {iworld & ioTasks = {done = done, todo = todo}} 
    # iworld = processIOTask
        i chList opts.ExternalProcessInstanceOpts.taskId opts.ExternalProcessInstanceOpts.connectionId
        False sds externalProcessIOOps onClose onData onShareChange onTick
        (\(pHandle, pIO) -> ExternalProcessInstance opts pHandle pIO) (pHandle, pIO) iworld
    = process (i+1) chList iworld
where
    (ExternalProcessTask handlers sds) = opts.externalProcessTask

    onData :: !((!ProcessOutChannel, !String)) !Dynamic !Dynamic !*IWorld
           -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld)
    onData (channel, data) l r iworld
        # handler = case channel of
            StdOut = handlers.onOutData
            StdErr = handlers.onErrData
        # (mbl, mbw, out, close) = handler data l r
        = (mbl, mbw, out, close, iworld)

    onShareChange :: !Dynamic !Dynamic !*IWorld
                  -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld)
    onShareChange l r iworld
        # (mbl, mbw, out, close) = handlers.ExternalProcessHandlers.onShareChange l r
        = (mbl, mbw, out, close, iworld)

    // do nothing to external proc tasks
    onTick :: !Dynamic !Dynamic !*IWorld
           -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld)
    onTick l r iworld
        = (Ok l, Nothing, [], False, iworld)

    onClose :: !ExitCode !Dynamic !Dynamic !*IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, !*IWorld)
    onClose exitCode l r iworld
        # (mbl, mbw) = handlers.onExit exitCode l r
        = (mbl, mbw, iworld)

process i chList iworld=:{ioTasks={done,todo=[BackgroundInstance opts bt=:(BackgroundTask eval):todo]}}
    # (mbe,iworld=:{ioTasks={done,todo}}) = eval {iworld & ioTasks = {done=done,todo=todo}}
	| mbe =: (Error _) = abort (snd (fromError mbe)) //TODO Handle the error without an abort
    = process (i+1) chList {iworld & ioTasks={done=[BackgroundInstance opts bt:done],todo=todo}}
process i chList iworld=:{ioTasks={done,todo=[t:todo]}}
    = process (i+1) chList {iworld & ioTasks={done=[t:done],todo=todo}}

// Definitions of IO tasks (tcp connections, external processes, ...)

:: IOTaskOperations ioChannels readData closeInfo =
    { readData  :: !(Int [(Int, SelectResult)] *(!ioChannels, !*IWorld) -> *(!IOData readData closeInfo, !ioChannels, !*IWorld))
    , writeData :: !(String                    *(!ioChannels, !*IWorld) -> *(!ioChannels, !*IWorld))
    , closeIO   :: !(                          *(!ioChannels, !*IWorld) -> *IWorld)
    }
:: IOData data closeInfo = IODClosed closeInfo
                         | IODNoData
                         | IODData !data

tcpConnectionIOOps :: IOTaskOperations *TCP_DuplexChannel String ()
tcpConnectionIOOps = {readData = readData, writeData = writeData, closeIO = closeIO}
where
    readData :: !Int
                ![(Int, SelectResult)]
                !(!*TCP_DuplexChannel, !*IWorld)
             -> (!IOData String (), !*TCP_DuplexChannel, !*IWorld)
    readData i chList (channel, iworld)
        # (mbSelect, chList) = checkSelect i chList
        | mbSelect =: (Just SR_Disconnected) || mbSelect=:(Just SR_EOM)
            = (IODClosed (), channel, iworld)
        | mbSelect =: (Just SR_Available)
            # (data, rChannel, world) = receive channel.rChannel iworld.world
            = (IODData (toString data), {channel & rChannel = rChannel}, {iworld & world = world})
        | otherwise
            = (IODNoData, channel, iworld)

    writeData :: !String !(!*TCP_DuplexChannel, !*IWorld) -> (!*TCP_DuplexChannel, !*IWorld)
    writeData data (channel, iworld)
        # (sChannel, world) = send (toByteSeq data) channel.sChannel iworld.world
        = ({channel & sChannel = sChannel}, {iworld & world = world})

    closeIO :: !(!*TCP_DuplexChannel, !*IWorld) -> *IWorld
    closeIO ({rChannel, sChannel}, iworld=:{world})
        # world = closeRChannel rChannel world
        # world = closeChannel  sChannel world
        = {iworld & world = world} 

:: ProcessOutChannel = StdOut | StdErr

externalProcessIOOps :: IOTaskOperations (!ProcessHandle, !ProcessIO) (!ProcessOutChannel, !String) ExitCode
externalProcessIOOps = {readData = readData, writeData = writeData, closeIO = closeIO}
where
    readData :: !Int
                ![(Int, SelectResult)]
                !(!(!ProcessHandle, !ProcessIO), !*IWorld)
             -> (!IOData (!ProcessOutChannel, !String) ExitCode, !(!ProcessHandle, !ProcessIO), !*IWorld)
    readData _ _ ((pHandle, pIO), iworld)
        // try to read StdOut
        # (mbData, world) = 'Process'.readPipeNonBlocking pIO.stdOut iworld.world
        = case mbData of
            Error _ = abort "TODO: handle error"
            Ok data
                | data == ""
                    // try to read StdErr
                    # (mbData, world) = 'Process'.readPipeNonBlocking pIO.stdErr iworld.world
                    = case mbData of
                        Error _ = abort "TODO: handle error"
                        Ok data
                            | data == ""
                                # (mbMbRetCode, world) = 'Process'.checkProcess pHandle world
                                = case mbMbRetCode of
                                    Error _ = abort "TODO: handle error"
                                    Ok Nothing   = (IODNoData,               (pHandle, pIO), {iworld & world = world})
                                    Ok (Just ec) = (IODClosed (ExitCode ec), (pHandle, pIO), {iworld & world = world})
                            | otherwise = (IODData (StdErr, data), (pHandle, pIO), {iworld & world = world})
                | otherwise = (IODData (StdOut, data), (pHandle, pIO), {iworld & world = world})

    writeData :: !String !(!(!ProcessHandle, !ProcessIO), !*IWorld) -> (!(!ProcessHandle, !ProcessIO), !*IWorld)
    writeData data ((pHandle, pIO), iworld)
        # (mbErr, world) = 'Process'.writePipe data pIO.stdIn iworld.world
        = case mbErr of
            Error e = abort "TODO: handle error"
            _       = ((pHandle, pIO), {iworld & world = world})

    closeIO :: !(!(!ProcessHandle, !ProcessIO), !*IWorld) -> *IWorld
    closeIO ((pHandle, pIO), iworld)
        # (mbErr1, world) = 'Process'.terminateProcess pHandle iworld.world
        # (mbErr2, world) = 'Process'.closeProcessIO pIO world
        // TODO: handle errors
        = {iworld & world = world} 

processIOTask :: !Int
                 ![(Int, SelectResult)]
                 !TaskId
                 !ConnectionId
                 !Bool
                 !(RWShared () Dynamic Dynamic)
                 !(IOTaskOperations .ioChannels readData closeInfo)
                 !(closeInfo Dynamic Dynamic *IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, !*IWorld))
                 !(readData Dynamic Dynamic *IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld))
                 !(Dynamic Dynamic *IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld))
                 !(Dynamic Dynamic *IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld))
                 !(.ioChannels -> *IOTaskInstance)
                 !.ioChannels
                 !*IWorld
              -> *IWorld
processIOTask i chList taskId connectionId removeOnClose sds ioOps onCloseHandler onDataHandler
              onShareChangeHandler onTickHandler mkIOTaskInstance ioChannels iworld=:{ioStates}
    = case 'DM'.get taskId ioStates of
        Just (IOActive taskStates)
            # (TaskId instanceNo _) = taskId
            // get task state
            # mbTaskState = 'DM'.get connectionId taskStates
            | isNothing mbTaskState
                # iworld   = if (instanceNo > 0) (queueRefresh [(instanceNo, "Exception for " <+++ instanceNo)] iworld) iworld
                # ioStates = 'DM'.put taskId (IOException "Missing IO task state") ioStates
                = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
            # taskState = fst (fromJust mbTaskState)
            // read sds
            # (mbr,iworld=:{ioTasks={done,todo},world}) = 'SDS'.read sds iworld
            | mbr =: (Error _)
                # iworld   = if (instanceNo > 0) (queueRefresh [(instanceNo, "Exception for " <+++ instanceNo)] iworld) iworld
                # ioStates = 'DM'.put taskId (IOException (snd (fromError mbr))) ioStates
                = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
            # r = fromOk mbr
            // on tick handler
            # (mbTaskState, mbw, out, close, iworld) = onTickHandler taskState r iworld
            // TODO: START duplication
            # (mbSdsErr, iworld) = writeShareIfNeeded sds mbw iworld
            // write data
            # (ioChannels, iworld) = seq [ioOps.writeData o \\ o <- out] (ioChannels, iworld)
            | mbTaskState =: (Error _)
                # iworld = if (instanceNo > 0) (queueRefresh [(instanceNo, "Exception for " <+++ instanceNo)] iworld) iworld
                # ioStates = 'DM'.put taskId (IOException (fromError mbTaskState)) ioStates
                = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
            | isError mbSdsErr
                # iworld = if (instanceNo > 0) (queueRefresh [(instanceNo, "Exception for " <+++ instanceNo)] iworld) iworld
                # ioStates = 'DM'.put taskId (IOException (snd (fromError mbSdsErr))) ioStates
                = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
            // TODO: END duplication
            // TODO: START duplication
            | close
                //Remove the connection state if configured in the connection listener options
                # taskStates = if removeOnClose
                    ('DM'.del connectionId taskStates)
                    taskStates
                # ioStates = 'DM'.put taskId (IOActive taskStates) ioStates
                = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
            // TODO: END duplication
            // TODO: START duplication
            // read sds
            # (mbr,iworld=:{ioTasks={done,todo},world}) = 'SDS'.read sds iworld
            | mbr =: (Error _)
                # iworld   = if (instanceNo > 0) (queueRefresh [(instanceNo, "Exception for " <+++ instanceNo)] iworld) iworld
                # ioStates = 'DM'.put taskId (IOException (snd (fromError mbr))) ioStates
                = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
            # r = fromOk mbr
            // TODO: END duplication
            # taskState = fromOk mbTaskState
            // try to read data
            # (mbData, ioChannels, iworld) = ioOps.readData i chList (ioChannels, iworld)
            = case mbData of
                IODClosed closeInfo
                    # (mbTaskState, mbw, iworld) = onCloseHandler closeInfo taskState r iworld
                    # ioStates = case mbTaskState of
                        Ok state
                            = 'DM'.put taskId (IOActive ('DM'.put connectionId (state, True) taskStates)) ioStates
                        Error e
                            = 'DM'.put taskId (IOException e) ioStates
                    # (mbSdsErr, iworld) = writeShareIfNeeded sds mbw iworld
                    | isError mbSdsErr
                        # iworld = if (instanceNo > 0) (queueRefresh [(instanceNo, "Exception for " <+++ instanceNo)] iworld) iworld
                        # ioStates = 'DM'.put taskId (IOException (snd (fromError mbSdsErr))) ioStates
                        = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
                    # iworld = if (instanceNo > 0) (queueRefresh [(instanceNo, "IO closed for " <+++ instanceNo)] iworld) iworld
                    = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
                IODNoData
                    // persist connection
                    # {done, todo} = iworld.ioTasks
                    = {iworld & ioStates = ioStates, ioTasks = {done = [mkIOTaskInstance ioChannels : done], todo = todo}}
                IODData data
                    # (mbTaskState, mbw, out, close, iworld) = onDataHandler data taskState r iworld
                    # iworld = if (instanceNo > 0) (queueRefresh [(instanceNo, "New data for "<+++ instanceNo)] iworld)  iworld 
                    # (mbSdsErr, iworld) = writeShareIfNeeded sds mbw iworld
                    // write data
                    # (ioChannels, iworld) = seq [ioOps.writeData o \\ o <- out] (ioChannels, iworld)
                    | mbTaskState =: (Error _)
                        # iworld = if (instanceNo > 0) (queueRefresh [(instanceNo, "Exception for " <+++ instanceNo)] iworld) iworld
                        # ioStates = 'DM'.put taskId (IOException (fromError mbTaskState)) ioStates
                        = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
                    | isError mbSdsErr
                        # iworld = if (instanceNo > 0) (queueRefresh [(instanceNo, "Exception for " <+++ instanceNo)] iworld) iworld
                        # ioStates = 'DM'.put taskId (IOException (snd (fromError mbSdsErr))) ioStates
                        = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
                    # ioStates = 'DM'.put taskId (IOActive ('DM'.put connectionId (fromOk mbTaskState, close) taskStates)) ioStates
                    | close
                        //Remove the connection state if configured in the connection listener options
                        # taskStates = if removeOnClose
                            ('DM'.del connectionId taskStates)
                            taskStates
                        # ioStates = 'DM'.put taskId (IOActive taskStates) ioStates
                        = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
                    | otherwise
                        // persist connection
                        # {done, todo} = iworld.ioTasks
                        = {iworld & ioStates = ioStates, ioTasks = {done = [mkIOTaskInstance ioChannels : done], todo = todo}}
        Just (IODestroyed taskStates)
            # iworld = ioOps.closeIO (ioChannels, iworld)
            //Remove the state for this connection
            # taskStates = 'DM'.del connectionId taskStates
            //If this is the last connection for this task, we can clean up.
            # ioStates = if ('DM'.mapSize taskStates == 0) ('DM'.del taskId ioStates) ioStates
            = {iworld & ioStates = ioStates}
        _ = ioOps.closeIO (ioChannels, iworld)

writeShareIfNeeded :: !(RWShared () r w) !(Maybe w) !*IWorld -> (!MaybeError TaskException (), !*IWorld)
writeShareIfNeeded sds Nothing iworld  = (Ok (), iworld)
writeShareIfNeeded sds (Just w) iworld = 'SDS'.write w sds iworld

addListener :: !TaskId !Int !Bool !ConnectionTask !*IWorld -> (!MaybeError TaskException (),!*IWorld)
addListener taskId port removeOnClose connectionTask iworld=:{ioTasks={todo,done}, ioStates, world}
    //Open listener
    # (success, mbListener, world) = openTCP_Listener port world
    | not success
        = (Error (exception ("Error: port "+++ toString port +++ " already in use.")), {iworld & ioTasks = {done=done,todo=todo},world = world})
    # opts = {ListenerInstanceOpts|taskId = taskId, nextConnectionId = 0, port = port, connectionTask= connectionTask, removeOnClose = removeOnClose}
    # todo = todo ++ [ListenerInstance opts (fromJust mbListener)]
    # ioStates = 'DM'.put taskId (IOActive 'DM'.newMap) ioStates
    = (Ok (),{iworld & ioTasks = {done=done,todo=todo}, ioStates = ioStates, world = world})

addConnection :: !TaskId !String !Int !ConnectionTask !*IWorld -> (!MaybeError TaskException (),!*IWorld)
addConnection taskId host port connectionTask=:(ConnectionTask handlers sds) iworld
    = addIOTask taskId sds init tcpConnectionIOOps onInitHandler mkIOTaskInstance iworld
where
    init :: !*IWorld -> (!MaybeErrorString (!IPAddress, !*TCP_DuplexChannel), !*IWorld)
    init iworld
        # (mbIP, world) = lookupIPAddress host iworld.world
        = case mbIP of
            Nothing = (Error ("Failed to connect to host " +++ host), {iworld & world = world})
            Just ip
                # (tReport, mbConn, world) = connectTCP_MT Nothing (fromJust mbIP,port) world
                = case mbConn of
                    Nothing = (Error ("Failed to connect to host " +++ host), {iworld & world = world})
                    Just channel = (Ok (ip, channel), {iworld & world = world})
                
    onInitHandler :: !IPAddress !Dynamic !*IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld)
    onInitHandler ip r iworld = handlers.ConnectionHandlersIWorld.onConnect (toString ip) r iworld

    mkIOTaskInstance :: !IPAddress !*TCP_DuplexChannel -> *IOTaskInstance
    mkIOTaskInstance ip channel
        # opts = {ConnectionInstanceOpts|taskId = taskId, connectionId = 0, remoteHost = ip, connectionTask = connectionTask, removeOnClose = False}
        = ConnectionInstance opts channel

addExternalProc :: !TaskId !FilePath ![String] !(Maybe FilePath) !ExternalProcessTask !IWorld -> (!MaybeError TaskException (), !*IWorld)
addExternalProc taskId cmd args dir extProcTask=:(ExternalProcessTask handlers sds) iworld
    = addIOTask taskId sds init externalProcessIOOps onInitHandler mkIOTaskInstance iworld
where
    init :: !*IWorld -> (!MaybeErrorString (!(), (!ProcessHandle, !ProcessIO)), !*IWorld)
    init iworld
        # (mbRes, world) = 'Process'.runProcessIO cmd args dir iworld.world
        = case mbRes of
            Error (_, e) = (Error e,       {iworld & world = world})
            Ok proc      = (Ok ((), proc), {iworld & world = world})
                
    onInitHandler :: !() !Dynamic !*IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld)
    onInitHandler _ r iworld
        # (mbl, mbw, out, close) = handlers.ExternalProcessHandlers.onStartup r
        = (mbl, mbw, out, close, iworld)

    mkIOTaskInstance :: !() !(!ProcessHandle, !ProcessIO) -> *IOTaskInstance
    mkIOTaskInstance _ (pHandle, pIO)
        # opts = {ExternalProcessInstanceOpts|taskId = taskId, connectionId = 0, externalProcessTask = extProcTask}
        = ExternalProcessInstance opts pHandle pIO

addIOTask :: !TaskId
             !(RWShared () Dynamic Dynamic)
             !(*IWorld -> (!MaybeErrorString (!initInfo, !.ioChannels), !*IWorld))
             !(IOTaskOperations .ioChannels readData closeInfo)
             !(initInfo Dynamic *IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld))
             !(initInfo .ioChannels -> *IOTaskInstance)
             !*IWorld
          -> (!MaybeError TaskException (), !*IWorld)
addIOTask taskId sds init ioOps onInitHandler mkIOTaskInstance iworld
    # (mbInitRes, iworld) = init iworld
    = case mbInitRes of
        Error e = (Error (exception e), iworld)
        Ok (initInfo, ioChannels)
            // Read share
            # (mbr, iworld) = 'SDS'.read sds iworld
            | isError mbr = (liftError mbr, iworld)
            // Evaluate onInit handler
            # (mbl, mbw, out, close, iworld) = onInitHandler initInfo (fromOk mbr) iworld
            // write output
            # (ioChannels, iworld) = seq [ioOps.writeData o \\ o <- out] (ioChannels, iworld)
            //Close or add to queue
            | close
                # iworld = ioOps.closeIO (ioChannels, iworld)
                = (Ok (), iworld)
            # ioStates = iworld.ioStates
            # ioStates = case mbl of
                Ok l    = 'DM'.put taskId (IOActive ('DM'.fromList [(0,(l, False))])) ioStates
                Error e = 'DM'.put taskId (IOException e) ioStates
            # {done, todo} = iworld.ioTasks
            = (Ok (), {iworld & ioStates = ioStates, ioTasks = {done = [mkIOTaskInstance initInfo ioChannels : done], todo = todo}})

//Dynamically add a background task
addBackgroundTask :: !BackgroundTask !*IWorld -> (!MaybeError TaskException BackgroundTaskId,!*IWorld)
addBackgroundTask bt iworld=:{ioTasks={done,todo}}
# (todo, i) = appSnd (\is->1 + maxList is) (unzip (map transform todo))
# todo = todo ++ [BackgroundInstance {BackgroundInstanceOpts|bgInstId=i} bt]
= (Ok i, {iworld & ioTasks={done=done, todo=todo}})
	where
		transform a=:(BackgroundInstance {bgInstId} _) = (a, bgInstId)
		transform a = (a, 1)

//Dynamically remove a background task
removeBackgroundTask :: !BackgroundTaskId !*IWorld -> (!MaybeError TaskException (),!*IWorld)
removeBackgroundTask btid iworld=:{ioTasks={done,todo}} 
//We filter the tasks and use the boolean state to hold whether a task was dropped
# (r, todo) = foldr (\e (b, l)->let (b`, e`)=drop e in (b` || b, if b` l [e`:l])) (False, []) todo
# iworld = {iworld & ioTasks={done=done, todo=todo}}
| not r = (Error (exception "No backgroundtask with that id"), iworld)
= (Ok (), iworld)
	where
		drop a=:(BackgroundInstance {bgInstId} _) = (bgInstId == btid, a)
		drop a = (False, a)

checkSelect :: !Int ![(!Int,!SelectResult)] -> (!Maybe SelectResult,![(!Int,!SelectResult)])
checkSelect i chList =:[(who,what):ws] | (i == who) = (Just what,ws)
checkSelect i chList = (Nothing,chList)

halt :: !Int !*IWorld -> *IWorld
halt exitCode iworld=:{ioTasks={todo=[],done},world}
	# world = setReturnCode exitCode world
	= {IWorld|iworld & world = world}
halt exitCode iworld=:{ioTasks={todo=[ListenerInstance _ listener:todo],done},world}
 	# world = closeRChannel listener world
    = halt exitCode {iworld & ioTasks = {todo=todo,done=done}}
halt exitCode iworld=:{ioTasks={todo=[ConnectionInstance _ {rChannel,sChannel}:todo],done},world}
 	# world = closeRChannel rChannel world
    # world = closeChannel sChannel world
    = halt exitCode {iworld & ioTasks = {todo=todo,done=done}}
halt exitCode iworld=:{ioTasks={todo=[BackgroundInstance _ _ :todo],done},world}
    = halt exitCode {iworld & ioTasks= {todo=todo,done=done}}

