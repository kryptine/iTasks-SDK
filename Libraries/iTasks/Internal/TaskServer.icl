implementation module iTasks.Internal.TaskServer

import StdFile, StdBool, StdInt, StdClass, StdList, StdMisc, StdArray, StdTuple, StdOrdList
import Data.Maybe, Data.Functor, Data.Func, Data.Error, System.Time, Text, Data.Tuple
from StdFunc import seq
from Data.Map import :: Map (..)
import System.CommandLine
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified iTasks.Internal.SDS as SDS
import TCPChannelClass, TCPChannels, TCPEvent, TCPStringChannels, TCPDef, tcp

import iTasks.Engine, iTasks.Internal.IWorld, iTasks.Internal.TaskEval, iTasks.Internal.TaskStore
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskEval
from iTasks.Internal.TaskStore import queueRefresh
import iTasks.WF.Tasks.IO
import iTasks.SDS.Combinators.Common

MAX_EVENTS :== 5

//Helper type that holds the mainloop instances during a select call
//in these mainloop instances the unique listeners and read channels
//have been temporarily removed.
:: *IOTaskInstanceDuringSelect
    = ListenerInstanceDS !ListenerInstanceOpts
    | ConnectionInstanceDS !ConnectionInstanceOpts !*TCP_SChannel

serve :: ![TaskWrapper] ![(!Int,!ConnectionTask)] (*IWorld -> (!Maybe Timeout,!*IWorld)) *IWorld -> *IWorld
serve its cts determineTimeout iworld
    = loop determineTimeout (init its cts iworld)

init :: ![TaskWrapper] ![(!Int,!ConnectionTask)] !*IWorld -> *IWorld
init its cts iworld
	// Check if the initial tasks have been added already
	# iworld = createInitialInstances its iworld
	// All persistent task instances should receive a reset event to continue their work
    # iworld=:{IWorld|ioTasks,world} = queueAll iworld
	# (listeners,world) = connectAll cts world
    # ioStates = 'DM'.fromList [(TaskId 0 0, IOActive 'DM'.newMap)]
    = {iworld & ioTasks = {done=[],todo=listeners}, ioStates=ioStates, world=world}
where
	createInitialInstances :: [TaskWrapper] !*IWorld -> *IWorld
	createInitialInstances its iworld
		# (mbNextNo,iworld) = read nextInstanceNo EmptyContext iworld
		| (mbNextNo =: (Ok (ReadResult 1 _))) = createAll its iworld //This way we check if it is the initial run of the program
                               = iworld

	createAll :: [TaskWrapper] !*IWorld -> *IWorld
	createAll [] iworld = iworld
	createAll [TaskWrapper task:ts] iworld
		= case createTaskInstance task iworld of
			(Ok _,iworld) = createAll ts iworld
			(Error (_,e),iworld) = abort e

	queueAll :: !*IWorld -> *IWorld
	queueAll iworld
		# (mbIndex,iworld) = read (sdsFocus defaultValue filteredInstanceIndex) EmptyContext iworld
		= case mbIndex of
			Ok (ReadResult index _)    = foldl (\w (instanceNo,_,_,_) -> queueEvent instanceNo ResetEvent w) iworld index
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
 
import StdDebug
loop :: !(*IWorld -> (!Maybe Timeout,!*IWorld)) !*IWorld -> *IWorld
loop determineTimeout iworld=:{ioTasks}
    | not (trace_tn "Start loop") = undef
    // Also put all done tasks at the end of the todo list, as the previous event handling may have yielded new tasks.
    # (mbTimeout,iworld=:{IWorld|ioTasks={todo},world}) = determineTimeout {iworld & ioTasks = {done=[], todo = ioTasks.todo ++ (reverse ioTasks.done)}}
    //Check which mainloop tasks have data available
    # (todo,chList,world) = select mbTimeout todo world
	//Write the clock
	# (timespec, world) = nsTime world
    # (mbe, iworld) = write timespec (sdsFocus {start=zero,interval=zero} iworldTimespec) EmptyContext {iworld & world=world, ioTasks = {done=[],todo=todo}}
    | mbe =:(Error _) = iworld
    //Process the select result
    | not(trace_tn "Processing selects") = undef
    # iworld =:{shutdown,ioTasks={done}} = process 0 chList iworld
    //Move everything from the done list back to the todo list and process events
    | not(trace_tn "Processing events") = undef
    # (mbe, iworld) = processEvents MAX_EVENTS {iworld & ioTasks={todo = reverse done,done=[]}}
	| mbe =:(Error _) = abort "Error in event processing"
    | not (trace_tn "looping") = undef
    //Everything needs to be re-evaluated
	= case shutdown of
    	(Just exitCode) = halt exitCode iworld
        _               = loop determineTimeout iworld

processEvents :: !Int !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
processEvents max iworld
    | not (trace_tn "Processing events") = undef
	| max <= 0 = (Ok (), iworld)
	| otherwise
		= case dequeueEvent iworld of 
			(Nothing,iworld) 
            | not (trace_tn "No events") = undef
            = (Ok (),iworld)
			(Just (instanceNo,event),iworld)
            | not (trace_tn ("Evaluate task " <+++ instanceNo)) = undef
			= case evalTaskInstance instanceNo event iworld of 
				(Ok taskValue,iworld) = processEvents (max - 1) iworld
				(Error msg,iworld=:{IWorld|world}) = (Ok (),{IWorld|iworld & world = world})

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

    ulength [] = (0,[])
    ulength [x:xs]
        # (n,xs) = ulength xs
        = (n + 1,[x:xs])

//TODO: Use share notification to trigger task re-evaluation based on io events
process :: !Int [(!Int,!SelectResult)] !*IWorld -> !*IWorld
process i chList iworld=:{ioTasks={done,todo=[]}} = trace_n "End of processing" iworld
process i chList iworld=:{ioTasks={done,todo=[ListenerInstance lopts listener:todo]},ioStates,world}
    | not (trace_tn ("Process listener instance " <+++ i)) = undef
    # taskId=:(TaskId instanceNo _) = lopts.ListenerInstanceOpts.taskId
    = case 'DM'.get lopts.ListenerInstanceOpts.taskId ioStates of
        //Active listener:
        Just (IOActive conStates)
            # (mbSelect,chList) = checkSelect i chList
            | mbSelect =:(Just _)
     	        # (tReport, mbNewConn, listener, world)   = receive_MT (Just 0) listener world
                | tReport == TR_Success
                    # (ip,{rChannel,sChannel}) = fromJust mbNewConn
                    # (ConnectionTask handlers sds) = lopts.ListenerInstanceOpts.connectionTask
                    # (mbr,iworld) = 'SDS'.read sds EmptyContext {iworld & ioTasks={done=done,todo=todo},world=world}
                    | mbr =:(Error _)
                        # iworld=:{ioTasks={done,todo},world} = if (instanceNo > 0) (queueRefresh [(taskId,"IO Exception for instance "<+++instanceNo)] iworld) iworld
                        # ioStates = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOException (snd (fromError mbr))) ioStates
 	                    # world = closeRChannel listener world
                        = process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}
                    # (mbConState,mbw,out,close,iworld) = handlers.ConnectionHandlersIWorld.onConnect (toString ip) (directResult (fromOk mbr)) iworld
                    # iworld = if (instanceNo > 0) (queueRefresh [(taskId,"New TCP connection for instance "<+++instanceNo)] iworld) iworld
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
                //We did not properly accept a connection
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
    | not (trace_tn (("Process connectionInstance no " <+++ i) +++ (", taskId: " <+++ opts.ConnectionInstanceOpts.taskId) +++ (", remoteHost " +++ toString opts.ConnectionInstanceOpts.remoteHost))) = undef
    # iworld = processIOTask
        i chList opts.ConnectionInstanceOpts.taskId opts.ConnectionInstanceOpts.connectionId
        opts.ConnectionInstanceOpts.removeOnClose sds tcpConnectionIOOps
        (\_ -> handlers.ConnectionHandlersIWorld.onDisconnect) handlers.ConnectionHandlersIWorld.onData
        handlers.ConnectionHandlersIWorld.onShareChange handlers.ConnectionHandlersIWorld.onTick (ConnectionInstance opts) duplexChannel iworld
    | not (trace_tn ("Done process connectionInstance no " <+++ i)) = undef
    = process (i+1) chList iworld
where
    (ConnectionTask handlers sds) = opts.ConnectionInstanceOpts.connectionTask

process i chList iworld=:{ioTasks={done,todo=[t:todo]}}
    = trace_n "Other task" (process (i+1) chList {iworld & ioTasks={done=[t:done],todo=todo}})

// Definitions of IO tasks (tcp connections)

:: IOTaskOperations ioChannels readData closeInfo =
    { readData  :: !(Int [(Int, SelectResult)] *(!ioChannels, !*IWorld) -> *(!IOData readData closeInfo, !ioChannels, !*IWorld))
    , writeData :: !(String                    *(!ioChannels, !*IWorld) -> *(!ioChannels, !*IWorld))
    , closeIO   :: !(                          *(!ioChannels, !*IWorld) -> *IWorld)
    }
:: IOData data closeInfo = IODClosed closeInfo
                         | IODNoData
                         | IODData !data & TC data

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

processIOTask :: !Int
                 ![(Int, SelectResult)]
                 !TaskId
                 !Int
                 !Bool
                 !(SDSLens () Dynamic Dynamic)
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
    | not (trace_tn ("Processing IO task " <+++ i)) = undef
    = case 'DM'.get taskId ioStates of
        Just (IOActive taskStates)
            | not (trace_tn ("Active " <+++ i)) = undef
            # (TaskId instanceNo _) = taskId
            // get task state
            # mbTaskState = 'DM'.get connectionId taskStates
            | isNothing mbTaskState
                # iworld   = if (instanceNo > 0) (queueRefresh [(taskId, "Exception for " <+++ instanceNo)] iworld) iworld
                # ioStates = 'DM'.put taskId (IOException "Missing IO task state") ioStates
                = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
            # taskState = fst (fromJust mbTaskState)

            // *** onTick handler ***
            // read sds
            | not (trace_tn ("OnTick " <+++ i)) = undef
            # (mbr,iworld=:{ioTasks={done,todo},world}) = 'SDS'.read sds EmptyContext iworld
            | mbr =: (Error _) = sdsException mbr instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
            # r = directResult (fromOk mbr)
            // call handler
            # (mbTaskState, mbw, out, close, iworld) = onTickHandler taskState r iworld
            # (mbSdsErr, iworld) = writeShareIfNeeded sds mbw iworld
            // write data
            # (ioChannels, iworld) = seq [ioOps.writeData o \\ o <- out] (ioChannels, iworld)
            | mbTaskState =: (Error _) = taskStateException mbTaskState instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
            | isError mbSdsErr         = sdsException       mbSdsErr    instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
            | close = closeConnection taskStates ioStates ioOps.closeIO (ioChannels, iworld)

            // *** onShareChange handler ***
            // read sds
            | not (trace_tn ("onShareChange " <+++ i)) = undef
            # (mbr,iworld=:{ioTasks={done,todo},world}) = 'SDS'.read sds EmptyContext iworld
            | mbr =: (Error _) = sdsException mbr instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
            # r = directResult (fromOk mbr)
            // call handler
            # (mbTaskState, mbw, out, close, iworld) = onShareChangeHandler taskState r iworld
            # (mbSdsErr, iworld) = writeShareIfNeeded sds mbw iworld
            | mbTaskState =: (Error _) = taskStateException mbTaskState instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
            | isError mbSdsErr         = sdsException       mbSdsErr    instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
            # ioStates = 'DM'.put taskId (IOActive ('DM'.put connectionId (fromOk mbTaskState, close) taskStates)) ioStates
            // write data
            # (ioChannels, iworld) = seq [ioOps.writeData o \\ o <- out] (ioChannels, iworld)
            | mbTaskState =: (Error _) = taskStateException mbTaskState instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
            | isError mbSdsErr         = sdsException       mbSdsErr    instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
            | close = closeConnection taskStates ioStates ioOps.closeIO (ioChannels, iworld)

            // ** onData handler ***
            // read sds
            | not (trace_tn ("OnData " <+++ i)) = undef
            # (mbr,iworld=:{ioTasks={done,todo},world}) = 'SDS'.read sds EmptyContext iworld
            | mbr =: (Error _) = sdsException mbr instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
            # r = directResult (fromOk mbr)
            # taskState = fromOk mbTaskState
            // try to read data
            # (mbData, ioChannels, iworld) = ioOps.readData i chList (ioChannels, iworld)
            | not (trace_tn ("Got mbData " <+++ i)) = undef
            = case mbData of
                IODClosed closeInfo
                    | not (trace_tn ("Closed " <+++ i)) = undef
                    # (mbTaskState, mbw, iworld) = onCloseHandler closeInfo taskState r iworld
                    # ioStates = case mbTaskState of
                        Ok state
                            = 'DM'.put taskId (IOActive ('DM'.put connectionId (state, True) taskStates)) ioStates
                        Error e
                            = 'DM'.put taskId (IOException e) ioStates
                    # (mbSdsErr, iworld) = writeShareIfNeeded sds mbw iworld
                    | isError mbSdsErr
                        # iworld = if (instanceNo > 0) (queueRefresh [(taskId, "Exception for " <+++ instanceNo)] iworld) iworld
                        # ioStates = 'DM'.put taskId (IOException (snd (fromError mbSdsErr))) ioStates
                        = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
                    # iworld = if (instanceNo > 0) (queueRefresh [(taskId, "IO closed for " <+++ instanceNo)] iworld) iworld
                    = ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
                IODNoData
                    // persist connection
                    | not (trace_tn ("NoData " <+++ i)) = undef
                    # {done, todo} = iworld.ioTasks
                    = trace_n "Returning NoData" {iworld & ioStates = ioStates, ioTasks = {done = [mkIOTaskInstance ioChannels : done], todo = todo}}
                IODData data
                    | not (trace_tn ("Data " <+++ i)) = undef
                    # (mbTaskState, mbw, out, close, iworld) = onDataHandler data taskState r iworld
                    # iworld = if (instanceNo > 0) (queueRefresh [(taskId, "New data for "<+++ instanceNo)] iworld) iworld
                    # (mbSdsErr, iworld) = writeShareIfNeeded sds mbw iworld
                    // write data
                    # (ioChannels, iworld) = seq [ioOps.writeData o \\ o <- out] (ioChannels, iworld)
                    | mbTaskState =: (Error _) = taskStateException mbTaskState instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
                    | isError mbSdsErr         = sdsException       mbSdsErr    instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
                    # ioStates = 'DM'.put taskId (IOActive ('DM'.put connectionId (fromOk mbTaskState, close) taskStates)) ioStates
                    | close = closeConnection taskStates ioStates ioOps.closeIO (ioChannels, iworld)
                    | otherwise
                        // persist connection
                        # {done, todo} = iworld.ioTasks
                        = {iworld & ioStates = ioStates, ioTasks = {done = [mkIOTaskInstance ioChannels : done], todo = todo}}
        Just (IODestroyed taskStates)
            | not (trace_tn ("Destroyed " <+++ i)) = undef
            # iworld = ioOps.closeIO (ioChannels, iworld)
            //Remove the state for this connection
            # taskStates = 'DM'.del connectionId taskStates
            //If this is the last connection for this task, we can clean up.
            # ioStates = if ('DM'.mapSize taskStates == 0) ('DM'.del taskId ioStates) ioStates
            = {iworld & ioStates = ioStates}
        _ = ioOps.closeIO (ioChannels, iworld)
where
    taskStateException :: (MaybeError String Dynamic)
                          InstanceNo
                          (Map TaskId IOState)
                          (*(!.ioChannels, !*IWorld) -> *IWorld)
                          *(!.ioChannels, !*IWorld)
                       -> *IWorld
    taskStateException mbTaskState instanceNo ioStates closeIO (ioChannels, iworld)
        # iworld = if (instanceNo > 0) (queueRefresh [(taskId, "Exception for " <+++ instanceNo)] iworld) iworld
        # ioStates = 'DM'.put taskId (IOException (fromError mbTaskState)) ioStates
        = closeIO (ioChannels, {iworld & ioStates = ioStates})

    sdsException :: (MaybeError TaskException a)
                    InstanceNo
                    (Map TaskId IOState)
                    (*(!.ioChannels, !*IWorld) -> *IWorld)
                    *(!.ioChannels, !*IWorld)
                    -> *IWorld
    sdsException mbSdsErr instanceNo ioStates closeIO (ioChannels, iworld)
        # iworld = if (instanceNo > 0) (queueRefresh [(taskId, "Exception for " <+++ instanceNo)] iworld) iworld
        # ioStates = 'DM'.put taskId (IOException (snd (fromError mbSdsErr))) ioStates
        = closeIO (ioChannels, {iworld & ioStates = ioStates})

    closeConnection :: (Map ConnectionId (Dynamic,Bool))
                       (Map TaskId IOState)
                       (*(!.ioChannels, !*IWorld) -> *IWorld)
                       *(!.ioChannels, !*IWorld)
                    -> *IWorld
    closeConnection taskStates ioStates closeIO (ioChannels, iworld)
        //Remove the connection state if configured in the connection listener options
        # taskStates = if removeOnClose
            ('DM'.del connectionId taskStates)
            taskStates
        # ioStates = 'DM'.put taskId (IOActive taskStates) ioStates
        = closeIO (ioChannels, iworld)//{iworld & ioStates = ioStates})

writeShareIfNeeded :: !(sds () r w) !(Maybe w) !*IWorld -> (!MaybeError TaskException (), !*IWorld) | TC r & TC w & Writeable sds
writeShareIfNeeded sds Nothing iworld  = (Ok (), iworld)
writeShareIfNeeded sds (Just w) iworld = case 'SDS'.write w sds EmptyContext iworld of 
    (Error e, iworld) = (Error e, iworld)
    (Ok Done, iworld) = (Ok (), iworld)

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

addConnection :: !TaskId !String !Int !ConnectionTask !*IWorld -> (!MaybeError TaskException (ConnectionId, Dynamic),!*IWorld)
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

    mkIOTaskInstance :: ConnectionId !IPAddress !*TCP_DuplexChannel -> *IOTaskInstance
    mkIOTaskInstance connectionId ip channel
        # opts = {ConnectionInstanceOpts|taskId = taskId, connectionId = connectionId, remoteHost = ip, connectionTask = connectionTask, removeOnClose = False}
        = ConnectionInstance opts channel

addIOTask :: !TaskId
             !(sds () Dynamic Dynamic)
             !(*IWorld -> (!MaybeErrorString (!initInfo, !.ioChannels), !*IWorld))
             !(IOTaskOperations .ioChannels readData closeInfo)
             !(initInfo Dynamic *IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld))
             !(ConnectionId initInfo .ioChannels -> *IOTaskInstance)
             !*IWorld
          -> (!MaybeError TaskException (ConnectionId, Dynamic), !*IWorld) | Readable sds
addIOTask taskId sds init ioOps onInitHandler mkIOTaskInstance iworld
    # (mbInitRes, iworld) = init iworld
    = case mbInitRes of
        Error e = (Error (exception e), iworld)
        Ok (initInfo, ioChannels)
            // Read share
            # (mbr, iworld) = 'SDS'.read sds EmptyContext iworld
            | isError mbr = (liftError mbr, iworld)
            // Evaluate onInit handler
            # (mbl, mbw, out, close, iworld) = onInitHandler initInfo (directResult (fromOk mbr)) iworld
            // Check initialization of local state 
            = case mbl of   
                Error e = (Error (exception e), iworld)
                Ok l
                    // write output
            		# (ioChannels, iworld) = seq [ioOps.writeData o \\ o <- out] (ioChannels, iworld)
            		//Close or add to queue
            		| close
                		# iworld = ioOps.closeIO (ioChannels, iworld)
                		= (Ok (0, dynamic l), iworld)
					| otherwise
            			# ioStates = iworld.ioStates

                        # (connectionId, connectionMap) = case 'DM'.get taskId ioStates of
                            Nothing                             = (0, IOActive ('DM'.fromList [(0,(l, False))]))
                            (Just (IOActive connectionMap))    = let key = inc ('DL'.maximum ('DM'.keys connectionMap)) in (key, IOActive ('DM'.put key (l, False) connectionMap))
            			# ioStates = 'DM'.put taskId connectionMap ioStates
            			# {done, todo} = iworld.ioTasks
                        = (Ok (connectionId, l), {iworld & ioStates = ioStates, ioTasks = {done = [mkIOTaskInstance connectionId initInfo ioChannels : done], todo = todo}})

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
