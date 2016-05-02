implementation module iTasks._Framework.TaskServer

import StdFile, StdBool, StdInt, StdClass, StdList, StdMisc, StdArray, StdTuple, StdOrdList
import Data.Maybe, Data.Functor, Data.Error, System.Time, Text
from Data.Map import :: Map (..)
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified iTasks._Framework.SDS as SDS
import TCPChannelClass, TCPChannels, TCPEvent, TCPStringChannels, TCPDef, tcp

import iTasks._Framework.IWorld
import iTasks._Framework.Task
import iTasks._Framework.TaskEval

//Helper type that holds the mainloop instances during a select call
//in these mainloop instances the unique listeners and read channels
//have been temporarily removed.
:: *IOTaskInstanceDuringSelect
    = ListenerInstanceDS !ListenerInstanceOpts
    | ConnectionInstanceDS !ConnectionInstanceOpts !*TCP_SChannel
    | BackgroundInstanceDS !BackgroundTask

serve :: !Int !ConnectionTask ![BackgroundTask] (*IWorld -> (!Maybe Timeout,!*IWorld)) *IWorld -> *IWorld
serve port ct bt determineTimeout iworld
    = loop determineTimeout (init port ct bt iworld)

init :: !Int !ConnectionTask ![BackgroundTask] !*IWorld -> *IWorld
init port ct bt iworld=:{IWorld|ioTasks,world}
    # (success, mbListener, world) = openTCP_Listener port world
    | not success = abort ("Error: port "+++ toString port +++ " already in use.\n")
    # opts = {ListenerInstanceOpts|taskId=TaskId 0 0, nextConnectionId=0, port=port, connectionTask=ct, removeOnClose = True}
    # ioStates = 'DM'.fromList [(TaskId 0 0, IOActive 'DM'.newMap)]
    = {iworld & ioTasks = {done=[],todo=[ListenerInstance opts (fromJust mbListener):map BackgroundInstance bt]}, ioStates = ioStates,  world = world}

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
    | shutdown  = halt iworld
    | otherwise = loop determineTimeout iworld

select :: (Maybe Timeout) *[IOTaskInstance] *World -> (!*[IOTaskInstance],![(Int,SelectResult)],!*World)
select mbTimeout mlInstances world
    # (listeners,rChannels,mlInstances)
        = toSelectSet mlInstances
    # (chList,(TCP_Pair (TCP_Listeners listeners) (TCP_RChannels rChannels)),_,world)	
        = selectChannel_MT mbTimeout (TCP_Pair (TCP_Listeners listeners) (TCP_RChannels rChannels)) TCP_Void world
    # (mlInstances, chList)
        = fromSelectSet listeners rChannels mlInstances chList
    = (mlInstances, chList, world)

toSelectSet :: !*[IOTaskInstance] -> *(!*[*TCP_Listener],!*[*TCP_RChannel],!*[*IOTaskInstanceDuringSelect])
toSelectSet [] = ([],[],[])
toSelectSet [i:is]
    # (ls,rs,is) = toSelectSet is
    = case i of
        ListenerInstance opts l = ([l:ls],rs,[ListenerInstanceDS opts:is])
        ConnectionInstance opts {rChannel,sChannel} = (ls,[rChannel:rs],[ConnectionInstanceDS opts sChannel:is])
        BackgroundInstance bt = (ls,rs,[BackgroundInstanceDS bt:is])

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
    //Background tasks
    fromSelectSet` i numListeners numSeenListeners numSeenReceivers ls rs ch [BackgroundInstanceDS bt:is]
        # (is,ch) = fromSelectSet` (i+1) numListeners numSeenListeners numSeenReceivers ls rs ch is
        = ([BackgroundInstance bt:is],ch)

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
                        # iworld=:{ioTasks={done,todo},world} = queueRefresh [(instanceNo,"IO Exception for instance "<+++instanceNo)] iworld
                        # ioStates = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOException (snd (fromError mbr))) ioStates
 	                    # world = closeRChannel listener world
                        = process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}
                    # (mbConState,mbw,out,close,iworld) = handlers.ConnectionHandlersIWorld.onConnect (toString ip) (fromOk mbr) iworld
                    # iworld = queueRefresh [(instanceNo,"New TCP connection for instance "<+++instanceNo)] iworld
                    # iworld=:{ioTasks={done,todo},world}  = writeShareIfNeeded sds mbw iworld
                    | mbConState =:(Error _)
                        # ioStates = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOException (fromError mbConState)) ioStates
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
           
process i chList iworld=:{ioTasks={done,todo=[ConnectionInstance opts {rChannel,sChannel}:todo]},ioStates,world}
    # (TaskId instanceNo _) = opts.ConnectionInstanceOpts.taskId
    = case 'DM'.get opts.ConnectionInstanceOpts.taskId ioStates of
        Just (IOActive conStates)
            # (ConnectionTask handlers sds) = opts.ConnectionInstanceOpts.connectionTask
            # (mbSelect,chList) = checkSelect i chList

            # mbConState = 'DM'.get opts.ConnectionInstanceOpts.connectionId conStates
            | mbConState =: Nothing
                //Set exception, close connection and continue 
                # ioStates = 'DM'.put opts.ConnectionInstanceOpts.taskId (IOException "Missing connection state") ioStates
 	            # world = closeRChannel rChannel world
                # world = closeChannel sChannel world
                = process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}
            # conState = fst (fromJust mbConState)
            //Read sds
            # (mbr,iworld=:{ioTasks={done,todo},world}) = 'SDS'.read sds {iworld & ioTasks={done=done,todo=todo},world=world}
            | mbr =:(Error _)
                # ioStates = 'DM'.put opts.ConnectionInstanceOpts.taskId (IOException (snd (fromError mbr))) ioStates
 	            # world = closeRChannel rChannel world
                # world = closeChannel sChannel world
                = process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}
            //Check if disconnected
            | mbSelect =:(Just SR_Disconnected) || mbSelect=:(Just SR_EOM)
                //Call disconnect function
                # (conState,mbw,iworld) = handlers.ConnectionHandlersIWorld.onDisconnect conState (fromOk mbr) {iworld & ioTasks={done=done,todo=todo},ioStates=ioStates,world=world}
                # iworld = queueRefresh [(instanceNo,"TCP connection disconnected for "<+++instanceNo)] iworld
                # iworld=:{world,ioStates} = writeShareIfNeeded sds mbw iworld
                # ioStates = case conState of
                    Ok state
                        = 'DM'.put opts.ConnectionInstanceOpts.taskId (IOActive ('DM'.put opts.ConnectionInstanceOpts.connectionId (state,True) conStates)) ioStates
                    Error e
                        = 'DM'.put opts.ConnectionInstanceOpts.taskId (IOException e) ioStates
 	            # world = closeRChannel rChannel world
                # world = closeChannel sChannel world
                = process (i+1) chList {iworld & ioStates = ioStates, world=world}
            //Read channel data
            # (data,rChannel,world) = case mbSelect of
                Just SR_Available
		            # (data,rChannel,world) = receive rChannel world
                    = (Just (toString data),rChannel,world)
                _
                    = (Nothing,rChannel,world)
            //Call whileConnected function
            # (mbConState,mbw,out,close,iworld)
                = handlers.ConnectionHandlersIWorld.whileConnected data conState (fromOk mbr) {iworld & ioTasks={done=done,todo=todo},ioStates=ioStates,world=world} 
            //Queue refresh when there was new data or when the connection was closed
            # iworld = if (isNothing data) iworld (queueRefresh [(instanceNo, "New TCP data for "<+++instanceNo)] iworld)
            # iworld = if close (queueRefresh [(instanceNo, "TCP connection closed for "<+++instanceNo)] iworld) iworld
            //Write share
            # iworld=:{ioTasks={todo,done},ioStates,world} = writeShareIfNeeded sds mbw iworld
            | mbConState =:(Error _)
                # ioStates = 'DM'.put opts.ConnectionInstanceOpts.taskId (IOException (fromError mbConState)) ioStates
 		        # world = closeRChannel rChannel world
                # world = closeChannel sChannel world
                = process (i+1) chList {iworld & ioTasks={done=done,todo=todo},ioStates=ioStates,world=world}
            # conStates = 'DM'.put opts.ConnectionInstanceOpts.connectionId (fromOk mbConState,close) conStates
            //Send data if produced
            # (sChannel,world) = case out of
                []          = (sChannel,world)
                data        = foldl (\(s,w) d -> send (toByteSeq d) s w) (sChannel,world) data
            | close
                //Remove the connection state if configured in the connection listener options
                # conStates = if opts.ConnectionInstanceOpts.removeOnClose
                    ('DM'.del opts.ConnectionInstanceOpts.connectionId conStates)
                    conStates
                # ioStates  = 'DM'.put opts.ConnectionInstanceOpts.taskId (IOActive conStates) ioStates
 		        # world = closeRChannel rChannel world
                # world = closeChannel sChannel world
                = process (i+1) chList {iworld & ioTasks={done=done,todo=todo},ioStates=ioStates,world=world}
            | otherwise
                //Perssist connection
                # ioStates  = 'DM'.put opts.ConnectionInstanceOpts.taskId (IOActive conStates) ioStates
                = process (i+1) chList {iworld & ioTasks={done=[ConnectionInstance opts {rChannel=rChannel,sChannel=sChannel}:done],todo=todo},ioStates=ioStates,world=world}
        Just (IODestroyed conStates)
 	        # world = closeRChannel rChannel world
            # world = closeChannel sChannel world
            //Remove the state for this connection
            # conStates = 'DM'.del opts.ConnectionInstanceOpts.connectionId conStates
            //If this is the last connection for this task, we can clean up.
            # ioStates = if ('DM'.mapSize conStates == 0) ('DM'.del opts.ConnectionInstanceOpts.taskId ioStates) ioStates
            = process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}
        _
            //No state, just close
 	        # world = closeRChannel rChannel world
            # world = closeChannel sChannel world
            = process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}
           
process i chList iworld=:{ioTasks={done,todo=[BackgroundInstance bt=:(BackgroundTask eval):todo]}}
    # iworld=:{ioTasks={done,todo}} = eval {iworld & ioTasks = {done=done,todo=todo}}
    = process (i+1) chList {iworld & ioTasks={done=[BackgroundInstance bt:done],todo=todo}}
process i chList iworld=:{ioTasks={done,todo=[t:todo]}}
    = process (i+1) chList {iworld & ioTasks={done=[t:done],todo=todo}}

writeShareIfNeeded sds Nothing iworld = iworld
writeShareIfNeeded sds (Just w) iworld 
    # (_,iworld) = 'SDS'.write w sds iworld //TODO: Deal with exceptions at this level
    = iworld

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
addConnection taskId=:(TaskId instanceNo _) host port connectionTask iworld=:{ioTasks={done,todo},ioStates,world}
    # (mbIP,world) = lookupIPAddress host world
    | mbIP =: Nothing
        = (Error (exception ("Failed to connect to host "+++ host)), {iworld & ioTasks = {done=done,todo=todo}, world = world})
    # (tReport,mbConn,world) = connectTCP_MT Nothing (fromJust mbIP,port) world
    = case mbConn of
        Nothing
            = (Error (exception ("Failed to connect to host "+++ host)), {iworld & ioTasks = {done=done,todo=todo}, world = world})
        Just {DuplexChannel|rChannel,sChannel}
            # ip                                = fromJust mbIP
            # (ConnectionTask handlers sds)     = connectionTask
            // Read share
            # (mbr,iworld) = 'SDS'.read sds {iworld & ioTasks = {done=done,todo=todo}, ioStates = ioStates, world = world}
            | mbr =: (Error _)
                = (liftError mbr, iworld)
            // Evaluate onConnect handler
            # (mbl,mbw,out,close,iworld=:{IWorld|ioTasks={done,todo},ioStates,world}) = handlers.ConnectionHandlersIWorld.onConnect (toString ip) (fromOk mbr) iworld
            // Write possible output
            # (sChannel,world) = case out of
                []          = (sChannel,world)
                data        = foldl (\(s,w) d -> send (toByteSeq d) s w) (sChannel,world) data
            //Close connection, or add to queue
            | close
         	    # world = closeRChannel rChannel world
                # world = closeChannel sChannel world
                = (Ok (), {iworld & ioTasks = {done=done,todo=todo}, ioStates = ioStates, world = world})
            | otherwise
                # opts = {ConnectionInstanceOpts|taskId = taskId, connectionId = 0, remoteHost = ip, connectionTask = connectionTask, removeOnClose = False}
                # todo = todo ++ [ConnectionInstance opts {rChannel=rChannel,sChannel=sChannel}]
                # ioStates = case mbl of
                    Ok l        = 'DM'.put taskId (IOActive ('DM'.fromList [(0,(l,False))])) ioStates
                    Error e     = 'DM'.put taskId (IOException e) ioStates
                = (Ok (),{iworld & ioTasks = {done=done,todo=todo}, ioStates = ioStates, world = world})

checkSelect :: !Int ![(!Int,!SelectResult)] -> (!Maybe SelectResult,![(!Int,!SelectResult)])
checkSelect i chList =:[(who,what):ws] | (i == who) = (Just what,ws)
checkSelect i chList = (Nothing,chList)

halt :: !*IWorld -> *IWorld
halt iworld=:{ioTasks={todo=[],done}} = iworld
halt iworld=:{ioTasks={todo=[ListenerInstance _ listener:todo],done},world}
 	# world = closeRChannel listener world
    = halt {iworld & ioTasks = {todo=todo,done=done}}
halt iworld=:{ioTasks={todo=[ConnectionInstance _ {rChannel,sChannel}:todo],done},world}
 	# world = closeRChannel rChannel world
    # world = closeChannel sChannel world
    = halt {iworld & ioTasks = {todo=todo,done=done}}
halt iworld=:{ioTasks={todo=[BackgroundInstance _ :todo],done},world}
    = halt {iworld & ioTasks= {todo=todo,done=done}}

