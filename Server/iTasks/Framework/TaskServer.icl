implementation module iTasks.Framework.TaskServer

import StdFile, StdBool, StdInt, StdClass, StdList, StdMisc, StdArray, StdTuple, StdOrdList
import Data.Maybe, Data.Functor, System.Time, Text
from Data.Map import :: Map, :: Size
import qualified Data.List as DL
import qualified Data.Map as DM
import TCPChannelClass, TCPChannels, TCPEvent, TCPStringChannels, TCPDef, tcp

import iTasks.Framework.IWorld
import iTasks.Framework.Task

//Helper type that holds the mainloop instances during a select call
//in these mainloop instances the unique listeners and read channels
//have been temporarily removed.
:: *IOTaskInstanceDuringSelect
    = ListenerInstanceDS !Int !ConnectionTask
    | ConnectionInstanceDS !IPAddress !*TCP_SChannel !ConnectionTask !Dynamic
    | BackgroundInstanceDS !BackgroundTask

serve :: !Int !ConnectionTask ![BackgroundTask] (*IWorld -> (!Maybe Timeout,!*IWorld)) *IWorld -> *IWorld
serve port ct bt determineTimeout iworld
    = loop determineTimeout (init port ct bt iworld)

init :: !Int !ConnectionTask ![BackgroundTask] !*IWorld -> *IWorld
init port ct bt iworld=:{IWorld|io,world}
    # (success, mbListener, world) = openTCP_Listener port world
    | not success = abort ("Error: port "+++ toString port +++ " already in use.\n")
    = {iworld & io = {done=[],todo=[ListenerInstance port (fromJust mbListener) ct:map BackgroundInstance bt]}, world = world}

loop :: !(*IWorld -> (!Maybe Timeout,!*IWorld)) !*IWorld -> *IWorld
loop determineTimeout iworld
    # (mbTimeout,iworld=:{IWorld|io={todo},world}) = determineTimeout iworld
    //Check which mainloop tasks have data available
    # (todo,chList,world) = select mbTimeout todo world
    //Process the select result
    # iworld =:{shutdown,io={done}} = process 0 chList {iworld & io = {done=[],todo=todo}, world = world}
    //Move everything from the done list  back to the todo list
    # iworld = {iworld & io={todo = reverse done,done=[]}}
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
        ListenerInstance port l nt = ([l:ls],rs,[ListenerInstanceDS port nt:is])
        ConnectionInstance ip {rChannel,sChannel} nt state = (ls,[rChannel:rs],[ConnectionInstanceDS ip sChannel nt state:is])
        BackgroundInstance bt = (ls,rs,[BackgroundInstanceDS bt:is])

/* Restore the list of main loop instances.
    In the same pass also update the indices in the select result to match the
    correct indices of the main loop instance list.
*/
fromSelectSet :: !*[*TCP_Listener] !*[*TCP_RChannel] !*[*IOTaskInstanceDuringSelect] ![(!Int,!SelectResult)] -> *(![*IOTaskInstance],![(!Int,!SelectResult)])
fromSelectSet ls rs is chList = fromSS 0 0 ls rs is (sortBy (\(x,_) (y,_) -> (x < y)) chList)
where
    fromSS offset i ls rs [] [] = ([],[])
    fromSS offset i [l:ls] rs [ListenerInstanceDS port nt:is] []
        # (is,_) = fromSS offset (i+1) ls rs is []
        = ([ListenerInstance port l nt:is],[])
    fromSS offset i [l:ls] rs [ListenerInstanceDS port nt:is] chList=:[(who,what):ws]
        | who + offset == i
            # (is,ws) = fromSS offset (i+1) ls rs is ws
            = ([ListenerInstance port l nt:is],[(who + offset,what):ws])
        | otherwise
            # (is,chList) = fromSS offset (i+1) ls rs is chList
            = ([ListenerInstance port l nt:is],chList)
    fromSS offset i ls [rChannel:rs] [ConnectionInstanceDS ip sChannel nt state:is] []
        # (is,_) = fromSS offset (i+1) ls rs is []
        = ([ConnectionInstance ip {rChannel=rChannel,sChannel=sChannel} nt state:is],[])
    fromSS offset i ls [rChannel:rs] [ConnectionInstanceDS ip sChannel nt state:is] chList=:[(who,what):ws]
        | who + offset == i
            # (is,ws) = fromSS offset (i+1) ls rs is ws
            = ([ConnectionInstance ip {rChannel=rChannel,sChannel=sChannel} nt state:is],[(who + offset,what):ws])
        | otherwise
            # (is,chList) = fromSS offset (i+1) ls rs is chList
            = ([ConnectionInstance ip {rChannel=rChannel,sChannel=sChannel} nt state:is],chList)
    fromSS offset i ls rs [BackgroundInstanceDS bt:is] chList
        # (is,chList) = fromSS (offset+1) (i+1) ls rs is chList
        = ([BackgroundInstance bt:is],chList)

process :: !Int [(!Int,!SelectResult)] !*IWorld -> *IWorld
process i chList iworld=:{io={done,todo=[]}} = iworld
process i chList iworld=:{io={done,todo=[ListenerInstance port listener nt=:(ConnectionTask init _ _):todo]},world}
    # (mbSelect,chList) = checkSelect i chList
    | mbSelect =:(Just _)
 	    # (tReport, mbNewConn, listener, world)   = receive_MT (Just 0) listener world
        | tReport == TR_Success
            # (ip,{rChannel,sChannel}) = fromJust mbNewConn
            # (out,close,state,iworld=:{io={todo,done},world}) = init (toString ip) {iworld & io={done=done,todo=todo},world=world}
            # (sChannel,world) = case out of
                []          = (sChannel,world)
                data        = foldl (\(s,w) d -> send (toByteSeq d) s w) (sChannel,world) data
            | close
 		        # world = closeRChannel rChannel world
                # world = closeChannel sChannel world
                = process (i+1) chList {iworld & io={done=[ListenerInstance port listener nt:done],todo=todo}, world=world}
            # todo          = todo ++ [ConnectionInstance ip {rChannel=rChannel,sChannel=sChannel} nt state]
            = process (i+1) chList {iworld & io={done=[ListenerInstance port listener nt:done],todo=todo}, world=world}
        = process (i+1) chList {iworld & io={done=[ListenerInstance port listener nt:done],todo=todo}, world=world}
    = process (i+1) chList {iworld & io={done=[ListenerInstance port listener nt:done],todo=todo}, world=world}
process i chList iworld=:{io={done,todo=[ConnectionInstance ip {rChannel,sChannel} nt=:(ConnectionTask _ eval close) state:todo]},world}
    # (mbSelect,chList) = checkSelect i chList
    //Check if disconnected
    | mbSelect =:(Just SR_Disconnected) || mbSelect=:(Just SR_EOM)
        //Call disconnect function
        # (state,iworld=:{world}) = close state {iworld & io={done=done,todo=todo},world=world}
 	    # world = closeRChannel rChannel world
        # world = closeChannel sChannel world
        = process (i+1) chList {iworld & world=world}
    //Read data
    # (data,rChannel,world) = case mbSelect of
        Just SR_Available
		    # (data,rChannel,world) = receive rChannel world
            = (Just (toString data),rChannel,world)
        _
            = (Nothing,rChannel,world)
    //Eval main loop task
    # (out,close,state,iworld=:{io={todo,done},world})
        = eval data state {iworld & io={done=done,todo=todo},world=world}
    //Send data if produced
    # (sChannel,world) = case out of
        []          = (sChannel,world)
        data        = foldl (\(s,w) d -> send (toByteSeq d) s w) (sChannel,world) data
    | close
 		# world = closeRChannel rChannel world
        # world = closeChannel sChannel world
        = process (i+1) chList {iworld & io={done=done,todo=todo},world=world}
    = process (i+1) chList {iworld & io={done=[ConnectionInstance ip {rChannel=rChannel,sChannel=sChannel} nt state:done],todo=todo},world=world}
process i chList iworld=:{io={done,todo=[BackgroundInstance bt=:(BackgroundTask eval):todo]}}
    # iworld=:{io={done,todo}} = eval {iworld & io = {done=done,todo=todo}}
    = process (i+1) chList {iworld & io={done=[BackgroundInstance bt:done],todo=todo}}

checkSelect :: !Int ![(!Int,!SelectResult)] -> (!Maybe SelectResult,![(!Int,!SelectResult)])
checkSelect i chList =:[(who,what):ws] | (i == who) = (Just what,ws)
checkSelect i chList = (Nothing,chList)

halt :: !*IWorld -> *IWorld
halt iworld=:{io={todo=[],done}} = iworld
halt iworld=:{io={todo=[ListenerInstance _ listener _:todo],done},world}
 	# world = closeRChannel listener world
    = halt {iworld & io = {todo=todo,done=done}}
halt iworld=:{io={todo=[ConnectionInstance _ {rChannel,sChannel} _ _:todo],done},world}
 	# world = closeRChannel rChannel world
    # world = closeChannel sChannel world
    = halt {iworld & io = {todo=todo,done=done}}
halt iworld=:{io={todo=[BackgroundInstance _ :todo],done},world}
    = halt {iworld & io= {todo=todo,done=done}}

