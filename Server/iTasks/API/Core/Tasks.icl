implementation module iTasks.API.Core.Tasks

import StdList, StdBool, StdInt, StdTuple, StdMisc, StdDebug
import System.Time, Data.Error, System.OSError, Data.Tuple, Text, Text.JSON
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil, iTasks._Framework.TaskServer
import iTasks._Framework.Generic, iTasks._Framework.Task, iTasks._Framework.TaskState
import iTasks._Framework.TaskEval, iTasks._Framework.TaskStore, iTasks.UI.Definition, iTasks._Framework.IWorld
import iTasks.UI.Layout, iTasks.UI.Editor, iTasks.UI.Prompt
import iTasks.API.Core.SDSs, iTasks.API.Common.SDSCombinators

from iTasks._Framework.SDS as SDS import qualified read, readRegister, write
from StdFunc					import o, id
from Data.Map as DM				import qualified newMap, get, put, del, toList, fromList
from TCPChannels                import lookupIPAddress, class ChannelEnv, instance ChannelEnv World, connectTCP_MT
from TCPChannels                import toByteSeq, send, class Send, instance Send TCP_SChannel_
from TCPChannels                import :: TimeoutReport, :: Timeout, :: Port
from TCPChannels                import instance toString IPAddress
from TCPChannels                import class closeRChannel(..), instance closeRChannel TCP_RChannel_, openTCP_Listener
from TCPChannelClass            import :: DuplexChannel(..), closeChannel

treturn :: !a -> (Task a) | iTask a
treturn a  = mkInstantTask (\taskId iworld-> (Ok a, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (Error (dynamic e,toString e), iworld))

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime}}
		# (val,iworld) = 'SDS'.read shared iworld
		= case val of
			Ok val		= (Ok val,iworld)
			Error e		= (Error e, iworld)
	
set :: !a !(ReadWriteShared r a)  -> Task a | iTask a
set val shared = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime,taskInstance}}
		# (res,iworld)	='SDS'.write val shared iworld
		= case res of
			Ok _	= (Ok val, iworld)
			Error e	= (Error e, iworld)

upd :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
upd fun shared = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime,taskInstance}}
		# (er, iworld)	= 'SDS'.read shared iworld
		= case er of
			Error e		= (Error e, iworld)
			Ok r	
				# w				= fun r
				# (er, iworld)	=  'SDS'.write w shared iworld
				= case er of
					Ok _	= (Ok w, iworld)
					Error e = (Error e, iworld)
					
watch :: !(ReadWriteShared r w) -> Task r | iTask r
watch shared = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (val,iworld)	= 'SDS'.readRegister taskId shared iworld
		# res = case val of
			Ok val		= ValueResult (Value val False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (rep event) (TCInit taskId ts)
			Error e		= ExceptionResult e
		= (res,iworld)
	eval event repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

	rep ResetEvent  = ReplaceUI (ui UIEmpty) 
	rep _ 			= NoChange


interact :: !d !EditMode !(RWShared () r w) l v
				(v l v -> (l, v, Maybe (r -> w))) 	//On edit
				(r l v -> (l, v, Maybe (r -> w)))  	//On refresh
				(Maybe (Editor v)) -> Task (l,v) | toPrompt d & iTask l & iTask r & iTask v
interact prompt mode shared l v editFun refreshFun mbEditor = Task eval
where
	eval event evalOpts tree iworld=:{current={taskTime}}
		//Decode or initialize state
		# (mbd,iworld) = case tree of
			(TCInit taskId ts)
				= case 'SDS'.readRegister taskId shared iworld of
					(Ok r,iworld)
						# (l,v,f) = refreshFun r l v
						= (Ok (taskId,ts,l,v,newFieldMask),iworld)
					(Error e,iworld)  = (Error e,iworld)
			(TCInteract taskId ts encl encv m)
				//Just decode the initially stored values
				= case (fromJSON encl, fromJSON encv) of
					(Just l,Just v) = (Ok (taskId,ts,l,v,m),iworld)
					_				= (Error (exception "Failed to decode stored view in interact"),iworld)
		| mbd =:(Error _) = (ExceptionResult (fromError mbd), iworld)
		# (taskId,ts,l,v,m) = fromOk mbd
		//Apply event (if there is one for this interact)	
		= case matchAndApplyEvent_ event taskId mode mbEditor taskTime editFun l v m ts prompt iworld of
			(Error e,iworld) = (ExceptionResult (exception e),iworld)
			(Ok (l,v,ce,m,ts),iworld) 
				//Refresh the editor with a view based on the share editor
				= case refreshView_ taskId mode mbEditor shared refreshFun l v m iworld of
					(Error e,iworld) = (ExceptionResult e,iworld)
					(Ok (l,v,cr,m),iworld)
						//Construct the result
						# change    = mergeUIChanges ce cr
						# valid     = not (containsInvalidFields m)
						# value     = if valid (Value (l,v) False) NoValue
						# info      = {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
						= (ValueResult value info change (TCInteract taskId ts (toJSON l) (toJSON v) m), iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

matchAndApplyEvent_ event taskId mode mbEditor taskTime editFun l ov m ts prompt iworld
	# editor = fromMaybe gEditor{|*|} mbEditor
	# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
	= case event of
		ResetEvent
			= case editor.Editor.genUI [] ov vst of
				(Ok (ui,m),{VSt|iworld}) = (Ok (l,ov,ReplaceUI (uic UIInteract [toPrompt prompt,ui]),m,taskTime),iworld)
				(Error e,{VSt|iworld})   = (Error e,iworld)
		(EditEvent eTaskId name edit) | eTaskId == taskId 
			= case editor.Editor.onEdit [] (s2dp name,edit) ov m vst of
				(Ok (change,m),v,{VSt|iworld}) 
					# (l,v,f) = editFun ov l v
					# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
					= (Ok (l,v,change,m,taskTime),iworld)
				(Error e,_,{VSt|iworld}) = (Error e,iworld)
		_   = (Ok (l,ov,NoChange,m,ts),iworld)

refreshView_ taskId mode mbEditor shared refreshFun l ov m iworld
	//Read the shared source and refresh the editor
	= case 'SDS'.readRegister taskId shared iworld of
		(Error e,iworld) = (Error e,iworld)
		(Ok r,iworld)
			# (l,v,f) = refreshFun r l ov
			# editor = fromMaybe gEditor{|*|} mbEditor
			# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
			= case editor.Editor.onRefresh [] v ov m vst of
				(Ok (change,_),_,vst=:{VSt|iworld})
					# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
					= (Ok (l,v,change,m), iworld)
				(Error e,_,vst=:{VSt|iworld}) = (Error (exception e),iworld)

tcplisten :: !Int !Bool !(RWShared () r w) (ConnectionHandlers l r w) -> Task [l] | iTask l & iTask r & iTask w
tcplisten port removeClosed sds handlers = Task eval
where
	eval event evalOpts tree=:(TCInit taskId ts) iworld
        = case addListener taskId port removeClosed (wrapConnectionTask handlers sds) iworld of
            (Error e,iworld)
                = (ExceptionResult (exception ("Error: port "+++ toString port +++ " already in use.")), iworld)
            (Ok _,iworld)
                = (ValueResult (Value [] False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (rep port)
                                                    (TCBasic taskId ts JSONNull False),iworld)

    eval event evalOpts tree=:(TCBasic taskId ts _ _) iworld=:{ioStates} 
        = case 'DM'.get taskId ioStates of 
            Just (IOException e)
                = (ExceptionResult (exception e), iworld)
            Just (IOActive values)
                # value = Value [l \\ (_,(l :: l^,_)) <- 'DM'.toList values] False
                = (ValueResult value {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (rep port) (TCBasic taskId ts JSONNull False),iworld)
            Nothing
                = (ValueResult (Value [] False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (rep port) (TCBasic taskId ts JSONNull False), iworld)

    eval event evalOpts tree=:(TCDestroy (TCBasic taskId ts _ _)) iworld=:{ioStates}
        # ioStates = case 'DM'.get taskId ioStates of
            Just (IOActive values)  = 'DM'.put taskId (IODestroyed values) ioStates
            _                       = ioStates
        = (DestroyedResult,{iworld & ioStates = ioStates})

    rep port = ReplaceUI (stringDisplay ("Listening for connections on port "<+++ port))

tcpconnect :: !String !Int !(RWShared () r w) (ConnectionHandlers l r w) -> Task l | iTask l & iTask r & iTask w
tcpconnect host port sds handlers = Task eval
where
	eval event evalOpts tree=:(TCInit taskId ts) iworld=:{IWorld|ioTasks={done,todo},ioStates,world}
        = case addConnection taskId host port (wrapConnectionTask handlers sds) iworld of
            (Error e,iworld)
                = (ExceptionResult e, iworld)
            (Ok _,iworld)
                = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange (TCBasic taskId ts JSONNull False),iworld)

    eval event evalOpts tree=:(TCBasic taskId ts _ _) iworld=:{ioStates}
        = case 'DM'.get taskId ioStates of
            Nothing
                = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange tree, iworld)
            Just (IOActive values)
                = case 'DM'.get 0 values of 
                    Just (l :: l^, s)
                        = (ValueResult (Value l s) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange tree, iworld)
                    _
                        = (ExceptionResult (exception "Corrupt IO task result"),iworld)
            Just (IOException e)
                = (ExceptionResult (exception e),iworld)

    eval event evalOpts tree=:(TCDestroy (TCBasic taskId ts _ _)) iworld=:{ioStates}
        # ioStates = case 'DM'.get taskId ioStates of
            Just (IOActive values)  = 'DM'.put taskId (IODestroyed values) ioStates
            _                       = ioStates
        = (DestroyedResult,{iworld & ioStates = ioStates})

appWorld :: !(*World -> *World) -> Task ()
appWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		= (Ok (), {IWorld|iworld & world = fun world})
		
accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		# (res,world) = fun world
		= (Ok res, {IWorld|iworld & world = world})
	
accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|current={taskTime},world}
		# (res,world)	= fun world
		= case res of
			Error e
				# err = errf e		
				= (Error (dynamic err,toString err), {IWorld|iworld & world = world})	
			Ok v
				= (Ok v, {IWorld|iworld & world = world})
	
accWorldOSError :: !(*World -> (!MaybeOSError a, !*World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException

traceValue :: a -> Task a | iTask a
traceValue v = mkInstantTask eval
where
    eval _ iworld
       # iworld = trace_n (toSingleLineText v) iworld
       = (Ok v,iworld)

shutDown :: Task ()
shutDown = mkInstantTask (\taskId iworld -> (Ok (), {IWorld|iworld & shutdown = True}))
