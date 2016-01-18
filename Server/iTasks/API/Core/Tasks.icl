implementation module iTasks.API.Core.Tasks

import StdList, StdBool, StdInt, StdTuple, StdMisc, StdDebug
import System.Time, Data.Error, System.OSError, Data.Tuple, Text, Text.JSON
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil, iTasks._Framework.TaskServer
import iTasks._Framework.Generic, iTasks._Framework.Generic.Interaction, iTasks._Framework.Task, iTasks._Framework.TaskState
import iTasks._Framework.TaskEval, iTasks._Framework.TaskStore, iTasks.UI.Definition, iTasks._Framework.IWorld
import iTasks.UI.Layout, iTasks.UI.Layout.Auto, iTasks.UI.Editor
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

interact :: !d !(ReadOnlyShared r)
				(r -> (l,Masked v))
				(l r (Masked v) Bool Bool Bool -> (l,(Masked v)))
				(Maybe (Editor v)) -> Task l | descr d & iTask l & iTask r & iTask v
interact desc shared initFun refreshFun mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		= case mbr of
			Error e		= (ExceptionResult e, iworld)
			Ok r
				# (l,(v,mask))	= initFun r
				= eval event evalOpts (TCInteract taskId ts (toJSON l) (toJSON r) (toJSON v) mask) iworld
				
	eval event evalOpts (TCInteract taskId=:(TaskId instanceNo _) ts encl encr encv m) iworld=:{current={taskTime}}
		//Decode stored values
		# (l,r,v)				= (fromJust (fromJSON encl), fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable	
		# ((nv,nm),nts,iworld)  = matchAndApplyEvent_ event taskId evalOpts mbEditor taskTime (v,m) ts desc iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		| isError mbr			= (ExceptionResult (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# rChanged				= nr =!= r
		# vChanged				= nts =!= ts
		# vValid				= isValid (verifyMaskedValue (nv,nm))
		# (nl,(nv,nm)) 			= if (rChanged || vChanged) (refreshFun l nr (nv,nm) rChanged vChanged vValid) (l,(nv,nm))
		//Update visualization v
		# (change,valid,iworld) = visualizeView_ taskId evalOpts mbEditor event (v,m) (nv,nm) desc iworld
		# value 				= if valid (Value nl False) NoValue
		# info 					= {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True}
		= (ValueResult value info change (TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nm), iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

matchAndApplyEvent_ :: Event TaskId TaskEvalOpts (Maybe (Editor v)) TaskTime (Masked v) TaskTime d *IWorld -> *(!Masked v,!TaskTime,!*IWorld) | iTask v & descr d
matchAndApplyEvent_ (EditEvent taskId name value) matchId evalOpts mbEditor taskTime (v,m) ts desc iworld
	| taskId == matchId
		# ((nv,nm),iworld) = updateValueAndMask_ taskId (s2dp name) mbEditor value (v,m) iworld
		= ((nv,nm),taskTime,iworld)
	| otherwise	= ((v,m),ts,iworld)
matchAndApplyEvent_ _ matchId evalOpts mbEditor taskTime (v,m) ts desc iworld
	= ((v,m),ts,iworld)

updateValueAndMask_ :: TaskId DataPath (Maybe (Editor v)) JSONNode (Masked v) *IWorld -> *(!Masked v,*IWorld) | iTask v
updateValueAndMask_ taskId path mbEditor diff (v,m) iworld
	# editor = fromMaybe gEditor{|*|} mbEditor
    # (nv,nm,ust=:{USt|iworld}) = editor.Editor.appDiff path diff v m {USt|taskId=toString taskId,iworld=iworld}
    = ((nv,nm),iworld)

visualizeView_ :: TaskId TaskEvalOpts (Maybe (Editor v)) Event (Masked v) (Masked v) d *IWorld -> *(!UIChangeDef,!Bool,!*IWorld) | iTask v & descr d
visualizeView_ taskId evalOpts mbEditor event old=:(v,m) new=:(nv,nm) desc iworld
	# editor 	= fromMaybe gEditor{|*|} mbEditor
	# ver 		= verifyMaskedValue (nv,nm)
	# vst = {VSt| selectedConsIndex = -1, optional = False, disabled = False, taskId = toString taskId, iworld = iworld}
	# (change,vst=:{VSt|iworld}) = case event of
		ResetEvent		//(re)generate the initial UI
			# (editUI,vst)	= editor.Editor.genUI [] nv nm vst
			# promptUI  	= toPrompt desc
			# change 		= ReplaceUI (uic UIInteract [promptUI,editUI])
			= (change,vst)
		_				//compare old and new value to determine changes
			# (editChange,vst)  = editor.Editor.genDiff [] v m nv nm vst
			# promptChange 		= NoChange
			# change 			= ChangeUI [] [ChangeChild 0 promptChange, ChangeChild 1 editChange]
			= (change,vst)
	# change		= if evalOpts.autoLayout (fst (autoLayoutInteract (change,JSONNull))) change
	= (change,isValid ver,iworld)

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
