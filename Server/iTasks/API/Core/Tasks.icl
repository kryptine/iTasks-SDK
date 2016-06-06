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

interact :: !p !EditMode !(ReadOnlyShared r)
				(r -> (l,Masked v))
				(l r (Masked v) Bool Bool Bool -> (l,(Masked v)))
				(Maybe (Editor v)) -> Task l | toPrompt p & iTask l & iTask r & iTask v
interact prompt mode shared initFun refreshFun mbEditor = Task eval
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
		# ((nv,nm),nts,iworld)  = matchAndApplyEvent_ event taskId evalOpts mode mbEditor taskTime (v,m) ts prompt iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		| isError mbr			= (ExceptionResult (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# rChanged				= nr =!= r
		# vChanged				= nts =!= ts
		# vValid				= not (containsInvalidFields nm)
		# (nl,(nv,nm)) 			= if (rChanged || vChanged) (refreshFun l nr (nv,nm) rChanged vChanged vValid) (l,(nv,nm))
		//Update visualization v
		= case visualizeView_ taskId evalOpts mode mbEditor event (v,m) (nv,nm) prompt iworld of
			(Ok change,valid,iworld)
				# value 				= if valid (Value nl False) NoValue
				# info 					= {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True}
				= (ValueResult value info change (TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nm), iworld)
			(Error e,valid,iworld) = (ExceptionResult (exception e),iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

matchAndApplyEvent_ :: Event TaskId TaskEvalOpts EditMode (Maybe (Editor v)) TaskTime (Masked v) TaskTime d *IWorld -> *(!Masked v,!TaskTime,!*IWorld) | iTask v & toPrompt d
matchAndApplyEvent_ (EditEvent taskId name value) matchId evalOpts mode mbEditor taskTime (v,m) ts prompt iworld
	| taskId == matchId
		# ((nv,nm),iworld) = updateValueAndMask_ taskId mode (s2dp name) mbEditor value (v,m) iworld
		= ((nv,nm),taskTime,iworld)
	| otherwise	= ((v,m),ts,iworld)
matchAndApplyEvent_ _ matchId evalOpts mode mbEditor taskTime (v,m) ts prompt iworld
	= ((v,m),ts,iworld)

updateValueAndMask_ :: TaskId EditMode DataPath (Maybe (Editor v)) JSONNode (Masked v) *IWorld -> *(!Masked v,*IWorld) | iTask v
updateValueAndMask_ taskId mode path mbEditor diff (v,m) iworld
	# editor = fromMaybe gEditor{|*|} mbEditor
    # (nm,nv,vst=:{VSt|iworld}) = editor.Editor.onEdit path diff v m {VSt|mode = mode, taskId=toString taskId, optional=False, selectedConsIndex= -1, iworld=iworld}
	= case nm of
		Ok (_,m) 	= ((nv,m),iworld)
		_ 			= ((v,m),iworld)

visualizeView_ :: TaskId TaskEvalOpts EditMode (Maybe (Editor v)) Event (Masked v) (Masked v) d *IWorld -> *(!MaybeErrorString UIChange,!Bool,!*IWorld) | iTask v & toPrompt d
visualizeView_ taskId evalOpts mode mbEditor event old=:(v,m) new=:(nv,nm) prompt iworld
	# editor 	= fromMaybe gEditor{|*|} mbEditor
	# valid     = not (containsInvalidFields nm)
	# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
	# (change,vst=:{VSt|iworld}) = case event of
		ResetEvent		//(re)generate the initial UI
			= case editor.Editor.genUI [] nv vst of
				(Ok (editUI,nm),vst)
					# promptUI  	= toPrompt prompt
					# change 		= ReplaceUI (uic UIInteract [promptUI,editUI])
					= (Ok change,vst)
		_				//compare old and new value to determine changes
			= case editor.Editor.onRefresh [] nv v m vst of
				(Ok (editChange,_),_,vst)
					= (Ok (ChangeUI [] [(0,ChangeChild NoChange), (1,ChangeChild editChange)]) ,vst)
				(Error e,_,vst) = (Error e,vst)
	= (change,valid,iworld)

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
