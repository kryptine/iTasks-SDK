implementation module iTasks.API.Core.Tasks

import StdList, StdBool, StdInt, StdTuple, StdMisc, StdDebug
import System.Time, Data.Error, System.OSError, Data.Tuple, Text, Text.JSON
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil, iTasks._Framework.TaskServer
import iTasks._Framework.Generic, iTasks._Framework.Generic.Interaction, iTasks._Framework.Task, iTasks._Framework.TaskState
import iTasks._Framework.TaskEval, iTasks._Framework.TaskStore, iTasks.UI.Definition, iTasks._Framework.IWorld
import iTasks.UI.Layout
import iTasks.API.Core.SDSs, iTasks.API.Common.SDSCombinators

from iTasks._Framework.SDS as SDS import qualified read, readRegister, write
from StdFunc					import o, id
from Data.Map as DM				import qualified newMap, singleton, get, put, del, toList, fromList
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
			Ok val		= ValueResult (Value val False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoRep (TCInit taskId ts) 'DM'.newMap
			Error e		= ExceptionResult e
		= (res,iworld)
	eval event repAs (TCDestroy _) iworld = (DestroyedResult,iworld)


interact :: !d !(ReadOnlyShared r) (r -> (l,(v,InteractionMask))) (l r (v,InteractionMask) Bool Bool Bool -> (l,(v,InteractionMask)))
			-> Task l | descr d & iTask l & iTask r & iTask v
interact desc shared initFun refreshFun = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		= case mbr of
			Error e		= (ExceptionResult e, iworld)
			Ok r
				# (l,(v,mask))	= initFun r
				= eval event evalOpts (TCInteract taskId ts (toJSON l) (toJSON r) (toJSON v) mask) iworld
				
	eval event evalOpts (TCInteract taskId=:(TaskId instanceNo _) ts encl encr encv mask) iworld=:{current={taskTime}}
		//Decode stored values
		# (l,r,v)				= (fromJust (fromJSON encl), fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable	
		# (nv,nmask,nts,iworld) = matchAndApplyEvent event taskId taskTime v mask ts iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		| isError mbr			= (ExceptionResult (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# rChanged				= nr =!= r
		# vChanged				= nts =!= ts
		# vValid				= isValid (verifyMaskedValue (nv,nmask))
		# (nl,(nv,nmask)) 		= if (rChanged || vChanged) (refreshFun l nr (nv,nmask) rChanged vChanged vValid) (l,(nv,nmask))
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (taskUI,iworld)		= visualizeView taskId evalOpts (nv,nmask,nver) desc iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		= (ValueResult value {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True} 
		                     (finalizeRep evalOpts (TaskRep taskUI)) 
		                     (TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nmask)
		                     ('DM'.singleton taskId (Just taskUI,[],Nothing)), iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	matchAndApplyEvent (EditEvent taskId name value) matchId taskTime v mask ts iworld
		| taskId == matchId
			| otherwise
				# ((nv,nmask),iworld)	= updateValueAndMask taskId (s2dp name) value (v,mask) iworld
				= (nv,nmask,taskTime,iworld)
		| otherwise	= (v,mask,ts,iworld)
	matchAndApplyEvent _ matchId taskTime v mask ts iworld
		= (v,mask,ts,iworld)

	visualizeView taskId evalOpts value=:(v,vmask,vver) desc iworld
		# layout	= repLayoutRules evalOpts
		# (controls,iworld) = visualizeAsEditor value taskId layout iworld
		= ({UIDef|content=UIForm (layout.LayoutRules.accuInteract (toPrompt desc) {UIForm|attributes='DM'.newMap,controls=controls,size=defaultSizeOpts}),windows=[]}, iworld)

tcplisten :: !Int !Bool !(RWShared () r w) (ConnectionHandlers l r w) -> Task [l] | iTask l & iTask r & iTask w
tcplisten port removeClosed sds handlers = Task eval
where
	eval event evalOpts tree=:(TCInit taskId ts) iworld
        = case addListener taskId port removeClosed (wrapConnectionTask handlers sds) iworld of
            (Error e,iworld)
                = (ExceptionResult (exception ("Error: port "+++ toString port +++ " already in use.")), iworld)
            (Ok _,iworld)
                = (ValueResult (Value [] False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (TaskRep taskUI)
                                                    (TCBasic taskId ts JSONNull False) ('DM'.singleton taskId (Just taskUI,[],Nothing)),iworld)

    eval event evalOpts tree=:(TCBasic taskId ts _ _) iworld=:{ioStates} 
        = case 'DM'.get taskId ioStates of 
            Just (IOException e)
                = (ExceptionResult (exception e), iworld)
            Just (IOActive values)
                # value = Value [l \\ (_,(l :: l^,_)) <- 'DM'.toList values] False
                = (ValueResult value {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (TaskRep taskUI) (TCBasic taskId ts JSONNull False) ('DM'.singleton taskId (Just taskUI,[],Nothing)),iworld)
            Nothing
                = (ValueResult (Value [] False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (TaskRep taskUI) (TCBasic taskId ts JSONNull False) ('DM'.singleton taskId (Just taskUI,[],Nothing)), iworld)

    eval event evalOpts tree=:(TCDestroy (TCBasic taskId ts _ _)) iworld=:{ioStates}
        # ioStates = case 'DM'.get taskId ioStates of
            Just (IOActive values)  = 'DM'.put taskId (IODestroyed values) ioStates
            _                       = ioStates
        = (DestroyedResult,{iworld & ioStates = ioStates})

    taskUI = {UIDef|content=UIForm {UIForm|attributes ='DM'.newMap
                                            ,controls= [(stringDisplay ("Listening for connections on port "<+++ port),'DM'.newMap)]
                                            ,size=defaultSizeOpts}
                     ,windows = []
               }

tcpconnect :: !String !Int !(RWShared () r w) (ConnectionHandlers l r w) -> Task l | iTask l & iTask r & iTask w
tcpconnect host port sds handlers = Task eval
where
	eval event evalOpts tree=:(TCInit taskId ts) iworld=:{IWorld|ioTasks={done,todo},ioStates,world}
        = case addConnection taskId host port (wrapConnectionTask handlers sds) iworld of
            (Error e,iworld)
                = (ExceptionResult e, iworld)
            (Ok _,iworld)
                = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoRep (TCBasic taskId ts JSONNull False) ('DM'.singleton taskId (Nothing,[],Nothing)),iworld)

    eval event evalOpts tree=:(TCBasic taskId ts _ _) iworld=:{ioStates}
        = case 'DM'.get taskId ioStates of
            Nothing
                = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoRep tree ('DM'.singleton taskId (Nothing,[],Nothing)), iworld)
            Just (IOActive values)
                = case 'DM'.get 0 values of 
                    Just (l :: l^, s)
                        = (ValueResult (Value l s) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoRep tree ('DM'.singleton taskId (Nothing,[],Nothing)), iworld)
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
