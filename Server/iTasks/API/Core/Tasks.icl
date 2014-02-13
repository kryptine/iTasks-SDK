implementation module iTasks.API.Core.Tasks

import StdList, StdBool, StdInt, StdTuple,StdMisc
import System.Time, Data.Error, System.OSError, Data.Tuple, Text, Text.JSON
import iTasks.Framework.Util, iTasks.Framework.HtmlUtil
import iTasks.Framework.Generic, iTasks.Framework.Generic.Interaction, iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.TaskEval, iTasks.Framework.TaskStore
import iTasks.Framework.UIDefinition, iTasks.Framework.IWorld
import iTasks.API.Core.LayoutCombinators

from iTasks.Framework.SDS as SDS import qualified read, readRegister, write, writeFilterMsg
from iTasks.API.Core.SDSs       import topLevelTasks
from StdFunc					import o, id
from Data.Map					import qualified newMap, get, put, del
from TCPChannels                import lookupIPAddress, class ChannelEnv, instance ChannelEnv World, connectTCP_MT
from TCPChannels                import toByteSeq, send, class Send, instance Send TCP_SChannel_
from TCPChannels                import :: TimeoutReport, :: Timeout, :: Port
from TCPChannels                import instance toString IPAddress
from TCPChannels                import class closeRChannel(..), instance closeRChannel TCP_RChannel_
from TCPChannelClass            import :: DuplexChannel(..), closeChannel

mkTaskIdent tid = Just (TaskIdentifier "iTasks.API.Core.Tasks" tid)

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask (\taskId iworld-> (Ok a, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (Error (dynamic e,toString e), iworld))

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime}}
		# (val,iworld) = 'SDS'.read shared iworld
		= case val of
			Ok val		= (Ok val,iworld)
			Error e		= (Error (dynamic (SharedException e), e), iworld)
	
set :: !a !(ReadWriteShared r a)  -> Task a | iTask a
set val shared = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime,taskInstance}}
		# (res,iworld)	='SDS'.write val shared iworld
		= case res of
			Ok _	= (Ok val,iworld)
			Error e	= (Error (dynamic (SharedException e), e), iworld)

upd :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
upd fun shared = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime,taskInstance}}
		# (er, iworld)	= 'SDS'.read shared iworld
		= case er of
			Error e		= (Error (dynamic (SharedException e), e), iworld)
			Ok r	
				# w				= fun r
				# (er, iworld)	=  'SDS'.write w shared iworld
				= case er of
					Ok _	= (Ok w, iworld)
					Error e = (Error (dynamic (SharedException e), e), iworld)
					
watch :: !(ReadWriteShared r w) -> Task r | iTask r
watch shared = Task (mkTaskIdent "watch") eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (val,iworld)	= 'SDS'.readRegister instanceNo shared iworld
		# res = case val of
			Ok val		= ValueResult (Value val False) {TaskInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=True}
				(finalizeRep repOpts NoRep) (TCInit taskId ts)
			Error e		= exception (SharedException e)
		= (res,iworld)
	eval event repAs (TCDestroy _) iworld = (DestroyedResult,iworld)


interact :: !d !(ReadOnlyShared r) (r -> (l,(v,InteractionMask))) (l r (v,InteractionMask) Bool Bool Bool -> (l,(v,InteractionMask)))
			-> Task l | descr d & iTask l & iTask r & iTask v
interact desc shared initFun refreshFun = Task (mkTaskIdent "interact") eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SDS'.readRegister instanceNo shared iworld
		= case mbr of
			Error e		= (exception e, iworld)
			Ok r
				# (l,(v,mask))	= initFun r
				= eval event repOpts (TCInteract taskId ts (toJSON l) (toJSON r) (toJSON v) mask) iworld
				
	eval event repOpts (TCInteract taskId=:(TaskId instanceNo _) ts encl encr encv mask) iworld=:{current={taskTime}}
		//Decode stored values
		# (l,r,v)				= (fromJust (fromJSON encl), fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = matchAndApplyEvent event taskId taskTime v mask ts iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'SDS'.readRegister instanceNo shared iworld
		| isError mbr			= (exception (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# rChanged				= nr =!= r
		# vChanged				= nts =!= ts
		# vValid				= isValid (verifyMaskedValue (nv,nmask))
		# (nl,(nv,nmask)) 		= if (rChanged || vChanged) (refreshFun l nr (nv,nmask) rChanged vChanged vValid) (l,(nv,nmask))
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (rep,iworld) 			= visualizeView taskId repOpts (nv,nmask,nver) desc (visualizeAsLabel nl) iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,involvedUsers=[],refreshSensitive=True} (finalizeRep repOpts rep)
			(TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nmask), iworld)

	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	matchAndApplyEvent (EditEvent eventNo taskId name value) matchId taskTime v mask ts iworld
		| taskId == matchId
			| otherwise
				# ((nv,nmask),iworld)	= updateValueAndMask taskId (s2dp name) value (v,mask) iworld
				= (nv,nmask,taskTime,iworld)
		| otherwise	= (v,mask,ts,iworld)
	matchAndApplyEvent (FocusEvent eventNo taskId) matchId taskTime v mask ts iworld
		= (v,mask, if (taskId == matchId) taskTime ts, iworld)
	matchAndApplyEvent _ matchId taskTime v mask ts iworld
		= (v,mask,ts,iworld)

	visualizeView taskId repOpts value=:(v,vmask,vver) desc valueAttr iworld
		# layout	= repLayoutRules repOpts
		# (controls,iworld) = visualizeAsEditor value taskId layout iworld
		# uidef		= {UIDef|content=UIControlStack (layout.LayoutRules.accuInteract (toPrompt desc) {UIControlStack|attributes='Data.Map'.put VALUE_ATTRIBUTE valueAttr 'Data.Map'.newMap,controls=controls,size=defaultSizeOpts}),windows=[]}
		= (TaskRep uidef [(toString taskId,toJSON v)], iworld)

tcpconnect :: !String !Int !(ReadOnlyShared r) (r -> (MaybeErrorString l,[String],Bool)) (l r [String] Bool Bool -> (MaybeErrorString l,[String],Bool)) -> Task l | iTask l & iTask r
tcpconnect host port shared initFun commFun = Task (mkTaskIdent "tcpconnect") eval
where
	eval event repOpts tree=:(TCInit taskId ts) iworld=:{IWorld|io={done,todo},world}
        //Connect
        # (mbIP,world) = lookupIPAddress host world
        | mbIP =: Nothing
            = (ExceptionResult (dynamic lookupErr) lookupErr, {iworld & io = {done=done,todo=todo},world = world})
        # (tReport,mbConn,world) = connectTCP_MT Nothing (fromJust mbIP,port) world
        = case mbConn of
            Nothing
                = (ExceptionResult (dynamic connectErr) connectErr, {iworld & io = {done=done,todo=todo},world = world})
            Just {DuplexChannel|rChannel,sChannel}
                # ip = fromJust mbIP
                # task=:(ConnectionTask init _ _) = connTask taskId shared initFun commFun
                # (out,close,state,iworld=:{IWorld|io={done,todo},world}) = init (toString ip) {iworld & io = {done=done,todo=todo},world = world}
                # (sChannel,world) = case out of
                    []          = (sChannel,world)
                    data        = foldl (\(s,w) d -> send (toByteSeq d) s w) (sChannel,world) data
                | close
 		            # world = closeRChannel rChannel world
                    # world = closeChannel sChannel world
                    = (ValueResult NoValue {TaskInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=True} NoRep (TCBasic taskId ts JSONNull False),{iworld & io = {done=done,todo=todo},world=world})
                | otherwise
                    //Add connection task to todo queue
                    # todo = todo ++ [ConnectionInstance ip {rChannel=rChannel,sChannel=sChannel} task state]
                    = (ValueResult NoValue {TaskInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=True} NoRep (TCBasic taskId ts JSONNull False),{iworld & io = {done=done,todo=todo},world=world})

    eval event repOpts tree=:(TCBasic taskId ts _ _) iworld=:{ioValues}
        = case 'Data.Map'.get taskId ioValues of
            Nothing
                = (ValueResult NoValue {TaskInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=True} NoRep tree, iworld)
            Just (IOValue (l :: l^) s)
                = (ValueResult (Value l s) {TaskInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=True} NoRep tree, iworld)
            Just (IOException e)
                = (ExceptionResult (dynamic e) e,iworld)
            _
                # e = "Corrupt IO task result"
                = (ExceptionResult (dynamic e) e,iworld)

    eval event repOpts tree=:(TCDestroy (TCBasic taskId ts _ _)) iworld=:{ioValues}
        # iworld = {iworld & ioValues = 'Data.Map'.del taskId ioValues}
        = (DestroyedResult,iworld)

    lookupErr = "Failed to lookup host "+++ host
    connectErr = "Failed to connect to host "+++ host

    connTask :: !TaskId (ReadOnlyShared r) (r -> (MaybeErrorString l,[String],Bool)) (l r [String] Bool Bool -> (MaybeErrorString l,[String],Bool)) -> ConnectionTask | iTask r & iTask l
    connTask taskId=:(TaskId instanceNo _) shared initFun commFun = ConnectionTask init eval close
    where
        init host iworld
		    # (val,iworld=:{ioValues}) = 'SDS'.read shared iworld
		    = case val of
			    Ok r
                    # (mbl,sends,close) = initFun r
                    = case mbl of
                        Ok l
                            # iworld = {iworld & ioValues = 'Data.Map'.put taskId (IOValue (dynamic l) False) ioValues}
                            = (sends, close, dynamic (r,l), iworld)
                        Error e
                            # iworld = {iworld & ioValues = 'Data.Map'.put taskId (IOException e) ioValues}
                            = (sends,True, dynamic e, iworld)
			    Error e
                    = ([],True, dynamic (toString e), iworld)

        eval (Just data) ((prevr,l) :: (r^,l^)) iworld
		    # (val,iworld=:{ioValues}) = 'SDS'.read shared iworld
            = case val of
                Ok r
                    # (mbl,sends,close) = commFun l r [data] (r =!= prevr) False
                    = case mbl of
                        Ok l
                            # iworld = {iworld & ioValues = 'Data.Map'.put taskId (IOValue (dynamic l) False) ioValues}
                            # iworld = addOutdatedInstances [(instanceNo,Nothing)] iworld
                            = (sends,close,dynamic (r,l), iworld)
                        Error e
                            # iworld = {iworld & ioValues = 'Data.Map'.put taskId (IOException e) ioValues}
                            # iworld = addOutdatedInstances [(instanceNo,Nothing)] iworld
                            = (sends,True,dynamic e, iworld)
			    Error e
                    = ([],True,dynamic (toString e), iworld)
        eval _  state iworld = ([],False,state,iworld) //TODO: ALSO DEAL WITH CASE WHERE SHARE CHANGED, BUT NO DATA

        close ((prevr,l) :: (r^,l^)) iworld
		    # (val,iworld=:{ioValues}) = 'SDS'.read shared iworld
            = case val of
                Ok r
                    # (mbl,_,_) = commFun l r [] (r =!= prevr) True
                    = case mbl of
                        Ok l
                            # iworld = {iworld & ioValues = 'Data.Map'.put taskId (IOValue (dynamic l) True) ioValues}
                            # iworld = addOutdatedInstances [(instanceNo,Nothing)] iworld
                            = (dynamic (r,l), iworld)
			            Error e
                            # iworld = {iworld & ioValues = 'Data.Map'.put taskId (IOException e) ioValues}
                            # iworld = addOutdatedInstances [(instanceNo,Nothing)] iworld
                            = (dynamic e, iworld)
			    Error e
                    = (dynamic (toString e), iworld)

appWorld :: !(*World -> *World) -> Task Void
appWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		= (Ok Void, {IWorld|iworld & world = fun world})
		
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

shutDown :: Task Void 
shutDown = mkInstantTask (\taskId iworld -> (Ok Void, {IWorld|iworld & shutdown = True}))
