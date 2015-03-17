implementation module iTasks.Framework.TaskEval

import StdList, StdBool, StdTuple, StdMisc, StdDebug
import Data.Error, Data.Func, Data.Tuple, Data.Either, Data.Functor, Data.List, Text, Text.JSON
import iTasks.Framework.IWorld, iTasks.Framework.Task, iTasks.Framework.TaskState
import iTasks.Framework.TaskStore, iTasks.Framework.Util, iTasks.Framework.Generic
import iTasks.API.Core.Types, iTasks.API.Core.LayoutCombinators
import iTasks.Framework.UIDiff
import iTasks.Framework.SDSService

from iTasks.API.Core.TaskCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)
from Data.Map as DM				        import qualified newMap, fromList, toList, get, put, del
from iTasks.Framework.SDS as SDS        import qualified read, write, modify
from iTasks.API.Common.SDSCombinators   import sdsFocus, >+|, mapReadWrite, mapReadWriteError
from StdFunc import const

derive gEq TIMeta

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{current=current=:{TaskEvalState|taskInstance,nextTaskNo}}
    = (TaskId taskInstance nextTaskNo, {IWorld|iworld & current = {TaskEvalState|current & nextTaskNo = nextTaskNo + 1}})

//Evaluate a single task instance
evalTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (!EventNo,!TaskValue JSONNode,![UIUpdate]),!*IWorld)
evalTaskInstance instanceNo event iworld
    # iworld            = resetUIUpdates instanceNo event iworld
    # (res,iworld)      = evalTaskInstance` instanceNo event iworld
    = (res,iworld)
where
    evalTaskInstance` instanceNo event iworld=:{clocks={localDate,localTime},current}
    # (constants, iworld)       = 'SDS'.read (sdsFocus instanceNo taskInstanceConstants) iworld
	| isError constants         = ((\(Error (e,msg)) -> Error msg) constants, iworld)
	# constants=:{InstanceConstants|instanceKey,session,listId} = fromOk constants
	# (oldReduct, iworld)		= 'SDS'.read (sdsFocus instanceNo taskInstanceReduct) iworld
	| isError oldReduct			= ((\(Error (e,msg)) -> Error msg) oldReduct, iworld)
	# oldReduct=:{TIReduct|task=Task eval,tree,nextTaskNo=curNextTaskNo,nextTaskTime,tasks} = fromOk oldReduct
    # (oldProgress,iworld)      = 'SDS'.read (sdsFocus instanceNo taskInstanceProgress) iworld
	| isError oldProgress       = ((\(Error (e,msg)) -> Error msg) oldProgress, iworld)
    # oldProgress=:{InstanceProgress|value,attachedTo} = fromOk oldProgress
    //Check exeption
    | value === Exception
	    # (oldValue, iworld)		= 'SDS'.read (sdsFocus instanceNo taskInstanceValue) iworld
        = case oldValue of
            (Error (e,msg))             = (Error msg, iworld)
		    (Ok (TIException e msg))    = (Error msg, iworld)
            (Ok _)                      = (Error "Exception no longer available", iworld)
    //Eval instance
    # (currentUser,currentSession,currentAttachment) = case (session,attachedTo) of
        (True,_)                                                = (AnonymousUser instanceKey,Just instanceNo,[])
        (_,Nothing)                                             = (SystemUser,Nothing,[])
        (_,Just (worker,[]))                                    = (worker,Nothing,[])
        (_,Just (worker,attachment=:[TaskId sessionNo _:_]))    = (worker,Just sessionNo,attachment)
    # evalOpts					= {TaskEvalOpts|useLayout=Nothing,modLayout=Nothing,noUI=False,callTrace=[]}
	//Update current process id & eval stack in iworld
	# taskId					= TaskId instanceNo 0
	//# eventRoute				= determineEventRoute event lists
    # eventRoute                = 'DM'.newMap //TODO: Check if eventroute is still necessary
	# iworld					= {iworld & current =
                                        { taskInstance = instanceNo
                                        , sessionInstance = currentSession
                                        , attachmentChain = currentAttachment
										, taskTime = oldReduct.TIReduct.nextTaskTime
                                        , nextTaskNo = oldReduct.TIReduct.nextTaskNo
										, user = currentUser
										, eventRoute = eventRoute
                                        , editletDiffs = current.editletDiffs //FIXME: MEMLEAK//'DM'.newMap
										}}
	//Apply task's eval function and take updated nextTaskId from iworld
	# (newResult,iworld=:{current})	= eval event evalOpts tree iworld
    //Finalize task UI
    # newResult                 = finalizeUI session newResult
    # tree                      = case newResult of
        (ValueResult _ _ _ newTree)  = newTree
        _                                                   = tree
    //Reset necessary 'current' values in iworld
    # iworld = {IWorld|iworld & current = {TaskEvalState|current & taskInstance = 0}}
    // Check if instance was deleted by trying to reread the instance constants share
	# (deleted,iworld) = appFst isError ('SDS'.read (sdsFocus instanceNo taskInstanceConstants) iworld)
    // Write the updated progress
    # (mbErr,iworld)            = 'SDS'.modify (updateProgress (DateTime localDate localTime) newResult currentUser) (sdsFocus instanceNo taskInstanceProgress) iworld
    = case mbErr of
        Error (e,msg)          = (Error msg,iworld)
        Ok _
            //Store updated reduct
            # (nextTaskNo,iworld)		= getNextTaskNo iworld
            # (_,iworld)                = 'SDS'.modify (\r -> {TIReduct|r & tree = tree, nextTaskNo = nextTaskNo, nextTaskTime = nextTaskTime + 1,lastEventNo = lastEventNo event})
                                                (sdsFocus instanceNo taskInstanceReduct) iworld //FIXME: Don't write the full reduct (all parallel shares are triggered then!)
            //Store update value
            # newValue                  = case newResult of
                (ValueResult val _ _ _)     = TIValue val
                (ExceptionResult (e,str))   = TIException e str
            # (mbErr,iworld)            = if deleted (Ok (),iworld) ('SDS'.write newValue (sdsFocus instanceNo taskInstanceValue) iworld)
            = case mbErr of
                Error (e,msg)          = (Error msg,iworld)
                Ok _
                //Determine user interface updates by comparing the previous UI to the newly calculated one
                = case newResult of
                    (ValueResult value _ newRep _)	
                        = case 'SDS'.read (sdsFocus instanceNo taskInstanceRep) iworld of
                            (Ok oldRep, iworld)
                                # oldUI = case oldRep of (TaskRep oldUI) = oldUI; _ = emptyUI
                                # newUI = case newRep of (TaskRep newUI) = newUI; _ = emptyUI
                                //Editlets compute their own diffs we pass to the diff algorithm
                                # (editletDiffs,iworld)		= getEditletDiffs iworld
                                # (updates,editletDiffs)    = diffUIDefinitions oldUI newUI event editletDiffs
                                # iworld                    = setEditletDiffs editletDiffs iworld
                                //Store the new reference UI for further updates
                                # (mbErr,iworld) = if deleted (Ok (),iworld) ('SDS'.write newRep (sdsFocus instanceNo taskInstanceRep) iworld)
                                //Flush the share cache
                                # iworld = flushShareCache iworld
                                = case mbErr of
                                    (Error (e,msg)) = (Error msg,iworld)
                                    Ok _            = (Ok (lastEventNo event,value,updates), iworld)
                            (Error (e,msg),iworld)
                                = (Error msg, iworld)
                    (ExceptionResult (e,msg))
                        = (Error msg, iworld)

	getNextTaskNo iworld=:{IWorld|current={TaskEvalState|nextTaskNo}}	    = (nextTaskNo,iworld)
	getEditletDiffs iworld=:{IWorld|current={editletDiffs}}	= (editletDiffs,iworld)
    setEditletDiffs editletDiffs iworld=:{current} = {IWorld|iworld & current = {current & editletDiffs = editletDiffs}}

    finalizeUI session (ValueResult value info (TaskRep ui) tree)
        # ui = if session (uiDefSetAttribute "session" "true" ui) ui
        = (ValueResult value info (TaskRep (autoLayoutFinal ui)) tree)
    finalizeUI session res = res

	updateProgress now result currentUser progress
        # attachedTo = case progress.InstanceProgress.attachedTo of //Release temporary attachment after first evaluation
            (Just (_,[]))   = Nothing
            attachment      = attachment
		# progress = {InstanceProgress|progress & firstEvent = Just (fromMaybe now progress.InstanceProgress.firstEvent), lastEvent = Nothing} //EXPERIMENT
		= case result of
			(ExceptionResult _)				    = {InstanceProgress|progress & value = Exception}
			(ValueResult (Value _ stable) {TaskEvalInfo|involvedUsers} _ _)	
                = {InstanceProgress|progress & value = if stable Stable Unstable, involvedUsers = [currentUser:involvedUsers]}
			(ValueResult _ {TaskEvalInfo|involvedUsers} _ _)	
                = {InstanceProgress|progress & value = None, involvedUsers = [currentUser:involvedUsers]}
			_									= {InstanceProgress|progress & value = None}

	lastEventNo (EditEvent eventNo _ _ _)     = eventNo
	lastEventNo (ActionEvent eventNo _ _)     = eventNo
	lastEventNo (FocusEvent eventNo _)        = eventNo
	lastEventNo (RefreshEvent (Just eventNo)) = eventNo
	lastEventNo _ = 0

    resetUIUpdates instanceNo ResetEvent iworld 
		# (_,iworld) = 'SDS'.modify (\output -> 'DM'.del instanceNo output) taskOutput iworld
		= iworld
    resetUIUpdates _ _ iworld = iworld

//Evaluate a task instance, just to refresh its state
refreshTaskInstance :: !InstanceNo !(Maybe String) !*IWorld -> *IWorld
refreshTaskInstance instanceNo mbReason iworld
    //# iworld            = trace_n ("Evaluating "<+++ instanceNo <+++ ": " <+++ mbReason) iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo (RefreshEvent Nothing) iworld
	= case mbResult of
		(Ok (_,_,updates)) 
			# (_,iworld) = 'SDS'.modify (\output -> 'DM'.put instanceNo (maybe updates (\u -> u ++ updates) ('DM'.get instanceNo output)) output) taskOutput iworld
			= iworld	
		_	= iworld

resetTaskInstance :: !InstanceNo !*IWorld -> *IWorld
resetTaskInstance instanceNo iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo ResetEvent iworld
    = case mbResult of
		(Ok (_,_,updates))
			# (_,iworld) = 'SDS'.modify (\output -> 'DM'.put instanceNo (maybe updates (\u -> u ++ updates) ('DM'.get instanceNo output)) output) taskOutput iworld
			= iworld	
        _                  = iworld

/*
//The event route determines for every parallel which branch the event is in
determineEventRoute :: Event (Map TaskId [ParallelTaskState]) -> Map TaskId Int
determineEventRoute (ResetEvent) _			    = 'DM'.newMap
determineEventRoute (RefreshEvent _) _			= 'DM'.newMap
determineEventRoute (EditEvent _ id _ _) lists	= determineEventRoute` id ('DM'.toList lists)
determineEventRoute (ActionEvent _ id _) lists	= determineEventRoute` id ('DM'.toList lists)
determineEventRoute (FocusEvent _ id) lists		= determineEventRoute` id ('DM'.toList lists)

//TODO: Optimize this search function
determineEventRoute` :: TaskId [(TaskId,[ParallelTaskState])] -> Map TaskId Int 
determineEventRoute` eventId lists = 'DM'.fromList (search eventId)
where
	search searchId = case searchInLists searchId lists of	
		Just (parId, index)	= [(parId,index):search parId]
		Nothing				= []

	searchInLists searchId [] = Nothing
	searchInLists searchId [(parId,entries):xs] = case [i \\ e <- entries & i <- [0..] | inEntry searchId e] of
		[index] = Just (parId,index)
		_		= searchInLists searchId xs

	inEntry searchId {ParallelTaskState|lastEval=ValueResult _ _ _ tree} = inTree searchId tree
	inEntry _ _ = False

	inTree searchId (TCInit taskId _) = searchId == taskId
	inTree searchId (TCBasic taskId _ _ _) = searchId == taskId
	inTree searchId (TCInteract taskId _ _ _ _ _) = searchId == taskId
	inTree searchId (TCInteract1 taskId _ _ _) = searchId == taskId
	inTree searchId (TCInteract2 taskId _ _ _ _) = searchId == taskId
	inTree searchId (TCProject taskId _ tree) = searchId == taskId || inTree searchId tree
	inTree searchId (TCStep taskId _ (Left tree)) = searchId == taskId || inTree searchId tree
	inTree searchId (TCStep taskId _ (Right (_,_,tree))) = searchId == taskId || inTree searchId tree
	inTree searchId (TCParallel taskId _ trees) = searchId == taskId || any (map (inTree searchId o fst) trees)
	inTree searchId (TCShared taskId _ tree) = searchId == taskId || inTree searchId tree
	inTree searchId (TCStable taskId _ _) = searchId == taskId
	inTree searchId _ = False
*/
queueRefresh :: ![InstanceNo] [String] !*IWorld -> *IWorld
queueRefresh instanceNos reasons iworld=:{refreshQueue}
    //Clear the instance's share change registrations, we are going to evaluate anyway
	# iworld	= clearInstanceSDSRegistrations instanceNos iworld
	= {iworld & refreshQueue = removeDup (refreshQueue ++ [(i,r) \\ i <- instanceNos & r <- map Just reasons ++ repeat Nothing])}

queueUrgentRefresh :: ![InstanceNo] [String] !*IWorld -> *IWorld
queueUrgentRefresh instanceNos reasons iworld=:{refreshQueue}
    //Clear the instance's share change registrations, we are going to evaluate anyway
	# iworld	= clearInstanceSDSRegistrations instanceNos iworld
	= {iworld & refreshQueue = removeDup ([(i,r) \\ i <- instanceNos & r <- map Just reasons ++ repeat Nothing] ++ refreshQueue)}

dequeueRefresh :: !*IWorld -> (!Maybe InstanceNo, !Maybe String, !*IWorld)
dequeueRefresh iworld=:{refreshQueue=[]} = (Nothing,Nothing,iworld)
dequeueRefresh iworld=:{refreshQueue=[(instanceNo,mbReason):refreshQueue]} = (Just instanceNo,mbReason,{iworld & refreshQueue = refreshQueue})

updateInstanceLastIO ::![InstanceNo] !*IWorld -> *IWorld
updateInstanceLastIO [] iworld = iworld
updateInstanceLastIO [instanceNo:instanceNos] iworld=:{IWorld|clocks={localDate,localTime}}
    # (_,iworld) = 'SDS'.modify (\p -> {InstanceProgress|p & lastIO =Just (DateTime localDate localTime)}) (sdsFocus instanceNo taskInstanceProgress) iworld
    = updateInstanceLastIO instanceNos iworld

updateInstanceConnect :: !String ![InstanceNo] !*IWorld -> *IWorld //TODO Check error
updateInstanceConnect client [] iworld = iworld
updateInstanceConnect client [instanceNo:instanceNos] iworld=:{IWorld|clocks={localDate,localTime}}
    # (_,iworld) = 'SDS'.modify (\p -> {InstanceProgress|p & connectedTo = Just client, lastIO = Just (DateTime localDate localTime)}) (sdsFocus instanceNo taskInstanceProgress) iworld
    = updateInstanceConnect client instanceNos iworld

updateInstanceDisconnect :: ![InstanceNo] !*IWorld -> *IWorld //TODO Check error
updateInstanceDisconnect [] iworld = iworld
updateInstanceDisconnect [instanceNo:instanceNos] iworld=:{IWorld|clocks={localDate,localTime}}
    # (_,iworld) = 'SDS'.modify (\p -> {InstanceProgress|p & connectedTo = Nothing, lastIO = Just (DateTime localDate localTime)}) (sdsFocus instanceNo taskInstanceProgress) iworld
    = updateInstanceDisconnect instanceNos iworld

currentInstanceShare :: ReadOnlyShared InstanceNo
currentInstanceShare = createReadOnlySDS (\() iworld=:{current={TaskEvalState|taskInstance}} -> (taskInstance,iworld))
