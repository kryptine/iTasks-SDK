implementation module iTasks._Framework.TaskEval

import StdList, StdBool, StdTuple, StdMisc, StdDebug
import Data.Error, Data.Func, Data.Tuple, Data.Either, Data.Functor, Data.List, Text, Text.JSON
import iTasks._Framework.IWorld, iTasks._Framework.Task, iTasks._Framework.TaskState
import iTasks._Framework.TaskStore, iTasks._Framework.Util, iTasks._Framework.Generic
import iTasks.API.Core.Types
import iTasks.UI.Diff, iTasks.UI.Layout
import iTasks._Framework.SDSService

from iTasks.API.Core.TaskCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)
from Data.Map as DM				        import qualified newMap, fromList, toList, get, put, del 
from Data.Queue as DQ					import qualified newQueue, enqueue, dequeue, empty
from iTasks._Framework.SDS as SDS       import qualified read, write, modify
from iTasks.API.Common.SDSCombinators   import sdsFocus, >+|, mapReadWrite, mapReadWriteError
from StdFunc import const

derive gEq TIMeta

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{current=current=:{TaskEvalState|taskInstance,nextTaskNo}}
    = (TaskId taskInstance nextTaskNo, {IWorld|iworld & current = {TaskEvalState|current & nextTaskNo = nextTaskNo + 1}})

processEvents :: !Int *IWorld -> *IWorld
processEvents max iworld  
	| max <= 0 = iworld
	| otherwise
		= case dequeueEvent iworld of 
			(Nothing,iworld) = iworld 
			(Just (instanceNo,event),iworld)
				= case evalTaskInstance instanceNo event iworld of 
					(Ok taskValue,iworld)
						= processEvents (max - 1) iworld
					(Error msg,iworld)
						= processEvents (max - 1) iworld //TODO: Do something useful with this error

//Evaluate a single task instance
evalTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (TaskValue JSONNode),!*IWorld)
evalTaskInstance instanceNo event iworld
    # iworld            = mbResetUIState instanceNo event iworld
    # (res,iworld)      = evalTaskInstance` instanceNo event iworld
    = (res,iworld)
where
    evalTaskInstance` instanceNo event iworld=:{clocks={localDate,localTime},current}
    # (constants, iworld)       = 'SDS'.read (sdsFocus instanceNo taskInstanceConstants) iworld
	| isError constants         = ((\(Error (e,msg)) -> Error msg) constants, iworld)
	# constants=:{InstanceConstants|instanceKey,session,listId} = fromOk constants
	# (oldReduct, iworld)		= 'SDS'.read (sdsFocus instanceNo taskInstanceReduct) iworld
	| isError oldReduct			= ((\(Error (e,msg)) -> Error msg) oldReduct, iworld)
	# oldReduct=:{TIReduct|task=Task eval,tree,nextTaskNo=curNextTaskNo,nextTaskTime,tasks,tonicRedOpts} = fromOk oldReduct
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
    # (currentSession,currentAttachment) = case (session,attachedTo) of
        (True,_)                                  = (Just instanceNo,[])
        (_,[])                                    = (Nothing,[])
        (_,attachment=:[TaskId sessionNo _:_])    = (Just sessionNo,attachment)
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
										, eventRoute = eventRoute
                                        , editletDiffs = current.editletDiffs //FIXME: MEMLEAK//'DM'.newMap
										}}
	//Apply task's eval function and take updated nextTaskId from iworld
	# (newResult,iworld=:{current})	= eval event {mkEvalOpts & tonicOpts = tonicRedOpts} tree iworld
    # tree                      = case newResult of
        (ValueResult _ _ _ newTree)  = newTree
        _                                                   = tree
    //Reset necessary 'current' values in iworld
    # iworld = {IWorld|iworld & current = {TaskEvalState|current & taskInstance = 0}}
    // Check if instance was deleted by trying to reread the instance constants share
	# (deleted,iworld) = appFst isError ('SDS'.read (sdsFocus instanceNo taskInstanceConstants) iworld)
    // Write the updated progress
    # (mbErr,iworld)            = 'SDS'.modify (\p -> ((),updateProgress (DateTime localDate localTime) newResult p)) (sdsFocus instanceNo taskInstanceProgress) iworld
    = case mbErr of
        Error (e,msg)          = (Error msg,iworld)
        Ok _
            //Store updated reduct
            # (nextTaskNo,iworld)		= getNextTaskNo iworld
            # (_,iworld)                = 'SDS'.modify (\r -> ((),{TIReduct|r & tree = tree, nextTaskNo = nextTaskNo, nextTaskTime = nextTaskTime + 1}))
                                                (sdsFocus instanceNo taskInstanceReduct) iworld
												//FIXME: Don't write the full reduct (all parallel shares are triggered then!)
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
						= case 'SDS'.read (sdsFocus instanceNo taskInstanceUI) iworld of
							(Ok UIDisabled, iworld)
								= (Ok value, iworld) //Nothing to do, the UI is disabled
							(Ok (UIEnabled uiVersion prevRep),iworld)
								//Determine output
								//OLD
                                # oldUI = case prevRep of (TaskRep oldUI _) = oldUI; _ = emptyUI
                                # newUI = case newRep of (TaskRep newUI _) = newUI; _ = emptyUI
							    # (editletDiffs,iworld)		= getEditletDiffs iworld
                                # (changes,editletDiffs)    = diffUIDefinitions oldUI newUI event editletDiffs
                                # iworld                    = setEditletDiffs editletDiffs iworld
                                # (mbErr,iworld) 		= if deleted 
									(Ok (),iworld)
									('SDS'.write (UIEnabled (uiVersion + 1) newRep) (sdsFocus instanceNo taskInstanceUI) iworld)
                                # iworld 		= if deleted iworld (queueUIChanges instanceNo changes iworld)
								//NEW
								/*
								# change 		= case newRep of (TaskRep _ change) = change ; _ = NoChange
                                # iworld 		= if deleted iworld (queueUIChange instanceNo change iworld)
								*/
                                //Flush the share cache 
                                # iworld = flushShareCache iworld
								= (Ok value, iworld)
							(Ok (UIException msg), iworld)
								= (Error msg, iworld) //Just dump the old error message for now
							(Error (e,msg),iworld)
								= case 'SDS'.write (UIException msg) (sdsFocus instanceNo taskInstanceUI) iworld of
									(Ok _,iworld)          = (Error msg, iworld)
									(Error (e,msg),iworld) = (Error msg, iworld)	
                    (ExceptionResult (e,msg))
						= case 'SDS'.write (UIException msg) (sdsFocus instanceNo taskInstanceUI) iworld of
							(Ok _,iworld)          = (Error msg, iworld)
							(Error (e,msg),iworld) = (Error msg, iworld)	

	getNextTaskNo iworld=:{IWorld|current={TaskEvalState|nextTaskNo}}	    = (nextTaskNo,iworld)
	getEditletDiffs iworld=:{IWorld|current={editletDiffs}}	= (editletDiffs,iworld)
    setEditletDiffs editletDiffs iworld=:{current} = {IWorld|iworld & current = {current & editletDiffs = editletDiffs}}

	updateProgress now result progress
        # attachedTo = case progress.InstanceProgress.attachedTo of //Release temporary attachment after first evaluation
            (Just (_,[]))   = Nothing
            attachment      = attachment
		# progress = {InstanceProgress|progress & firstEvent = Just (fromMaybe now progress.InstanceProgress.firstEvent), lastEvent = Nothing} //EXPERIMENT
		= case result of
			(ExceptionResult _)				    = {InstanceProgress|progress & value = Exception}
			(ValueResult (Value _ stable) _  _ _)	
                = {InstanceProgress|progress & value = if stable Stable Unstable}
			(ValueResult _ _ _ _)	
                = {InstanceProgress|progress & value = None}
			_									= {InstanceProgress|progress & value = None}

    mbResetUIState instanceNo ResetEvent iworld 
		# (_,iworld) = 'SDS'.write (UIEnabled -1 NoRep) (sdsFocus instanceNo taskInstanceUI) iworld 
		# (_,iworld) = 'SDS'.write 'DQ'.newQueue (sdsFocus instanceNo taskInstanceUIChanges) iworld 
		//Remove all editlet state for this instance
		# (diffs,iworld) = getEditletDiffs iworld
		# diffs = 'DM'.fromList [d \\ d=:((t,_),_) <- 'DM'.toList diffs | let (TaskId i _) = fromString t in i <> instanceNo]
		# iworld = setEditletDiffs diffs iworld
		//Remove all js compiler state for this instance
		# iworld=:{jsCompilerState=jsCompilerState} = iworld
		# jsCompilerState = fmap (\state -> {state & skipMap = 'DM'.del instanceNo state.skipMap}) jsCompilerState
		# iworld = {iworld & jsCompilerState = jsCompilerState}
		= iworld

    mbResetUIState _ _ iworld = iworld
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

updateInstanceLastIO ::![InstanceNo] !*IWorld -> *IWorld
updateInstanceLastIO [] iworld = iworld
updateInstanceLastIO [instanceNo:instanceNos] iworld=:{IWorld|clocks={localDate,localTime}}
    # (_,iworld) = 'SDS'.modify (\p -> ((),{InstanceProgress|p & lastIO =Just (DateTime localDate localTime)})) (sdsFocus instanceNo taskInstanceProgress) iworld
    = updateInstanceLastIO instanceNos iworld

updateInstanceConnect :: !String ![InstanceNo] !*IWorld -> *IWorld //TODO Check error
updateInstanceConnect client [] iworld = iworld
updateInstanceConnect client [instanceNo:instanceNos] iworld=:{IWorld|clocks={localDate,localTime}}
    # (_,iworld) = 'SDS'.modify (\p -> ((),{InstanceProgress|p & connectedTo = Just client, lastIO = Just (DateTime localDate localTime)})) (sdsFocus instanceNo taskInstanceProgress) iworld
    = updateInstanceConnect client instanceNos iworld

updateInstanceDisconnect :: ![InstanceNo] !*IWorld -> *IWorld //TODO Check error
updateInstanceDisconnect [] iworld = iworld
updateInstanceDisconnect [instanceNo:instanceNos] iworld=:{IWorld|clocks={localDate,localTime}}
    # (_,iworld) = 'SDS'.modify (\p -> ((),{InstanceProgress|p & connectedTo = Nothing, lastIO = Just (DateTime localDate localTime)})) (sdsFocus instanceNo taskInstanceProgress) iworld
    = updateInstanceDisconnect instanceNos iworld

currentInstanceShare :: ReadOnlyShared InstanceNo
currentInstanceShare = createReadOnlySDS (\() iworld=:{current={TaskEvalState|taskInstance}} -> (taskInstance,iworld))
