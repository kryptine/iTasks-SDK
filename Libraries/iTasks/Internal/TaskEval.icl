implementation module iTasks.Internal.TaskEval

import StdList, StdBool, StdTuple, StdMisc, StdString
import Data.Error, Data.Func, Data.Tuple, Data.Either, Data.Functor, Data.List, Text, Text.GenJSON
import iTasks.Internal.IWorld, iTasks.Internal.Task, iTasks.Internal.TaskState, iTasks.Internal.SDS, iTasks.Internal.AsyncSDS
import iTasks.Internal.Store, iTasks.Internal.TaskStore, iTasks.Internal.Util
import iTasks.UI.Layout
import iTasks.Internal.SDSService
import iTasks.Internal.Util

from iTasks.WF.Combinators.Core import :: SharedTaskList
from iTasks.WF.Combinators.Core import :: ParallelTaskType(..), :: ParallelTask(..)
from Data.Map as DM				        import qualified newMap, fromList, toList, get, put, del 
from Data.Queue import :: Queue (..)
from Data.Queue as DQ					import qualified newQueue, enqueue, dequeue, empty
import qualified iTasks.Internal.SDS as SDS
from iTasks.SDS.Combinators.Common      import sdsFocus, mapReadWrite, mapReadWriteError
from StdFunc import const, o

import qualified Data.CircularStack as DCS
from Data.CircularStack import :: CircularStack
from iTasks.Internal.Tonic.AbsSyn import :: ExprId (..)

derive gEq TIMeta

mkEvalOpts :: TaskEvalOpts
mkEvalOpts =
  { TaskEvalOpts
  | noUI        = False
  , tonicOpts   = defaultTonicOpts
  }

defaultTonicOpts :: TonicOpts
defaultTonicOpts = { TonicOpts
                   | inAssignNode            = Nothing
                   , inParallel              = Nothing
                   , captureParallel         = False
                   , currBlueprintModuleName = ""
                   , currBlueprintFuncName   = ""
                   , currBlueprintTaskId     = TaskId 0 0
                   , currBlueprintExprId     = []
                   , callTrace               = 'DCS'.newStack 1024
                   }

extendCallTrace :: !TaskId !TaskEvalOpts -> TaskEvalOpts
extendCallTrace taskId repOpts=:{TaskEvalOpts|tonicOpts = {callTrace = xs}}
  = case 'DCS'.peek xs of
      Just topTaskId
        | taskId == topTaskId = repOpts
      _ = {repOpts & tonicOpts = {repOpts.tonicOpts & callTrace = 'DCS'.push taskId repOpts.tonicOpts.callTrace}}


getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{current=current=:{TaskEvalState|taskInstance,nextTaskNo}}
    = (TaskId taskInstance nextTaskNo, {IWorld|iworld & current = {TaskEvalState|current & nextTaskNo = nextTaskNo + 1}})

processEvents :: !Int *IWorld -> *(!MaybeError TaskException (), !*IWorld)
processEvents max iworld
	| max <= 0 = (Ok (), iworld)
	| otherwise
		= case dequeueEvent iworld of 
			(Nothing,iworld) = (Ok (),iworld)
			(Just (instanceNo,event),iworld)
				= case evalTaskInstance instanceNo event iworld of 
					(Ok taskValue,iworld)
						= processEvents (max - 1) iworld
					(Error msg,iworld=:{IWorld|world})
						= (Ok (),{IWorld|iworld & world = world})

//Evaluate a single task instance
import StdDebug
evalTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (TaskValue JSONNode),!*IWorld)
evalTaskInstance instanceNo event iworld
    # iworld            = mbResetUIState instanceNo event iworld
    # (res,iworld)      = evalTaskInstance` instanceNo event iworld
    = (res,iworld)
where
    evalTaskInstance` instanceNo event iworld=:{clock,current}
    # (constants, iworld)       = read (sdsFocus instanceNo taskInstanceConstants) EmptyContext iworld
	| isError constants         = exitWithException instanceNo ((\(Error (e,msg)) -> msg) constants) iworld
	# constants=:{InstanceConstants|session,listId} = directResult (fromOk constants)
	# (oldReduct, iworld)		= read (sdsFocus instanceNo taskInstanceReduct) EmptyContext iworld
	| isError oldReduct			= exitWithException instanceNo ((\(Error (e,msg)) -> msg) oldReduct) iworld
	# oldReduct=:{TIReduct|task=Task eval,tree,nextTaskNo=curNextTaskNo,nextTaskTime,tasks,tonicRedOpts} = directResult (fromOk oldReduct)
    # (oldProgress,iworld)      = read (sdsFocus instanceNo taskInstanceProgress) EmptyContext iworld
	| isError oldProgress       = exitWithException instanceNo ((\(Error (e,msg)) -> msg) oldProgress) iworld
    # oldProgress=:{InstanceProgress|value,attachedTo} = directResult (fromOk oldProgress)
    //Check exeption
    | value =: (Exception _)
		# (Exception description) = value
		= exitWithException instanceNo description iworld
	//Eval instance
    # (currentSession,currentAttachment) = case (session,attachedTo) of
        (True,_)                                  = (Just instanceNo,[])
        (_,[])                                    = (Nothing,[])
        (_,attachment=:[TaskId sessionNo _:_])    = (Just sessionNo,attachment)
	//Update current process id & eval stack in iworld
	# taskId					= TaskId instanceNo 0
	# iworld					= {iworld & current =
                                        { taskInstance = instanceNo
                                        , sessionInstance = currentSession
                                        , attachmentChain = currentAttachment
										                    , taskTime = oldReduct.TIReduct.nextTaskTime
                                        , nextTaskNo = oldReduct.TIReduct.nextTaskNo
										}}
	//Apply task's eval function and take updated nextTaskId from iworld
	# (newResult,iworld=:{current})	= trace_n "eval func" (eval event {mkEvalOpts & tonicOpts = tonicRedOpts} tree iworld)
    # tree                      = case newResult of
        (ValueResult _ _ _ newTree)  = newTree
        _                            = tree
    //Reset necessary 'current' values in iworld
    # iworld = {IWorld|iworld & current = {TaskEvalState|current & taskInstance = 0}}
    // Check if instance was deleted by trying to reread the instance constants share
	# (deleted,iworld) = appFst isError (read (sdsFocus instanceNo taskInstanceConstants) EmptyContext iworld)
    // Write the updated progress
    | not (trace_tn "Writing progress") = undef
	# (mbErr,iworld) = if (updateProgress clock newResult oldProgress === oldProgress)
		(Ok (),iworld)	//Only update progress when something changed
   		(case (modify (updateProgress clock newResult) (sdsFocus instanceNo taskInstanceProgress) EmptyContext iworld) of 
          (Error e, iworld) = (Error e, iworld)
          (Ok _, iworld) = (Ok (), iworld) )
    = case mbErr of
        Error (e,description)           = exitWithException instanceNo description iworld
        Ok _
            //Store updated reduct
            # (nextTaskNo,iworld)		= getNextTaskNo iworld
            # (_,iworld)                = modify (\r -> {TIReduct|r & tree = tree, nextTaskNo = nextTaskNo, nextTaskTime = nextTaskTime + 1})
                                                (sdsFocus instanceNo taskInstanceReduct) EmptyContext iworld
												//FIXME: Don't write the full reduct (all parallel shares are triggered then!)
            //Store update value
            # newValue                  = case newResult of
                (ValueResult val _ _ _)     = TIValue val
                (ExceptionResult (e,str))   = TIException e str
            # (mbErr,iworld)            = if deleted (Ok Done,iworld) (write newValue (sdsFocus instanceNo taskInstanceValue) EmptyContext iworld)
            = case mbErr of
                Error (e,description) = exitWithException instanceNo description iworld
                Ok _
                	= case newResult of
                    	(ValueResult value _ change _)	
							| deleted
								= (Ok value,iworld)
							//Only queue UI changes if something interesting is changed
							= case compactUIChange change of
								NoChange = (Ok value,iworld)
								change
									# iworld = queueUIChange instanceNo change iworld
									= (Ok value, iworld)
                    	(ExceptionResult (e,description))
							= exitWithException instanceNo description iworld

	exitWithException instanceNo description iworld
		# iworld = queueException instanceNo description iworld
		= (Error description, iworld)

	getNextTaskNo iworld=:{IWorld|current={TaskEvalState|nextTaskNo}}	    = (nextTaskNo,iworld)

	updateProgress now result progress
        # attachedTo = case progress.InstanceProgress.attachedTo of //Release temporary attachment after first evaluation
            (Just (_,[]))   = Nothing
            attachment      = attachment
		# progress = {InstanceProgress|progress
					 &firstEvent = Just (fromMaybe now progress.InstanceProgress.firstEvent)
					 ,lastEvent = Just now
					 }
		= case result of
			(ExceptionResult (_,msg))             = {InstanceProgress|progress & value = Exception msg}
			(ValueResult (Value _ stable) _  _ _) = {InstanceProgress|progress & value = if stable Stable Unstable}
			_                                     = {InstanceProgress|progress & value = Unstable }

    mbResetUIState instanceNo ResetEvent iworld 
		# (_,iworld) = write 'DQ'.newQueue (sdsFocus instanceNo taskInstanceOutput) EmptyContext iworld 
		//Remove all js compiler state for this instance
		# iworld=:{jsCompilerState=jsCompilerState} = iworld
		# jsCompilerState = fmap (\state -> {state & skipMap = 'DM'.del instanceNo state.skipMap}) jsCompilerState
		# iworld = {iworld & jsCompilerState = jsCompilerState}
		= iworld

    mbResetUIState _ _ iworld = iworld

updateInstanceLastIO ::![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceLastIO [] iworld = (Ok (),iworld)
updateInstanceLastIO [instanceNo:instanceNos] iworld=:{IWorld|clock}
    = case modify (\io -> fmap (appSnd (const clock)) io) (sdsFocus instanceNo taskInstanceIO) EmptyContext iworld of
    	(Ok (ModifyResult _ _ _),iworld) = updateInstanceLastIO instanceNos iworld
		(Error e,iworld) = (Error e,iworld)

updateInstanceConnect :: !String ![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceConnect client [] iworld = (Ok (),iworld)
updateInstanceConnect client [instanceNo:instanceNos] iworld=:{IWorld|clock}
    = case write (Just (client,clock)) (sdsFocus instanceNo taskInstanceIO) EmptyContext iworld of
		(Ok _,iworld) = updateInstanceConnect client instanceNos iworld
		(Error e,iworld) = (Error e,iworld)

updateInstanceDisconnect :: ![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceDisconnect [] iworld = (Ok (),iworld)
updateInstanceDisconnect [instanceNo:instanceNos] iworld=:{IWorld|clock}
    = case modify (\io -> fmap (appSnd (const clock)) io) (sdsFocus instanceNo taskInstanceIO) EmptyContext iworld of
		(Ok (ModifyResult _ _ _),iworld) = updateInstanceDisconnect instanceNos iworld
		(Error e,iworld) = (Error e,iworld)

currentInstanceShare :: SDSSource () InstanceNo ()
currentInstanceShare = createReadOnlySDS (\() iworld=:{current={TaskEvalState|taskInstance}} -> (taskInstance,iworld))
