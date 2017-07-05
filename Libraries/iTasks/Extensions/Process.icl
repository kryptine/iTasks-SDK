implementation module iTasks.Extensions.Process

import iTasks.WF.Definition
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Definition
import iTasks.UI.Prompt
import iTasks.UI.Editor.Builtin
import iTasks.UI.Editor.Combinators
import iTasks._Framework.Task
import iTasks._Framework.TaskEval
import iTasks._Framework.TaskState
import iTasks._Framework.TaskStore
import iTasks._Framework.IWorld

from StdFunc import const
import StdTuple, StdBool
import Data.Maybe, Data.Error, Text.JSON
import qualified System.Process
import qualified Data.Map as DM

:: ProcessStatus
	= RunningProcess !String
	| CompletedProcess !Int

derive class iTask ProcessStatus, CallException
derive JSONEncode ProcessHandle
derive JSONDecode ProcessHandle

instance toString CallException
where
	toString (CallFailed (_,err)) = "Error calling external process: " +++ err

callProcess :: !d ![ViewOption ProcessStatus] !FilePath ![String] !(Maybe FilePath) -> Task ProcessStatus | toPrompt d
callProcess desc opts cmd args dir = Task eval
where
    //Start the process
    eval event evalOpts (TCInit taskId ts) iworld=:{IWorld|world}
        //Call the external process
        # (res,world) = 'System.Process'.runProcess cmd args dir world
        = case res of
			Error e	= (ExceptionResult (dynamic e,snd e), {IWorld|iworld & world = world})
			Ok handle
		        = eval event evalOpts (TCBasic taskId ts (toJSON handle) False) {IWorld|iworld & world = world}
    //Check the process
	eval event evalOpts state=:(TCBasic taskId lastEvent encv stable) iworld=:{IWorld|world,current={TaskEvalState|taskInstance}}
		| stable
            # status        = fromJust (fromJSON encv)
			= case makeRep event taskId evalOpts status False iworld of
            	(Ok rep,iworld)
            		# iworld = queueRefresh [(taskInstance,"Checked OS process for instance "<+++ taskInstance)] iworld
					= (ValueResult (Value status True) {TaskEvalInfo|lastEvent=lastEvent,removedTasks=[],refreshSensitive=True} rep state, iworld)
				(Error e,iworld) = (ExceptionResult (exception e),iworld)
		| otherwise
            //Check status
            # handle = fromJust (fromJSON encv)
            # (res,world) = 'System.Process'.checkProcess handle world
            = case res of
			    Error e	= (ExceptionResult (dynamic e,snd e), {IWorld|iworld & world = world})
                Ok mbExitCode
                    # (status,stable,state) = case mbExitCode of
                        Just c  = (CompletedProcess c,True, TCBasic taskId lastEvent (toJSON (CompletedProcess c)) True)
                        Nothing = (RunningProcess cmd,False, state)
                    = case makeRep event taskId evalOpts status stable {IWorld|iworld & world = world} of
                    	(Ok rep,iworld)
                    		# iworld = queueRefresh [(taskInstance,"Checked OS process for instance "<+++ taskInstance)] iworld
                    		= (ValueResult (Value status stable) {TaskEvalInfo|lastEvent=lastEvent,removedTasks=[],refreshSensitive=True} rep state, iworld)
						(Error e,iworld) = (ExceptionResult (exception e),iworld)

	eval event repAs (TCDestroy _) iworld
		= (DestroyedResult,iworld)

    makeRep event taskId evalOpts status stateChange iworld
		| stateChange || (event =: ResetEvent)
			= case makeView opts status taskId iworld of
			(Ok (content,mask),iworld)
				# prompt			= toPrompt desc
				# change 			= ReplaceUI (uic UIContainer [prompt,content])
				= (Ok change, iworld)
			(Error e,iworld) = (Error e,iworld)
		| otherwise
			= (Ok NoChange, iworld)
						
	makeView _ status taskId iworld
		= makeEditor (status,newFieldMask) taskId iworld

	makeEditor value=:(v,vmask) taskId iworld
		# vst = {VSt| taskId = toString taskId, mode = View, optional = False, selectedConsIndex = -1, iworld = iworld}
		# (editUI,vst=:{VSt|iworld}) = defaultEditor.Editor.genUI [] v vst
		= (editUI,iworld)

	//By default show a progress bar 
	defaultEditor = liftEditor viewFun (const defaultValue) (progressBar 'DM'.newMap)

	viewFun (RunningProcess cmd) = (Nothing, Just ("Running " +++ cmd +++ "..."))
	viewFun (CompletedProcess exit) =(Just 100, Just (cmd +++ " done (" +++ toString exit +++ ")"))
		
callInstantProcess :: !FilePath ![String] !(Maybe FilePath) -> Task Int
callInstantProcess cmd args dir = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime},world}
		# (res,world)	= 'System.Process'.callProcess cmd args dir world
		= case res of
			Error e
				# ex = CallFailed e
				= (Error (dynamic ex,toString ex), {IWorld|iworld & world = world})
			Ok i	= (Ok i, {IWorld|iworld & world = world})

