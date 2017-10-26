implementation module iTasks.WF.Tasks.IO

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.UI.Editor
import iTasks.UI.Prompt

import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import iTasks.Internal.TaskServer
import iTasks.Internal.Generic.Visualization
import iTasks.Internal.Generic.Defaults

import Text, Text.JSON
import qualified Data.Map as DM

:: ConnectionHandlers l r w = 
    { onConnect         :: !(String r   -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onData            :: !(String l r -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onShareChange     :: !(       l r -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onDisconnect      :: !(       l r -> (!MaybeErrorString l, Maybe w                  ))
	}

:: ExitCode = ExitCode !Int
:: ExternalProcessHandlers l r w =
    { onStartup     :: !(           r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onOutData     :: !(String   l r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onErrData     :: !(String   l r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onShareChange :: !(         l r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onExit        :: !(ExitCode l r -> (!MaybeErrorString l, !Maybe w                  ))
    }

externalProcess :: !FilePath ![String] !(Maybe FilePath) !(SDS () r w) !(ExternalProcessHandlers l r w) !(Editor l) -> Task l | iTask l & TC r & TC w
externalProcess cmd args dir sds handlers editor = Task eval
where
    eval event evalOpts tree=:(TCInit taskId ts) iworld
        = case addExternalProc taskId cmd args dir (wrapExternalProcTask handlers sds) iworld of
            (Error e, iworld)
                = (ExceptionResult e, iworld)
            (Ok (l :: l^), iworld)
				= case replaceUI taskId l iworld of
					(Ok change, iworld) = (ValueResult (Value l False) (info ts) change (TCBasic taskId ts JSONNull False),iworld)
                    (Error e, iworld)   = (ExceptionResult (exception e),iworld)
            (Ok _, iworld)
                = (ExceptionResult (exception "Corrupt IO task init in externalProcess"), iworld)

    eval event evalOpts tree=:(TCBasic taskId ts _ _) iworld=:{ioStates}
        = case 'DM'.get taskId ioStates of
            Nothing
                = (ValueResult NoValue (info ts) NoChange tree, iworld)
            Just (IOActive values)
                = case 'DM'.get 0 values of 
                    Just (l :: l^, s) = case replaceUI taskId l iworld of 
						(Ok change,iworld) = (ValueResult (Value l s) (info ts) change tree, iworld)
						(Error e,iworld)   = (ExceptionResult (exception e),iworld)
                    _
                        = (ExceptionResult (exception "Corrupt IO task result in externalProcess"),iworld)
            Just (IOException e)
                = (ExceptionResult (exception e),iworld)

    eval event evalOpts tree=:(TCDestroy (TCBasic taskId ts _ _)) iworld=:{ioStates}
        # ioStates = case 'DM'.get taskId ioStates of
            Just (IOActive values)  = 'DM'.put taskId (IODestroyed values) ioStates
            _                       = ioStates
        = (DestroyedResult,{iworld & ioStates = ioStates})

	info ts = {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}

	replaceUI taskId view iworld
		# vst = {VSt| taskId = toString taskId, mode = View, optional = False, selectedConsIndex = -1, iworld = iworld}
		= case editor.Editor.genUI [] view vst of
         	(Ok (editorUI,mask), {VSt|iworld}) = (Ok (ReplaceUI (uic UIInteract [ui UIEmpty, editorUI])), iworld)
            (Error e,{VSt|iworld})             = (Error e,iworld)

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
                = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} rep (TCBasic taskId ts JSONNull False),iworld)

    eval event evalOpts tree=:(TCBasic taskId ts _ _) iworld=:{ioStates}
        = case 'DM'.get taskId ioStates of
            Nothing
                = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} rep tree, iworld)
            Just (IOActive values)
                = case 'DM'.get 0 values of 
                    Just (l :: l^, s)
                        = (ValueResult (Value l s) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} rep tree, iworld)
                    _
                        = (ExceptionResult (exception "Corrupt IO task result"),iworld)
            Just (IOException e)
                = (ExceptionResult (exception e),iworld)

    eval event evalOpts tree=:(TCDestroy (TCBasic taskId ts _ _)) iworld=:{ioStates}
        # ioStates = case 'DM'.get taskId ioStates of
            Just (IOActive values)  = 'DM'.put taskId (IODestroyed values) ioStates
            _                       = ioStates
        = (DestroyedResult,{iworld & ioStates = ioStates})

    rep = ReplaceUI (stringDisplay ("TCP client " <+++ host <+++ ":" <+++ port))

