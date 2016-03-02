implementation module iTasks.API.Core.IntegrationTasks

import StdInt, StdFile, StdTuple, StdList

import System.Directory, System.File, System.FilePath, Data.Error, System.OSError, Text.Encodings.UrlEncoding, Text, Data.Tuple, Text.JSON
import Data.Either, System.OS, Text.URI, Internet.HTTP

import iTasks._Framework.IWorld, iTasks._Framework.Task, iTasks._Framework.TaskState
import iTasks._Framework.SDS, iTasks._Framework.TaskStore, iTasks._Framework.TaskEval
import iTasks.API.Core.Types, iTasks.API.Core.Tasks, iTasks.UI.Layout
import iTasks.API.Core.SDSs
import iTasks.API.Common.InteractionTasks, iTasks.API.Common.TaskCombinators //TODO don't import from Common in Core
import iTasks.UI.Editor

from iTasks.API.Common.ImportTasks		import importTextFile

from System.File				import qualified fileExists, readFile
from Data.Map				import qualified newMap, put
from Data.Void 				import :: Void(..)
from System.Process			import qualified ::ProcessHandle, runProcess, checkProcess,callProcess
from System.Process			import :: ProcessHandle(..)
from StdFunc			import o

derive JSONEncode ProcessHandle
derive JSONDecode ProcessHandle

instance toString (OSErrorCode,String)
where
    toString (_,e) = e

worldIO :: (*World -> *(!MaybeError e a,!*World)) -> Task a | iTask a & TC e & toString e
worldIO f = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime},world}
		= case f world of
			(Ok a,world)	= (Ok a, {IWorld|iworld & world = world})
			(Error e,world)	= (Error (dynamic e,toString e), {IWorld|iworld & world = world})

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
            # (rep,iworld)  = makeRep taskId evalOpts status iworld
            # iworld = queueRefresh [(taskInstance,"Checked OS process for instance "<+++ taskInstance)] iworld
			= (ValueResult (Value status True) {TaskEvalInfo|lastEvent=lastEvent,removedTasks=[],refreshSensitive=True} rep state, iworld)
		| otherwise
            //Check status
            # handle = fromJust (fromJSON encv)
            # (res,world) = 'System.Process'.checkProcess handle world
            = case res of
			    Error e	= (ExceptionResult (dynamic e,snd e), {IWorld|iworld & world = world})
                Ok mbExitCode
                    # (status,stable,state) = case mbExitCode of
                        Just c  = (CompletedProcess c,True, TCBasic taskId lastEvent (toJSON (CompletedProcess c)) False)
                        Nothing = (RunningProcess cmd,False, state)
                    # (rep,iworld)  = makeRep taskId evalOpts status {IWorld|iworld & world = world}
                    # iworld = queueRefresh [(taskInstance,"Checked OS process for instance "<+++ taskInstance)] iworld
                    = (ValueResult (Value status stable) {TaskEvalInfo|lastEvent=lastEvent,removedTasks=[],refreshSensitive=True} rep state, iworld)

	eval event repAs (TCDestroy _) iworld
		= (DestroyedResult,iworld)

    makeRep taskId evalOpts status iworld
		# (content,iworld)	= makeView opts status taskId iworld
		# prompt			= toPrompt desc
		# change 			= ReplaceUI (uic (UIEditor {UIEditor|optional=False}) [prompt,content])
		= (change, iworld)
						
	makeView [ViewWith viewFun] status taskId iworld
		= makeEditor (Display (viewFun status),Touched) taskId iworld
	makeView _ status taskId iworld
		= makeEditor (Display (defaultViewFun status),Touched) taskId iworld

	makeEditor value=:(v,vmask) taskId iworld
		# vst = {VSt| selectedConsIndex = -1, optional = False, disabled = False, taskId = toString taskId, iworld = iworld}
		# (editUI,vst=:{VSt|iworld}) = gEditor{|*|}.Editor.genUI [] v vmask vst
		= (editUI,iworld)

	//By default show a progress bar 
	defaultViewFun (RunningProcess cmd) = {Progress|progress=ProgressUndetermined,description="Running " +++ cmd +++ "..."}
	defaultViewFun (CompletedProcess exit) = {Progress|progress=ProgressRatio 1.0,description=cmd +++ " done (" +++ toString exit +++ ")"}
		
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

callHTTP :: !HTTPMethod !URI !String !(HTTPResponse -> (MaybeErrorString a)) -> Task a | iTask a
callHTTP method url=:{URI|uriScheme,uriRegName=Just uriRegName,uriPort,uriPath,uriQuery,uriFragment} data parseFun
    =   tcpconnect uriRegName port (constShare ()) {ConnectionHandlers|onConnect=onConnect,whileConnected=whileConnected,onDisconnect=onDisconnect}
    @?  taskResult
where
    port = fromMaybe 80 uriPort
    path = uriPath +++ maybe "" (\q -> ("?"+++q)) uriQuery +++ maybe "" (\f -> ("#"+++f)) uriFragment
    //VERY SIMPLE HTTP 1.1 Request
    req = toString method +++ " " +++ path +++ " HTTP/1.1\r\nHost:"+++uriRegName+++"\r\nConnection: close\r\n\r\n"+++data

    onConnect _ _
        = (Ok (Left []),Nothing,[req],False)
    whileConnected (Just data) (Left acc) _ 
        = (Ok (Left (acc ++ [data])),Nothing,[],False)
    whileConnected Nothing acc _ 
        = (Ok acc,Nothing,[],False)

    onDisconnect (Left acc) _
        = case parseResponse (concat acc) of
			Nothing    = (Error "Invalid response",Nothing)
            (Just rsp) = case parseFun rsp of
 				               	Ok a    = (Ok (Right a),Nothing)
                				Error e = (Error e,Nothing)

    taskResult (Value (Right a) _)  = Value a True
    taskResult _                    = NoValue

callHTTP _ url _ _
    = throw ("Invalid url: " +++ toString url)

callRPCHTTP :: !HTTPMethod !URI ![(String,String)] !(HTTPResponse -> a) -> Task a | iTask a
callRPCHTTP method url params transformResult
	= callHTTP method url (urlEncodePairs params) (Ok o transformResult)

from iTasks.API.Common.ExportTasks import exportTextFile
from iTasks.API.Common.ImportTasks import importDocument

withTemporaryDirectory :: (FilePath -> Task a) -> Task a | iTask a
withTemporaryDirectory taskfun = Task eval
where
	eval event evalOpts (TCInit taskId ts) iworld=:{server={buildID,paths={dataDirectory}}}
		# tmpDir 			= dataDirectory </> "tmp"</> (buildID +++ "-" +++ toString taskId +++ "-tmpdir")
		# (taskIda,iworld=:{world})	= getNextTaskId iworld
		# (mbErr,world)		= createDirectory tmpDir world
		= case mbErr of
			Ok Void
				= eval event evalOpts (TCShared taskId ts (TCInit taskIda ts)) {iworld & world = world}
			Error e=:(ecode,emsg)
				= (ExceptionResult (dynamic e,emsg), {iworld & world = world})

	eval event evalOpts (TCShared taskId ts treea) iworld=:{server={buildID,paths={dataDirectory}},current={taskTime},world}
		# tmpDir 			        = dataDirectory </> "tmp"</> (buildID +++ "-" +++ toString taskId +++ "-tmpdir")
        # (mbCurdir,world)          = getCurrentDirectory world
        | isError mbCurdir          = (ExceptionResult (exception (fromError mbCurdir)), {IWorld|iworld & world = world})
        # (mbErr,world)             = setCurrentDirectory tmpDir world
        | isError mbErr             = (ExceptionResult (exception (fromError mbErr)), {IWorld|iworld & world = world})
		# ts						= case event of
			(FocusEvent focusId)	= if (focusId == taskId) taskTime ts
			_						= ts
		# (Task evala)			= taskfun tmpDir
		# (resa,iworld=:{world})	= evala event evalOpts treea {IWorld|iworld & world = world}
        # (_,world)                 = setCurrentDirectory (fromOk mbCurdir) world
        | isError mbErr             = (ExceptionResult (exception (fromError mbErr)), {IWorld|iworld & world = world})
		= case resa of
			ValueResult value info rep ntreea
				# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
				= (ValueResult value info rep (TCShared taskId info.TaskEvalInfo.lastEvent ntreea),{IWorld|iworld & world = world})
			ExceptionResult e = (ExceptionResult e,{IWorld|iworld & world = world})
	
	eval event evalOpts (TCDestroy (TCShared taskId ts treea)) iworld=:{server={buildID,paths={dataDirectory}}} //First destroy inner task
		# tmpDir 			= dataDirectory </> "tmp"</> (buildID +++ "-" +++ toString taskId +++ "-tmpdir")
		# (Task evala)	= taskfun tmpDir
		# (resa,iworld)		= evala event evalOpts (TCDestroy treea) iworld
		//TODO: recursive delete of tmp dir to not fill up the task store
		= (resa,iworld)

	eval _ _ _ iworld
		= (ExceptionResult (exception "Corrupt task state in withShared"), iworld)	

