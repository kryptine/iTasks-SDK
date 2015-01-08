implementation module iTasks.API.Core.IntegrationTasks

import StdInt, StdFile, StdTuple, StdList

import System.Directory, System.File, System.FilePath, Data.Error, System.OSError, Text.Encodings.UrlEncoding, Text, Data.Tuple, Text.JSON
import Data.Either, System.OS, Text.URI, Internet.HTTP

import iTasks.Framework.IWorld, iTasks.Framework.Task, iTasks.Framework.TaskState
import iTasks.Framework.SDS, iTasks.Framework.TaskEval
import iTasks.Framework.Generic.Interaction
import iTasks.API.Core.Types, iTasks.API.Core.Tasks, iTasks.API.Core.LayoutCombinators
import iTasks.API.Core.SDSs
import iTasks.API.Common.InteractionTasks, iTasks.API.Common.TaskCombinators //TODO don't import from Common in Core

from iTasks.API.Common.ImportTasks		import importTextFile

from System.File				import qualified fileExists, readFile
from Data.Map				import qualified newMap, put
from System.Process			import qualified ::ProcessHandle, runProcess, checkProcess,callProcess
from System.Process			import :: ProcessHandle(..)
from Email 				import qualified sendEmail
from Email 				import :: Email(..), :: EmailOption(..)
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

callProcess :: !d ![ViewOption ProcessStatus] !FilePath ![String] !(Maybe FilePath) -> Task ProcessStatus | descr d
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
            # iworld = queueRefresh [taskInstance] ["Checked OS process for instance "<+++ taskInstance] iworld
			= (ValueResult (Value status True) {TaskEvalInfo|lastEvent=lastEvent,involvedUsers=[],removedTasks=[],refreshSensitive=True} rep state, iworld)
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
                    # iworld = queueRefresh [taskInstance] ["Checked OS process for instance "<+++ taskInstance] iworld
                    = (ValueResult (Value status stable) {TaskEvalInfo|lastEvent=lastEvent,involvedUsers=[],removedTasks=[],refreshSensitive=True} rep state, iworld)

	eval event repAs (TCDestroy _) iworld
		= (DestroyedResult,iworld)

    makeRep taskId evalOpts status iworld
	    # layout			= repLayoutRules evalOpts
		# (controls,iworld)	= makeView opts status taskId layout iworld
		# prompt			= toPrompt desc
		# editor			= {UIForm| attributes = 'Data.Map'.newMap, controls = controls, size = defaultSizeOpts}
		= (TaskRep ({UIDef|content=UIForm (layout.LayoutRules.accuInteract prompt editor),windows=[]}),iworld)
						
	makeView [ViewWith viewFun] status taskId layout iworld
		# ver = verifyMaskedValue (Display (viewFun status),Touched)
		= visualizeAsEditor (Display (viewFun status),Touched,ver) taskId layout iworld
	makeView _ status taskId layout iworld
		# ver = verifyMaskedValue (Display (defaultViewFun status),Touched)
		= visualizeAsEditor (Display (defaultViewFun status),Touched,ver) taskId layout iworld
	
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

curlError :: Int -> String
curlError exitCode = 
	case exitCode of
        1       = "Unsupported protocol. This build of curl has no support for this protocol."
        2       = "Failed to initialize."
        3       = "URL malformed. The syntax was not correct."
        5       = "Couldn't resolve proxy. The given proxy host could not be resolved."
        6       = "Couldn't resolve host. The given remote host was not resolved."
        7       = "Failed to connect to host."
        8       = "FTP weird server reply. The server sent data curl couldn't parse."
        9       = "FTP access denied. The server denied login or denied access to the particular resource or directory you wanted to reach."
        11      = "FTP weird PASS reply. Curl couldn't parse the reply sent to the PASS request."
        13      = "FTP weird PASV reply, Curl couldn't parse the reply sent to the PASV request."
        14      = "FTP weird 227 format. Curl couldn't parse the 227-line the server sent."
        15      = "FTP can't get host. Couldn't resolve the host IP we got in the 227-line."
        17      = "FTP couldn't set binary. Couldn't change transfer method to binary."
        18      = "Partial file. Only a part of the file was transferred."
        19      = "FTP couldn't download/access the given file, the RETR (or similar) command failed."
        21      = "FTP quote error. A quote command returned error from the server."
        22      = "HTTP page not retrieved. The requested url was not found or returned another error with the HTTP error code being 400 or above."
        23      = "Write error. Curl couldn't write data to a local filesystem or similar."
        25      = "FTP couldn't STOR file. The server denied the STOR operation, used for FTP uploading."
        26      = "Read error. Various reading problems."
        27      = "Out of memory. A memory allocation request failed."
        28      = "Operation timeout. The specified time-out period was reached according to the conditions."
        30      = "FTP PORT failed. The PORT command failed. Not all FTP servers support the PORT command, try doing a transfer using PASV instead!"
        31      = "FTP couldn't use REST. The REST command failed. This command is used for resumed FTP transfers."
        33      = "HTTP range error. The range \"command\" didn't work."
        34      = "HTTP post error. Internal post-request generation error."
        35      = "SSL connect error. The SSL handshaking failed."
        36      = "FTP bad download resume. Couldn't continue an earlier aborted download."
        37      = "FILE couldn't read file. Failed to open the file. Permissions?"
        38      = "LDAP cannot bind. LDAP bind operation failed."
        39      = "LDAP search failed."
        41      = "Function not found. A required LDAP function was not found."
        42      = "Aborted by callback. An application told curl to abort the operation."
        43      = "Internal error. A function was called with a bad parameter."
        45      = "Interface error. A specified outgoing interface could not be used."
        47      = "Too many redirects. When following redirects, curl hit the maximum amount."
        48      = "Unknown TELNET option specified."
        49      = "Malformed telnet option."
        51      = "The peer's SSL certificate or SSH MD5 fingerprint was not OK."
        52      = "The server didn't reply anything, which here is considered an error."
        53      = "SSL crypto engine not found."
        54      = "Cannot set SSL crypto engine as default."
        55      = "Failed sending network data."
        56      = "Failure in receiving network data."
        58      = "Problem with the local certificate."
        59      = "Couldn't use specified SSL cipher."
        60      = "Peer certificate cannot be authenticated with known CA certificates."
        61      = "Unrecognized transfer encoding."
        62      = "Invalid LDAP URL."
        63      = "Maximum file size exceeded."
        64      = "Requested FTP SSL level failed."
        65      = "Sending the data requires a rewind that failed."
        66      = "Failed to initialise SSL Engine."
        67      = "The user name, password, or similar was not accepted and curl failed to log in."
        68      = "File not found on TFTP server."
        69      = "Permission problem on TFTP server."
        70      = "Out of disk space on TFTP server."
        71      = "Illegal TFTP operation."
        72      = "Unknown TFTP transfer ID."
        73      = "File already exists (TFTP)."
        74      = "No such user (TFTP)."
        75      = "Character conversion failed."
        76      = "Character conversion functions required."
        77      = "Problem with reading the SSL CA cert (path? access rights?)."
        78      = "The resource referenced in the URL does not exist."
        79      = "An unspecified error occurred during the SSH session."
        80      = "Failed to shut down the SSL connection."
        82      = "Could not load CRL file, missing or wrong format (added in 7.19.0)."
        83      = "Issuer check failed (added in 7.19.0)."
        84      = "The FTP PRET command failed"
        85      = "RTSP: mismatch of CSeq numbers"
        86      = "RTSP: mismatch of Session Identifiers"
        87      = "unable to parse FTP file list"
        88      = "FTP chunk callback reported error "

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
			(FocusEvent _ focusId)	= if (focusId == taskId) taskTime ts
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

	//Inline copy of function from CoreCombinators.icl
	//I don't want to export it there because CoreCombinators is an API module
	getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
	getNextTaskId iworld=:{current=current=:{TaskEvalState|taskInstance,nextTaskNo}} = (TaskId taskInstance nextTaskNo, {IWorld|iworld & current = {TaskEvalState|current & nextTaskNo = nextTaskNo + 1}})

sendEmail :: !String !Note !sndr ![rcpt] -> Task [EmailAddress] | toEmail sndr & toEmail rcpt
sendEmail subject (Note body) sender recipients = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|current={taskTime},config}
		# sender		= toEmail sender
		# recipients	= map toEmail recipients
		# iworld		= foldr (sendSingle config.smtpServer sender) iworld recipients
		= (Ok recipients, iworld)
				
	sendSingle server (EmailAddress sender) (EmailAddress address) iworld=:{IWorld|world}
		# (_,world)	= 'Email'.sendEmail [EmailOptSMTPServer server]
						{email_from = sender
						,email_to = address
						,email_subject = subject
						,email_body = body
						} world
		= {IWorld|iworld & world = world}		

instance toEmail EmailAddress where toEmail e = e
instance toEmail String where toEmail s = EmailAddress s
