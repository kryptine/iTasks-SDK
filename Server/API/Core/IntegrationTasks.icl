implementation module IntegrationTasks

import StdInt, StdFile, StdTuple, StdList, StdArray

import Directory, File, FilePath, OSError, UrlEncoding, Text

import ExceptionCombinators
import InteractionTasks
import Shared
import TSt
import UserDB

from CoreCombinators	import >>=, >>|
from CoreTasks			import return
from CommonCombinators	import transform
from ImportTasks		import importTextFile
from File				import qualified fileExists, readFile
from Process			import qualified ::ProcessHandle, runProcess, checkProcess
from Email 				import qualified sendEmail
from Email 				import :: Email(..), :: EmailOption(..)

:: AsyncResult = 
	{ success	:: !Bool
	, exitcode	:: !Int
	, message	:: !String
	}
	
derive JSONDecode AsyncResult
derive bimap Maybe, (,)

callProcess :: !FilePath ![String] -> Task (ReadOnlyShared (Maybe Int))
callProcess cmd args = mkInstantTask ("Call process","Calls a process and give shared reference to return code.") callProcess`
where
	callProcess` tst=:{TSt | taskNr, iworld = {IWorld | config, tmpDirectory} }
		# outfile			= tmpDirectory </> (iTaskId taskNr "callprocess")
		# asyncArgs			=	[ "--taskid"
								, toString (last taskNr)
								, "--outfile"
								, outfile
								, "--process"
								, cmd
								]
								++ args
		# (res, tst)		= accWorldTSt ('Process'.runProcess config.Config.runAsyncPath asyncArgs Nothing) tst
		| isError res		= (callException res,tst)
		= (TaskFinished (makeReadOnlySharedError (check outfile)),tst)
	
	check :: !String *IWorld -> *(!MaybeErrorString (Maybe Int),!*IWorld)
	check outfile iworld=:{world}
		# (exists,world) = 'File'.fileExists outfile world
		| not exists = (Ok Nothing, {iworld & world = world})
		# (res, world) = 'File'.readFile outfile world
		| isError res = (Error ("callProcess: Failed to read file " +++ outfile), {iworld & world = world})
		# mbAsync = fromJSON (fromString (fromOk res))
		# callResult = case mbAsync of
			Nothing		= Error ("callProcess: Failed to parse JSON in file " +++ outfile)
			Just async	= if async.AsyncResult.success 
							(Ok (Just async.AsyncResult.exitcode))
							(Error async.AsyncResult.message)
		= (callResult, {iworld & world = world})

	callException res = taskException (CallFailed (fromError res))


callRPCHTTP :: !HTTPMethod !String ![(String,String)] !(String -> a) -> Task (ReadOnlyShared (Maybe a)) | iTask a
callRPCHTTP method url params transformResult
	# options = case method of
		GET	 = "--get"
		POST = ""
	# args = urlEncodePairs params
	= callRPC options url args transformResult

callRPC :: !String !String !String !(String -> a) -> Task (ReadOnlyShared (Maybe a)) | iTask a			
callRPC options url args transformResult =
	mkInstantTask ("Call RPC", "Initializing") initRPC
	>>= \(cmd,args,outfile) -> callProcess cmd args
	>>= \sMbExitCode -> mkInstantTask ("Call RPC", "Waiting for remote server") (readRPC sMbExitCode outfile transformResult)
	where
		initRPC :: *TSt -> *(!TaskResult (String,[String],String),!*TSt)
		initRPC tst=:{TSt|taskNr,iworld=iworld=:{IWorld|config,world,tmpDirectory},properties=p=:{systemProperties=s=:{SystemProperties|taskId}}}
			# infile  = tmpDirectory </> (mkFileName taskId taskNr "request")
			  outfile = tmpDirectory </> (mkFileName taskId taskNr "response")
			  (res,tst) = accWorldTSt (writeFile infile args) tst
			| isError res = (taskException (RPCException ("Write file " +++ infile +++ " failed: " +++ toString (fromError res))),tst)
			# cmd = config.Config.curlPath
			  args =	[ options
						, "--data-binary"
						, "@" +++ infile
						, "-o"
						, outfile
						, url
						]
			= (TaskFinished (cmd,args,outfile),tst)
		
		readRPC :: (ReadOnlyShared (Maybe Int)) !String !(String -> a) *TSt -> *(TaskResult (ReadOnlyShared (Maybe a)),!*TSt)
		readRPC sMbExitCode outfile transformResult tst
			= (TaskFinished (makeReadOnlySharedError (read sMbExitCode outfile transformResult)),tst)
			
		read :: (ReadOnlyShared (Maybe Int)) !String !(String -> a) *IWorld -> *(!MaybeErrorString (Maybe a),!*IWorld)
		read sMbExitCode outfile transformResult iworld
		# (res, iworld) = readShared sMbExitCode iworld
		| isError res = (liftError res, iworld)
		# mbExitCode = fromOk res
		| isNothing mbExitCode = (Ok Nothing, iworld) //Process still running
		# exitCode = fromJust mbExitCode
		| exitCode > 0 = (Error (curlError exitCode), iworld)
		# (res, world) = readFile outfile iworld.IWorld.world
		  iworld = {iworld & world = world}
		| isError res = (Error (toString (fromError res)), iworld)
		= (Ok (Just (transformResult (fromOk res))), iworld)		
			
		mkFileName :: !TaskId !TaskNr !String -> String
		mkFileName taskId taskNr part = iTaskId taskId ("rpc-" +++ part +++ "-"  +++ taskNrToString taskNr)

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


sendEmail :: !String !Note ![EmailAddress] -> Task [EmailAddress]
sendEmail subject (Note body) recipients = mkInstantTask ("Send e-mail", "Send out an e-mail") sendEmail`
where
	sendEmail` tst=:{TSt|properties}
		//Find out the user details of the sending user
		# (mbUser,tst)	= getUserDetails properties.ProcessProperties.managerProperties.worker tst
		= case mbUser of
			Just user
				# (server,tst)	= getConfigSetting (\config -> config.smtpServer) tst
				# tst 			= foldr (sendSingle server user.emailAddress) tst recipients
				= (TaskFinished recipients, tst)
			Nothing
				= (taskException "sendEmail: No e-mail address defined for the current user",tst)
				
	sendSingle server (EmailAddress sender) (EmailAddress address) tst
		//For correct e-mail addresses send immediately
		| indexOf "@" address <> -1
			= sendSingle` server sender (EmailAddress address) tst
		//Lookup user details
		# (mbUser,tst)	= getUserDetails (NamedUser address) tst
		= case mbUser of
			(Just user)	= sendSingle` server sender user.emailAddress tst //Send
			Nothing		= tst //Don't send
			
	sendSingle` server sender (EmailAddress address) tst	
		# (_,tst)	= accWorldTSt ('Email'.sendEmail [EmailOptSMTPServer server]
						{email_from = sender
						,email_to = address
						,email_subject = subject
						,email_body = body
						}) tst
		= tst		