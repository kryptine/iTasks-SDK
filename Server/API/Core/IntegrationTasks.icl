implementation module IntegrationTasks

import StdInt, StdFile, StdTuple, StdList

import Directory, File, FilePath, Error, OSError, UrlEncoding, Text, Tuple, JSON

import SystemTypes, IWorld, Task, TaskState
import LayoutCombinators
import CoreTasks, InteractionTasks, CommonCombinators
import Shared

from ImportTasks		import importTextFile
from File				import qualified fileExists, readFile
from Process			import qualified ::ProcessHandle, runProcess, checkProcess
from Process			import :: ProcessHandle(..)
from Email 				import qualified sendEmail
from Email 				import :: Email(..), :: EmailOption(..)
from StdFunc			import o

:: AsyncResult = 
	{ success	:: !Bool
	, exitcode	:: !Int
	, message	:: !String
	}

derive JSONEncode MaybeError
derive JSONDecode MaybeError

derive JSONDecode AsyncResult

callProcess :: !FilePath ![String] -> Task Int
callProcess cmd args 
	= mkTask init eval
where
	//Start the process
	init :: TaskId *IWorld -> (!TaskState,!*IWorld)
	init taskId iworld =:{IWorld |taskTime,build,dataDirectory,sdkDirectory,world}
		# outfile 		= dataDirectory </> "tmp-" +++ build </> (toString taskId +++ "-callprocess")
		# runAsync		= sdkDirectory </> "Tools" </> "RunAsync" </> (IF_POSIX_OR_WINDOWS "RunAsync" "RunAsync.exe")
		# runAsyncArgs	=	[ "--taskid"
							, toString taskId 
							, "--outfile"
							, outfile
							, "--process"
							, cmd
							: args]
		# (res,world)	= 'Process'.runProcess runAsync runAsyncArgs Nothing world
		= case res of
			Error e	= (state taskId taskTime (Left e), {IWorld|iworld & world = world})
			Ok _	= (state taskId taskTime (Right outfile), {IWorld|iworld & world = world})
	where
		state :: TaskId TaskTime (Either OSError FilePath) -> TaskState
		state taskId taskTime val = TCBasic taskId (toJSON val) taskTime False

	eval eevent cevent repAs context=:(TCBasic taskId encv lastEvent stable) iworld=:{world}
		| stable
			= (ValueResult (Value (fromJust (fromJSON encv)) Stable) lastEvent NoRep context, iworld)
		| otherwise
			= case fromJSON encv of
				Just (Right outfile)
					//Check status
					# (exists,world) = 'File'.fileExists outfile world
					| not exists
						//Still busy
						# gui 			= [(ViewPart, Just (stringDisplay ("Calling " +++ cmd)), [], [])]
						# attributes	= [(TITLE_ATTRIBUTE,"Calling external process")]
						# rep = case repAs of
							(RepAsTUI Nothing layout)	
								= TUIRep ((fromMaybe DEFAULT_LAYOUT layout) SingleTask gui [] attributes) 
							(RepAsTUI (Just target) layout)
								| target == taskId
									= TUIRep ((fromMaybe DEFAULT_LAYOUT layout) SingleTask gui [] attributes) 
								| otherwise
									= NoRep
							_
								= ServiceRep ([(toString taskId, 0, JSONNull)], [], [])
						
						= (ValueResult NoValue lastEvent rep context,{IWorld|iworld & world = world})
					# (res, world) = 'File'.readFile outfile world
					| isError res
						//Failed to read file
						= (exception (CallFailed (1,"callProcess: Failed to read output")), {IWorld|iworld & world = world})
					= case fromJSON (fromString (fromOk res)) of
						//Failed to parse file
						Nothing
							= (exception (CallFailed (2,"callProcess: Failed to parse JSON in file " +++ outfile)), {IWorld|iworld & world = world})
						Just async	
							| async.AsyncResult.success
								# result = async.AsyncResult.exitcode 
								= (ValueResult (Value result Stable) lastEvent NoRep (TCBasic taskId (toJSON result) lastEvent True), {IWorld|iworld & world = world})
							| otherwise
								= (exception (CallFailed (async.AsyncResult.exitcode,"callProcess: " +++ async.AsyncResult.message)), {IWorld|iworld & world = world})
				//Error during initialization
				(Just (Left e))
					= (exception (CallFailed e), {IWorld|iworld & world = world})
				Nothing
					= (exception (CallFailed (3,"callProcess: Unknown exception")), {IWorld|iworld & world = world})
					

callRPCHTTP :: !HTTPMethod !String ![(String,String)] !(String -> a) -> Task a | iTask a
callRPCHTTP method url params transformResult
	= callHTTP method url (urlEncodePairs params) (Ok o transformResult)

callHTTP :: !HTTPMethod !String !String !(String -> (MaybeErrorString b)) -> Task b | iTask b	
callHTTP method url request parseResult =
		initRPC
	>>= \(cmd,args,outfile) -> callProcess cmd args <<@ Title "Call RPC"
	>>= \exitCode -> if (exitCode > 0)
		(throw (RPCException (curlError exitCode)))
		(importTextFile outfile >>= \result -> case parseResult result of
			Ok res	= return res
			Error e = throw (RPCException e)
		)
where
	options	= case method of
		GET	 = "--get"
		POST = ""
		
	initRPC = mkInstantTask eval
	
	eval taskId iworld=:{IWorld|taskTime,build,sdkDirectory,dataDirectory,world}
		# infile  = dataDirectory </> "tmp-" +++ build </> (mkFileName taskId "request")
		# outfile = dataDirectory </> "tmp-" +++ build </> (mkFileName taskId "response")
		# (res,world) = writeFile infile request world
		| isError res
			= (exception (RPCException ("Write file " +++ infile +++ " failed: " +++ toString (fromError res))),{IWorld|iworld & world = world})
		# cmd	= IF_POSIX_OR_WINDOWS "curl" (sdkDirectory </> "Tools" </> "Curl" </> "curl.exe" )
		# args	=	[ options
						, "--data-binary"
						, "@" +++ infile
						, "-o"
						, outfile
						, url
						]
		= (ValueResult (Value (cmd,args,outfile) Stable) taskTime NoRep (TCEmpty taskId taskTime), {IWorld|iworld & world = world})
	
	mkFileName :: !TaskId !String -> String
	mkFileName taskId part = toString taskId +++  "-rpc-" +++ part

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
sendEmail subject (Note body) recipients = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|taskTime,currentUser,config}
		# iworld = foldr (sendSingle config.smtpServer (toEmail currentUser)) iworld recipients
		= (ValueResult (Value recipients Stable) taskTime NoRep (TCEmpty taskId taskTime), iworld)
				
	sendSingle server (EmailAddress sender) (EmailAddress address) iworld=:{IWorld|world}
		# (_,world)	= 'Email'.sendEmail [EmailOptSMTPServer server]
						{email_from = sender
						,email_to = address
						,email_subject = subject
						,email_body = body
						} world
		= {IWorld|iworld & world = world}		
