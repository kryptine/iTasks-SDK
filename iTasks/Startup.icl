implementation module Startup
// *********************************************************************************************************************************
// The iTasks library enables the specification of interactive multi-user workflow tasks (iTask) for the web.
// This module contains iTask kernel.
// This library is still under construction - MJP
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdEnv
import iDataSettings, iDataForms, iDataWidgets, iDataFormlib, iDataTrivial
import iTasksSettings, InternaliTasksCommon, InternaliTasksThreadHandling
import iTasksBasicCombinators, iTasksProcessHandling, iTasksHtmlSupport

import Http, HttpUtil, HttpServer, HttpTextUtil, sapldebug
import IndexHandler, AuthenticationHandler, FilterListHandler, WorkListHandler, WorkTabHandler
import TaskTree, TaskTreeFilters

import JSON
import StdDebug
derive JSONDecode HtmlState, StorageFormat, Lifespan

:: UserStartUpOptions
				= 	{ traceOn			:: !Bool			
					, threadStorageLoc	:: !Lifespan		
					, showUsersOn		:: !Maybe Int	
					, versionCheckOn	:: !Bool
					, headerOff			:: !Maybe [HtmlTag]
					, testModeOn		:: !Bool
					}

// Initial values
initTst :: !UserId !Lifespan !Lifespan !*HSt -> *TSt
initTst thisUser itaskstorage threadstorage hst
				=	{ tasknr		= [-1]
					, activated 	= True
					, staticInfo	= initStaticInfo thisUser threadstorage
					, userId		= if (thisUser >= 0) defaultUser thisUser
					, workflowLink	= (0,(defaultUser,0,defaultWorkflowName))
					, html 			= BT [] []
					, trace			= Nothing
					, hst 			= hst
					, options 		= initialOptions thisUser itaskstorage
					}

initialOptions ::  !UserId !Lifespan  -> Options 
initialOptions thisUser location 
				=	{ tasklife 		= if (thisUser >= 0) location Session 
					, taskstorage 	= PlainString
					, taskmode 		= Edit 
					, gc			= Collect
					}

initStaticInfo :: UserId !Lifespan -> StaticInfo
initStaticInfo thisUser location
=					{ currentUserId	= thisUser 
					, threadTableLoc= location
					}

defaultStartUpOptions :: UserStartUpOptions
defaultStartUpOptions
= 	{ traceOn			= True		
	, threadStorageLoc	= TxtFile				// KLOPT DIT WEL ????		
	, showUsersOn		= Just 5	
	, versionCheckOn	= False
	, headerOff			= Nothing
	, testModeOn		= True
	}

// ******************************************************************************************************
// *** Server / Client startup
// ******************************************************************************************************
:: UserTaskPage a	:== ((Task a) -> .(*HSt -> .((!Bool,!String),HtmlTag,!*HSt)))

doTaskWrapper :: !(Task a) !*World -> *World | iData a	// Combined wrapper which starts the server or client wrapper
doTaskWrapper mainTask world = doHtmlServer mainTask world

doHtmlServer :: (Task a) !*World -> *World | iData a
doHtmlServer mainTask world
| ServerKind == Internal
	# world	= instructions world
	= startServer mainTask world		// link in the Clean http 1.0 server	
//| ServerKind == CGI					// build as CGI application
| otherwise
	= unimplemented world
where
	instructions :: *World -> *World
	instructions world
		# (console, world)	= stdio world
		# console			= fwrites "HTTP server started...\n" console
		# console			= fwrites ("Please point your browser to http://localhost/" +++ ThisExe +++ "/\n") console
		# (_,world)			= fclose console world
		= world
		
	unimplemented :: *World -> *World
	unimplemented world
		# (console, world)	= stdio world
		# console			= fwrites "The chosen server mode is not supported.\n" console
		# console			= fwrites "Please select ServerKind Internal in iDataSettings.dcl.\n" console
		# (_,world)			= fclose console world
		= world

startServer :: (Task a) !*World -> *World | iData a
startServer mainTask world
	# options = ServerOptions ++ (if TraceHTTP [HTTPServerOptDebug True] [])
	= http_startServer options   [((==) "/", handleRedirectRequest)
								 ,((==) ("/" +++ ThisExe), handleRedirectRequest)
								 ,((==) ("/" +++ ThisExe +++ "/new"), handleRedirectRequest) //TEMP: Remove when everyone knows about the new url
								 ,((==) ("/" +++ ThisExe +++ "/"), handleIndexRequest)
								 ,((==) ("/" +++ ThisExe +++ "/handlers/authenticate"), handleAuthenticationRequest)
								 ,((==) ("/" +++ ThisExe +++ "/handlers/filters"), handleFilterListRequest)
								 ,((==) ("/" +++ ThisExe +++ "/handlers/worklist"), handleTaskRequest (handleWorkListRequest mainTask))
								 ,((==) ("/" +++ ThisExe +++ "/handlers/work"), handleTaskRequest (handleWorkTabRequest mainTask))
								 ,(\_ -> True, handleStaticResourceRequest)
								 ] world

// Request handler which points the browser to the index page
handleRedirectRequest	:: !HTTPRequest *World -> (!HTTPResponse, !*World)
handleRedirectRequest req world = ({http_emptyResponse & rsp_headers = [("Status", "301 Moved Permanently"),("Location","/" +++ ThisExe +++ "/")]},world)

// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...

handleStaticResourceRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)
handleStaticResourceRequest req world
	# filename				= MyAbsDir +++ req.req_path	
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	| ok					= ({rsp_headers = [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   ,rsp_data = content}, world)
	# filename				= ResourceDir +++ (req.req_path % ((size ThisExe) + 1, size req.req_path)) //Remove the /(ThisExe)/ from the filename
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	|  ok 					= ({rsp_headers = [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   	,rsp_data = content}, world)		 							   
	= http_notfoundResponse req world

handleTaskRequest :: (HTTPRequest *HSt -> (!HTTPResponse, !*HSt)) !HTTPRequest *World -> (!HTTPResponse, !*World)
handleTaskRequest handler request world
	# (gerda,world)				= openDatabase ODCBDataBaseName world						// open the relational database if option chosen
	# (datafile,world)			= openmDataFile DataFileName world							// open the datafile if option chosen
	# nworld 					= mkNWorld world datafile gerda								// Wrap all io states in an NWorld state
	# updates					= decodeFormUpdates request.arg_post						// Get the form updates from the post
	# states					= decodeHtmlStates request.arg_post							// Fetch stored states from the post
	# fstates	 				= mkFormStates states updates 								
	# hst						= mkHSt request fstates nworld								// Create the HSt
	# (response,hst =:{world = nworld =: {worldC = world, gerda, datafile}})
		= handler request hst																// Apply handler
	# world						= closeDatabase gerda world									// close the relational database if option chosen
	# world						= closemDataFile datafile world								// close the datafile if option chosen
	= (response,world)
where
	decodeFormUpdates :: ![(!String, !String)] -> [FormUpdate]
	decodeFormUpdates args = [update \\ (Just update) <- map mbUpdate args]
	where
		mbUpdate (name, value)	= case mbInputId name ((size name) - 1) of
			Nothing			= Nothing
			Just inputid	= Just {FormUpdate | formid = name % (0, (size name) - (size inputid) - 2), inputid = toInt inputid, value = value}
	
		mbInputId "" _		= Nothing
		mbInputId name i
			| name.[i] == '-' && i < ((size name) - 1)	= Just (name % (i + 1, size name))	//Found the marker
			| isDigit name.[i]							= mbInputId name (i - 1)			//Move cursor one position to the left
														= Nothing							//We've hit an unexpected character
	
	decodeHtmlStates :: ![(!String, !String)] -> [HtmlState]
	decodeHtmlStates args = case fromJSON (http_getValue "state" args "") of
		Nothing	= []			//Parsing failed
		Just states = states 
														
// Database OPTION
openDatabase database world
	:== IF_Database (openGerda database world) (abort "Trying to open a relational database while this option is switched off",world)
closeDatabase database world
	:== IF_Database (closeGerda database world) world

// DataFile OPTION
openmDataFile datafile world
	:== IF_DataFile (openDataFile  datafile world) (abort "Trying to open a dataFile while this option is switched off",world)
closemDataFile datafile world
	:== IF_DataFile (closeDataFile datafile world) world


// ******************************************************************************************************
// *** wrappers for the end user, to be used in combination with an iData wrapper...
// ******************************************************************************************************

singleUserTask 	:: ![StartUpOptions] !(Task a) !*World -> *World  	| iData a
singleUserTask startUpOptions maintask world = doTaskWrapper maintask world
/*
where
	singleUserTask` maintask hst 
	# userOptions					= determineUserOptions [ThreadStorage TxtFile:startUpOptions]
	# tst							= initTst 0 Session userOptions.threadStorageLoc hst
	# (toserver_prefix,html,hst)	= startTstTask 0 False (False,[]) userOptions maintask tst
	= mkHtmlExcep "singleUser" (toserver_prefix) html hst
*/
multiUserTask :: ![StartUpOptions] !(Task a) !*World -> *World   | iData a 
multiUserTask startUpOptions maintask world = doTaskWrapper maintask world
/*
where
	multiUserTask` maintask hst 
	# userOptions 					= determineUserOptions [TestModeOff, VersionCheck, ThreadStorage TxtFile:startUpOptions] 
	# nusers						= case userOptions.showUsersOn of
										Nothing -> 0
										Just n	-> n
//	| nusers == 0			= singleUserTask startUpOptions maintask  hst 
	# (idform,hst) 					= FuncMenu (Init,nFormId "User_Selected" 
										(0,[("User " +++ toString i,\_ -> i) \\ i<-[0..nusers - 1] ])) hst
	# currentWorker					= snd idform.value
	# tst							= initTst currentWorker TxtFile userOptions.threadStorageLoc hst
	# (toserver_prefix,html,hst) 	= startTstTask currentWorker True 
										(if userOptions.traceOn (idform.changed,idform.form) (False,[])) userOptions maintask tst
	= mkHtmlExcep "multiUser" (toserver_prefix) html hst

determineUserOptions :: ![StartUpOptions] -> UserStartUpOptions		
determineUserOptions startUpOptions = determineUserOptions` startUpOptions defaultStartUpOptions
where
	determineUserOptions` [] 						options = options
	determineUserOptions` [TraceOn:xs] 				options	= determineUserOptions` xs {options & traceOn = True}
	determineUserOptions` [TraceOff:xs] 			options	= determineUserOptions` xs {options & traceOn = False}
	determineUserOptions` [ThreadStorage nloc:xs] 	options = determineUserOptions` xs {options & threadStorageLoc = nloc}
	determineUserOptions` [ShowUsers max:xs] 		options = determineUserOptions` xs {options & showUsersOn = if (max <= 0) Nothing (Just max)}
	determineUserOptions` [VersionCheck:xs] 		options = determineUserOptions` xs {options & versionCheckOn = True}
	determineUserOptions` [NoVersionCheck:xs] 		options = determineUserOptions` xs {options & versionCheckOn = False}
	determineUserOptions` [MyHeader bodytag:xs] 	options = determineUserOptions` xs {options & headerOff = Just bodytag}
	determineUserOptions` [TestModeOn:xs] 			options = determineUserOptions` xs {options & testModeOn = True}
	determineUserOptions` [TestModeOff:xs] 			options = determineUserOptions` xs {options & testModeOn = False}
*/
// ******************************************************************************************************
// *THE* main routine for the determination of the current state and the creation of a new workflow page
// ******************************************************************************************************

startTstTask :: !Int !Bool  !(!Bool,![HtmlTag]) UserStartUpOptions !(Task a) !*TSt -> (!(!Bool,!String),![HtmlTag],!*HSt) | iData a 
startTstTask thisUser multiuser (userchanged,multiuserform) useroptions=:{traceOn, threadStorageLoc, showUsersOn, versionCheckOn, headerOff, testModeOn} maintask tst=:{hst,tasknr,staticInfo}

// prologue

| thisUser < 0 			= abort "Users should have id's >= 0 !\n"
# (refresh,hst) 		= simpleButton refreshId "Refresh" id hst
# (traceAsked,hst) 		= simpleButton traceId "ShowTrace" (\_ -> True) hst
# doTrace				= traceAsked.Form.value False
	
# versionsOn			= IF_ClientTasks False versionCheckOn										// no version control on client
# noNewVersion			= not versionsOn || refresh.changed || traceAsked.changed || userchanged 	// no version control in these cases
# (appversion,hst)	 	= setAppversion inc hst
# (pversion,hst)	 	= setPUserNr thisUser id hst
# (sversion,hst)	 	= setSVersionNr thisUser id hst
# versionconflict		= sversion > 0 && sversion < pversion.versionNr && not noNewVersion 		// test if there is a version conflict				

| versionconflict	 
	# iTaskInfo			=
		(mkDiv True "debug-client" [showLabel "Client: ", HrTag []]) ++
		(mkDiv True "debug-server" [showLabel "Server: Cannot apply request. Version conflict. Please refresh the page!", HrTag []])
	= ((True,""),[DivTag [ClassAttr "itasks-ajax",IdAttr "thePage"] [] /* (iTaskHeader ++  iTaskInfo)]*/ ],hst)

// Here the iTasks are evaluated ...
													    
# ((toServer,thrOwner,event,thrinfo,threads),tst=:{html,hst,trace,activated})	
						=  calculateTasks thisUser pversion maintask {tst & hst = hst, trace = if doTrace (Just []) Nothing, activated = True, html = BT [] []}

// epilogue

# newUserVersionNr		= 1 + if (pversion.versionNr > sversion) pversion.versionNr sversion					// increment user querie version number
# (_,hst)				= clearIncPUser thisUser (\_ -> newUserVersionNr) hst									// store in session
# (sversion,hst)	 	= setSVersionNr thisUser (\_ -> newUserVersionNr) hst									// store in persistent memory

# showCompletePage		= IF_Ajax (hd threads == [-1]) True
# prefix				= if showCompletePage "" (determine_prefix thisUser threads)
# (threadtrace,tst)	
						= if TraceThreads showThreadTable nilTable {tst & hst = hst} 
# threadsText			= if showCompletePage "" (foldl (+++) "" [showThreadNr tasknrs +++ " + " \\ tasknrs <- reverse threads])
# (processadmin,tst=:{hst})	= showWorkflows activated tst
# (threadcode,threadinputs,taskname,mainbuts,subbuts,seltask,hst)	
						= Filter showCompletePage thisUser thrOwner html hst
	 	
# iTaskInfo				=	case headerOff of
								Nothing ->
									(	IF_Ajax (IF_ClientTasks [showLabel "Client: "] [showLabel "Server: "]) [] ++
										if multiuser 
											[showText "User: " , showLabel thisUser, showText " - "] [] ++
										if (thrinfo == "" ) [] [showLowLight thrinfo, showText " - "] ++
										if (multiuser && versionsOn)
											 [showText "Query " , showTrace ((sversion +++> " / " )<+++ appversion)] [] ++
										IF_Ajax
											( [showText " - Task#: ", showTrace (showTaskNr  event)] ++
											  if (isEmpty threads || showCompletePage) [] [showText (" - Thread(s)#: "/* +++ prefix*/), showTrace threadsText]
											 ) [] ++
										[BrTag,HrTag []]
									)
								Just userInfo -> userInfo
								
# iTaskInfoDivs			=	IF_Ajax (
							(IF_ClientServer (mkDiv showCompletePage "debug-client" [showLabel "Client: ",HrTag []]) []) ++ 
							(mkDiv showCompletePage "debug-server" iTaskInfo)
							) []
							
# iTaskTraceInfo		=	showOptions staticInfo.threadTableLoc ++ processadmin ++ threadtrace ++ [showTaskTree trace ]
| showCompletePage		=	((toServer,""),[DivTag [ClassAttr "itasks-ajax",IdAttr "thePage"] [] /* ++
															iTaskInfoDivs ++
															if (doTrace && traceOn)
																	iTaskTraceInfo
																	[	leftright taskname subbuts
																		, mainbuts <=>  seltask
																	]
													*/
									] 
							,hst)
# (newthread,oldthreads)=	(hd threads, tl threads)
| otherwise				=	((toServer,""),[DivTag [ClassAttr "itasks-ajax", IdAttr (IF_Client "debug-client" "debug-server")] [] ]
/*
											[iTaskInfo ++			// header info
											[(showTaskNr childthreads,[showText " "]) \\ childthreads <- oldthreads] ++ //clear childthreads, since parent thread don't need to be on this page
											[(showTaskNr newthread, if (isEmpty threadcode) seltask threadcode)]	// task info
											]]
*/
									
							,hst)
where
	determine_prefix:: !UserId ![TaskNr] -> String
	determine_prefix user [] 		= ""
	determine_prefix user [[-1]] 	= ""
	determine_prefix user threads
	# smallest	= hd (sortBy (\l1 l2 ->  length l1 <  length l2) (map tl threads))
	= iTaskId user smallest ""

	leftright left right 
	=	TableTag [WidthAttr "100%"] 
			[TrTag []	[ TdTag [] left
						, TdTag [AlignAttr "right"] right
						]
			]

	nilTable tst = 	([],tst)

	mbUpdate True _ = id
	mbUpdate _ f = f

	ifTraceOn form = if traceOn form []

	showOptions location
	= [showText "Version nr: ", showTrace iTaskVersion] ++
	  [showText " - Enabled: "] ++
	  [showTrace (IF_Ajax 	(" + Ajax (" <+++ location <+++ ") ") "")] ++
	  [showTrace (IF_ClientServer	(IF_Ajax " + Client" "") "")] ++
	  [showTrace (IF_Database " + Database" "")] ++
	  [showTrace (IF_DataFile " + DataFile" "")] ++
	  [showText " - Disabled: "] ++
	  [showTrace (IF_Ajax 	"" " - Ajax " )] ++
	  [showTrace (IF_ClientServer	"" " - Client" )] ++
	  [showTrace (IF_Database "" " - Database" )] ++
	  [showTrace (IF_DataFile "" " - DataFile" )] ++
	  [BrTag [],HrTag []]


// ******************************************************************************************************
// Html Printing Utilities...
// ******************************************************************************************************

mkDiv :: !Bool !String ![HtmlTag] -> [HtmlTag]
mkDiv False id bodytags = bodytags
mkDiv True id bodytags = [DivTag [IdAttr id, ClassAttr "itasks-thread"] bodytags]


