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
import iDataSettings, iDataForms, iDataTrivial, iDataButtons, iDataFormlib
import iTasksSettings, InternaliTasksCommon, InternaliTasksThreadHandling
import iTasksBasicCombinators, iTasksProcessHandling, iTasksHtmlSupport
import TaskTreeFilters
import Http, HttpUtil, HttpServer, HttpTextUtil, sapldebug
import IndexHandler, AuthenticationHandler, FilterListHandler, WorkListHandler, WorkTabHandler
import TaskTree, StdStrictLists


:: UserStartUpOptions
				= 	{ traceOn			:: !Bool			
					, threadStorageLoc	:: !Lifespan		
					, showUsersOn		:: !Maybe Int	
					, versionCheckOn	:: !Bool
					, headerOff			:: !Maybe HtmlCode
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
					, html 			= BT []
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

doTaskWrapper	:: !(UserTaskPage a) !(Task a) !*World -> *World | iData a	// Combined wrapper which starts the server or client wrapper
doTaskWrapper userpageHandler mainTask world = doHtmlServer userpageHandler mainTask world

doHtmlServer :: !(UserTaskPage a) (Task a) !*World -> *World | iData a
doHtmlServer userpageHandler mainTask world
| ServerKind == Internal
	# world	= instructions world
	= StartServer  userpageHandler mainTask world					// link in the Clean http 1.0 server	
//| ServerKind == External											// connect with http 1.1 server
//| ServerKind == CGI												// build as CGI application
| otherwise
	= unimplemented world
where
	instructions :: *World -> *World
	instructions world
		# (console, world)	= stdio world
		# console			= fwrites "HTTP server started...\n" console
		# console			= fwrites ("Please point your browser to http://localhost/" +++ ThisExe +++ "\n") console
		# (_,world)			= fclose console world
		= world
		
	unimplemented :: *World -> *World
	unimplemented world
		# (console, world)	= stdio world
		# console			= fwrites "The chosen server mode is not supported.\n" console
		# console			= fwrites "Please select ServerKind Internal in iDataSettings.dcl.\n" console
		# (_,world)			= fclose console world
		= world

StartServer :: !(UserTaskPage a) (Task a)  !*World -> *World | iData a
StartServer userpageHandler mainTask world
	# options = ServerOptions ++ (if TraceHTTP [HTTPServerOptDebug True] [])

	= http_startServer options   [((==) ("/" +++ ThisExe +++ "/new"), handleIndexRequest)
								 ,((==) ("/" +++ ThisExe +++ "/handlers/authenticate"), handleAuthenticationRequest)
								 ,((==) ("/" +++ ThisExe +++ "/handlers/filters"), handleFilterListRequest)
								 ,((==) ("/" +++ ThisExe +++ "/handlers/worklist"), handleTaskRequest (handleWorkListRequest mainTask))
								 ,((==) ("/" +++ ThisExe +++ "/handlers/work"), handleTaskRequest (handleWorkTabRequest mainTask))
								 ,(\_ -> True, doStaticResource)
								 ] world


// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...

doStaticResource :: !HTTPRequest *World -> (!HTTPResponse, !*World)
doStaticResource req world
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
	# (initforms,nworld)	 	= retrieveFormStates request.arg_post nworld				// Retrieve the state information stored in an html page, other state information is collected lazily
	# hst						= mkHSt request initforms nworld							// Create the HSt
	# (response,hst =:{states,world})	= handler request hst								// Apply handler

	# (debugOutput,states)		= if TraceOutput (traceStates states) (BodyTag [] [],states)	// Optional show debug information
	# (pagestate, focus, world =: {worldC,gerda,datafile})	
		= storeFormStates "" states world													// Store all state information
	# worldC					= closeDatabase gerda worldC								// close the relational database if option chosen
	# worldC					= closemDataFile datafile worldC							// close the datafile if option chosen
	= (response,worldC)

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

mkPage :: [HtmlAttr] [HtmlTag] [HtmlAttr] [HtmlTag] -> HtmlTag
mkPage headattr headtags bodyattr bodytags = HtmlTag [] [HeadTag headattr headtags, BodyTag bodyattr bodytags]

mkCssTag :: HtmlTag
mkCssTag = LinkTag [TypeAttr "text/css", RelAttr "stylesheet", HrefAttr "css/clean.css"] []

mkJsTag :: HtmlTag
mkJsTag = ScriptTag [SrcAttr (ThisExe +++ "/js/clean.js"), TypeAttr "text/javascript"] []

mkInfoDiv :: String String -> HtmlTag
mkInfoDiv state focus =
	DivTag [StyleAttr "display: none"] [
		DivTag [IdAttr "GS"] [Text state],
		DivTag [IdAttr "FS"] [Text focus],
		DivTag [IdAttr "AN"] [Text ThisExe],
		DivTag [IdAttr "OPT-ajax"] [Text (IF_Ajax "true" "false")]
	]

// ******************************************************************************************************
// *** wrappers for the end user, to be used in combination with an iData wrapper...
// ******************************************************************************************************

singleUserTask 	:: ![StartUpOptions] !(Task a) !*World -> *World  	| iData a
singleUserTask startUpOptions maintask world = doTaskWrapper singleUserTask` maintask world
where
	singleUserTask` maintask hst 
	# userOptions					= determineUserOptions [ThreadStorage TxtFile:startUpOptions]
	# tst							= initTst 0 Session userOptions.threadStorageLoc hst
	# (toserver_prefix,html,hst)	= startTstTask 0 False (False,[]) userOptions maintask tst
	= mkHtmlExcep "singleUser" (toserver_prefix) html hst

multiUserTask :: ![StartUpOptions] !(Task a) !*World -> *World   | iData a 
multiUserTask startUpOptions maintask world = doTaskWrapper multiUserTask` maintask world
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

// ******************************************************************************************************
// *THE* main routine for the determination of the current state and the creation of a new workflow page
// ******************************************************************************************************

startTstTask :: !Int !Bool  !(!Bool,!HtmlCode) UserStartUpOptions !(Task a) !*TSt -> (!(!Bool,!String),!HtmlCode,!*HSt) | iData a 
startTstTask thisUser multiuser (userchanged,multiuserform) useroptions=:{traceOn, threadStorageLoc, showUsersOn, versionCheckOn, headerOff, testModeOn} maintask tst=:{hst,tasknr,staticInfo}

// prologue

| thisUser < 0 			= abort "Users should have id's >= 0 !\n"
# (refresh,hst) 		= simpleButton refreshId "Refresh" id hst
# (traceAsked,hst) 		= simpleButton traceId "ShowTrace" (\_ -> True) hst
# doTrace				= traceAsked.value False
	
# versionsOn			= IF_ClientTasks False versionCheckOn										// no version control on client
# noNewVersion			= not versionsOn || refresh.changed || traceAsked.changed || userchanged 	// no version control in these cases
# (appversion,hst)	 	= setAppversion inc hst
# (pversion,hst)	 	= setPUserNr thisUser id hst
# (sversion,hst)	 	= setSVersionNr thisUser id hst
# versionconflict		= sversion > 0 && sversion < pversion.versionNr && not noNewVersion 		// test if there is a version conflict				


/*
# iTaskHeader			=	[TableTag [WidthAttr "100%"] [TrTag [] 
							[ TdTag [] [ImgTag [SrcAttr (ThisExe +++ "/img/clean-logo.jpg"),Img_Align Alo_Middle]
									,showHighLight " i -Task", showLabel " Workflow System "]
							, Td [Td_Align Aln_Right] (multiuserform ++ refresh.form ++ ifTraceOn traceAsked.form)] ]]++
							[Hr []]
*/
| versionconflict	 
	# iTaskInfo			=
		(mkDiv True "debug-client" [showLabel "Client: ", HrTag []]) ++
		(mkDiv True "debug-server" [showLabel "Server: Cannot apply request. Version conflict. Please refresh the page!", HrTag []])
	= ((True,""),[DivTag [ClassAttr "itasks-ajax",IdAttr "thePage"] [] /* (iTaskHeader ++  iTaskInfo)]*/ ],hst)

// Here the iTasks are evaluated ...
													    
# ((toServer,thrOwner,event,thrinfo,threads),tst=:{html,hst,trace,activated})	
						=  calculateTasks thisUser pversion doTrace maintask {tst & hst = hst, trace = if doTrace (Just []) Nothing, activated = True, html = BT []}

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
# (threadcode,taskname,mainbuts,subbuts,seltask,hst)	
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
							
# iTaskTraceInfo		=	showOptions staticInfo.threadTableLoc ++ processadmin ++ threadtrace ++ [printTrace2 trace ]
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

mkDiv :: !Bool !String !HtmlCode -> HtmlCode
mkDiv False id bodytags = bodytags
mkDiv True id bodytags = [DivTag [IdAttr id, ClassAttr "thread"] bodytags]


