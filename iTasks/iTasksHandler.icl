implementation module iTasksHandler

// *********************************************************************************************************************************
// The iTasks library enables the specification of interactive multi-user workflow tasks (iTask) for the web.
// This module contains iTask kernel.
// This library is still under construction - MJP
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdEnv
import iDataSettings, iDataHandler, iDataTrivial, iDataButtons, iDataFormlib
import iTasksSettings, InternaliTasksCommon, InternaliTasksThreadHandling
import iTasksBasicCombinators, iTasksProcessHandling, iTasksHtmlSupport
import TaskTreeFilters

:: UserStartUpOptions
				= 	{ traceOn			:: !Bool			
					, threadStorageLoc	:: !Lifespan		
					, showUsersOn		:: !Maybe !Int	
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

initialOptions ::  !UserId !Lifespan  -> !Options 
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
multiUserTask startUpOptions maintask world = doHtmlWrapper multiUserTask` world
where
	multiUserTask` hst 
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

workFlowTask :: ![StartUpOptions] !(Task ((Bool,UserId),a)) !(UserId a -> LabeledTask b)!*World -> *World  | iData b 
workFlowTask  startUpOptions taska userTask world = doHtmlWrapper workFlowTask` world 
where
	workFlowTask` hst 
	# userOptions 					= determineUserOptions startUpOptions 
	# tst							= initTst -1 Session userOptions.threadStorageLoc hst
	# (((new,i),a),tst=:{activated,html,hst})	
									= taska tst									// for doing the login 
	| not activated
		# iTaskHeader				= [showHighLight "i-Task", showLabel " - Multi-User Workflow System ",Hr []]
		# iTaskInfo					= mkDiv True "debug-server" [showText "Login procedure... ", Hr []]
		= mkHtmlExcep "workFlow" (True,"") [Ajax [ ("thePage",iTaskHeader ++ iTaskInfo ++ noFilter html) // Login ritual cannot be handled by client
											]] hst
	# userOptions 					= determineUserOptions [TestModeOff, VersionCheck, ThreadStorage TxtFile:startUpOptions] 
	# tst							= initTst i Session userOptions.threadStorageLoc hst
	# (toserver_prefix,body,hst) 	= startTstTask i True (False,[]) userOptions (newUserTask ((new,i),a) <<@ TxtFile) tst
	= mkHtmlExcep "workFlow" (toserver_prefix) body hst

	newUserTask ((True,i),a) 	= (spawnWorkflow i True (userTask i a)) =>> \_ -> return_V Void
	newUserTask _ 				= return_V Void

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

# iTaskHeader			=	[Table [Tbl_Width (Percent 100)] [Tr [] 
							[ Td [] [Img [Img_Src (ThisExe +++ "/img/clean-logo.jpg"),Img_Align Alo_Middle]
									,showHighLight " i -Task", showLabel " Workflow System "]
							, Td [Td_Align Aln_Right] (multiuserform ++ refresh.form ++ ifTraceOn traceAsked.form)] ]]++
							[Hr []]
| versionconflict	 
	# iTaskInfo			=
		(mkDiv True "debug-client" [showLabel "Client: ", Hr []]) ++
		(mkDiv True "debug-server" [showLabel "Server: Cannot apply request. Version conflict. Please refresh the page!", Hr []])
	= ((True,""),[Ajax [("thePage",iTaskHeader ++ iTaskInfo)]],hst)

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
										[Br,Hr []]
									)
								Just userInfo -> userInfo
								
# iTaskInfoDivs			=	IF_Ajax (
							(IF_ClientServer (mkDiv showCompletePage "debug-client" [showLabel "Client: ",Hr []]) []) ++ 
							(mkDiv showCompletePage "debug-server" iTaskInfo)
							) []
							
# iTaskTraceInfo		=	showOptions staticInfo.threadTableLoc ++ processadmin ++ threadtrace ++ [printTrace2 trace ]
| showCompletePage		=	((toServer,""),[Ajax [("thePage",iTaskHeader ++
															iTaskInfoDivs ++
															if (doTrace && traceOn)
																	iTaskTraceInfo
																	[	leftright taskname subbuts
																		, mainbuts <=>  seltask
																	]
											)]
									] 
							,hst)
# (newthread,oldthreads)=	(hd threads, tl threads)
| otherwise				=	((toServer,""),[Ajax ([(IF_Client "debug-client" "debug-server", iTaskInfo)] ++			// header info
											[(showTaskNr childthreads,[showText " "]) \\ childthreads <- oldthreads] ++ //clear childthreads, since parent thread don't need to be on this page
											[(showTaskNr newthread, if (isEmpty threadcode) seltask threadcode)]	// task info
										   )
									]
							,hst)
where
	determine_prefix:: !UserId ![TaskNr] -> String
	determine_prefix user [] 		= ""
	determine_prefix user [[-1]] 	= ""
	determine_prefix user threads
	# smallest	= hd (sortBy (\l1 l2 ->  length l1 <  length l2) (map tl threads))
	= iTaskId user smallest ""

	leftright left right 
	=	Table [Tbl_Width (Percent 100)] 
			[Tr []	[ Td [] left
					, Td [Td_Align Aln_Right] right]
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
	  [Br,Hr []]


// ******************************************************************************************************
// Html Printing Utilities...
// ******************************************************************************************************

mkDiv :: !Bool !String !HtmlCode -> HtmlCode
mkDiv False id bodytag = bodytag
mkDiv True id bodytag = [Div [`Div_Std [Std_Id id, Std_Class "thread"]] bodytag]


// ******************************************************************************************************
// Global Effects Storage Management
// ******************************************************************************************************

// Version number control for multiple user workflows
// To support Ajax calls, it is remembered which of the threads of a user has been deleted by someone else
// 		if information from that thread still arives, the input is thrown away since the thread does not exists anymore.
// 		if information from another thread is received, the task tree is calculated starting from the root
// To support Ajax calls, it is remembered whther new threads have been created for the user by other users
//		if so, the task tree is calculated starting from the root 


setAppversion :: !(Int -> Int) !*HSt -> (!Int,!*HSt) 
setAppversion f hst	
= IF_ClientTasks 
	(\hst -> (0,hst))						// application version number cannot be set by client
	(\hst -> myStoreForm f hst)				// else set application version number
	hst
where
	myStoreForm f hst
	# (form,hst) = mkStoreForm (Init, pFormId applicationVersionNr 0) f hst
	= (form.value,hst)

getCurrentAppVersionNr :: !*TSt -> !(!Int,!*TSt)
getCurrentAppVersionNr tst=:{hst}
# (nr,hst) = setAppversion id hst
= (nr,{tst & hst = hst})



