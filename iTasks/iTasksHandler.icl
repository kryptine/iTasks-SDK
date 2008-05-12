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

derive gForm 	Void
derive gUpd 	Void
derive gParse 	Void
derive gPrint 	Void
derive gerda 	Void
derive read 	Void
derive write 	Void

instance == GarbageCollect
where
	(==) Collect   Collect 		= True
	(==) NoCollect NoCollect 	= True
	(==) _ _ 					= False

:: UserStartUpOptions
				= 	{ traceOn			:: !Bool			
					, threadStorageLoc	:: !Lifespan		
					, showUsersOn		:: !Maybe !Int	
					, versionCheckOn	:: !Bool
					, headerOff			:: !Maybe HtmlCode
					, testModeOn		:: !Bool
					}

// Initial values

defaultUser			:== 0								// default id of user

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
// Overloaded Functions on Tasks
// ******************************************************************************************************

class 	(<<@) infixl 3 b ::  !(Task a) !b  -> (Task a)
instance <<@  Lifespan
where   (<<@) task lifespan			= setTaskLifespan
		where
			setTaskLifespan tst=:{options}
			
			= IF_Ajax 
				(IF_ClientServer															// we running both client and server
					(IF_ClientTasks												
						(if (options.tasklife == Client && (lifespan == TxtFile || lifespan == DataFile || lifespan == Database))
							(abort "Cannot make persistent storage on Client\n")
							(\tst -> task {tst & options.tasklife = lifespan}))						// assign option on client
						(\tst -> task {tst & options.tasklife = lifespan})tst							// assign option on server
					)
					(task {tst & options.tasklife = lifespan})								// assign option on server
				)
				(task {tst & options.tasklife = lifespan}) 									// assign option on server

instance <<@  StorageFormat
where   (<<@) task storageformat 	= \tst -> task {tst & options.taskstorage = storageformat}
instance <<@  Mode
where   (<<@) task mode 			= \tst -> task {tst & options.taskmode = mode}
instance <<@  GarbageCollect
where   (<<@) task gc 				= \tst -> task {tst & options.gc = gc}

class 	(@>>) infixl 7 b ::  !b !(Task a)   -> (Task a) | iData a
instance @>>  SubPage
where   (@>>) UseAjax task			= \tst -> IF_Ajax 
												(mkTaskThread UseAjax task tst)
												(newTask "Ajax Thread Disabled" task tst) 
		(@>>) OnClient  task 		= \tst -> IF_Ajax 
												(mkTaskThread OnClient task tst)
												(newTask "Client Thread Disabled" task tst) 

// ******************************************************************************************************
// *** wrappers for the end user, to be used in combination with an iData wrapper...
// ******************************************************************************************************

singleUserTask :: ![StartUpOptions] !(Task a) !*HSt -> (!Bool,Html,*HSt) | iData a 
singleUserTask startUpOptions maintask hst 
# userOptions			= determineUserOptions [ThreadStorage TxtFile:startUpOptions]
# tst					= initTst 0 Session userOptions.threadStorageLoc hst
# (exception,html,hst)	= startTstTask 0 False (False,[]) userOptions maintask tst
= mkHtmlExcep "singleUser" exception html hst

multiUserTask :: ![StartUpOptions] !(Task a) !*HSt -> (!Bool,Html,*HSt) | iData a 
multiUserTask startUpOptions maintask  hst 
# userOptions 			= determineUserOptions [TestModeOff, VersionCheck, ThreadStorage TxtFile:startUpOptions] 
# nusers				= case userOptions.showUsersOn of
							Nothing -> 0
							Just n	-> n
| nusers == 0			= singleUserTask startUpOptions maintask  hst 
# (idform,hst) 			= FuncMenu (Init,nFormId "User_Selected" 
							(0,[("User " +++ toString i,\_ -> i) \\ i<-[0..nusers - 1] ])) hst
# currentWorker			= snd idform.value
# tst					= initTst currentWorker TxtFile userOptions.threadStorageLoc hst
# (exception,html,hst) 	= startTstTask currentWorker True 
							(if userOptions.traceOn (idform.changed,idform.form) (False,[])) userOptions maintask tst
= mkHtmlExcep "multiUser" exception html hst

workFlowTask :: ![StartUpOptions] !(Task ((Bool,UserId),a)) !(UserId a -> LabeledTask b) !*HSt -> (!Bool,Html,*HSt) | iData b 
workFlowTask  startUpOptions taska userTask hst 
# userOptions 			= determineUserOptions startUpOptions 
# tst					= initTst -1 Session userOptions.threadStorageLoc hst
# (((new,i),a),tst=:{activated,html,hst})	
						= taska tst									// for doing the login 
| not activated
	# iTaskHeader		= [showHighLight "i-Task", showLabel " - Multi-User Workflow System ",Hr []]
	# iTaskInfo			= mkDiv "iTaskInfo" [showText "Login procedure... ", Hr []]
	= mkHtmlExcep "workFlow" True [Ajax [ ("thePage",iTaskHeader ++ iTaskInfo ++ noFilter html) // Login ritual cannot be handled by client
										]] hst
# userOptions 			= determineUserOptions [TestModeOff, VersionCheck, ThreadStorage TxtFile:startUpOptions] 
# tst					= initTst i Session userOptions.threadStorageLoc hst
# (exception,body,hst) 	= startTstTask i True (False,[]) userOptions (newUserTask ((new,i),a) <<@ TxtFile) tst
= mkHtmlExcep "workFlow" exception body hst
where
	noFilter :: HtmlTree -> HtmlCode
	noFilter (BT body) 			= body
	noFilter (_ @@: html) 		= noFilter html
	noFilter (_ -@: html) 		= noFilter html
	noFilter (htmlL +-+ htmlR) 	= [noFilter htmlL  <=>  noFilter htmlR]
	noFilter (htmlL +|+ htmlR) 	= noFilter htmlL <|.|> noFilter htmlR
	noFilter (DivCode str html) = noFilter html

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

startTstTask :: !Int !Bool  !(!Bool,!HtmlCode) UserStartUpOptions !(Task a) !*TSt -> (!Bool,!HtmlCode,!*HSt) | iData a 
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
	# iTaskInfo			= mkDiv "iTaskInfo" [showLabel "Cannot apply request. Version conflict. Please refresh the page!", Hr []]
	= (True,[Ajax [("thePage",iTaskHeader ++ iTaskInfo)]],hst)

// Here the iTasks are evaluated ...
													    
# maintask				= scheduleWorkflows maintask												// schedule all active tasks, not only maintask
# ((toServer,thrOwner,event,thrinfo,threads),tst=:{html,hst,trace,activated})	
						=  ((IF_Ajax 
								(startAjaxApplication thisUser pversion) 
								startMainTask
							) maintask) {tst & hst = hst, trace = if doTrace (Just []) Nothing, activated = True, html = BT []}

// epilogue

# newUserVersionNr		= 1 + if (pversion.versionNr > sversion) pversion.versionNr sversion					// increment user querie version number
# (_,hst)				= clearIncPUser thisUser (\_ -> newUserVersionNr) hst									// store in session
# (sversion,hst)	 	= setSVersionNr thisUser (\_ -> newUserVersionNr) hst									// store in persistent memory

# showCompletePage		= IF_Ajax (hd threads == [-1]) True
# (threadtrace,tst)	
						= if TraceThreads showThreadTable nilTable {tst & hst = hst} 
# threadsText			= if showCompletePage "" (foldl (+++) "" [showThreadNr tasknrs +++ " + " \\ tasknrs <- reverse threads])
# (processadmin,tst=:{hst})	= showWorkflows activated tst
# (threadcode,taskname,mainbuts,subbuts,seltask,hst)	
						= Filter showCompletePage thrOwner html hst

# iTaskInfo				= 	mkDiv "iTaskInfo" 
							case headerOff of
								Nothing ->
									(	IF_Ajax (IF_ClientServer (IF_ClientTasks [showLabel "Client: "] [showLabel "Server: "]) []) [] ++
										if multiuser 
											[showText "User: " , showLabel thisUser, showText " - "] [] ++
										if (thrinfo == "" ) [] [showLowLight thrinfo, showText " - "] ++
										if (multiuser && versionsOn)
											 [showText "Query " , showTrace ((sversion +++> " / " )<+++ appversion)] [] ++
										IF_Ajax
											( [showText " - Task#: ", showTrace (showTaskNr  event)] ++
											  if (isEmpty threads || showCompletePage) [] [showText " - Thread(s)#: ", showTrace threadsText]
											 ) [] ++
										[Br,Hr []]
									)
								Just userInfo -> userInfo
# iTaskTraceInfo		=	showOptions staticInfo.threadTableLoc ++ processadmin ++ threadtrace ++ [printTrace2 trace ]
| showCompletePage		=	(toServer,[Ajax [("thePage",	iTaskHeader ++
															iTaskInfo  ++
															if (doTrace && traceOn)
																	iTaskTraceInfo
																	[	leftright taskname subbuts
																		, mainbuts <=>  seltask
																	]
											)]
									] 
							,hst)
# (newthread,oldthreads)=	(hd threads, tl threads)
| otherwise				=	(toServer,[Ajax (	[("iTaskInfo", iTaskInfo)] ++			// header ino
											[(showTaskNr childthreads,[showText " "]) \\ childthreads <- oldthreads] ++ //clear childthreads, since parent thread don't need to be on this page
											[(showTaskNr newthread, if (isEmpty threadcode) seltask threadcode)]	// task info
										   )
									]
							,hst)
where
//	wrap maintask = scheduleWorkflows (newTask "main" (assignTaskTo False 0 ("main",maintask)))				
//	where
//		clearIStore hst=:{world}								/* would be nice but don't know how to clear this */
//		# world = if testModeOn deleteAllStateFiles id world
//		= (Void,{hst & world = world})

	leftright left right 
	=	Table [Tbl_Width (Percent 100)] 
			[Tr []	[ Td [] left
					, Td [Td_Align Aln_Right] right]
					]

	nilTable tst = 	([],tst)

	startMainTask :: !(Task a) !*TSt -> ((!Bool,!Int,!TaskNr,!String,![TaskNr]),*TSt) 	// No threads, always start from scratch		
	startMainTask task tst
	# (_,tst=:{activated}) = task tst
	= ((True,defaultUser,[0],if activated "iTask application has ended" "",[]),{tst & activated = activated})

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

	Filter :: !Bool !UserId !HtmlTree !*HSt -> *(![BodyTag],![BodyTag],![BodyTag],![BodyTag],![BodyTag],!*HSt)
	Filter wholepage thrOwner tree hst
	# startuser			= if wholepage defaultUser thrOwner
	# (threadcode,accu) = Collect thisUser startuser []((startuser,0,defaultWorkflowName,"main") @@: tree)  // KLOPT DIT WEL ??
	| isEmpty accu		= (threadcode,[],[],[],[],hst)
	# accu				= sortBy (\(i,_,_,_) (j,_,_,_) -> i < j) accu
	# (workflownames,subtasks) 						= unziptasks accu
	# ((mainSelected,mainButtons,chosenMain),hst) 	= mkTaskButtons True ("User " <+++ thisUser) thisUser [] 
															(initialOptions thisUser Session) workflownames hst 
	# (subtasksnames,tcode)							= unzipsubtasks (subtasks!!mainSelected)
	# ((taskSelected,subButtons,chosenTask),hst) 	= mkTaskButtons False ("User " <+++ thisUser <+++ "subtask" <+++ mainSelected) thisUser [] 
															(initialOptions thisUser Session) subtasksnames hst 
	# subButtons		= if (length subtasksnames > 1) subButtons []
	= (threadcode,[showMainLabel chosenMain, showTrace " / ", showLabel chosenTask],mainButtons,subButtons,tcode!!taskSelected,hst)
	where
		unziptasks :: ![(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])] -> (![WorkflowLabel],![[(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])]])
		unziptasks [] 			= ([],[])
		unziptasks all=:[(pid,wlabel,tlabel,tcode):tasks] 
		# (wsubtask,other) 		= span (\(mpid,_,_,_) ->  mpid == pid) all 
		# (wlabels,wsubtasks)	= unziptasks other
		= ([wlabel:wlabels],[wsubtask:wsubtasks])

		unzipsubtasks :: ![(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])] -> (![TaskLabel],![[BodyTag]])
		unzipsubtasks []		= ([],[])
		unzipsubtasks [(pid,wlabel,tlabel,tcode):subtasks]		
		# (labels,codes)		= unzipsubtasks subtasks
		= ([tlabel:labels],[tcode:codes])

	Collect :: !UserId !UserId ![(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])] !HtmlTree -> (![BodyTag],![(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])])
	Collect thisuser taskuser accu ((nuserid,processnr,workflowLabel,taskname) @@: tree) 	// Collect returns the wanted code, and the remaining code
	# (myhtml,accu)	= Collect thisuser nuserid accu tree									// Collect all code of this user belonging to this task
	| thisuser == nuserid && not (isEmpty myhtml)
							= ([],[(processnr,workflowLabel,taskname,myhtml):accu])
	| otherwise				= ([],accu)
	Collect thisuser taskuser accu (nuser -@: tree)
	| thisuser == nuser 	= ([],accu)
	| otherwise				= Collect thisuser taskuser accu tree
	Collect thisuser taskuser accu (tree1 +|+ tree2)
	# (lhtml,accu)	= Collect thisuser taskuser accu tree1
	# (rhtml,accu)	= Collect thisuser taskuser accu tree2
	= (lhtml <|.|> rhtml,accu)
	Collect thisuser taskuser accu (tree1 +-+ tree2)
	# (lhtml,accu)	= Collect thisuser taskuser accu tree1
	# (rhtml,accu)	= Collect thisuser taskuser accu tree2
	= ([lhtml <=> rhtml],accu)
	Collect thisuser taskuser accu (BT bdtg)
	| thisuser == taskuser	= (bdtg,accu)
	| otherwise				= ([],accu)
	Collect thisuser taskuser accu (DivCode id tree)
	# (html,accu)			= Collect thisuser taskuser accu tree
	| thisuser == taskuser 	= (mkDiv id html,accu)
	= ([],accu)


// ******************************************************************************************************
// Event handling for Ajax calls and Sapl handling on the client
// ******************************************************************************************************

// The following functions are defined to support "Ajax technologie" and Client site evaluation of i-Tasks.
// To make this possible, a part of the iTask task tree must be assigened to be a thread such that it can be evaluated as a stand-alone i-Task.
// The programmer has to decide which iTask should become a thread.
// For each event (iData triplet), the system will search for the thread to handle it.
// If a thread task is finished, the parent thread task is activated, and so on.
// Any action requiering the calculation of the Task Tree from scratch will be done one the server
// Watch it: the Client cannot create new Server threads

startAjaxApplication :: !Int !GlobalInfo !(Task a) !*TSt -> ((!Bool,!Int,TaskNr,!String,![TaskNr]),*TSt) 		// determines which threads to execute and calls them..
startAjaxApplication thisUser versioninfo maintask tst=:{tasknr,options,html,trace,userId}
# tst					= copyThreadTableFromClient	versioninfo tst				// synchronize thread tables of client and server, if applicable

// first determine whether we should start calculating the task tree from scratch starting at the root

# (mbevent,tst)			= getTripletTaskNrs tst									// see if there are any events, i.e. triplets received
| isNothing mbevent																// no events
						= startFromRoot versioninfo tasknr [tasknr] "No events, page refreshed" maintask tst			
# event					= fromJust mbevent										// event found
# (table,tst)			= ThreadTableStorage id tst								// read thread table
| isEmpty table																	// events, but no threads, evaluate main application from scratch
						= startFromRoot versioninfo event [tasknr] "No threads, page refreshed" maintask tst			
# (mbthread,tst)		= findParentThread event tst							// look for thread to evaluate
| isEmpty mbthread																// no thread can be found, happens e.g. when one switches from tasks
						= startFromRoot versioninfo event [tasknr] "No matching thread, page refreshed" maintask tst			
# thread 				= hd mbthread											// thread found
| isMember thread.thrTaskNr versioninfo.deletedThreads							// thread has been deleted is some past, version conflict
	# tst				= copyThreadTableToClient tst							// copy thread table to client
	= ((True,defaultUser,event,"Task does not exist anymore, please refresh",[tasknr]), tst)
| versioninfo.newThread															// newthread added by someone
						= startFromRoot versioninfo event [tasknr] "New tasks added, page refreshed" maintask tst			
| not (isEmpty versioninfo.deletedThreads) 										// some thread has been deleted										
						= startFromRoot versioninfo event [tasknr] "Tasks deleted, page refreshed" maintask tst			
| thread.thrUserId <> thisUser													// updating becomes too complicated
						= startFromRoot versioninfo event [tasknr] ("Thread of user " <+++ thread.thrUserId <+++ ", page refreshed") maintask tst			

// ok, we have found a matching thread

# (_,tst=:{activated}) 	= evalTaskThread thread {tst & html = BT []}			// evaluate the thread
| not activated																	// thread / task not yet finished
	# tst				= copyThreadTableToClient tst							// copy thread table to client
	= ((False,thisUser,event,"",[thread.thrTaskNr]),tst)						// no further evaluation, aks user for more input

# (mbthread,tst)		= findParentThread (tl thread.thrTaskNr) tst			// look for thread to evaluate
= doParent mbthread maintask event [thread.thrTaskNr] {tst & html = BT [], options = options}				// more to evaluate, call thread one level higher
where
	doParent [] maintask event accu tst											// no more parents of current event, do main task
						= startFromRoot versioninfo event [tasknr:accu] "No more threads, page refreshed" maintask {tst & html = BT []}			

	doParent [parent:next] maintask event accu tst								// do parent of current thread
	| parent.thrUserId <> thisUser												// updating becomes too complicated
						= startFromRoot versioninfo event [tasknr:accu] ("Parent thread of user " <+++ parent.thrUserId <+++ ", page refreshed") maintask {tst & html = BT []}			

	# (_,tst=:{activated}) 	= evalTaskThread parent {tst & html = BT []}		// start parent
	| not activated																// parent thread not yet finished
		# tst				= copyThreadTableToClient tst						// copy thread table to client
		= ((False,thisUser,event, "",[parent.thrTaskNr:accu]),tst)				// no further evaluation, aks user for more input
	# (mbthread,tst)		= findParentThread (tl parent.thrTaskNr) tst		// look for thread to evaluate
	= doParent mbthread maintask event [parent.thrTaskNr:accu] {tst & options = options}// continue with grand parent ...

startFromRoot :: !GlobalInfo !TaskNr ![TaskNr] !String !(Task a) !*TSt -> ((!Bool,!Int,TaskNr,!String,![TaskNr]),*TSt)
startFromRoot versioninfo eventnr tasknrs message maintask tst
=	IF_ClientServer																// we are running client server
		(IF_ClientTasks
			(stopClient eventnr tasknrs message)								// client cannot evaluate from root of task tree, give it up
			(evaluateFromRoot versioninfo eventnr tasknrs message maintask) tst	// sever can evaluate from scratch
		)
	(evaluateFromRoot versioninfo eventnr tasknrs message maintask tst)			// ajax can evaluate from scratch as well
where
	stopClient :: !TaskNr ![TaskNr]  !String  !*TSt -> ((!Bool,!Int,TaskNr,!String,![TaskNr]),*TSt)
	stopClient eventnr tasknrs message tst
	= ((True,defaultUser,eventnr,message,tasknrs), tst)
	
	evaluateFromRoot :: !GlobalInfo !TaskNr ![TaskNr] !String !(Task a) !*TSt -> ((!Bool,!Int,TaskNr,!String,![TaskNr]),*TSt)
	evaluateFromRoot versioninfo eventnr tasknrs message maintask tst
	# tst					= deleteAllSubTasks versioninfo.deletedThreads tst	// delete subtasks being obsolute
	# (_,tst) 				= maintask tst										// evaluate main application from scratch
	# tst=:{activated}		= copyThreadTableToClient tst						// copy thread table to client, if applicable
	# message				= if activated "iTask application finished" message
	= (((True,defaultUser,eventnr,message,tasknrs), {tst & activated = activated}))

// ******************************************************************************************************
// Html Printing Utilities...
// ******************************************************************************************************

mkDiv :: String HtmlCode -> HtmlCode
mkDiv id bodytag = [normaldiv]
where
	normaldiv = Div [`Div_Std [Std_Id id, Std_Class	"thread"]] bodytag


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

setSVersionNr :: !Int !(Int -> Int) !*HSt -> (!Int,!*HSt) 
setSVersionNr user f hst	
# (form,hst) = mkStoreForm (Init, nFormId (usersessionVersionNr user) 0 <@ NoForm) f hst
= (form.value,hst)

getTripletTaskNrs :: !*TSt -> *(Maybe TaskNr,*TSt)								// get list of tasknr belonging to events received
getTripletTaskNrs tst=:{hst = hst=:{states}}
# (triplets,states) = getAllTriplets states
= (lowestTaskNr [mkTasknr (getDigits s) \\ ((s,_,_),_) <- triplets | s%(0,5) == "iTask_"],{tst & hst = {hst & states = states}})
where
	getDigits s = takeWhile ((<>) '-') (stl (dropWhile ((<>) '_') (mkList s)))

	mkTasknr list = reverse (map digitToInt [c \\ c <- list | isDigit c])

	lowestTaskNr [] 	= Nothing
	lowestTaskNr [x:xs] = Just (lowest x xs)									// lowest number gives highest position in tree

	lowest :: TaskNr [TaskNr] -> TaskNr
	lowest x [] 	= x
	lowest x [y:ys]
	| x < y = lowest x ys
	= lowest y ys

