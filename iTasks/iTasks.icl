implementation module iTasks

// (c) iTask & iData Concept and Implementation by Rinus Plasmeijer, 2006-2008 - MJP

// iTasks library for defining interactive multi-user workflow tasks (iTask) for the web.
// Defined on top of the iData library.

import StdEnv, StdBimap, StdOrdList
import iDataSettings, iDataHandler, iDataTrivial, iDataButtons, iDataFormlib, iDataStylelib
import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string
import DrupBasic
import iTasksSettings

derive gForm 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, GlobalInfo, TaskThread, ThreadKind, Wid, WorkflowStatus, Maybe, []
derive gUpd 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, GlobalInfo, TaskThread, ThreadKind, Wid, WorkflowStatus, Maybe, []
derive gParse 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, GlobalInfo, TaskThread, ThreadKind, Wid, WorkflowStatus, Maybe
derive gPrint 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, GlobalInfo, TaskThread, ThreadKind, Wid, WorkflowStatus, Maybe
derive gerda 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, GlobalInfo, TaskThread, ThreadKind, Wid, WorkflowStatus
derive read 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, GlobalInfo, TaskThread, ThreadKind, Wid, WorkflowStatus
derive write 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, GlobalInfo, TaskThread, ThreadKind, Wid, WorkflowStatus

:: *TSt 		=	{ tasknr 		:: !TaskNr			// for generating unique form-id's
					, activated		:: !Bool   			// if true activate task, if set as result task completed	
					, userId		:: !Int				// id of user to which task is assigned
					, workflowName	:: !WorkflowName	// wid and name of the workflow process a task is part of
					, staticInfo	:: !StaticInfo		// info which does not change during a run
					, html			:: !HtmlTree		// accumulator for html code
					, options		:: !Options			// iData lifespan and storage format
					, trace			:: !Maybe [Trace]	// for displaying task trace
					, hst			:: !HSt				// iData state
					}
:: UserId		:== !Int
:: TaskNr		:== [Int]								// task nr i.j is adminstrated as [j,i]
:: WorkflowName	:== !(WorkflowId,WorkflowLabel)			// wid and name of the workflow process a task is part of
:: WorkflowId	:== !Int
:: WorkflowLabel:== !String
:: HtmlTree		=	BT HtmlCode							// simple code
				|	(@@:) infix  0 TaskName HtmlTree	// code with id of user attached to it
				|	(-@:) infix  0 UserId 	HtmlTree	// skip code with this id if it is the id of the user 
				|	(+-+) infixl 1 HtmlTree HtmlTree	// code to be placed next to each other				
				|	(+|+) infixl 1 HtmlTree HtmlTree	// code to be placed below each other				
				|	DivCode String HtmlTree				// code that should be labeled with a div, used for Ajax and Client technology
:: TaskName		:== !(UserId,WorkflowName,!TaskLabel)	// id of user, workflow process name, task name
:: Options		=	{ tasklife		:: !Lifespan		// default: Session		
					, taskstorage	:: !StorageFormat	// default: PlainString
					, taskmode		:: !Mode			// default: Edit
					, gc			:: !GarbageCollect	// default: Collect
					}
:: StaticInfo	=	{ currentUserId	:: UserId			// id of application user 
					, threadTableLoc:: !Lifespan		// where to store the server thread table, default is Session
					}
:: GarbageCollect =	Collect | NoCollect
:: Trace		=	Trace !TraceInfo ![Trace]			// traceinfo with possibly subprocess
:: TraceInfo	:== Maybe (!Bool,!(!UserId,!TaskNr,!Options,!String,!String))	// Task finished? who did it, task nr, task name (for tracing) value produced

:: ThreadTable	:== [TaskThread]						// thread table is used for Ajax and OnClient options
:: TaskThread	=	{ thrTaskNr			:: !TaskNr		// task number to recover
					, thrUserId			:: !UserId		// which user has to perform the task
					, thrWorkflowName	:: !WorkflowName// what was the name of workflow process it was part off
					, thrOptions		:: !Options		// options of the task
					, thrCallback		:: !String		// serialized callback function for the server
					, thrCallbackClient	:: !String		// serialized callback function for the client (optional, empty if not applicable)
					, thrKind			:: !ThreadKind 	// kind of thread
					, thrVersionNr		:: !Int			// version number of application when thread was created
					}
:: ThreadKind	=	ServerThread						// Thread which can only be executed on Server
				|	ClientServerThread					// Thread preferably to be executed on Client, but also runs on Server
				|	ClientThread						// Thread which can only be executed on the Client 
				|	ExceptionHandler					// Exception handler only works on server
				|	AnyThread							// Used for garbage collection
:: GlobalInfo	=	{ versionNr			:: !Int			// latest querie number of a user
					, newThread			:: !Bool		// is a new thread assigned to this user (used for Ajax)?
					, deletedThreads	:: ![TaskNr]	// are there threads deleted (used for Ajax)?
					}
:: UserStartUpOptions
				= 	{ traceOn			:: !Bool			
					, threadStorageLoc	:: !Lifespan		
					, showUsersOn		:: !Maybe Int	
					, versionCheckOn	:: !Bool
					, headerOff			:: !Maybe HtmlCode
					, testModeOn		:: !Bool
					}
:: Wid a			= Wid WorkflowName					// id of workflow process
:: WorflowProcess 	= ActiveWorkflow 	!(!UserId,!WorkflowLabel) !(TCl Dynamic)
					| SuspendedWorkflow !(!UserId,!WorkflowLabel) !(TCl Dynamic)
					| FinishedWorkflow 	!(!UserId,!WorkflowLabel) !Dynamic !(TCl Dynamic)
					| DeletedWorkflow	!(!UserId,!WorkflowLabel)

// Initial values

defaultUser			:== 0								// default id of user
defaultWorkflowName :== "start"							// name of initial workflow process
defaultWid			:== 0								// initial workflow process id

initTst :: UserId !Lifespan !*HSt -> *TSt
initTst thisUser location hst
				=	{ tasknr		= [-1]
					, activated 	= True
					, staticInfo	= initStaticInfo thisUser location
					, userId		= if (thisUser >= 0) defaultUser thisUser
					, workflowName	= (defaultWid,defaultWorkflowName)
					, html 			= BT []
					, trace			= Nothing
					, hst 			= hst
					, options 		= initialOptions
					}

initialOptions :: Options
initialOptions	=	{ tasklife 		= Session
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

instance == GarbageCollect
where
	(==) Collect   Collect 		= True
	(==) NoCollect NoCollect 	= True
	(==) _ _ 					= False
	
instance == ThreadKind
where
	(==) ServerThread     		ServerThread 	   		= True
	(==) ClientThread    		ClientThread 	   		= True
	(==) ClientServerThread    	ClientServerThread 	   	= True
	(==) ExceptionHandler 		ExceptionHandler		= True
	(==) AnyThread    			_				 	   	= True
	(==) _ 						_ 						= False

instance toString ThreadKind
where
	toString ServerThread     	= "ServerThread"
	toString ClientThread    	= "ClientThread"
	toString ClientServerThread	= "ClientServerThread"
	toString ExceptionHandler 	= "ExceptionHandler"
	toString AnyThread    		= "AnyThread"
	toString _    				= "??? print error in thread"


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
// *** wrappers for the end user, to be used in combination with an iData wrapper...
// ******************************************************************************************************

singleUserTask :: ![StartUpOptions] !(Task a) !*HSt -> (!Bool,Html,*HSt) | iData a 
singleUserTask startUpOptions maintask hst 
# userOptions			= determineUserOptions startUpOptions
# tst					= initTst 0 userOptions.threadStorageLoc hst
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
# tst					= initTst currentWorker userOptions.threadStorageLoc hst
# (exception,html,hst) 	= startTstTask currentWorker True 
							(if userOptions.traceOn (idform.changed,idform.form) (False,[])) userOptions maintask tst
= mkHtmlExcep "multiUser" exception html hst

workFlowTask :: ![StartUpOptions] !(Task ((Bool,UserId),a)) !(UserId a -> LabeledTask b) !*HSt -> (!Bool,Html,*HSt) | iData b 
workFlowTask  startUpOptions taska userTask hst 
# userOptions 						= determineUserOptions startUpOptions 
# tst								= initTst -1 userOptions.threadStorageLoc hst
# (((new,i),a),tst=:{activated,html,hst})	= taska tst									// for doing the login 
| not activated
	# iTaskHeader					= [showHighLight "i-Task", showLabel " - Multi-User Workflow System ",Hr []]
	# iTaskInfo						= mkDiv "iTaskInfo" [showText "Login procedure... ", Hr []]
	= mkHtmlExcep "workFlow" True [Ajax [ ("thePage",iTaskHeader ++ iTaskInfo ++ noFilter html) // Login ritual cannot be handled by client
										]] hst
# userOptions 						= determineUserOptions [TestModeOff, VersionCheck, ThreadStorage TxtFile:startUpOptions] 
# tst								= initTst i userOptions.threadStorageLoc hst
# (exception,body,hst) 				= startTstTask i True (False,[]) userOptions (newUserTask ((new,i),a) <<@ TxtFile) tst
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


// ******************************************************************************************************
// Main routine for the creation of the workflow page
// ******************************************************************************************************

startTstTask :: !Int !Bool  !(!Bool,!HtmlCode) UserStartUpOptions !(Task a) !*TSt -> (!Bool,!HtmlCode,!*HSt) | iData a 
startTstTask thisUser multiuser (userchanged,multiuserform) useroptions=:{traceOn, threadStorageLoc, showUsersOn, versionCheckOn, headerOff, testModeOn} maintaskorg tst=:{hst,tasknr,staticInfo}

// prologue

# maintask				= scheduleWorkflows maintaskorg												// force main process to start on tasknr 0.1

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
							[ Td [] [Img [Img_Src (ThisExe +++ "/scleanlogo.jpg"),Img_Align Alo_Middle]
									,showHighLight " i -Task", showLabel " Workflow System "]
							, Td [Td_Align Aln_Right] (multiuserform ++ refresh.form ++ ifTraceOn traceAsked.form)] ]]++
							[Hr []]
| versionconflict	 
	# iTaskInfo			= mkDiv "iTaskInfo" [showLabel "Cannot apply request. Version conflict. Please refresh the page!", Hr []]
	= (True,[Ajax 	[ ("thePage",iTaskHeader ++ iTaskInfo)
						]
				],hst)


// Here the iTasks are evaluated ...
													    
# ((toServer,thrOwner,event,thrinfo,threads),tst=:{html,hst,trace,activated})	
						= calculateNewWorkflows ((IF_Ajax (startAjaxApplication thisUser pversion) startMainTask)
							maintask) {tst & hst = hst, trace = if doTrace (Just []) Nothing, activated = True, html = BT []}

// epilogue

# newUserVersionNr		= 1 + if (pversion.versionNr > sversion) pversion.versionNr sversion					// increment user querie version number
# (_,hst)				= clearIncPUser thisUser (\_ -> newUserVersionNr) hst									// store in session
# (sversion,hst)	 	= setSVersionNr thisUser (\_ -> newUserVersionNr) hst									// store in persistent memory

# showCompletePage		= IF_Ajax (hd threads == [-1]) True
# (threadtrace,tst=:{hst})	
						= if TraceThreads showThreadTable nilTable {tst & hst = hst} 
# threadsText			= if showCompletePage "" (foldl (+++) "" [showThreadNr tasknrs +++ " + " \\ tasknrs <- reverse threads])
# (processadmin,hst)	= showWorkflows activated hst
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
											  if (isNil threads || showCompletePage) [] [showText " - Thread(s)#: ", showTrace threadsText]
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
											[(showTaskNr newthread, if (isNil threadcode) seltask threadcode)]	// task info
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


	mkSTable2 :: [HtmlCode] -> BodyTag
	mkSTable2 table
	= Table []	(mktable table)
	where
		mktable table 	= [Tr [] (mkrow rows) \\ rows <- table]	
		mkrow rows 		= [Td [Td_VAlign Alo_Top] [row] \\ row <- rows] 

	Filter :: !Bool !UserId !HtmlTree !*HSt -> *(![BodyTag],![BodyTag],![BodyTag],![BodyTag],![BodyTag],!*HSt)
	Filter wholepage thrOwner tree hst
	# startuser			= if wholepage defaultUser thrOwner
	# (threadcode,accu) = Collect thisUser startuser [] ((startuser,(defaultWid,defaultWorkflowName),"main") @@: tree)  // KLOPT DIT WEL ??
	| isNil accu		= (threadcode,[],[],[],[],hst)
	# accu				= sortBy (\((i,_),_,_) ((j,_),_,_) -> i < j) accu
	# (workflownames,subtasks) 						= unziptasks accu
	# ((mainSelected,mainButtons,chosenMain),hst) 	= mkTaskButtons True ("User " <+++ thisUser) thisUser [] initialOptions workflownames hst 
	# (subtasksnames,tcode)							= unzipsubtasks (subtasks!!mainSelected)
	# ((taskSelected,subButtons,chosenTask),hst) 	= mkTaskButtons False ("User " <+++ thisUser <+++ "subtask" <+++ mainSelected) 
																							thisUser [] initialOptions subtasksnames hst 
	# subButtons		= if (length subtasksnames > 1) subButtons []
	= (threadcode,[showMainLabel chosenMain, showTrace " / ", showLabel chosenTask],mainButtons,subButtons,tcode!!taskSelected,hst)
	where
		unziptasks [] 			= ([],[])
		unziptasks all=:[((wid,wlabel),tlabel,tcode):tasks] 
		# (wsubtask,other) 		= span (\((mwid,_),_,_) ->  mwid == wid) all 
		# (wlabels,wsubtasks)	= unziptasks other
		= ([wlabel:wlabels],[wsubtask:wsubtasks])

		unzipsubtasks []		= ([],[])
		unzipsubtasks [(_,tlabel,tcode):subtasks]		
		# (labels,codes)		= unzipsubtasks subtasks
		= ([tlabel:labels],[tcode:codes])

	Collect :: !UserId !UserId [(WorkflowName,TaskLabel,[BodyTag])] !HtmlTree -> (![BodyTag],![(WorkflowName,TaskLabel,[BodyTag])])
	Collect thisuser taskuser accu ((ntaskuser,workflowName,taskname) @@: tree) 	// Collect returns the wanted code, and the remaining code
	# (myhtml,accu)	= Collect thisuser ntaskuser accu tree							// Collect all code of this user belonging to this task
	| thisuser == ntaskuser && not (isNil myhtml)
							= ([],[(workflowName,taskname,myhtml):accu])
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

	showThreadTable :: *TSt -> (HtmlCode,*TSt)	// watch it: the actual threadnumber stored is one level deaper, so [-1:nr] instead of nr !!
	showThreadTable tst=:{staticInfo}
	# thisUser		= staticInfo.currentUserId
	# (tableS,tst)	= ThreadTableStorage id tst													// read thread table from server
	# tableS		= sortBy (\e1=:{thrTaskNr = t1} e2=:{thrTaskNr =t2} = t1 < t2) tableS
	# (tableC,tst)	= IF_ClientServer
						(\tst -> ClientThreadTableStorage id tst)								// read thread table from client
						(\tst -> ([],tst)) tst
	
	# tableC		= sortBy (\e1=:{thrTaskNr = t1} e2=:{thrTaskNr =t2} = t1 < t2) tableC
	# bodyS			= 	if (isNil tableS)
						[]
						[showLabel "Server Thread Table: ",
						STable []	(   [[showTrace "UserNr:", showTrace "Kind:", showTrace "TaskNr:", showTrace "Created:"
										 ,showTrace "Storage"]] ++
										[	[ showText (toString entry.thrUserId)
											, showText (toString entry.thrKind)
											, showText (showThreadNr entry.thrTaskNr)
											, showText (toString entry.thrVersionNr)
											, showText (toString entry.thrOptions.tasklife)
											] 
											\\ entry <- tableS
										]
									),
						Hr []
						]
	# bodyC			= if (isNil tableC)
						[]
						[showLabel ("Client User " +++ toString thisUser +++ " Thread Table: "),
						STable []	(   [[showTrace "UserNr:", showTrace "Kind:", showTrace "TaskNr:", showTrace "Created:"
										 ,showTrace "Storage"]] ++
										[	[ showText (toString entry.thrUserId)
											, showText (toString entry.thrKind)
											, showText (showThreadNr entry.thrTaskNr)
											, showText (toString entry.thrVersionNr)
											, showText (toString entry.thrOptions.tasklife)
											] 
											\\ entry <- tableC
										]
									),
						Hr []
						]
	= (bodyS ++ bodyC,tst)

mkTaskButtons :: !Bool !String !Int !TaskNr !Options ![String] *HSt -> ((Int,HtmlCode,String),*HSt)
mkTaskButtons vertical myid userId tasknr info btnnames hst
# btnsId			= iTaskId userId tasknr (myid <+++ "genBtns")
# myidx				= length btnnames
//| myidx == 1		= ((0,[],[]),hst)													// no task button if there is only one task to choose from
# (chosen,hst)		= SelectStore (myid,myidx) tasknr info id hst						// which choice was made in the past
# (buttons,hst)		= SelectButtons Init btnsId info (chosen,btnnames) hst				// create buttons
# (chosen,hst)		= SelectStore (myid,myidx) tasknr info  buttons.value hst			// maybe a new button was pressed
# (buttons,hst)		= SelectButtons Set btnsId info (chosen,btnnames) hst				// adjust look of that button
= ((chosen,buttons.form,btnnames!!chosen),hst)
where
	SelectButtons init id info (idx,btnnames) hst 
		= if vertical
			(TableFuncBut2 (init,pageFormId info id [[(mode idx n, but txt,\_ -> n)] \\ txt <- btnnames & n <- [0..]]) hst)
			(TableFuncBut2 (init,pageFormId info id [[(mode idx n, but txt,\_ -> n) \\ txt <- btnnames & n <- [0..]]]) hst)
	but i = iTaskButton i

	mode i j
	| i==j = Display

	= Edit

	SelectStore :: !(String,Int) !TaskNr !Options (Int -> Int) *HSt -> (Int,*HSt)
	SelectStore (myid,idx) tasknr info fun hst 
	# storeId 			= iTaskId userId tasknr (myid <+++ "BtnsS" <+++ idx)
	# (storeform,hst)	= mkStoreForm (Init,storageFormId info storeId 0) fun hst
	= (storeform.value,hst)

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

startAjaxApplication :: !Int !GlobalInfo !(Task a) !*TSt -> ((!Bool,!Int,TaskNr,!String,![TaskNr]),*TSt) 		// determines which threads to execute and calls them..
startAjaxApplication thisUser versioninfo maintask tst=:{tasknr,options,html,trace,userId}
# tst					= copyThreadTableFromClient	versioninfo tst				// synchronize thread tables of client and server, if applicable

// first determine whether we should start calculating the task tree from scratch starting at the root

# (mbevent,tst)			= getTripletTaskNrs tst									// see if there are any events, i.e. triplets received
| isNothing mbevent																// no events
						= startFromRoot versioninfo tasknr [tasknr] "No events, page refreshed" maintask tst			
# event					= fromJust mbevent										// event found
# (table,tst)			= ThreadTableStorage id tst								// read thread table
| isNil table																	// events, but no threads, evaluate main application from scratch
						= startFromRoot versioninfo event [tasknr] "No threads, page refreshed" maintask tst			
# (mbthread,tst)		= findParentThread event tst							// look for thread to evaluate
| isNil mbthread																// no thread can be found, happens e.g. when one switches from tasks
						= startFromRoot versioninfo event [tasknr] "No matching thread, page refreshed" maintask tst			
# thread 				= hd mbthread											// thread found
| isMember thread.thrTaskNr versioninfo.deletedThreads							// thread has been deleted is some past, version conflict
	# tst				= copyThreadTableToClient tst							// copy thread table to client
	= ((True,defaultUser,event,"Task does not exist anymore, please refresh",[tasknr]), tst)
| versioninfo.newThread															// newthread added by someone
						= startFromRoot versioninfo event [tasknr] "New tasks added, page refreshed" maintask tst			
| not (isNil versioninfo.deletedThreads) 										// some thread has been deleted										
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

// ******************************************************************************************************
// Workflow process management
// ******************************************************************************************************

workflowProcessStoreName :== "Application" +++  "-ProcessTable"

derive gForm	WorflowProcess
derive gUpd		WorflowProcess
derive gPrint	WorflowProcess
derive gParse	WorflowProcess

gPrint{|Dynamic|} dyn pst 	= gPrint{|*|} (dynamic_to_string dyn) pst
gParse{|Dynamic|} expr 		= case parseString expr of
								(Just string) 	= Just (string_to_dynamic {s` \\ s` <-: string})
								Nothing			= Nothing
where
	parseString :: Expr -> Maybe String
	parseString expr = gParse{|*|} expr
	
gForm{|Dynamic|} (init, formid) hst = ({changed=False,form=[],value=formid.ival},(incrHSt 1 hst))
gUpd{|Dynamic|} (UpdSearch _ 0) a 	= (UpdDone,a)
gUpd{|Dynamic|} (UpdSearch v i) a 	= (UpdSearch v (i-1),a)
gUpd{|Dynamic|} (UpdCreate c) a 	= (UpdCreate c,dynamic 0)
gUpd{|Dynamic|} UpdDone a 			= (UpdDone,a)

workflowProcessStore ::  !([WorflowProcess] -> [WorflowProcess]) !*HSt -> (![WorflowProcess],!*HSt) 
workflowProcessStore wfs hst	
# (form,hst) = mkStoreForm (Init, pFormId workflowProcessStoreName [] <@ NoForm) wfs hst
= (form.value,hst)

scheduleWorkflows :: !(Task a) -> (Task a) | iData a
scheduleWorkflows maintask = scheduleWorkflows`
where
	scheduleWorkflows` tst=:{hst} 
	# (a,tst=:{activated,hst}) 	= newTask defaultWorkflowName (assignTaskTo False 0 ("main",maintask)) tst	// start maintask
	# (wfls,hst) 				= workflowProcessStore id hst												// read workflow process administration
	# (done,tst)				= calculateWorkflows True wfls 0 {tst & hst = hst,activated = True}				// all added workflows processes are inspected (THIS NEEDS TO BE OPTIMIZED AT SOME STAGE)
	= (a,{tst & activated = activated && done})																// whole application ends when all processes have ended

calculateWorkflows done [] _ tst = (done,tst)
calculateWorkflows done [ActiveWorkflow _ (TCl dyntask):wfls] procid tst
# (_,tst=:{activated}) = dyntask {tst & activated = True}
= calculateWorkflows (done && activated) wfls (inc procid) {tst & activated = activated}
calculateWorkflows done [SuspendedWorkflow _ _:wfls] procid tst
= calculateWorkflows done wfls (inc procid) tst
calculateWorkflows done [FinishedWorkflow _ _ (TCl dyntask):wfls] procid tst	// just to show result in trace..
# (_,tst) = dyntask tst
= calculateWorkflows done wfls (inc procid) tst
calculateWorkflows done [DeletedWorkflow _:wfls] procid tst
= calculateWorkflows done wfls (inc procid) tst

calculateNewWorkflows main tst=:{hst} 
# (wfls,hst) 				= workflowProcessStore id hst				// read workflow process administration
# lengthwfls				= length wfls
# (res,tst=:{hst})			= main {tst & hst = hst}					// calculate workflows
# (nwfls,hst) 				= workflowProcessStore id hst				// read workflow process administration
# lengthnwfls				= length nwfls
| lengthnwfls > lengthwfls 												// more workflows processes have been added in the mean time
	# (done,tst) = (calculateWorkflows True (drop (length wfls) nwfls) lengthwfls) {tst & hst = hst,activated = True}	// calculate this one as well
	= calculateNewWorkflows (\tst -> (res,tst)) tst
= (res,{tst & hst = hst})

spawnWorkflow :: !UserId !Bool !(LabeledTask a) -> Task (Wid a) | iData a
spawnWorkflow userid active (label,task) = \tst=:{options,staticInfo} -> (newTask ("spawn " +++ label) (spawnWorkflow` options)<<@ staticInfo.threadTableLoc) tst
where
	spawnWorkflow` options tst=:{hst}
	# (wfls,hst) 		= workflowProcessStore id hst							// read workflow process administration
	# processid			= length wfls + 1										// process id currently given by length list, used as offset in list
	# wfl				= mkdyntask options processid task 						// convert user task in a dynamic task
	# nwfls				= wfls ++ [if active ActiveWorkflow SuspendedWorkflow (userid,label) wfl]					// turn task into a dynamic task
	# (wfls,hst) 		= workflowProcessStore (\_ -> nwfls) hst				// write workflow process administration
	= (Wid (processid,label),{tst & hst = hst, activated = True})

	mkdyntask options processid task = TCl (\tst -> convertTask processid label task 
										{tst & tasknr = [processid - 1],activated = active,userId = userid, options = options,workflowName = (processid,label)})
	
	convertTask processid label task tst
	# (a,tst=:{hst,activated})		= newTask label (assignTaskTo False userid ("main",task)) tst//newTask label task tst			
	# dyn							= dynamic a
	| not activated					= (dyn,tst)									// not finished, return
	# (wfls,hst) 					= workflowProcessStore id hst				// read workflow process administration
	# wfls							= case (wfls!!(processid - 1)) of			// update process administration
											(ActiveWorkflow _ entry) -> updateAt (processid - 1) (FinishedWorkflow (userid,label) dyn entry) wfls
											_ -> wfls
	# (wfls,hst) 					= workflowProcessStore (\_ -> wfls) hst		// write workflow process administration
	= (dyn,{tst & hst = hst})												

waitForWorkflow :: !(Wid a) -> Task a | iData a
waitForWorkflow (Wid (processid,label)) = newTask ("waiting for " +++ label) waitForResult`
where
	waitForResult` tst=:{hst}
	# (wfls,hst) 		= workflowProcessStore id hst							// read workflow process administration
	# (done,val)		= case (wfls!!(processid - 1)) of						// update process administration
								(FinishedWorkflow _ (val::a^) _) -> (True,val)	// finished
								_ -> (False,createDefault)						// not yet
	= (val,{tst & hst = hst, activated = done})									// return value and release when done

deleteWorkflow :: !(Wid a) -> Task Bool 
deleteWorkflow (Wid (processid,label)) = newTask ("delete " +++ label) deleteWorkflow`
where
	deleteWorkflow` tst=:{hst}
	# (wfls,hst) 		= workflowProcessStore id hst							// read workflow process administration
	# nwfls				= updateAt (processid - 1) (DeletedWorkflow (-1,label)) wfls	// delete entry in table
	# (wfls,hst) 		= workflowProcessStore (\_ -> nwfls) hst				// update workflow process administration
	# tst				= deleteSubTasksAndThreads [processid] {tst & hst = hst}		// delete all iTask storage of this process ...
	= (True,{tst & activated = True})								// if everything is fine it should always succeed

suspendWorkflow :: !(Wid a) -> Task Bool
suspendWorkflow (Wid (processid,label)) = newTask ("suspend " +++ label) deleteWorkflow`
where
	deleteWorkflow` tst=:{hst}
	# (wfls,hst) 		= workflowProcessStore id hst							// read workflow process administration
	# (ok,nochange,wfl)	= case (wfls!!(processid - 1)) of
							(ActiveWorkflow label entry) -> (True,False,SuspendedWorkflow label entry)
							(DeletedWorkflow label) -> (False,True,DeletedWorkflow label) // a deleted workflow cannot be suspendend
							wfl -> (True,True,wfl)								// in case of finsihed or already suspended flows
	| nochange			= (ok,{tst & hst = hst, activated = True})				// no change needed
	# nwfls				= updateAt (processid - 1) wfl wfls						// update entry
	# (wfls,hst) 		= workflowProcessStore (\_ -> nwfls) hst				// update workflow process administration
	= (ok,{tst & hst = hst, activated = True})									// if everything is fine it should always succeed

activateWorkflow :: !(Wid a) -> Task Bool
activateWorkflow (Wid (processid,label)) = newTask ("activate " +++ label) activateWorkflow`
where
	activateWorkflow` tst=:{hst}
	# (wfls,hst) 		= workflowProcessStore id hst							// read workflow process administration
	# (ok,nochange,wfl)	= case (wfls!!(processid - 1)) of
							(SuspendedWorkflow label entry) -> (True,False,ActiveWorkflow label entry)
							(DeletedWorkflow label) -> (False,True,DeletedWorkflow label) // a deleted workflow cannot be suspendend
							wfl -> (True,True,wfl)								// in case of finsihed or already activated flows
	| nochange			= (ok,{tst & hst = hst, activated = True})				// no change needed
	# nwfls				= updateAt (processid - 1) wfl wfls						// update entry
	# (wfls,hst) 		= workflowProcessStore (\_ -> nwfls) hst				// update workflow process administration
	= (ok,{tst & hst = hst, activated = True})									// if everything is fine it should always succeed

getWorkflowStatus :: !(Wid a) -> Task WorkflowStatus
getWorkflowStatus (Wid (processid,label)) = newTask ("get status " +++ label) getWorkflowStatus`
where
	getWorkflowStatus` tst=:{hst}
	# (wfls,hst) 		= workflowProcessStore id hst							// read workflow process administration
	# status			= case (wfls!!(processid - 1)) of
							(ActiveWorkflow _ _) 		-> WflActive
							(SuspendedWorkflow _ _) 	-> WflSuspended
							(FinishedWorkflow _ _ _) 	-> WflFinished
							(DeletedWorkflow _) 		-> WflDeleted		
	= (status,{tst & hst = hst, activated = True})									// if everything is fine it should always succeed

showWorkflows :: !Bool !*HSt -> (![BodyTag],*HSt)
showWorkflows alldone hst
# (wfls,hst) 		= workflowProcessStore id hst								// read workflow process administration
= (mkTable wfls,hst)
where
	mkTable []		= []
	mkTable wfls	=	[showLabel ("Workflow Process Table:"),
						STable []	(   [ [showTrace "Workflow Id:", showTrace "User Id:", showTrace "Task Name:", showTrace "Status:"]
										, [Txt "0" , Txt "0", Txt defaultWorkflowName, if alldone (Txt "Finished") (Txt "Active")] 
										: [[Txt (toString i)] ++ showStatus wfl \\ wfl <- wfls & i <- [1..]]
										]
									),
						Hr []
						]
	showStatus (ActiveWorkflow 	 	(userid,label) dyntask)		= [Txt (toString userid), Txt label, Txt "Active"]
	showStatus (SuspendedWorkflow 	(userid,label) dyntask)		= [Txt (toString userid), Txt label, Txt "Suspended"]
	showStatus (FinishedWorkflow 	(userid,label) dyn dyntask)	= [Txt (toString userid), Txt label, Txt "Finished"]
	showStatus (DeletedWorkflow  	(userid,label))				= [Txt (toString userid), Txt label, Txt "Deleted"]

// ******************************************************************************************************
// Thread Creation and Deletion
// ******************************************************************************************************

mkTaskThread :: !SubPage !(Task a) -> Task a 	| iData a										
// wil only be called with IF_Ajax enabled
mkTaskThread UseAjax taska 
= IF_Ajax 																		// create an thread only if Ajax is enabled
	(IF_ClientServer															// we running both client and server
		(IF_ClientTasks												
			(abort "Cannot make Server thread on Client\n")						// cannot create server thread on client
			(newTask "Server Thread" (mkTaskThread2 ServerThread taska))		// create client thread, but executed on server
		)
		(newTask "Ajax Thread" (mkTaskThread2 ServerThread taska))				// create a server thread, no clients
	)
	taska 																		// no threads made at all
mkTaskThread OnClient taska 
= IF_Ajax 																		// create threads only if Ajax is enabled
	(IF_ClientServer															// we running both client and server
		(IF_ClientTasks												
			(newTask "Client Thread" (mkTaskThread2 ClientThread  taska))		// create and execute client thread on client
			(newTask "Client Thread" (mkTaskThread2 ClientServerThread taska)) 	// create client thread, but executed on server
		)
		(newTask "Ajax Thread (no Client)" (mkTaskThread2 ServerThread taska))	// create a server thread, no clients
	)
	taska 																		// no threads made at all

mkTaskThread2 :: !ThreadKind !(Task a) -> Task a 								// execute a thread
mkTaskThread2 threadkind task = evalTask																
where
	evalTask tst=:{tasknr,activated,options,userId,staticInfo,workflowName}					// thread - task is not yet finished
	# (mbthread,tst)	= findThreadInTable threadkind tasknr tst				// look if there is an entry for this task
	| isNothing mbthread														// not yet, insert new entry		
		# options 			= {options & tasklife = case threadkind of
													ServerThread 		= options.tasklife // staticInfo.threadTableLoc
													ClientServerThread 	= Client
													ClientThread 		= Client
													ExceptionHandler 	= options.tasklife  // staticInfo.threadTableLoc
													else 				= abort "Storing unexpected thread kind"}
		# (versionNr,tst)	= getCurrentAppVersionNr tst						// get current version number of the application
		# tst = insertNewThread 	{ thrTaskNr 		= tasknr
									, thrUserId 		= userId
									, thrWorkflowName	= workflowName
									, thrOptions 		= options
									, thrCallback 		= serializeThread task	
									, thrCallbackClient = serializeThreadClient task 
									, thrKind			= threadkind
									, thrVersionNr		= versionNr
									} tst 
		= evalTask tst															// try it again, entry point should now be there
	# (_,thread)		= fromJust mbthread										// entry point found
	# tst				= if (options.tasklife == Client && 					// if iTasks for this thread are stored on client
								(thread.thrOptions.tasklife <> Client ||		// but new thread is not to be stored on client 
								 staticInfo.currentUserId <> userId))			// or new thread is for someone else
							forceEvalutionOnServer id tst						// storing on client is no longer possible
	= evalTaskThread thread tst													// and evaluate it

forceEvalutionOnServer tst
=	IF_ClientServer																// we running both client and server
		(IF_ClientTasks												
			id																	// on client we cannot do anything
			forceEvalutionOnServer`												// force evaluation on server
			tst
		)
	tst
where
	forceEvalutionOnServer` tst=:{userId,tasknr} 
	# (mbparent,tst=:{hst})	= findNoClientParentThread tasknr tst
	| isNothing mbparent = {tst & hst = hst}									// cannot find parent, we should abort ????
	# parent 	= fromJust mbparent												// parent thread found which lifespan should be modified 
	# hst		= changeLifespanIData (iTaskId userId (tl parent.thrTaskNr) "") Client parent.thrOptions.tasklife hst
	# tst 		= changeLifespanThreadTable parent.thrTaskNr parent.thrOptions.tasklife {tst & hst = hst}
	= tst

	findNoClientParentThread tasknr tst
	# (mbparent,tst) 	= findParentThread tasknr tst
	| isNil mbparent 	= (Nothing,tst)
	# parent 			= hd mbparent										// thread found
	| parent.thrOptions.tasklife == Client = findNoClientParentThread (tl parent.thrTaskNr) tst
	= (Just parent,tst)

	changeLifespanThreadTable :: !TaskNr !Lifespan *TSt -> *TSt						// change lifespan of of indicated thread in threadtable
	changeLifespanThreadTable tasknr lifespan tst
	# (table,tst)	= ThreadTableStorage id tst										// read thread table on server
	# revtasknr		= reverse (tl tasknr)									
	# ntable 		= [{thread & thrOptions.tasklife = if (isChild revtasknr thread.thrTaskNr) lifespan thread.thrOptions.tasklife} \\ thread <- table]
	# (_,tst)		= ThreadTableStorage (\_ -> ntable) tst							// store thread table
	= tst

evalTaskThread :: !TaskThread -> Task a 										// execute the thread !!!!
evalTaskThread entry=:{thrTaskNr,thrUserId,thrOptions,thrCallback,thrCallbackClient,thrKind} = evalTaskThread` 
where
	evalTaskThread` tst=:{tasknr,options,userId,staticInfo,html}									
	# newThrOptions					= if (thrOptions.tasklife == Client && thrUserId <> staticInfo.currentUserId) 
											{thrOptions & tasklife = Temp}		// the information is not intended for this client, so dot store
											thrOptions
			
	# (a,tst=:{activated,html=nhtml}) 	
		= IF_ClientTasks	
			(case thrKind of		// we are running on Client, assume that IF_ClientServer and IF_Ajax is set
				 ClientThread 		= deserializeThreadClient thrCallbackClient
				 ClientServerThread	= deserializeThreadClient thrCallbackClient
				 ServerThread 		= abort "Cannot evaluate Server thread on Client\n"
				 else 				= abort "Thread administration error in evalTaskThread"
			)
			(case thrKind of		// we are running on the Server
				 ClientThread 		= abort "Cannot evaluate Client thread on Server\n"
				 ClientServerThread	= deserializeThread thrCallback
				 ServerThread 		= deserializeThread thrCallback
				 else 				= abort "Thread administration error in evalTaskThread"
			)
			{tst & tasknr = thrTaskNr, options = newThrOptions, userId = thrUserId,html = BT []} 
	| activated																	// thread is finished, delete the entry...
		# tst =  deleteThreads thrTaskNr {tst & html = html +|+ nhtml}			// remove thread from administration
		= (a,{tst & tasknr = tasknr, options = options, userId = userId})		// remove entry from table
	= (a,{tst & tasknr = tasknr, options = options, userId = userId,html = html +|+ DivCode (showTaskNr thrTaskNr) nhtml})

	
// ******************************************************************************************************
// Thread Table Storage Manipulation functions
// ******************************************************************************************************

// TO DO : Currently an unordered list is used, should become an ordered tree someday...
// TO DO: Put this stuf in another module

ThreadTableStorage :: !(ThreadTable -> ThreadTable) -> (Task ThreadTable)		// used to store Tasknr of callbackfunctions / threads
ThreadTableStorage fun = handleTable
where
	handleTable tst 
	= IF_Ajax 																	// threads only used when Ajax is enabled
		(IF_ClientServer														// we running both client and server
			(IF_ClientTasks												
				ClientThreadTableStorage										// thread table on client
				ServerThreadTableStorage										// threadtable on server
				fun tst
			)
			(ServerThreadTableStorage fun tst)									// thread table on server when ajax used
		)
		(ServerThreadTableStorage fun tst)										// thread table used for exception handling only ???
//		(abort "Thread table storage only used when Ajax enabled")				// no threads made at all

ServerThreadTableStorage:: !(ThreadTable -> ThreadTable) -> (Task ThreadTable)	// used to store Tasknr of callbackfunctions / threads
ServerThreadTableStorage fun = handleTable
where
	handleTable tst=:{staticInfo} = ThreadTableStorageGen serverThreadTableId staticInfo.threadTableLoc fun tst 

	serverThreadTableId 		= "Application" +++  "-ThreadTable"

ClientThreadTableStorage:: !(ThreadTable -> ThreadTable) -> (Task ThreadTable)	// used to store Tasknr of callbackfunctions / threads
ClientThreadTableStorage fun = handleTable
where
	handleTable tst=:{staticInfo} = ThreadTableStorageGen (clientThreadTableId staticInfo.currentUserId) Client fun tst 

	clientThreadTableId userid	= "User" <+++ userid  <+++ "-ThreadTable"

ThreadTableStorageGen :: !String !Lifespan !(ThreadTable -> ThreadTable) -> (Task ThreadTable)		// used to store Tasknr of callbackfunctions / threads
ThreadTableStorageGen tableid lifespan fun = handleTable						// to handle the table on server as well as on client
where
	handleTable tst
	# (table,tst) = LiftHst (mkStoreForm (Init,storageFormId 
						{ tasklife 		= lifespan
						, taskstorage 	= PlainString 
						, taskmode		= NoForm
						, gc			= Collect} tableid []) fun) tst
	= (table.value,tst)

copyThreadTableToClient ::  !*TSt -> !*TSt										// copies all threads for this user from server to client thread table
copyThreadTableToClient tst
=	IF_ClientServer										
		(IF_ClientTasks id copyThreadTableToClient` tst)						// only if we are on the server the copied can be made
		tst

copyThreadTableToClient` :: !*TSt -> !*TSt										// copies all threads for this user from server to client thread table
copyThreadTableToClient` tst
# ((mythreads,_),tst)	= splitServerThreadsByUser tst							// get thread table on server
# (clientThreads,tst)	= ClientThreadTableStorage (\_ -> mythreads) tst		// and store in client
= tst

splitServerThreadsByUser :: !*TSt -> !(!(!ThreadTable,!ThreadTable),!*TSt)		// get all threads from a given user from the server thread table
splitServerThreadsByUser tst=:{staticInfo}
# userid 				= staticInfo.currentUserId
# (serverThreads,tst)	= ServerThreadTableStorage id tst						// get thread table on server
# splitedthreads		= filterZip (\thr -> thr.thrUserId == userid &&			// only copy relevant part of thread table to client
							      (thr.thrKind == ClientServerThread || thr.thrKind == ClientThread)) serverThreads ([],[])
= (splitedthreads,tst)
where
	filterZip pred [] accu = accu
	filterZip pred [x:xs] (yes,no)
	| pred x = filterZip pred xs ([x:yes],no)
	| otherwise = filterZip pred xs (yes,[x:no])

copyThreadTableFromClient :: !GlobalInfo !*TSt -> !*TSt							// copies all threads for this user from client to server thread table
copyThreadTableFromClient versioninfo tst
=	IF_ClientServer										
		(IF_ClientTasks id (copyThreadTableFromClient` versioninfo) tst)		// only iff we are on the server the copied can be made
		tst

copyThreadTableFromClient` :: !GlobalInfo !*TSt -> !*TSt						// copies all threads for this user from client to server thread table
copyThreadTableFromClient` {newThread,deletedThreads} tst
# ((clienttableOnServer,otherClientsTable),tst)
						= splitServerThreadsByUser tst							// get latest thread table stored on server
# (clienttableOnClient,tst)		
						= ClientThreadTableStorage id tst						// get latest thread table stored on client
# clienttableOnClient	= case deletedThreads of
								[] -> 	clienttableOnClient						// remove threads in client table which have been deleted by global effects											
								_  -> 	[client 
										\\ client <- clienttableOnClient | not (isChildOf client.thrTaskNr deletedThreads) 
										]
# (clienttableOnClient,tst)		
						= ClientThreadTableStorage (\_ -> []) tst				// clear thread table stored on client
# tst					= deleteAllSubTasks deletedThreads tst					// remove corresponding tasks
# thrNrsActiveOnClient	= [thread.thrTaskNr \\ thread <- clienttableOnClient]	// all active thread numbers on client
# newClientsOnServer	= [thread \\ thread <- clienttableOnServer | not (isMember (thread.thrTaskNr) thrNrsActiveOnClient)]
# newtable				= newClientsOnServer ++ clienttableOnClient ++ otherClientsTable			// determine new thread situation
# (serverThreads,tst)	= ServerThreadTableStorage (\_ -> newtable) tst			// store table on server
= tst

findThreadInTable :: !ThreadKind !TaskNr *TSt -> *(Maybe (!Int,!TaskThread),*TSt)// find thread that belongs to given tasknr
findThreadInTable threadkind tasknr tst
# (table,tst)	= ThreadTableStorage id tst										// read thread table
# pos			= lookupThread tasknr 0 table									// look if there is an entry for this task
| pos < 0		= (Nothing, tst)
= (Just (pos,table!!pos),tst) 
where
	lookupThread :: !TaskNr !Int !ThreadTable -> Int
	lookupThread tableKey n []			
		= -1																	// no, cannot find thread
	lookupThread tasknrToFind n [entry:next]
		| (showTaskNr tasknrToFind == showTaskNr entry.thrTaskNr &&	foundThread threadkind entry.thrKind) =  n	// yes, thread is administrated
		= lookupThread tasknrToFind (inc n) next

// TODO foundThread kan niet kloppen !!!

	foundThread ServerThread     		ServerThread 	   		= True
	foundThread ServerThread     		ClientServerThread 	   	= True
	foundThread ServerThread     		ClientThread	 	   	= True
	foundThread ClientThread    		ClientThread 	   		= True
	foundThread ClientThread    		ServerThread   			= True
	foundThread ClientThread    		ClientServerThread 	   	= True
	foundThread ClientServerThread    	ClientServerThread 	   	= True
	foundThread ClientServerThread    	ServerThread 	   		= True // IF_ClientServer (IF_ClientTasks False True) True
	foundThread ClientServerThread    	ClientThread 	   		= True
	foundThread ExceptionHandler 		ExceptionHandler		= True
	foundThread AnyThread    			_				 	   	= True
	foundThread _ 						_ 						= abort "ZOU NIET MOGEN\n" //False

insertNewThread :: !TaskThread *TSt -> *TSt										// insert new thread in table
insertNewThread thread tst		
# (table,tst)	= ThreadTableStorage id tst										// read thread table
# (_,tst) 		= ThreadTableStorage (\_ -> [thread:table]) tst 				// insert the new thread
= tst

deleteThreads :: !TaskNr !*TSt -> *TSt
deleteThreads tasknr tst														// delete a thread and all its children
# (mbthread,tst)		= findThreadInTable AnyThread tasknr tst				// find the thread entry in the table
# mytasknr				= reverse tasknr
| isNothing mbthread	= deleteChildren mytasknr tst							// no entry, but delete children
# (pos,_)				= fromJust mbthread
# (_,tst)				= ThreadTableStorage (\table -> removeAt pos table) tst // remove entry
= deleteChildren mytasknr tst													// and all children
where
	deleteChildren mytasknr tst=:{staticInfo}
	# (table,tst)	 		= ThreadTableStorage id tst							// read thread table
	# allChildsPos			= [pos \\ entry <- table & pos <- [0..] | isChild mytasknr entry.thrTaskNr ]
	| isNil allChildsPos	= tst
	# otherUsersThreads		= [ ((table!!entry).thrUserId,(table!!entry).thrTaskNr) \\ entry <- allChildsPos | (table!!entry).thrUserId <> staticInfo.currentUserId]
	# tst					= administrateDeletedThreads otherUsersThreads tst 
	# table					= deleteChilds (reverse (sort allChildsPos)) table	// delete highest entries first !
	# (table,tst)	 		= ThreadTableStorage (\_ -> table) tst				// read thread table
	= tst

	deleteChilds [] table 			= table
	deleteChilds [pos:next] table 	= deleteChilds next (removeAt pos table)

isChild mytasknr mbchild = take (length mytasknr) (reverse mbchild) == mytasknr

isChildOf mytasknr [] = False
isChildOf mytasknr [x:xs] = isChild (reverse mytasknr) x || isChildOf mytasknr xs

administrateDeletedThreads [] tst = tst
administrateDeletedThreads [(user,tasknr):users] tst=:{hst}
# (_,hst)	= addPUserDeletedThread user tasknr hst								// administrate deleted thread in user administration
= administrateDeletedThreads users {tst & hst = hst}							// such that they are forced to recalculate the whole page

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

findParentThread ::  !TaskNr !*TSt -> *([TaskThread],*TSt)						// finds parent thread closest to given set of task numbers
findParentThread tasknr tst
# (table,tst)		= ThreadTableStorage id tst									// read thread table
| isNil table		= ([], tst)													// nothing in table, no parents
| length tasknr <= 1 = ([], tst)												// no tasks left up
# revtasknr			= reverse (tl tasknr)										// not relevant
# entries 			= filter (\entry -> revtasknr%(0,length entry.thrTaskNr - 2) == (reverse (tl entry.thrTaskNr))) table			// finds thread closest to this one
| isNil entries		= ([], tst)
= (sortBy compare entries,tst)
where
	compare :: !TaskThread !TaskThread -> Bool
	compare x y = length x.thrTaskNr > length y.thrTaskNr
	
// ******************************************************************************************************
// Serialization and De-Serialization of tasks to threads
// ******************************************************************************************************
//
// Watch it:  only works for one and the same application !!!!!
// Each time the application is recompiled, the existing administration has to be removed !!!!
// 
// The Client cannot make Server threads, so we have the following options:
// Server thread, made by Server, must be executed on Server
// Client thread, made by Server, also a Server thread is made: thread can be excuted either on Client or Server
// Sapl   thread, made by Client, can only be executed on Client
//
// IF_Ajax 
//	(IF_ClientServer 
//		(IF_ClientTasks (we are running in sapl on the client) 
//				 (we are running in clean on the server)
//		) (there is no client, all threads are on server) 
//  ) (there are no threads)

serializeThread :: !.(Task .a) -> .String
serializeThread task 
= IF_Ajax 
	(IF_ClientServer 
		(IF_ClientTasks ""													// cannot create server thread on client
				 (copy_to_string task)									// store server thread
		)
		(copy_to_string task)											// store server thread
	)
	(abort "Threads cannot be created, Ajax is switched off\n")			// this call should not happen
						
deserializeThread :: .String -> .(Task .a)
deserializeThread thread 
= IF_Ajax 
	(IF_ClientServer 
		(IF_ClientTasks (abort "Cannot de-serialize Server thread on Client\n")// this call should not happen 
				 (fst (copy_from_string {c \\ c <-: thread} ))			// retrieve server thread
		)
		(fst (copy_from_string {c \\ c <-: thread} ))					// retrieve server thread
	)
	(abort "De-serialization not possible when not running Ajax\n")		// this call should not happen



serializeThreadClient :: !(Task a) -> String
serializeThreadClient task
= IF_Ajax 
	(IF_ClientServer 
		(IF_ClientTasks (graph_to_sapl_string task)							// create client thread on client
				 (graph_to_sapl_string task)							// create client thread on server
		)
		""																// no clients, no need to create client thread
	)
	(abort "Threads cannot be created, Ajax is switched off\n")			// this call should not happen

deserializeThreadClient :: .String -> .(Task .a)
deserializeThreadClient thread
= IF_Ajax 
	(IF_ClientServer 
		(IF_ClientTasks (deserializeSapl thread)								// retrieve client thread
				 (abort "Cannot de-serialize Client thread on Server\n")// this call should not happen
		)
		(abort "Cannot de-serialize Client thread on Server\n")			// this call should not happen
	)
	(abort "Threads should not be evaluated, Ajax is switched off\n")	// this call should not happen

deserializeSapl thread = string_to_graph thread

// ******************************************************************************************************
// Global Effects Storage Management
// ******************************************************************************************************

// Version number control for multiple user workflows
// To support Ajax calls, it is remembered which of the threads of a user has been deleted by someone else
// 		if information from that thread still arives, the input is thrown away since the thread does not exists anymore.
// 		if information from another thread is received, the task tree is calculated starting from the root
// To support Ajax calls, it is remembered whther new threads have been created for the user by other users
//		if so, the task tree is calculated starting from the root 

traceId							= "User_Trace" 
refreshId						= "User_refresh"
applicationVersionNr			= ThisExe <+++ "_Version" 

userVersionNr thisUser			= "User" <+++ thisUser <+++ "_VersionPNr"
usersessionVersionNr thisUser	= "User" <+++ thisUser <+++ "_VersionSNr" 

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

setPUser :: !Int !(GlobalInfo -> GlobalInfo) !*HSt -> (!GlobalInfo,!*HSt) 
setPUser user f hst	
= IF_ClientTasks
	(\hst -> (defaultGlobalInfo,hst))		// persistent version number cannot be set by client
	(\hst -> myStoreForm user f hst)
	hst
where
	myStoreForm user f hst 
	# (form,hst) = mkStoreForm (Init, pFormId (userVersionNr user) defaultGlobalInfo <@ NoForm) f hst
	= (form.value,hst)

	defaultGlobalInfo = { versionNr = 0, newThread = False, deletedThreads = []}

addPUserDeletedThread :: !Int !TaskNr !*HSt -> (!GlobalInfo,!*HSt) 
addPUserDeletedThread user thread hst	= setPUser user (\r -> {r & deletedThreads = [thread:r.deletedThreads]}) hst

setPUserNr :: !Int !(Int -> Int) !*HSt -> (!GlobalInfo,!*HSt) 
setPUserNr user f hst	= setPUser user (\r -> {r & versionNr = f r.versionNr}) hst

setPUserNewThread :: !Int !*HSt -> (!GlobalInfo,!*HSt) 
setPUserNewThread user hst	= setPUser user (\r -> {r & newThread = True}) hst

clearIncPUser :: !Int !(Int -> Int) !*HSt -> (!GlobalInfo,!*HSt) 
clearIncPUser user f hst	= setPUser user (\r -> {r & newThread = False, deletedThreads = [], versionNr = f r.versionNr}) hst

setSVersionNr :: !Int !(Int -> Int) !*HSt -> (!Int,!*HSt) 
setSVersionNr user f hst	
# (form,hst) = mkStoreForm (Init, nFormId (usersessionVersionNr user) 0 <@ NoForm) f hst
= (form.value,hst)

// ******************************************************************************************************
// Finally: The iTask Combinators
// ******************************************************************************************************
// ******************************************************************************************************
// Basic iTask Combinators: Editors !

editTask :: !String !a -> (Task a) | iData a 
editTask prompt a = mkTask "editTask" (editTask` prompt a)

editTask` prompt a tst=:{tasknr,html,hst,userId}
# taskId			= iTaskId userId tasknr "EdFin"
# editId			= iTaskId userId tasknr "EdVal"
# buttonId			= iTaskId userId tasknr "EdBut"
# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) id hst  	// remember if the task has been done
| taskdone.value																			// test if task has completed
	# (editor,hst) 	= (mkEditForm  (Init,cFormId tst.options editId a <@ Display) hst)		// yes, read out current value, make editor passive
	= (editor.value,{tst & activated = True, html = html +|+ BT editor.form, hst = hst})	// return result task
# (editor,hst) 		= mkEditForm  (Init,cFormId tst.options editId a) hst					// no, read out current value from active editor
# (finbut,hst)  	= mySimpleButton tst.options buttonId prompt (\_ -> True) hst			// add button for marking task as done
# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) finbut.value hst 	// remember task status for next time
| taskdone.value	= editTask` prompt a {tst & hst = hst}									// task is now completed, handle as previously
= (editor.value,{tst & activated = taskdone.value, html = html +|+ BT (editor.form ++ finbut.form), hst = hst})

editTaskPred :: !a !(a -> (Bool, HtmlCode))-> (Task a) | iData a 
editTaskPred  a pred = mkTask "editTask" (editTaskPred` a)
where
	editTaskPred` a tst=:{tasknr,html,hst,userId}
	# taskId			= iTaskId userId tasknr "EdFin"
	# editId			= iTaskId userId tasknr "EdVal"
	# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) id hst  	// remember if the task has been done
	| taskdone.value																			// test if task has completed
		# (editor,hst) 	= (mkEditForm  (Init,cFormId tst.options editId a <@ Display) hst)		// yes, read out current value, make editor passive
		= (editor.value,{tst & activated = True, html = html +|+ BT editor.form, hst = hst})	// return result task
	# (editor,hst) 		= mkEditForm  (Init,cFormId tst.options editId a <@ Submit) hst			// no, read out current value from active editor
	| editor.changed
		| fst (pred editor.value)
			# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) (\_ -> True) hst 	// remember task status for next time
			= editTaskPred` a {tst & hst = hst, html = html}									// task is now completed, handle as previously
		= (editor.value,{tst & activated = taskdone.value, html = html +|+ BT (editor.form ++ snd (pred editor.value)), hst = hst})
	= (editor.value,{tst & activated = taskdone.value, html = html +|+ BT editor.form, hst = hst})

// ******************************************************************************************************
// monads for combining iTasks

(=>>) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iCreateAndPrint b
(=>>) taska taskb = mybind
where
	mybind tst=:{options}
	# (a,tst=:{activated}) = taska tst
	| activated	= taskb a {tst & options = options}
	= (createDefault,tst)

return_V :: !a -> (Task a) | iCreateAndPrint a
return_V a  = mkTask "return_V" dotask
where
	dotask tst = (a,tst) 

// ******************************************************************************************************
// adding Html code for prompting and feedback

(?>>) infixr 5 	:: !HtmlCode !(Task a) 					-> Task a		| iCreate a
(?>>) prompt task = doTask
where
	doTask tst=:{html=ohtml,activated}
	| not activated						= (createDefault,tst)
	# (a,tst=:{activated,html=nhtml}) 	= task {tst & html = BT []}
	| activated 						= (a,{tst & html = ohtml})
	= (a,{tst & html = ohtml +|+ BT prompt +|+ nhtml})

(<<?) infixl 5 	:: !(Task a) !HtmlCode 					-> Task a		| iCreate a
(<<?) task prompt = doTask
where
	doTask tst=:{html=ohtml,activated}
	| not activated						= (createDefault,tst)
	# (a,tst=:{activated,html=nhtml}) 	= task {tst & html = BT []}
	| activated 						= (a,{tst & html = ohtml})
	= (a,{tst & html = ohtml +|+ nhtml +|+ BT prompt})

(!>>) infixr 5 :: !HtmlCode !(Task a) -> (Task a) | iCreate a
(!>>) prompt task = doTask
where
	doTask tst=:{html=ohtml,activated=myturn}
	| not myturn			= (createDefault,tst)
	# (a,tst=:{html=nhtml}) = task {tst & html = BT []}
	= (a,{tst & html = ohtml +|+ BT prompt +|+ nhtml})

(<<!) infixl 5 :: !(Task a) !HtmlCode -> (Task a) | iCreate a
(<<!) task prompt = doTask
where
	doTask tst=:{html=ohtml,activated=myturn}
	| not myturn			= (createDefault,tst)
	# (a,tst=:{html=nhtml}) = task {tst & html = BT []}
	= (a,{tst & html = ohtml +|+ nhtml +|+ BT prompt})

// ******************************************************************************************************
// newTask needed for recursive task creation

newTask :: !String !(Task a) -> (Task a) 	| iData a 
newTask taskname mytask = mkTask taskname newTask`
where
	newTask` tst=:{tasknr,userId,options}		
	# taskId					= iTaskId userId tasknr taskname
	# (taskval,tst) 			= LiftHst (mkStoreForm (Init,storageFormId options taskId (False,createDefault)) id) tst  // remember if the task has been done
	# (taskdone,taskvalue)		= taskval.value										// select values
	| taskdone					= (taskvalue,tst)									// if rewritten return stored value
	# (val,tst=:{activated})	= mytask {tst & tasknr = [-1:tasknr]} 				// do task, first shift tasknr
	| not activated				= (createDefault,{tst & tasknr = tasknr, options = options})	// subtask not ready, return value of subtasks
	# tst						= deleteSubTasksAndThreads tasknr tst				// task ready, garbage collect it
	# (_,tst) 					= LiftHst (mkStoreForm (Init,storageFormId options taskId (False,createDefault)) (\_ -> (True,val))) tst  // remember if the task has been done
	= (val,{tst & tasknr = tasknr, options = options})

// ******************************************************************************************************
// looping tasks

// when gc option set and task finished, it will throw away all subtasks and start all over
// otherwise, when task finshed it will remember the new tasknr to prevent checking of previously finished tasks

foreverTask :: !(Task a) -> Task a | iData a
foreverTask task = mkTask "foreverTask" foreverTask`
where
	foreverTask` tst=:{tasknr,activated,userId,options,html} 
	| options.gc == Collect																				// garbace collect everything when task finsihed
		# (val,tst=:{activated})= task {tst & tasknr = [-1:tasknr]}										// shift tasknr
		| activated 			= foreverTask` (deleteSubTasksAndThreads tasknr {tst & tasknr = tasknr, options = options, html = html}) 			// loop
		= (val,tst)					
	# taskId					= iTaskId userId tasknr "ForSt"											// create store id
	# (currtasknr,tst)			= LiftHst (mkStoreForm (Init,storageFormId options taskId tasknr) id) tst		// fetch actual tasknr
	# (val,tst=:{activated})	= task {tst & tasknr = [-1:currtasknr.value]}
	| activated 																						// task is completed	
		# ntasknr				= incNr currtasknr.value												// incr tasknr
		# (currtasknr,tst)		= LiftHst (mkStoreForm (Init,storageFormId options taskId tasknr) (\_ -> ntasknr)) tst // store next task nr
		= foreverTask` {tst & tasknr = tasknr, options = options, html = html}										// initialize new task
	= (val,tst)					

(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iCreateAndPrint a
(<!) taska pred = mkTask "less!" doTask
where
	doTask tst=:{activated, tasknr}
	# (a,tst=:{activated}) 	= taska {tst & tasknr = [-1:tasknr]}
	| not activated 		= (a,tst)
	| not (pred a)			
		# tst = deleteSubTasksAndThreads tasknr tst
		= (a,{tst & activated = False})
	= (a,tst)

// ******************************************************************************************************
// Assigning tasks to users, each user has to be identified by an unique number >= 0

assignTaskTo :: !Bool !UserId !(LabeledTask a) -> Task a | iData a	
assignTaskTo verbose nuserId (taskname,taska) = assignTaskTo`
where
	assignTaskTo` tst=:{html=ohtml,activated,userId,workflowName}
	| not activated						= (createDefault,tst)
	# tst								= IF_Ajax (administrateNewThread userId tst) tst 
	# (a,tst=:{html=nhtml,activated})	= IF_Ajax (UseAjax @>> taska) taska {tst & html = BT [],userId = nuserId}		// activate task of indicated user
	| activated 						= (a,{tst & activated = True						// work is done	
												  ,	userId = userId							// restore previous user id						
												  ,	html = ohtml })							// plus new one tagged
	= (a,{tst & userId = userId																// restore user Id
			  , html = 	ohtml +|+ 															// show old code
						if verbose 
							( BT [showText ("Waiting for Task "), showLabel taskname, showText " from ", showUser nuserId,Br] +|+  // show waiting for
							  ((nuserId,workflowName,taskname) @@: BT [showText "Requested by ", showUser userId,Br,Br] +|+ nhtml)) 
							((nuserId,workflowName,taskname) @@: nhtml)
		 })												

	showUser nr = showLabel ("User " <+++ nr)

administrateNewThread ouserId tst =: {tasknr,userId,options}
| ouserId == userId		= tst
# newTaskId				= iTaskId userId tasknr "_newthread"
# (chosen,tst=:{hst})	= LiftHst (mkStoreForm  (Init,storageFormId options newTaskId False) id) tst	// first time here ?
| not chosen.value
	# (_,hst) 			= setPUserNewThread userId hst													// yes, new thread created
	# (_,tst)			= LiftHst (mkStoreForm  (Init,storageFormId options newTaskId False) (\_ -> True)) {tst & hst = hst}
	= tst
= tst	

// ******************************************************************************************************
// sequencingtasks

seqTasks :: ![LabeledTask a] -> (Task [a])| iCreateAndPrint a
seqTasks options = mkTask "seqTasks" seqTasks`
where
	seqTasks` tst=:{tasknr}
	# (val,tst)	 = doseqTasks options [] {tst & tasknr = [-1:tasknr]}
	= (val,{tst & tasknr = tasknr})

	doseqTasks [] accu tst 		= (reverse accu,{tst & activated = True})
	doseqTasks [(taskname,task):ts] accu tst=:{html,options} 
	# (a,tst=:{activated=adone,html=ahtml}) 
									= task {tst & activated = True, html = BT []}
	| not adone						= (reverse accu,{tst & html = html +|+ BT [showLabel taskname,Br,Br] +|+ ahtml})
	= doseqTasks ts [a:accu] {tst & html = html +|+ ahtml, options = options}

// ******************************************************************************************************
// choose one or more tasks on forehand out of a set

chooseTask_btn :: !HtmlCode !Bool![LabeledTask a] -> Task a | iCreateAndPrint a
chooseTask_btn htmlcode bool ltasks = chooseTask_btn` htmlcode bool ltasks
where
	chooseTask_btn` _ _ [] tst		= return createDefault tst				
	chooseTask_btn`  prompt horizontal taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
	# taskId						= iTaskId userId tasknr ("ChoSt" <+++ length taskOptions)
	# (chosen,tst)					= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.value == -1			// no choice made yet
		# buttonId					= iTaskId userId tasknr "ChoBut"
		# allButtons				= if horizontal 
											[[(but txt,\_ -> n)  \\ txt <- map fst taskOptions & n <- [0..]]]
											[[(but txt,\_ -> n)] \\ txt <- map fst taskOptions & n <- [0..]]
		# (choice,tst)				= LiftHst (TableFuncBut (Init,pageFormId options buttonId allButtons)) tst
		# (chosen,tst)				= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) choice.value) tst
		| chosen.value == -1		= (createDefault,{tst & activated =False,html = html +|+ BT prompt +|+ BT choice.form})
		# chosenTask				= snd (taskOptions!!chosen.value)
		# (a,tst=:{activated=adone,html=ahtml}) = chosenTask {tst & tasknr = [-1:tasknr], activated = True, html = BT []}
		= (a,{tst & tasknr = tasknr, activated = adone, html = html +|+ ahtml})
	# chosenTask					= snd (taskOptions!!chosen.value)
	# (a,tst=:{activated=adone,html=ahtml}) = chosenTask {tst & tasknr = [-1:tasknr], activated = True, html = BT []}
	= (a,{tst & tasknr = tasknr, activated = adone, html = html +|+ ahtml})

but i = iTaskButton i

chooseTask_pdm :: !HtmlCode !Int ![LabeledTask a] -> (Task a) |iCreateAndPrint a
chooseTask_pdm prompt defaultOn taskOptions = mkTask "chooseTask_pdm" (dochooseTask_pdm taskOptions)
where
	dochooseTask_pdm [] tst			= return createDefault tst	
	dochooseTask_pdm taskOptions tst=:{tasknr,html,userId,options}													// choose one subtask out of  a pulldown menu
	# taskId						= iTaskId userId tasknr ("ChoStPdm" <+++ length taskOptions)
	# (chosen,tst)					= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.value == -1			// no choice made yet
		# numberOfItems					= length taskOptions
		# defaultOn						= if (defaultOn >= 0 && defaultOn <= numberOfItems  - 1) defaultOn 0 		
		# taskPdMenuId					= iTaskId userId tasknr ("ChoPdm" <+++ numberOfItems)
		# (choice,tst)					= LiftHst (FuncMenu  (Init,sessionFormId options taskPdMenuId (defaultOn,[(txt,id) \\ txt <- map fst taskOptions]))) tst
		# (_,tst=:{activated=adone,html=ahtml})	
										= internEditSTask "" "Done" Void {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 	
		| not adone						= (createDefault,{tst & activated = False, html = html +|+ BT prompt +|+ BT choice.form +|+ ahtml, tasknr = tasknr})
		# chosenIdx						= snd choice.value
		# chosenTask					= snd (taskOptions!!chosenIdx)
		# (chosen,tst)					= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) (\_ -> chosenIdx)) tst
		# (a,tst=:{activated=bdone,html=bhtml}) 
										= chosenTask {tst & activated = True, html = BT [], tasknr = [0:tasknr]}
		= (a,{tst & tasknr = tasknr, activated = bdone, html = html +|+ bhtml})
	# chosenTask					= snd (taskOptions!!chosen.value)
	# (a,tst=:{activated=adone,html=ahtml}) 
									= chosenTask {tst & activated = True, html = BT [], tasknr = [0:tasknr]}
	= (a,{tst & activated = adone, html = html +|+ ahtml, tasknr = tasknr})

chooseTask_radio :: !HtmlCode !Int ![(HtmlCode,LabeledTask a)] -> (Task a) |iCreateAndPrint a
chooseTask_radio prompt defaultOn taskOptions = mkTask "chooseTask_radio" (dochooseTask_pdm taskOptions)
where
	dochooseTask_pdm [] tst			= return createDefault tst	
	dochooseTask_pdm taskOptions tst=:{tasknr,html,userId,options}													// choose one subtask out of  a pulldown menu
	# numberOfButtons				= length taskOptions
	# defaultOn						= if (defaultOn >= 0 && defaultOn <= numberOfButtons - 1) defaultOn 0 		
	# taskId						= iTaskId userId tasknr ("ChoStRadio" <+++ numberOfButtons)
	# (chosen,tst)					= LiftHst (mkStoreForm  (Init,storageFormId options taskId (False,defaultOn)) id) tst
	# (done,choice)					= chosen.value
	| not done			// no choice made yet
		# taskRadioMenuId				= iTaskId userId tasknr ("ChoRadio" <+++ numberOfButtons)
		# (nradio,tst)					= LiftHst (ListFuncRadio (Init,sessionFormId options taskRadioMenuId (defaultOn,[\i a -> i \\ j <- [0 .. numberOfButtons - 1]]))) tst
		# choice						= if nradio.changed (snd nradio.value) choice
		# (nradio,tst)					= LiftHst (ListFuncRadio (Set, sessionFormId options taskRadioMenuId (choice,[\i a -> i \\ j <- [0 .. numberOfButtons - 1]]))) tst
		# (_,tst=:{activated=adone,html=ahtml})	
										= internEditSTask "" "Done" Void {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 	
		# (_,tst)						= LiftHst (mkStoreForm  (Init,storageFormId options taskId (False,defaultOn)) (\_ -> (adone,choice))) {tst & activated = adone, html = BT []}
		# radioform						= nradio.form <=|>  [[showLabel label] <||> htmlcode \\ (htmlcode,(label,_)) <- taskOptions]
		| not adone						= (createDefault,{tst & activated = False, html = html +|+ BT prompt +|+ BT [radioform] +|+ ahtml, tasknr = tasknr})
		# (_,chosenTask)				= snd (taskOptions!!choice)
		# (a,tst=:{activated=bdone,html=bhtml}) 
										= chosenTask {tst & activated = True, html = BT [], tasknr = [0:tasknr]}
		= (a,{tst & tasknr = tasknr, activated = bdone, html = html +|+ bhtml})
	# (_,chosenTask)					= snd (taskOptions!!choice)
	# (a,tst=:{activated=adone,html=ahtml}) 
									= chosenTask {tst & activated = True, html = BT [], tasknr = [0:tasknr]}
	= (a,{tst & activated = adone, html = html +|+ ahtml, tasknr = tasknr})

chooseTask_cbox :: !([LabeledTask a] -> Task [a]) !HtmlCode ![((!Bool,!ChoiceUpdate,!HtmlCode),LabeledTask a)] -> Task [a] 	| iData a
chooseTask_cbox taskorderfun prompt taskOptions = mkTask "mchoiceTask" (domchoiceTasks taskOptions)
where
	domchoiceTasks [] tst	= ([],{tst& activated = True})
	domchoiceTasks taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
	# seltaskId				= iTaskId userId tasknr ("MtpChSel" <+++ length taskOptions)
	# donetaskId			= iTaskId userId tasknr "MtpChSt"
	# (cboxes,tst)			= LiftHst (ListFuncCheckBox (Init,cFormId options seltaskId initCheckboxes)) tst
	# (fun,nblist)			= cboxes.value
	# nsettings				= fun nblist
	# (cboxes,tst)			= LiftHst (ListFuncCheckBox (Set ,cFormId options seltaskId (setCheckboxes nsettings))) tst
	# (done,tst)			= LiftHst (mkStoreForm      (Init,storageFormId options donetaskId False) id) tst
	# optionsform			= cboxes.form <=|> [[showLabel label] <||> htmlcode \\ ((_,_,htmlcode),(label,_)) <- taskOptions]
	| done.value			= taskorderfun [labeledTask \\ (_,labeledTask) <- taskOptions & True <- snd cboxes.value] {tst & tasknr = [0:tasknr]}
	# (_,tst=:{html=ahtml,activated = adone})
							= (internEditSTask "" "OK" Void) {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 
	| not adone				= ([],{tst & html = html +|+ BT prompt +|+ BT [optionsform] +|+ ahtml})
	# (_,tst)				= LiftHst (mkStoreForm      (Init,storageFormId options donetaskId False) (\_ -> True)) tst
	= domchoiceTasks taskOptions {tst & tasknr = tasknr, html = html, options = options, userId =userId, activated = True}									// choose one subtask out of the list

	initCheckboxes  = 
		[(if set CBChecked CBNotChecked  label,  \b bs _ -> setfun b bs) \\ ((set,setfun,_),(label,_)) <- taskOptions & i <- [0..]]

	setCheckboxes  boollist = 
		[(if set CBChecked CBNotChecked  label,  \b bs _ -> setfun b bs) \\ ((_,setfun,_),(label,_)) <- taskOptions 
																	& i <- [0..] & set <- boollist]

// ******************************************************************************************************
// Speculative OR-tasks: task ends as soon as one of its subtasks completes

orTask2 :: !(Task a,Task b) -> (Task (EITHER a b)) | iCreateAndPrint a & iCreateAndPrint b
orTask2 (taska,taskb) = mkTask "orTask2" (doorTask2 (taska,taskb))
where
	doorTask2 (taska,taskb) tst=:{tasknr,html,options,userId}
	# taskId								= iTaskId userId tasknr "orTask2St"
	# (chosen,tst)							= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.value == 0						// task a was finished first in the past
		# (a,tst=:{html=ahtml})				= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT []}
		= (LEFT a,{tst & html = html})
	| chosen.value == 1						// task b was finished first in the past
		# (b,tst=:{html=bhtml})				= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
		= (RIGHT b,{tst & html = html})
	# (a,tst=:{activated=adone,html=ahtml})	= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT []}
	# (b,tst=:{activated=bdone,html=bhtml})	= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
	| adone
		# tst 								= deleteSubTasksAndThreads [1:tasknr] {tst & tasknr = tasknr}
		# (chosen,tst)						= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) (\_ -> 0)) {tst & html = BT []}
		= (LEFT a,{tst & html = html, activated = True})
	| bdone
		# tst 								= deleteSubTasksAndThreads [0:tasknr] {tst & tasknr = tasknr}
		# (chosen,tst)						= LiftHst (mkStoreForm  (Init,storageFormId tst.options taskId -1) (\_ -> 1)) {tst & html = BT []}
		= (RIGHT b,{tst & html = html, activated = True})
	= (LEFT a,{tst & activated = False, html = html +|+ ahtml +|+ bhtml})

// ******************************************************************************************************
// Parallel task ends when all it subtask are ended as well

andTask2 :: !(Task a,Task b) -> (Task (a,b)) | iCreateAndPrint a & iCreateAndPrint b
andTask2 (taska,taskb) = mkTask "andTask2" (doAndTask (taska,taskb))
where
	doAndTask (taska,taskb) tst=:{tasknr,html}
	# (a,tst=:{activated=adone,html=ahtml})	= mkParSubTask "andTask" 0 taska {tst & html = BT []}
	# (b,tst=:{activated=bdone,html=bhtml})	= mkParSubTask "andTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
	= ((a,b),{tst & activated = adone&&bdone, html = html +|+ ahtml +|+ bhtml})

andTasksCond :: !String !([a] -> Bool) ![LabeledTask a] -> (Task [a]) | iData a // predicate used to test whether tasks are finished
andTasksCond label pred taskCollection = mkTask "andTasksPred" (doandTasks taskCollection)
where
	doandTasks [] tst	= return [] tst
	doandTasks taskCollection tst=:{tasknr,html,options,userId}
	# (alist,tst=:{activated=finished})		
						= checkAllTasks label taskCollection (0,-1) True [] {tst & html = BT [], activated = True}
	# myalist			= map snd alist
	| finished			= (myalist,{tst & html = html}) 					// stop, all andTasks are finished
	| pred myalist		= (myalist,{tst & html = html, activated = True})  	// stop, all work done so far satisfies predicate
	# buttonnames		= map fst taskCollection
	# ((chosen,buttons,chosenname),tst) 									// user can select one of the tasks to work on
						= LiftHst (mkTaskButtons True "" userId tasknr options buttonnames) tst
	# chosenTask		= snd (taskCollection!!chosen)
	# (a,tst=:{activated=adone,html=ahtml}) 								// enable the selected task (finished or not)
						= mkParSubTask label chosen chosenTask {tst & tasknr = tasknr, activated = True, html = BT []}
	# (alist,tst=:{activated=finished,html=allhtml})						// check again whether all other tasks are now finished, collect their code		
						= checkAllTasks label taskCollection (0,chosen) True [] {tst & tasknr = tasknr, html = BT [], activated = True}
	| not adone			= ([a],{tst &	activated = False 					// not done, since chosen task not finished
									, 	html = 	html +|+ 
												(BT (if (length buttonnames > 1) [showMainLabel label: buttons] [])) +-+ 	
												(BT [showLabel chosenname] +|+ ahtml) +|+ 
												(userId -@: allhtml) 		// code for non selected alternatives are not shown for the owner of this task
							})
	# (alist,tst=:{activated=finished,html=allhtml})		
						= checkAllTasks label taskCollection (0,-1) True [] {tst & html = BT [],activated = True} 
	# myalist			= map snd alist
	| finished			= (myalist,{tst & html = html}) 					// stop, all andTasks are finished
	| pred myalist		= (myalist,{tst & html = html, activated = True}) 	// stop, all work done so far satisfies predicate
	= (map snd alist,{tst 	& activated = finished
							, html = 	html +|+ 
										(BT (if (length buttonnames > 1) [showMainLabel label: buttons] [])) +-+ 	
										(BT [showLabel chosenname] +|+ ahtml) +|+ 
										(userId -@: allhtml)
						})
	
	checkAllTasks :: !String [(String,(*TSt -> *(a,*TSt)))] (Int,Int) Bool [(String,a)] *TSt -> *([(String,a)],*TSt) | iCreateAndPrint a
	checkAllTasks traceid options (ctasknr,skipnr) bool alist tst=:{tasknr}
	| ctasknr == length options 	= (reverse alist,{tst & activated = bool})			// all tasks tested
	| ctasknr == skipnr				= checkAllTasks traceid options (inc ctasknr,skipnr) bool alist tst // skip this task such that it is not included
	# (taskname,task)				= options!!ctasknr
	# (a,tst=:{activated = adone})	= mkParSubTask traceid ctasknr task {tst & tasknr = tasknr, activated = True} // check tasks
	| adone							= checkAllTasks traceid options (inc ctasknr,skipnr) bool [(taskname,a):alist] {tst & tasknr = tasknr, activated = True}
	= checkAllTasks traceid options (inc ctasknr,skipnr) False alist {tst & tasknr = tasknr, activated = True}

// ******************************************************************************************************
// Higher order tasks ! Experimental

(-!>) infix 4  :: (Task s) (Task a) -> (Task (Maybe s,TCl a)) | iCreateAndPrint s & iCreateAndPrint a
(-!>)  stoptask task =  mkTask "-!>" stop`
where
	stop` tst=:{tasknr,html,options,userId}
	# (val,tst=:{activated = taskdone,html = taskhtml}) = task     {tst & activated = True, html = BT [], tasknr = normalTaskId,options = options}
	# (s,  tst=:{activated = stopped, html = stophtml})	= stoptask {tst & activated = True, html = BT [], tasknr = stopTaskId,  options = options}
	| stopped	= return_V (Just s, TCl (close task))   {tst & html = html, activated = True}
	| taskdone	= return_V (Nothing,TCl (return_V val)) {tst & html = html +|+ taskhtml, activated = True}
	= return_V (Nothing,TCl (return_V val)) {tst & html = html +|+ taskhtml +|+ stophtml, activated = False}
	where
		close t = \tst -> t {tst & tasknr = normalTaskId, options = options, userId = userId} // reset userId because it influences the task id

		stopTaskId 		= [-1,0:tasknr]
		normalTaskId  	= [-1,1:tasknr]

channel  :: String (Task a) -> (Task (TCl a,TCl a)) | iCreateAndPrint a
channel name task =  mkTask "channel" (doSplit name task)

doSplit name task tst=:{tasknr,options,userId}
= return_V (TCl (sender myTask),TCl (receiver myTask)) tst
where
	myTask tst = task {tst & tasknr = [-1:tasknr], options = options, userId = userId}

	sender task tst=:{activated,tasknr}
	| not activated				= (createDefault,tst)
	# (val,tst) 				= task tst
	= (val,{tst & tasknr = tasknr})

	receiver task  tst=:{activated,tasknr,html}
	| not activated			 	= (createDefault,tst)
	# (val,tst=:{activated}) 	= task tst
	| activated	= (val,{tst & html = html, activated = True , tasknr = tasknr})
	= (val,{tst & html = html /*+|+ BT [showText ("Waiting for completion of "<+++ name)]*/, tasknr = tasknr})

closureTask  :: String (Task a) -> (Task (TCl a)) | iCreateAndPrint a
closureTask name task = mkTask ("closure " +++ name) mkClosure
where
	mkClosure tst=:{tasknr,options,userId}
	# ((TCl sa,ra),tst) 	= doSplit name task tst
	# (_,tst)     			= sa {tst & activated = True}
	= (ra, {tst & activated = True})

closureLzTask  :: String (Task a) -> (Task (TCl a)) | iCreateAndPrint a
closureLzTask name task = mkTask ("closure " +++ name) mkClosure
where
	mkClosure tst=:{tasknr,options,userId}
	# ((TCl sa,ra),tst) 	= doSplit name task tst
	# (_,tst)     			= sa tst
	= (ra, {tst & activated = True})

	doSplit name task tst=:{tasknr,options,userId}
	= return_V (TCl (sender myTask),TCl (receiver myTask)) tst
	where
		myTask tst = task {tst & tasknr = [-1:tasknr], options = options, userId = userId}
	
		sender task tst=:{activated,tasknr}
		| not activated				= (createDefault,tst)
		# (requested,tst)			= (sharedMem id) tst  // is this task demanded ?
		| not requested.value		= (createDefault,tst)
		# (val,tst) 				= task tst
		= (val,{tst & tasknr = tasknr})
	
		receiver task  tst=:{activated,tasknr,html}
		| not activated			 	= (createDefault,tst)
		# (requested,tst)			= (sharedMem (\_ -> True)) tst  // this task is now demanded !
		# (val,tst=:{activated}) 	= task tst
		| activated	= (val,{tst & html = html, activated = True , tasknr = tasknr})
		= (val,{tst & html = html /*+|+ BT [showText ("Waiting for completion of "<+++ name)]*/, tasknr = tasknr})

		sharedStoreId	= iTaskId userId tasknr "Shared_Store"
		sharedMem fun	= LiftHst (mkStoreForm (Init,storageFormId options sharedStoreId False) fun)

write{|TCl|} write_a (TCl task) wst
	= write{|*|} (copy_to_string task) wst

read {|TCl|} read_a  wst 
	# (Read str i file) = read{|*|} wst
	= Read (TCl  (deserialize str)) i file
where
	deserialize :: .String -> .(Task .a)
	deserialize str = fst (copy_from_string {c \\ c <-: str })

gPrint{|TCl|} ga (TCl task) ps = ps <<- copy_to_string task

gParse{|TCl|} ga expr
# mbstring = parseString expr
| isNothing mbstring = Nothing
= Just (TCl (fst(copy_from_string {s` \\ s` <-: fromJust mbstring})))
where
	parseString :: Expr -> Maybe String
	parseString expr = gParse{|*|} expr

gUpd{|TCl|} gc (UpdSearch _ 0)	  	 c		= (UpdDone, c)								
gUpd{|TCl|} gc (UpdSearch val cnt)  c		= (UpdSearch val (cnt - 2),c)						
gUpd{|TCl|} gc (UpdCreate l)        _		
# (mode,default)	= gc (UpdCreate l) undef
= (UpdCreate l, TCl (\tst -> (default,tst)))			
gUpd{|TCl|} gc mode                 b		= (mode, b)										

gForm{|TCl|} gfa (init,formid) hst
= ({value=formid.ival,changed=False,form=[]},hst)

// ******************************************************************************************************
// Timer Tasks ending when timed out

waitForTimeTask:: !HtmlTime	-> (Task HtmlTime)
waitForTimeTask time = mkTask "waitForTimeTask" waitForTimeTask`
where
	waitForTimeTask` tst=:{tasknr,userId,hst}
	# taskId				= iTaskId userId tasknr "Time_"
	# (stime,hst) 			= mkStoreForm (Init,storageFormId tst.options taskId time) id hst  			// remember time
	# ((currtime,_),hst)	= getTimeAndDate hst
	| currtime < stime.value= (stime.value,{tst & activated = False,hst = hst})
	= (currtime - stime.value,{tst & hst = hst})

waitForDateTask:: !HtmlDate	-> (Task HtmlDate)
waitForDateTask date = mkTask "waitForDateTask" waitForDateTask`
where
	waitForDateTask` tst=:{tasknr,userId,hst}
	# taskId				= iTaskId userId tasknr "Date_"
	# (taskdone,hst) 		= mkStoreForm (Init,storageFormId tst.options taskId (False,date)) id hst  			// remember date
	# ((_,currdate),hst) 	= getTimeAndDate hst
	| currdate < date		= (date,{tst & activated = False, hst = hst})
	= (date,{tst & hst = hst})

// ******************************************************************************************************
// functions on TSt

taskId :: TSt -> (Int,TSt)
taskId tst=:{userId} = (userId,tst)

userId :: TSt -> (Int,TSt)
userId tst=:{staticInfo} = (staticInfo.currentUserId,tst)

addHtml :: HtmlCode TSt -> TSt
addHtml bodytag  tst=:{activated, html}  
| not activated = tst						// not active, return default value
= {tst & html = html +|+ BT bodytag}		// active, so perform task or get its result

// ******************************************************************************************************
// lifters to iTask state
// Lifting HSt domain to the TSt domain, for convenience

(*=>) infix 4 :: (TSt -> (a,TSt)) (a -> Task b) -> (Task b)
(*=>) ftst b = doit
where
	doit tst
	# (a,tst) = ftst tst
	= b a tst

(*#>) infix 4 :: (TSt -> TSt) (Task a) -> Task a
(*#>) ftst b = doit
where
	doit tst
	# tst = ftst tst
	= b tst

appIData :: (IDataFun a) -> (Task a) | iData a 
appIData idatafun = \tst -> mkTask "appIData" (appIData` idatafun) tst
where
	appIData` idata tst=:{tasknr,html,hst}
	# (idata,hst) 										= idatafun hst
	# (_,{tasknr,activated,html=ahtml,hst}) 			= internEditSTask "appIDataDone" "Done" Void {tst & activated = True, html = BT [],hst = hst}	
	= (idata.value,{tst & tasknr = tasknr,activated = activated, html = html +|+ 
															(if activated (BT idata.form) (BT idata.form +|+ ahtml)), hst = hst})

appIData2 :: (String *HSt -> *(Form a,*HSt)) -> (Task a) | iData a 
appIData2 idatafun = \tst -> mkTask "appIData" (appIData` idatafun) tst
where
	appIData` idata tst=:{tasknr,html,hst,userId}
	# taskId											= iTaskId userId tasknr "iData"
	# (idata,hst) 										= idatafun taskId hst
	# (_,{tasknr,activated,html=ahtml,hst}) 			= internEditSTask "appIDataDone" "Done" Void {tst & activated = True, html = BT [],hst = hst}	
	= (idata.value,{tst & tasknr = tasknr,activated = activated, html = html +|+ 
															(if activated (BT idata.form) (BT idata.form +|+ ahtml)), hst = hst})

appHStOnce :: !String (HSt -> (a,HSt)) -> (Task a) | iData a
appHStOnce label fun = Once label (liftHst fun)

appHSt :: !String (HSt -> (a,HSt)) -> (Task a) | iData a
appHSt label fun = mkTask label (liftHst fun)

liftHst fun tst=:{hst}
# (fvalue,hst)		= fun hst
= (fvalue,{tst & hst = hst})	

appWorldOnce :: !String (*World -> *(a,*World)) -> (Task a) | iData a
appWorldOnce label fun = Once label (liftWorld fun)

appWorld :: !String (*World -> *(a,*World)) -> (Task a) | iData a
appWorld label fun = mkTask label (liftWorld fun)

liftWorld :: (*World -> *(a,*World)) *TSt -> *(a,*TSt)
liftWorld fun tst=: {hst = hst=:{world = world=:{worldC}}}
# (fvalue,theWorld)	= fun worldC
= (fvalue,{tst & hst = {hst & world = {world & worldC = theWorld}}})	

/*
#! theWorld = tst.hst.world.worldC
# (fvalue,theWorld)	= fun theWorld
= (fvalue,{tst & hst.world.worldC = theWorld})	
*/

Once :: !String !(Task a) -> (Task a) | iData a
Once label task = mkTask label doit
where
	doit tst=:{activated,html,tasknr,hst,userId,options}
	# taskId			= iTaskId userId tasknr (label +++ "_")
	# (store,hst) 		= mkStoreForm (Init,storageFormId options taskId (False,createDefault)) id hst  			
	# (done,value)		= store.value
	| done 				= (value,{tst & hst = hst})													// if task has completed, don't do it again
	# (value,tst=:{hst})= task {tst & hst = hst}
	# (store,hst) 		= mkStoreForm (Init,storageFormId options taskId (False,createDefault)) (\_ -> (True,value)) hst 	// remember task status for next time
	# (done,value)		= store.value
	= (value,{tst & activated = done, hst = hst})													// task is now completed, handle as previously

// ******************************************************************************************************
// Exception Handling
// ******************************************************************************************************

// WORK IN PROGRESS !!!!!!!!!!!!!
// the following implementation is not finished, nor tested, nor working

// Exception handling is NOT supported when running on client.
// An abort is raised instead, in the hope that the Sapl interpreter will give up and pass the evaluation to the server.

serializeExceptionHandler :: !.(!Dynamic -> Task .a) -> .String 
serializeExceptionHandler task = IF_ClientServer
									(IF_ClientTasks (abort "Cannot serialize exception handler on Client\n") (copy_to_string task))
									(copy_to_string task)				

deserializeExceptionHandler :: .String -> .(!Dynamic -> Task a.)
deserializeExceptionHandler thread = IF_ClientServer
										(IF_ClientTasks (abort "Cannot de-serialize exception handler thread on Client\n") (fetchException thread))	
										(fetchException thread)

fetchException thread = fst (copy_from_string {c \\ c <-: thread})


Raise :: e -> Task a | iCreate a & TC e	
Raise e = RaiseDyn (dynamic e)

(<^>) infix  1  :: !(e -> a) !(Task a) -> Task a | iData a & TC e			// create an exception Handler
(<^>) exceptionfun task = newTask "exceptionHandler" evalTask			
where
	evalTask tst=:{tasknr=mytasknr,options=myoptions,userId=myuserId,workflowName}			// thread - task is not yet finished
	# (mbthread,tst)		= findThreadInTable ExceptionHandler mytasknr tst		// look if there is an exceptionhandler for this task
	| isNothing mbthread														// not yet, insert new entry		
		# (versionNr,tst)	= getCurrentAppVersionNr tst						// get current version number of the application
		# tst = insertNewThread 	{ thrTaskNr 		= mytasknr
									, thrUserId 		= myuserId
									, thrWorkflowName	= workflowName
									, thrOptions 		= myoptions
									, thrCallback 		= serializeExceptionHandler (Try exceptionfun)
									, thrCallbackClient = ""
									, thrKind			= ExceptionHandler
									, thrVersionNr		= versionNr
									} tst 
		= task tst																// do the regular task
	= task tst																	// do the regular task
	where
		Try :: !(e -> a) !Dynamic  -> Task a |  iCreateAndPrint a & TC e
		Try exceptionfun (exception :: e^) = catch1 							// handler for this type found
		with 
			catch1 tst=:{tasknr,userId,options}
			# tst 		= deleteSubTasksAndThreads mytasknr tst					// remove all work administrated below handler
			= return_V (exceptionfun exception) tst								// return exceptional result
		Try _ dynamicValue = catch2												// wrong handler
		with
			catch2 tst=:{tasknr} 
			# tst = deleteSubTasksAndThreads (tl tasknr) tst					// delete handler + task
			= RaiseDyn dynamicValue tst											// look for another handler

RaiseDyn :: !Dynamic -> Task a | iCreate a
RaiseDyn dynamicValue = raise
where
	raise tst=:{tasknr,staticInfo,activated}
	| not activated = (createDefault,tst)	
	# (mbthread,tst=:{hst})	= findParentThread tasknr tst						// look for parent threads
	# (version,hst)	 		= setPUserNr staticInfo.currentUserId id hst		// inspect global effects administration
	# mbthread				= [thread \\ thread <- mbthread 
									| thread.thrKind == ExceptionHandler		// which are exceptionhandlers
									&& not (isMember thread.thrTaskNr version.deletedThreads) // and not deleted by some global actions	
							  ] 
	| isNil mbthread		= abort	("\nException raised, but no handler installed or activa anymore\n")	// no handler installed
	= evalException (hd mbthread) dynamicValue {tst & html = BT [], hst = hst}				// yes, *finally*, we heave found an handler

evalException :: !TaskThread !Dynamic -> Task a 								// execute the thread !!!!
evalException entry=:{thrTaskNr,thrUserId,thrOptions,thrCallback,thrCallbackClient} dynval = evalException` 
where
	evalException` tst=:{tasknr,options,userId,html}									
	# (doClient,noThread)  				= IF_ClientTasks (True,thrCallbackClient == "") (False,False)  
	| doClient && noThread				= abort "Cannot execute thread on Client\n" 
	= IF_ClientTasks
		(abort "exception handling not implemeneted") 							//(deserializeThreadClient thrCallbackClient)
		(deserializeExceptionHandler thrCallback dynval	{tst & tasknr = thrTaskNr, options = thrOptions, userId = thrUserId,html = BT []})

// ******************************************************************************************************
// Task Creation and Deletion Utilities
// ******************************************************************************************************

// mkTask is an important wrapper function which should be wrapped around any task
// It takes care of
//		- deciding whether the task should be called (activated) or not
//		- adding of trace information
//		- generating task numbers in a systematic way
// It is very important that the numbering of the tasks is done systematically
// Every task should have a unique number
// Every sequential task should increase the task number
// If a task j is a subtask of task i, than it will get number i.j in reverse order
	

mkTask :: !String !(Task a) -> (Task a) | iCreateAndPrint a
mkTask taskname mytask = mkTaskNoInc taskname mytask o incTaskNr

mkTaskNoInc :: !String !(Task a) -> (Task a) | iCreateAndPrint a			// common second part of task wrappers
mkTaskNoInc taskname mytask = mkTaskNoInc`
where
	mkTaskNoInc` tst=:{activated,tasknr,userId,options}		
	| not activated							= (createDefault,tst)	// not active, don't call task, return default value
	# (val,tst=:{activated,trace})			= mytask tst			// active, so perform task and get its result
	# tst	= {tst & tasknr = tasknr, options = options, userId = userId}
	| isNothing trace || taskname == ""		= (val,tst)				// no trace, just return value
	= (val,{tst & trace = Just (InsertTrace activated tasknr userId options taskname (printToString val%(0,30)) (fromJust trace))}) // adjust trace, don't print to long values

mkParSubTask :: !String !Int (Task a) -> (Task a)  | iCreateAndPrint a					// two shifts are needed
mkParSubTask name i task = mkParSubTask`
where
	mkParSubTask` tst=:{tasknr, options}
	# (v,tst) = mkTaskNoInc (name <+++ "." <+++ i) mysubtask {tst & tasknr = [i:tasknr],activated = True} // shift task
	= (v,{tst & tasknr = tasknr, options = options})
	where
		mysubtask tst=:{tasknr} = task {tst & tasknr = [-1:tasknr], activated = True}	// shift once again!

incTaskNr tst 		= {tst & tasknr = incNr tst.tasknr}
newSubTaskNr tst	= {tst & tasknr = [-1:tst.tasknr]}

incNr [] = [0]
incNr [i:is] = [i+1:is]

addTasknr [] j = [j]
addTasknr [i:is] j = [i+j:is]

showTaskNr [] 		= ""
showTaskNr [i] 		= toString i
showTaskNr [i:is] 	= showTaskNr is <+++ "." <+++ toString i 

showThreadNr [-1]		= "Root"
showThreadNr [-1:is]	= showTaskNr is
showThreadNr else		= "*" <+++ showTaskNr else

iTaskId :: !Int !TaskNr !String -> String
iTaskId userid tasknr postfix 
# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }			// throw away characters not allowed in a file name
| postfix == ""
	| userid < 0	= "iLog_"  <+++ (showTaskNr tasknr) 
	| otherwise		= "iTask_" <+++ (showTaskNr tasknr) 
| userid < 0		= "iLog_"  <+++ (showTaskNr tasknr) <+++ "-" <+++ postfix
| otherwise			= "iTask_" <+++ (showTaskNr tasknr) <+++ "-" <+++ postfix <+++ "+" <+++ userid

internEditSTask tracename prompt task = \tst -> mkTask tracename ((editTask` prompt task <<@ Page) <<@ Edit) tst

// Garbage collection on iTask administration is done dependening on gc option chosen

deleteSubTasksAndThreads :: !TaskNr TSt -> TSt
deleteSubTasksAndThreads tasknr tst 
# tst=:{hst,userId,options}	= deleteThreads tasknr tst
| options.gc == NoCollect 	= tst
| otherwise					= {tst & hst = deleteIData (iTaskId userId tasknr "") hst}

deleteAllSubTasksAndThreads :: ![TaskNr] TSt -> TSt
deleteAllSubTasksAndThreads [] tst = tst
deleteAllSubTasksAndThreads [tx:txs] tst 
# tst = deleteSubTasksAndThreads tx tst
= deleteAllSubTasksAndThreads txs tst

deleteAllSubTasks :: ![TaskNr] TSt -> TSt
deleteAllSubTasks [] tst = tst
deleteAllSubTasks [tx:txs] tst=:{hst,userId} 
# hst	= deleteIData  (iTaskId userId (tl tx) "") hst
= deleteAllSubTasks txs {tst & hst = hst}

// ******************************************************************************************************
// Trace Printing...
// ******************************************************************************************************

InsertTrace :: !Bool !TaskNr !Int !Options String !String ![Trace] -> [Trace]
InsertTrace finished idx who options taskname val trace = InsertTrace` ridx who val trace
where
	InsertTrace` :: !TaskNr !Int !String ![Trace] -> [Trace]
	InsertTrace` [i] 	who str traces
	| i < 0					= abort ("negative task numbers:" <+++ showTaskNr idx <+++ "," <+++ who <+++ "," <+++ taskname)
	# (Trace _ itraces)		= select i traces
	= updateAt` i (Trace (Just (finished,(who,show,options,taskname,str))) itraces)  traces
	InsertTrace` [i:is] who str traces
	| i < 0					= abort ("negative task numbers:" <+++ showTaskNr idx <+++ "," <+++ who <+++ "," <+++ taskname)
	# (Trace ni itraces)	= select i traces
	# nistraces				= InsertTrace` is who str itraces
	= updateAt` i (Trace ni nistraces) traces

	select :: !Int ![Trace] -> Trace
	select i list
	| i < length list = list!!i 
	=  Trace Nothing []

	show 	= idx //showTaskNr idx
	ridx	= reverse idx

	updateAt`:: !Int !Trace ![Trace] -> [Trace]
	updateAt` n x list
	| n < 0		= abort "negative numbers not allowed"
	= updateAt` n x list
	where
		updateAt`:: !Int !Trace ![Trace] -> [Trace]
		updateAt` 0 x []		= [x]
		updateAt` 0 x [y:ys]	= [x:ys]
		updateAt` n x []		= [Trace Nothing []	: updateAt` (n-1) x []]
		updateAt` n x [y:ys]	= [y      			: updateAt` (n-1) x ys]

printTrace2 Nothing 	= EmptyBody
printTrace2 (Just a)  	= BodyTag [showLabel "Task Tree:", Br, STable emptyBackground (print False a),Hr []]
where
	print _ []		= []
	print b trace	= [pr b x ++ [STable emptyBackground (print (isDone x||b) xs)]\\ (Trace x xs) <- trace] 

	pr _ Nothing 			= []
	pr dprev (Just (dtask,(w,i,op,tn,s)))	
	| dprev && (not dtask)					= pr False Nothing	// subtask not important anymore (assume no milestone tasks)
	| not dtask	&& tn%(0,4) == "Ajax "		= showTask cellattr1b White Navy Aqua  Silver  (w,i,op,tn,s)
	| not dtask	&& tn%(0,6) == "Server "	= showTask cellattr1b White Navy Aqua  Silver  (w,i,op,tn,s)
	| not dtask	&& tn%(0,6) == "Client "	= showTask cellattr1b White Navy Aqua  Silver  (w,i,op,tn,s)
	| not dtask								= showTask cellattr1b White Navy Maroon Silver (w,i,op,tn,s)
	= showTask cellattr1a White Yellow Red White (w,i,op,tn,s)
	
	showTask2 attr1 c1 c2 c3 c4 (w,i,op,tn,s)
	= [Table doneBackground 	[ Tr [] [Td attr1 [font c1 (toString (last (reverse i)))],	Td cellattr2 [font c2 tn]]
								, Tr [] [Td attr1 [font c3 (toString w)], 					Td cellattr2 [font c4 s]]
								]
	  ,Br]

	showTask att c1 c2 c3 c4 (w,i,op,tn,s)
	= [STable doneBackground 	
		[ [font c1 (toString w),font c2 ("T" <+++ showTaskNr i)]
		, [showStorage op.tasklife, font c3 tn]
		, [EmptyBody, font c4 s]
		]
		]
	isDone Nothing = False
	isDone (Just (b,(w,i,op,tn,s))) = b

	showStorage Temp		= font Silver "Tmp"
	showStorage Client		= font Aqua "Cli"
	showStorage Page		= font Navy "Pag"
	showStorage Session		= font Navy "Ssn"
	showStorage TxtFileRO	= font Red   "TxF0"
	showStorage TxtFile		= font Red   "TxF"
	showStorage DataFile	= font Red   "DaF"
	showStorage Database	= font Red   "DaB"

	doneBackground = 	[ Tbl_CellPadding (Pixels 1), Tbl_CellSpacing (Pixels 0), cellwidth
						, Tbl_Rules Rul_None, Tbl_Frame Frm_Border 
						]
	doneBackground2 = 	[ Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0), cellwidth
						]
	emptyBackground = 	[Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)]
	cellattr1a		=	[Td_Bgcolor (`Colorname Green),  Td_Width (Pixels 10), Td_VAlign Alo_Absmiddle]
	cellattr1b		=	[Td_Bgcolor (`Colorname Silver), Td_Width (Pixels 10), Td_VAlign Alo_Absmiddle]
	cellattr2		=	[Td_VAlign Alo_Top]
	cellwidth		= 	Tbl_Width (Pixels 130)

	font color message
	= Font [Fnt_Color (`Colorname color), Fnt_Size -1] [B [] message]

// ******************************************************************************************************
// iTask Storage Utilities
// ******************************************************************************************************

cFormId  		{tasklife,taskstorage,taskmode} s d = {sFormId  s d & lifespan = tasklife, storage = taskstorage, mode = taskmode} 

sessionFormId  	options s d = cFormId options s d <@ if (options.tasklife == Client) Client Session
pageFormId  	options s d = cFormId options s d <@ if (options.tasklife == Client) Client Page
storageFormId  	options s d = cFormId options s d <@ NoForm

mySimpleButton :: !Options !String !String     !(a -> a) 				!*HSt -> (Form (a -> a),!*HSt)
mySimpleButton options id label fun hst	
							= FuncBut (Init, (nFormId id (iTaskButton label,fun)) <@ if (options.tasklife == Client) Client Page) hst

LiftHst fun tst=:{hst}
# (form,hst) = fun hst
= (form,{tst & hst = hst})

// ******************************************************************************************************
// Html Printing Utilities...
// ******************************************************************************************************

mkDiv :: String HtmlCode -> HtmlCode
mkDiv id bodytag = [normaldiv]
where
	normaldiv = Div [`Div_Std [Std_Id id, Std_Class	"thread"]] bodytag

iTaskButton :: String -> Button
iTaskButton label 
= LButton defpixel label

// ******************************************************************************************************
// very small util

isNil [] = True
isNil _ = False



