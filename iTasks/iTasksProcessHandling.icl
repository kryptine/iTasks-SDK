implementation module iTasksProcessHandling

// *********************************************************************************************************************************
// This module contains iTask combinators for creating iTask workflow processes
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdEnv 
import iDataFormlib
import InternaliTasksCommon, InternaliTasksThreadHandling
import BasicCombinators, iTasksSettings
import dynamic_string
import GenBimap

derive gForm 	Wid, WorkflowStatus, []
derive gUpd 	Wid, WorkflowStatus, []
derive gParse 	Wid, WorkflowStatus
derive gPrint 	Wid, WorkflowStatus
derive gerda 	Wid, WorkflowStatus
derive read 	Wid, WorkflowStatus
derive write 	Wid, WorkflowStatus

:: Wid a			= Wid WorkflowLink											// id of workflow process
:: WorkflowProcess 	= ActiveWorkflow 	ProcessIds !(Task Dynamic)
					| SuspendedWorkflow ProcessIds !(Task Dynamic)
					| FinishedWorkflow 	ProcessIds !Dynamic !(Task Dynamic)
					| DeletedWorkflow	ProcessIds

instance == WorkflowStatus
where
	(==) (WflActive _) 			(WflActive _)  	= True
	(==) (WflSuspended _)  		(WflSuspended _)= True
	(==) WflFinished    		WflFinished 	= True
	(==) WflDeleted 			WflDeleted		= True
	(==) _ 						_ 				= False

workflowProcessStoreName :== "Application" +++  "-ProcessTable"

derive gForm	WorkflowProcess
derive gUpd		WorkflowProcess
derive gPrint	WorkflowProcess
derive gParse	WorkflowProcess
derive gerda	WorkflowProcess	
derive read		WorkflowProcess	
derive write	WorkflowProcess	

gPrint{|Dynamic|} dyn pst 	= gPrint{|*|} (dynamic_to_string dyn) pst
gParse{|Dynamic|} expr 		= case parseString expr of
								(Just string) 	= Just (string_to_dynamic {s` \\ s` <-: string})
								Nothing			= Nothing
where
	parseString :: !Expr -> Maybe String
	parseString expr = gParse{|*|} expr
	
gForm{|Dynamic|} (init, formid) hst = ({changed=False,form=[], inputs = [],value=formid.ival},(incrHStCntr 1 hst))
gUpd{|Dynamic|} (UpdSearch 0 _) a 	= (UpdDone,a)
gUpd{|Dynamic|} (UpdSearch i v) a 	= (UpdSearch (i-1) v,a)
gUpd{|Dynamic|} (UpdCreate c) a 	= (UpdCreate c,dynamic 0)
gUpd{|Dynamic|} UpdDone a 			= (UpdDone,a)
write{|Dynamic|} dyn pst 	= write{|*|} (dynamic_to_string dyn) pst
read{|Dynamic|} pst 		= case myread pst of
								Read  string i f	= Read (string_to_dynamic {s` \\ s` <-: string}) i f
								Fail f				= Fail f
where
	myread :: !*Write -> *Read .String
	myread pst = read{|*|} pst

gerda{|Dynamic|} 	= abort "Cannot yet store a Dynamic in a Database\n" 
gerda{|Task|} ga		= abort "Cannot yet store an iTask of type TCL in a Database\n" 

import DrupBasic

isValidWorkflowReference :: !WorkflowProcess !ProcessIds -> Bool								// checks whether pointer to workflow is still refering to to right entry in the table
isValidWorkflowReference workflowprocess idsref = drop1tuple3 (getWorkflowWid workflowprocess) == drop1tuple3 idsref
where
	drop1tuple3 (x,y,z) = (y,z)

getWorkflowWid :: !WorkflowProcess -> ProcessIds 									// get wid of a process
getWorkflowWid (ActiveWorkflow 	ids _)			= ids
getWorkflowWid (SuspendedWorkflow ids _)		= ids
getWorkflowWid (FinishedWorkflow 	ids _ _)	= ids
getWorkflowWid (DeletedWorkflow	ids)			= ids

getWorkflowUser :: !WorkflowProcess -> UserId										// fetch user who should do the work
getWorkflowUser (ActiveWorkflow 	(userid,_,_) _)		= userid 
getWorkflowUser (SuspendedWorkflow  (userid,_,_) _)		= userid
getWorkflowUser (FinishedWorkflow 	(userid,_,_) _ _)	= userid
getWorkflowUser (DeletedWorkflow	(userid,_,_))		= userid

setWorkflowUser :: !UserId !WorkflowProcess -> WorkflowProcess						// fetch user who should do the work
setWorkflowUser nuserid (ActiveWorkflow 		(userid,procnr,wflab) task)		= (ActiveWorkflow 		(nuserid,procnr,wflab) task)
setWorkflowUser nuserid (SuspendedWorkflow  	(userid,procnr,wflab) task)		= (SuspendedWorkflow  	(nuserid,procnr,wflab) task)
setWorkflowUser nuserid (FinishedWorkflow 		(userid,procnr,wflab) dyn task)	= (FinishedWorkflow 	(userid,procnr,wflab) dyn task)
setWorkflowUser nuserid (DeletedWorkflow		(userid,procnr,wflab))			= (DeletedWorkflow		(nuserid,procnr,wflab))

getTask :: !WorkflowProcess -> Task Dynamic
getTask (ActiveWorkflow 	(_,_,_) task)		= task 
getTask (SuspendedWorkflow  (_,_,_) task)		= task
getTask (FinishedWorkflow 	(_,_,_) _ task)		= task

isDeletedWorkflow :: !WorkflowProcess -> Bool
isDeletedWorkflow (DeletedWorkflow _) = True
isDeletedWorkflow _	= False

workflowProcessStore ::  !((!Int,![WorkflowProcess]) -> (!Int,![WorkflowProcess])) !*TSt -> (!(!Int,![WorkflowProcess]),!*TSt) 
workflowProcessStore wfs tst	
= IF_Ajax 																		
	(IF_ClientServer															// we running both client and server
		(IF_ClientTasks												
			(abort "Cannot access workflow process table on client\n")			// workflow table only on server site
			(workflowProcessStore` wfs tst)										// access workflow store
		)
		(workflowProcessStore` wfs tst)
	)
	(workflowProcessStore` wfs tst)
where
	workflowProcessStore` wfs tst=:{hst}	
	# (form,hst) = mkStoreForm (Init, pFormId workflowProcessStoreName (0,[]) <@ NoForm) wfs hst
	= (form.Form.value,{tst & hst = hst})

scheduleWorkflows :: !(Task a) -> (Task a) | iData a
scheduleWorkflows maintask 
//# nmaintask	= newTask defaultWorkflowName (mkTask "StartMain" (assignTaskTo 0 ("main",maintask)))
# nmaintask	= assignTaskTo 0 ("main",maintask)
= IF_Ajax 																		
	(IF_ClientServer															// we running both client and server
		(IF_ClientTasks												
			nmaintask															// workflow table only on server site, do only maintask
			(Task (scheduleWorkflows` nmaintask))								// access workflow store
		)
		(Task (scheduleWorkflows` nmaintask))
	)
	(Task (scheduleWorkflows` nmaintask))
where
	scheduleWorkflows` nmaintask tst 
	# (a,tst=:{activated}) 	= appTaskTSt nmaintask tst	// start maintask
	# ((_,wfls),tst) 		= workflowProcessStore id tst												// read workflow process administration
	# (done,tst)			= scheduleWorkflowTable True wfls 0 {tst & activated = True}				// all added workflows processes are inspected (THIS NEEDS TO BE OPTIMIZED AT SOME STAGE)
	= (a,{tst & activated = activated && done})															// whole application ends when all processes have ended

scheduleWorkflowTable done [] _ tst = (done,tst)
scheduleWorkflowTable done [ActiveWorkflow _  dyntask:wfls] procid tst
# (_,tst=:{activated}) = appTaskTSt dyntask {tst & activated = True}
= scheduleWorkflowTable (done && activated) wfls (inc procid) {tst & activated = activated}
scheduleWorkflowTable done [SuspendedWorkflow _ _:wfls] procid tst
= scheduleWorkflowTable done wfls (inc procid) tst
scheduleWorkflowTable done [FinishedWorkflow _ _ dyntask:wfls] procid tst	// just to show result in trace..
# (_,tst) = appTaskTSt dyntask tst
= scheduleWorkflowTable done wfls (inc procid) tst
scheduleWorkflowTable done [DeletedWorkflow _:wfls] procid tst
= scheduleWorkflowTable done wfls (inc procid) tst

spawnWorkflow :: !UserId !Bool !(LabeledTask a) -> Task (Wid a) | iData a
spawnWorkflow userid active (label,task) = Task (\tst=:{options,staticInfo} -> appTaskTSt ((newTask ("spawn " +++ label) (Task (spawnWorkflow` options))<<@ staticInfo.threadTableLoc)) tst)
where
	spawnWorkflow` options tst
	# ((processid,wfls),tst) 		
						= workflowProcessStore id tst							// read workflow process administration
	# (found,entry)		= findFreeEntry wfls 1									// found entry in table
	# processid			= processid + 1											// process id currently given by length list, used as offset in list
	# wfl				= mkdyntask options entry processid task				// convert user task in a dynamic task
	# nwfls				= if found 
							(updateAt (entry - 1) (if active ActiveWorkflow SuspendedWorkflow (userid,processid,label) wfl) wfls)
							(wfls ++ [if active ActiveWorkflow SuspendedWorkflow (userid,processid,label) wfl])				// turn task into a dynamic task
	# (wfls,tst) 		= workflowProcessStore (\_ -> (processid,nwfls)) tst	// write workflow process administration
	# (_,tst)			= appTaskTSt (if active wfl (Task (\tst -> (undef,tst)))) tst	// if new workflow is active, schedule it in
	= (Wid (entry,(userid,processid,label)),{tst & activated = True})

	findFreeEntry :: [WorkflowProcess] Int -> (Bool,Int)
	findFreeEntry [] n	= (False,n)
	findFreeEntry [DeletedWorkflow _:wfls] n = (True,n)
	findFreeEntry [_:wfls] n = findFreeEntry wfls (n + 1)

	mkdyntask options entry processid task 
	=  Task (\tst -> convertTask entry processid label task 
				{tst & tasknr = [entry - 1],activated = True,userId = userid, options = options,workflowLink = (entry,(userid,processid,label))})
	
	convertTask entry processid label task tst

	# ((processid,wfls),tst) 	= workflowProcessStore id tst					// read workflow process administration
	# wfl						= wfls!!(entry - 1)								// fetch entry
	# currentWorker				= getWorkflowUser wfl							// such that worker can be changed dynamically !
//	# (a,tst=:{activated})		= appTaskTSt (newTask label (mkTask "StartMain" (assignTaskTo currentWorker ("main",task)))) tst			
	# (a,tst=:{activated})		= appTaskTSt (assignTaskTo currentWorker ("main",task)) tst			

	# dyn						= dynamic a
	| not activated				= (dyn,tst)										// not finished, return
	# ((_,wfls),tst) 			= workflowProcessStore id tst					// read workflow process administration
	# wfls						= case (wfls!!(entry - 1)) of					// update process administration
										(ActiveWorkflow wid acttask) -> updateAt (entry - 1) (FinishedWorkflow wid dyn acttask) wfls
										_ -> wfls
	# (wfls,tst) 				= workflowProcessStore (\_ -> (processid,wfls)) tst		// write workflow process administration
	= (dyn,tst)												

changeWorkflowUser :: !UserId !(Wid a) -> Task Bool 
changeWorkflowUser nuser (Wid (entry,ids=:(_,_,label))) = newTask ("changeUser " +++ label) (Task deleteWorkflow`)
where
	deleteWorkflow` tst
	| entry == 0		= (False,tst)											// main task cannot be handled
	# ((maxid,wfls),tst)= workflowProcessStore id tst							// read workflow process administration
	# wfl				= wfls!!(entry - 1)										// fetch entry
	# refok				= isValidWorkflowReference wfl ids
	| not refok			= (False,tst)											// wid does not refer to the correct entry anymore
	# wfl				= setWorkflowUser nuser wfl
	# nwfls				= updateAt (entry - 1) wfl wfls							// delete entry in table
	# (wfls,tst) 		= workflowProcessStore (\_ -> (maxid,nwfls)) tst		// update workflow process administration
	= (True,tst)																// if everything is fine it should always succeed

waitForWorkflow :: !(Wid a) -> Task (Maybe a) | iData a
waitForWorkflow (Wid (entry,ids=:(_,_,label))) = newTask ("waiting for " +++ label) (Task waitForResult`)
where
	waitForResult` tst
	# ((_,wfls),tst) 	= workflowProcessStore id tst							// read workflow process administration
	# wfl				= wfls!!(entry - 1)										// fetch entry
	# refok				= isValidWorkflowReference wfl ids
	| not refok			= (Nothing,{tst & activated = True})					// wid does not refer to the correct entry anymore
	= case wfl of																// update process administration
			(FinishedWorkflow _ (val::a^) _) -> (Just val,{tst & activated = True})	// finished
			_ 					->  (Nothing,{tst & activated = False})	// not yet

/*
waitForWorkflowWithName :: !String -> Task (Maybe a) | iData a
waitForWorkflowWithName labelSearched = newTask ("waiting for " +++ labelSearched) waitForResult`
where
	waitForResult` tst
	# ((_,wfls),tst) 	= workflowProcessStore id tst							// read workflow process administration
	# foundEntries		= [i \\ i <- [0 ..] & wfl <- wfls | thd3 (getWorkflowWid wfl) == labelSearched]
	| isEmpty foundEntries
						= (Nothing,{tst & activated = False})					// entry does not exist
	# entry				= hd foundEntries										// entry found; first entry is taken
	# wfl				= wfls!!(entry - 1)										// fetch entry
	= case wfl of																// update process administration
			(FinishedWorkflow _ (val::a^) _) -> (Just val,{tst & activated = True})	// finished
			_ 					->  (Nothing,{tst & activated = False})			// not yet
*/

waitForWorkflowWid :: !String -> Task (Maybe (Wid a)) | iData a
waitForWorkflowWid labelSearched = newTask ("waiting for " +++ labelSearched) (Task waitForResult`)
where
	waitForResult` tst
	# ((_,wfls),tst) 	= workflowProcessStore id tst							// read workflow process administration
	# foundEntries		= [i \\ i <- [1 ..] & wfl <- wfls | thd3 (getWorkflowWid wfl) == labelSearched]
	| isEmpty foundEntries
// set True as experiment...
						= (Nothing,{tst & activated = True})					// entry does not (yet) exist
	| length foundEntries <> 1
						= (Nothing,{tst & activated = True})					// there are more; illegal action; it is assumed that there is only one
	# entry				= hd foundEntries
	= (Just (Wid (entry,getWorkflowWid (wfls!!(entry - 1)))),{tst & activated = True})						// entry found

deleteMe :: (Task Void)
deleteMe = Task deleteMe`
where
	deleteMe` tst=:{workflowLink} 
	=	appTaskTSt (				deleteWorkflow (Wid workflowLink)
			=>> \_ ->	return_V Void ) tst

deleteWorkflow :: !(Wid a) -> Task Bool 
deleteWorkflow (Wid (entry,ids=:(_,_,label))) = newTask ("delete " +++ label) (Task deleteWorkflow`)
where
	deleteWorkflow` tst
	| entry == 0		= (False,tst)											// main task cannot be handled
	# ((maxid,wfls),tst)= workflowProcessStore id tst							// read workflow process administration
	# wfl				= wfls!!(entry - 1)										// fetch entry
	# refok				= isValidWorkflowReference wfl ids						// does the Wid indeed refers to this process
	| not refok			= (False,tst)											// wid does not refer to the correct entry anymore
	| isDeletedWorkflow wfl = (True,tst)										// already deleted
	# nwfls				= updateAt (entry - 1) (DeletedWorkflow ids) wfls		// delete entry in table
	# (wfls,tst=:{html}) = workflowProcessStore (\_ -> (maxid,nwfls)) tst		// update workflow process administration
	# (_,tst)			= appTaskTSt (getTask wfl) {tst & html = BT [] []}					// calculate workflow to delete for the last time to obtain all its itasks in the task tree
	# tst				= deleteSubTasksAndThreads [entry] tst					// delete all iTask storage of this process ...
	= (True,{tst & html = html, activated = True})												// if everything is fine it should always succeed

suspendMe :: (Task Void)
suspendMe = Task suspendMe`
where
	suspendMe` tst=:{workflowLink = workflowLink=:(entry,ids)} 
	| entry == 0		= (Void,tst)											// main task cannot be handled
	= appTaskTSt (	suspendWorkflow (Wid workflowLink)
			=>> \_ ->	return_V Void ) tst

suspendWorkflow :: !(Wid a) -> Task Bool
suspendWorkflow (Wid (entry,ids=:(_,_,label))) = newTask ("suspend " +++ label) (Task deleteWorkflow`)
where
	deleteWorkflow` tst
	| entry == 0		= (False,tst)											// main task cannot be handled
	# ((maxid,wfls),tst)= workflowProcessStore id tst							// read workflow process administration
	# wfl				= wfls!!(entry - 1)										// fetch entry
	# refok				= isValidWorkflowReference wfl ids
	| not refok			= (False,tst)											// wid does not refer to the correct entry anymore
	# (ok,nochange,wfl)	= case wfl of
							(ActiveWorkflow label acttask) -> (True,False,SuspendedWorkflow label acttask)
							(DeletedWorkflow label) -> (False,True,DeletedWorkflow label) // a deleted workflow cannot be suspendend
							wfl -> (True,True,wfl)								// in case of finsihed or already suspended flows
	| nochange			= (ok,{tst & activated = True})							// no change needed
	# nwfls				= updateAt (entry - 1) wfl wfls							// update entry
	# (wfls,tst) 		= workflowProcessStore (\_ -> (maxid,nwfls)) tst		// update workflow process administration
	= (ok,tst)																	// if everything is fine it should always succeed

activateWorkflow :: !(Wid a) -> Task Bool
activateWorkflow (Wid (entry,ids=:(_,_,label))) = newTask ("activate " +++ label) (Task activateWorkflow`)
where
	activateWorkflow` tst
	| entry == 0		= (False,tst)											// main task cannot be handled
	# ((maxid,wfls),tst)= workflowProcessStore id tst							// read workflow process administration
	# wfl				= wfls!!(entry - 1)										// fetch entry
	# refok				= isValidWorkflowReference wfl ids
	| not refok			= (False,tst)											// wid does not refer to the correct entry anymore
	= case wfl of
		(SuspendedWorkflow label susptask) -> scheduleWorkflow label maxid susptask wfls tst
		(ActiveWorkflow    label acttask)  -> (True,{tst & activated = True})
		wfl -> (False,{tst & activated = True})									// in case of finished or deleted task

	scheduleWorkflow label maxid wfl wfls tst										
	# nwfls				= updateAt (entry - 1) (ActiveWorkflow label wfl) wfls // mark workflow as activated
	# (wfls,tst) 		= workflowProcessStore (\_ -> (maxid,nwfls)) tst		// update workflow process administration
	# (_,tst)			= appTaskTSt wfl {tst & activated = True}				// schedule workflow
	= (True,tst)																// done


/*
activateWorkflow :: !(Wid a) -> Task Bool
activateWorkflow (Wid (entry,ids=:(_,_,label))) = newTask ("activate " +++ label) activateWorkflow`
where
	activateWorkflow` tst
	| entry == 0		= (False,tst)											// main task cannot be handled
	# ((maxid,wfls),tst)= workflowProcessStore id tst							// read workflow process administration
	# wfl				= wfls!!(entry - 1)										// fetch entry
	# refok				= isValidWorkflowReference wfl ids
	| not refok			= (False,tst)											// wid does not refer to the correct entry anymore
	# (ok,nochange,wfl,tst)	
						= case wfl of
								(SuspendedWorkflow label susptask) -> scheduleWorkflow label susptask tst
//								(DeletedWorkflow label) -> (False,True,DeletedWorkflow label,tst) // a deleted workflow cannot be suspendend
								wfl -> (True,True,wfl,tst)						// in case of finished or already activated flows
	| nochange			= (ok,{tst & activated = True})							// no change needed
	# nwfls				= updateAt (entry - 1) wfl wfls							// update entry
	# (wfls,tst) 		= workflowProcessStore (\_ -> (maxid,nwfls)) tst		// update workflow process administration
	= (ok,tst)																	// if everything is fine it should always succeed

	scheduleWorkflow label (Task wfl) tst										// schedule workflow
	# (_,tst)	= wfl {tst & activated = True}
	= (True,False,ActiveWorkflow label (Task wfl),{tst & activated = True})

*/
getWorkflowStatus :: !(Wid a) -> Task WorkflowStatus
getWorkflowStatus (Wid (entry,ids=:(_,_,label))) = newTask ("get status " +++ label) (Task getWorkflowStatus`)
where
	getWorkflowStatus` tst
	# ((_,wfls),tst) 	= workflowProcessStore id tst							// read workflow process administration
	# wfl				= wfls!!(entry - 1)										// fetch entry
	# refok				= isValidWorkflowReference wfl ids
	| not refok			= (WflDeleted,tst)										// wid does not refer to the correct entry anymore
	# status			= case wfl of
							(ActiveWorkflow (user,_,_) _) 		-> WflActive user
							(SuspendedWorkflow (user,_,_) _) 	-> WflSuspended user
							(FinishedWorkflow _ _ _) 			-> WflFinished
							(DeletedWorkflow _) 				-> WflDeleted		
	= (status,tst)																// if everything is fine it should always succeed

showWorkflows :: !Bool !*TSt -> ([HtmlTag],*TSt)
showWorkflows alldone tst 
= 	IF_ClientTasks												
		(\tst -> ([],tst))														// workflow table not available on clients
		(showWorkflows` alldone) tst											// show tables
where
	showWorkflows` alldone tst
	# ((_,wfls),tst) 		= workflowProcessStore id tst						// read workflow process administration
	= (mkTable wfls,tst)

	mkTable []		= []

	mkTable wfls	= [DivTag [IdAttr "itasks-workflow-process-table",ClassAttr "trace"] [H2Tag [] [Text "Workflow Process Table:"], TableTag [] [header : rows]]]
	where
		header	= TrTag [] 
					[   ThTag [] [Text "Entry"], 		ThTag [] [Text "User Id"],  ThTag [] [Text "Process Id"],  ThTag [] [Text "Task Name"], 		 ThTag [] [Text "Status"]]
		rows	= 	[	TrTag [] [TdTag [] [Text "0"],  TdTag [] [Text "0"], 		TdTag [] [Text "0"], 		   TdTag [] [Text defaultWorkflowName], TdTag [] [if alldone (Text "Finished") (Text "Active")]]
					: [ TrTag [] [TdTag [] [Text (toString i)]: showStatus wfl] \\ wfl <- wfls & i <- [1..]]
					]

	showStatus (ActiveWorkflow 	 	(userid,processid,label) dyntask)		= [TdTag [] [Text (toString userid)], TdTag [] [Text (toString processid)], TdTag [] [Text label], TdTag [] [Text "Active"]]
	showStatus (SuspendedWorkflow 	(userid,processid,label) dyntask)		= [TdTag [] [Text (toString userid)], TdTag [] [Text (toString processid)], TdTag [] [Text label], TdTag [] [Text "Suspended"]]
	showStatus (FinishedWorkflow 	(userid,processid,label) dyn dyntask)	= [TdTag [] [Text (toString userid)], TdTag [] [Text (toString processid)], TdTag [] [Text label], TdTag [] [Text "Finished"]]
	showStatus (DeletedWorkflow  	(userid,processid,label))				= [TdTag [] [Text (toString userid)], TdTag [] [Text (toString processid)], TdTag [] [Text label], TdTag [] [Text "Deleted"]]

	STable atts table		= TableTag atts (mktable table)
	where
		mktable table 	= [TrTag [] (mkrow rows)           \\ rows <- table]
		mkrow   rows 	= [TdTag [ValignAttr "top"]  [row] \\ row  <- rows ]
