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
import iTasksHandler, iTasksBasicCombinators, iTasksSettings
import dynamic_string

derive gForm 	Wid, WorkflowStatus, Maybe, []
derive gUpd 	Wid, WorkflowStatus, Maybe, []
derive gParse 	Wid, WorkflowStatus, Maybe
derive gPrint 	Wid, WorkflowStatus, Maybe
derive gerda 	Wid, WorkflowStatus
derive read 	Wid, WorkflowStatus
derive write 	Wid, WorkflowStatus

:: Wid a			= Wid WorkflowLink											// id of workflow process
:: WorflowProcess 	= ActiveWorkflow 	ProcessIds !(TCl !Dynamic)
					| SuspendedWorkflow ProcessIds !(TCl !Dynamic)
					| FinishedWorkflow 	ProcessIds !Dynamic !(TCl !Dynamic)
					| DeletedWorkflow	ProcessIds

instance == WorkflowStatus
where
	(==) (WflActive _) 			(WflActive _)  	= True
	(==) (WflSuspended _)  		(WflSuspended _)= True
	(==) WflFinished    		WflFinished 	= True
	(==) WflDeleted 			WflDeleted		= True
	(==) _ 						_ 				= False

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
	parseString :: !Expr -> Maybe String
	parseString expr = gParse{|*|} expr
	
gForm{|Dynamic|} (init, formid) hst = ({changed=False,form=[],value=formid.ival},(incrHSt 1 hst))
gUpd{|Dynamic|} (UpdSearch _ 0) a 	= (UpdDone,a)
gUpd{|Dynamic|} (UpdSearch v i) a 	= (UpdSearch v (i-1),a)
gUpd{|Dynamic|} (UpdCreate c) a 	= (UpdCreate c,dynamic 0)
gUpd{|Dynamic|} UpdDone a 			= (UpdDone,a)

isValidWorkflowReference :: !WorflowProcess !ProcessIds -> Bool								// checks whether pointer to workflow is still refering to to right entry in the table
isValidWorkflowReference (ActiveWorkflow 	ids _)		idsref = drop1tuple3 ids == drop1tuple3 idsref
isValidWorkflowReference (SuspendedWorkflow ids _)		idsref = drop1tuple3 ids == drop1tuple3 idsref
isValidWorkflowReference (FinishedWorkflow 	ids _ _)	idsref = drop1tuple3 ids == drop1tuple3 idsref
isValidWorkflowReference (DeletedWorkflow	ids)		idsref = drop1tuple3 ids == drop1tuple3 idsref

drop1tuple3 (x,y,z) = (y,z)

getWorkflowUser :: !WorflowProcess -> UserId						// fetch user who should do the work
getWorkflowUser (ActiveWorkflow 	(userid,_,_) _)		= userid 
getWorkflowUser (SuspendedWorkflow  (userid,_,_) _)		= userid
getWorkflowUser (FinishedWorkflow 	(userid,_,_) _ _)	= userid
getWorkflowUser (DeletedWorkflow	(userid,_,_))		= userid

setWorkflowUser :: !UserId !WorflowProcess -> WorflowProcess						// fetch user who should do the work
setWorkflowUser nuserid (ActiveWorkflow 		(userid,procnr,wflab) task)		= (ActiveWorkflow 		(nuserid,procnr,wflab) task)
setWorkflowUser nuserid (SuspendedWorkflow  	(userid,procnr,wflab) task)		= (SuspendedWorkflow  	(nuserid,procnr,wflab) task)
setWorkflowUser nuserid (FinishedWorkflow 		(userid,procnr,wflab) dyn task)	= (FinishedWorkflow 	(userid,procnr,wflab) dyn task)
setWorkflowUser nuserid (DeletedWorkflow		(userid,procnr,wflab))			= (DeletedWorkflow		(nuserid,procnr,wflab))

getTask :: !WorflowProcess -> Task Dynamic
getTask (ActiveWorkflow 	(_,_,_) (TCl task))		= task 
getTask (SuspendedWorkflow  (_,_,_) (TCl task))		= task
getTask (FinishedWorkflow 	(_,_,_) _ (TCl task))	= task

isDeletedWorkflow :: !WorflowProcess -> Bool
isDeletedWorkflow (DeletedWorkflow _) = True
isDeletedWorkflow _	= False

workflowProcessStore ::  !((!Int,![WorflowProcess]) -> (!Int,![WorflowProcess])) !*TSt -> (!(!Int,![WorflowProcess]),!*TSt) 
workflowProcessStore wfs tst	
= 	IF_ClientTasks												
			(abort "Cannot access workflow process table on cleint\n")			// workflow table only on server site
			(workflowProcessStore` wfs tst)										// access workflow store
where
	workflowProcessStore` wfs tst=:{hst}	
	# (form,hst) = mkStoreForm (Init, pFormId workflowProcessStoreName (0,[]) <@ NoForm) wfs hst
	= (form.value,{tst & hst = hst})

scheduleWorkflows :: !(Task a) -> (Task a) | iData a
scheduleWorkflows maintask = scheduleWorkflows`
where
	scheduleWorkflows` tst 
	# (a,tst=:{activated}) 	= newTask defaultWorkflowName (assignTaskTo False 0 ("main",maintask)) tst	// start maintask
	# ((_,wfls),tst) 		= workflowProcessStore id tst												// read workflow process administration
	# (done,tst)			= scheduleWorkflowTable True wfls 0 {tst & activated = True}				// all added workflows processes are inspected (THIS NEEDS TO BE OPTIMIZED AT SOME STAGE)
	= (a,{tst & activated = activated && done})															// whole application ends when all processes have ended

scheduleWorkflowTable done [] _ tst = (done,tst)
scheduleWorkflowTable done [ActiveWorkflow _ (TCl dyntask):wfls] procid tst
# (_,tst=:{activated}) = dyntask {tst & activated = True}
= scheduleWorkflowTable (done && activated) wfls (inc procid) {tst & activated = activated}
scheduleWorkflowTable done [SuspendedWorkflow _ _:wfls] procid tst
= scheduleWorkflowTable done wfls (inc procid) tst
scheduleWorkflowTable done [FinishedWorkflow _ _ (TCl dyntask):wfls] procid tst	// just to show result in trace..
//# (_,tst) = dyntask tst
= scheduleWorkflowTable done wfls (inc procid) tst
scheduleWorkflowTable done [DeletedWorkflow _:wfls] procid tst
= scheduleWorkflowTable done wfls (inc procid) tst

spawnWorkflow :: !UserId !Bool !(LabeledTask a) -> Task (Wid a) | iData a
spawnWorkflow userid active (label,task) = \tst=:{options,staticInfo} -> (newTask ("spawn " +++ label) (spawnWorkflow` options)<<@ staticInfo.threadTableLoc) tst
where
	spawnWorkflow` options tst
	# ((processid,wfls),tst) 		
						= workflowProcessStore id tst							// read workflow process administration
	# (found,entry)		= findFreeEntry wfls 1									// found entry in table
	# processid			= processid + 1											// process id currently given by length list, used as offset in list
	# wfl				= mkdyntask options entry processid task 				// convert user task in a dynamic task
	# nwfls				= if found 
							(updateAt (entry - 1) (if active ActiveWorkflow SuspendedWorkflow (userid,processid,label) (TCl wfl)) wfls)
							(wfls ++ [if active ActiveWorkflow SuspendedWorkflow (userid,processid,label) (TCl wfl)])				// turn task into a dynamic task
	# (wfls,tst) 		= workflowProcessStore (\_ -> (processid,nwfls)) tst	// write workflow process administration
	# (_,tst)			= if active wfl (\tst -> (undef,tst)) tst				// if new workflow is active, schedule it in
	= (Wid (entry,(userid,processid,label)),{tst & activated = True})

	findFreeEntry :: [WorflowProcess] Int -> (Bool,Int)
	findFreeEntry [] n	= (False,n)
	findFreeEntry [DeletedWorkflow _:wfls] n = (True,n)
	findFreeEntry [_:wfls] n = findFreeEntry wfls (n + 1)

	mkdyntask options entry processid task 
	=  (\tst -> convertTask entry processid label task 
				{tst & tasknr = [entry - 1],activated = True,userId = userid, options = options,workflowLink = (entry,(userid,processid,label))})
	
	convertTask entry processid label task tst

	# ((processid,wfls),tst) 	= workflowProcessStore id tst					// read workflow process administration
	# wfl						= wfls!!(entry - 1)								// fetch entry
	# currentWorker				= getWorkflowUser wfl							// such that worker can be changed dynamically !
	# (a,tst=:{activated})		= newTask label (assignTaskTo False currentWorker ("main",task)) tst			

//	# (a,tst=:{activated})		= newTask label (assignTaskTo False userid ("main",task)) tst			
	# dyn						= dynamic a
	| not activated				= (dyn,tst)										// not finished, return
	# ((_,wfls),tst) 			= workflowProcessStore id tst					// read workflow process administration
	# wfls						= case (wfls!!(entry - 1)) of					// update process administration
										(ActiveWorkflow _ acttask) -> updateAt (entry - 1) (FinishedWorkflow (currentWorker,processid,label) dyn acttask) wfls
										_ -> wfls
	# (wfls,tst) 				= workflowProcessStore (\_ -> (processid,wfls)) tst		// write workflow process administration
	= (dyn,tst)												

changeWorkflowUser :: !UserId !(Wid a) -> Task Bool 
changeWorkflowUser nuser (Wid (entry,ids=:(_,_,label))) = newTask ("changeUser " +++ label) deleteWorkflow`
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
waitForWorkflow (Wid (entry,ids=:(_,_,label))) = newTask ("waiting for " +++ label) waitForResult`
where
	waitForResult` tst
	# ((_,wfls),tst) 	= workflowProcessStore id tst							// read workflow process administration
	# wfl				= wfls!!(entry - 1)										// fetch entry
	# refok				= isValidWorkflowReference wfl ids
	| not refok			= (createDefault,{tst & activated = False})				// wid does not refer to the correct entry anymore
	= case wfl of																// update process administration
			(FinishedWorkflow _ (val::a^) _) -> (Just val,{tst & activated = True})	// finished
			_ 					->  (Nothing,{tst & activated = False})	// not yet

deleteMe :: (Task Void)
deleteMe = deleteMe`
where
	deleteMe` tst=:{workflowLink} 
	=	(				deleteWorkflow (Wid workflowLink)
			=>> \_ ->	return_V Void ) tst

deleteWorkflow :: !(Wid a) -> Task Bool 
deleteWorkflow (Wid (entry,ids=:(_,_,label))) = newTask ("delete " +++ label) deleteWorkflow`
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
	# (_,tst)			= (getTask wfl) {tst & html = BT []}					// calculate workflow to delete for the last time to obtain all its itasks in the task tree
	# tst				= deleteSubTasksAndThreads [entry] tst					// delete all iTask storage of this process ...
	= (True,{tst & html = html})												// if everything is fine it should always succeed

suspendMe :: (Task Void)
suspendMe = suspendMe`
where
	suspendMe` tst=:{workflowLink = workflowLink=:(entry,ids)} 
	| entry == 0		= (Void,tst)											// main task cannot be handled
	=	(				suspendWorkflow (Wid workflowLink)
			=>> \_ ->	return_V Void ) tst

suspendWorkflow :: !(Wid a) -> Task Bool
suspendWorkflow (Wid (entry,ids=:(_,_,label))) = newTask ("suspend " +++ label) deleteWorkflow`
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
								(SuspendedWorkflow label susptask) -> activateWorkflow label susptask tst
								(DeletedWorkflow label) -> (False,True,DeletedWorkflow label,tst) // a deleted workflow cannot be suspendend
								wfl -> (True,True,wfl,tst)						// in case of finished or already activated flows
	| nochange			= (ok,{tst & activated = True})							// no change needed
	# nwfls				= updateAt (entry - 1) wfl wfls							// update entry
	# (wfls,tst) 		= workflowProcessStore (\_ -> (maxid,nwfls)) tst		// update workflow process administration
	= (ok,tst)																	// if everything is fine it should always succeed

	activateWorkflow label (TCl wfl) tst										// schedule workflow
	# (_,tst)	= wfl {tst & activated = True}
	= (True,False,ActiveWorkflow label (TCl wfl),{tst & activated = True})

getWorkflowStatus :: !(Wid a) -> Task WorkflowStatus
getWorkflowStatus (Wid (entry,ids=:(_,_,label))) = newTask ("get status " +++ label) getWorkflowStatus`
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

showWorkflows :: !Bool !*TSt -> (![BodyTag],*TSt)
showWorkflows alldone tst
= 	IF_ClientTasks												
		(\tst -> ([],tst))														// workflow table not available on clients
		(showWorkflows` alldone) tst											// show tables
where
	showWorkflows` alldone tst
	# ((_,wfls),tst) 		= workflowProcessStore id tst						// read workflow process administration
	= (mkTable wfls,tst)

	mkTable []		= []
	mkTable wfls	=	[showLabel ("Workflow Process Table:"),
						STable []	(   [ [showTrace "Entry:", showTrace "User Id:", showTrace "Process Id:", showTrace "Task Name:", showTrace "Status:"]
										, [Txt "0" , Txt "0", Txt "0", Txt defaultWorkflowName, if alldone (Txt "Finished") (Txt "Active")] 
										: [[Txt (toString i)] ++ showStatus wfl \\ wfl <- wfls & i <- [1..]]
										]
									),
						Hr []
						]
	showStatus (ActiveWorkflow 	 	(userid,processid,label) dyntask)		= [Txt (toString userid), Txt (toString processid), Txt label, Txt "Active"]
	showStatus (SuspendedWorkflow 	(userid,processid,label) dyntask)		= [Txt (toString userid), Txt (toString processid), Txt label, Txt "Suspended"]
	showStatus (FinishedWorkflow 	(userid,processid,label) dyn dyntask)	= [Txt (toString userid), Txt (toString processid), Txt label, Txt "Finished"]
	showStatus (DeletedWorkflow  	(userid,processid,label))				= [Txt (toString userid), Txt (toString processid), Txt label, Txt "Deleted"]
