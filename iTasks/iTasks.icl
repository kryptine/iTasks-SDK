implementation module iTasks

// (c) MJP 2006 - 2007

import StdEnv, StdiData

derive gForm 	[], Void, Maybe
derive gUpd 	[], Void, Maybe
derive gParse 	Void, Maybe
derive gPrint 	Void, Maybe
derive gerda 	Void

:: *TSt 		=	{ tasknr 		:: !TaskNr			// for generating unique form-id's
					, activated		:: !Bool   			// if true activate task, if set as result task completed	
					, userId		:: !Int				// id of user to which task is assigned
					, currentUserId	:: !Int				// id of application user 
					, html			:: !HtmlTree		// accumulator for html code
					, options		:: !Options			// iData lifespan and storage format
					, trace			:: !Maybe [Trace]	// for displaying task trace
					, hst			:: !HSt				// iData state
					}
:: TaskNr		:== [Int]								// task nr i.j is adminstrated as [j,i]
:: HtmlTree		=	BT [BodyTag]						// simple code
				|	(@@:) infix  0 (Int,String) HtmlTree// code with id of user attached to it
				|	(-@:) infix  0 Int 			HtmlTree// skip code with this id if it is the id of the user 
				|	(+-+) infixl 1 HtmlTree HtmlTree	// code to be placed next to each other				
				|	(+|+) infixl 1 HtmlTree HtmlTree	// code to be placed below each other				

:: Options		=	{ tasklife		:: !Lifespan		// default: Session		
					, taskstorage	:: !StorageFormat	// default: PlainString
					, taskmode		:: !Mode			// default: Edit
					, gc			:: !GarbageCollect	// default: Collect
					}
:: GarbageCollect =	Collect | NoCollect
:: Trace		=	Trace TraceInfo [Trace]				// traceinfo with possibly subprocess

:: TraceInfo	:== Maybe (Bool,(Int,TaskNr,String,String))	// Task finished? who did it, task nr, task name (for tracing) value produced

// *** First some small utility functions

instance == GarbageCollect
where
	(==) Collect Collect 		= True
	(==) NoCollect NoCollect 	= True
	(==) _ _ 					= False

// setting global iData options for tasks

initialOptions	=	{ tasklife 		= Session
					, taskstorage 	= PlainString
					, taskmode 		= Edit 
					, gc			= Collect
					}

class 	(<<@) infixl 3 b ::  (Task a) b  -> (Task a) 
instance <<@  Lifespan
where   (<<@) task lifespan			= \tst -> task {tst & options.tasklife = lifespan}
instance <<@  StorageFormat
where   (<<@) task storageformat 	= \tst -> task {tst & options.taskstorage = storageformat}
instance <<@  Mode
where   (<<@) task mode 			= \tst -> task {tst & options.taskmode = mode}
instance <<@  GarbageCollect
where   (<<@) task gc 				= \tst -> task {tst & options.gc = gc}

// Lifting hst domain to the tst domain, for convenience

LiftHst fun tst=:{hst}
# (form,hst) = fun hst
= (form,{tst & hst = hst})

// Storage options settings

cFormId  {tasklife,taskstorage,taskmode} s d = {sFormId  s d & lifespan = tasklife, storage = taskstorage, mode = taskmode}
cdFormId {tasklife,taskstorage,taskmode} s d = {sdFormId s d & lifespan = tasklife, storage = taskstorage, mode = taskmode}

// Garbage collection on iTask administration is done dependening on gc option chosen

import Debug
debug name a = debugValue (\a -> ["\n*** " <+++ name <+++ a]) a

deleteSubTasks :: !TaskNr TSt -> TSt
deleteSubTasks tasknr tst=:{hst,userId,options} 
| options.gc == NoCollect 	= tst
| otherwise					= {tst & hst = deleteIData (iTaskId userId tasknr "") hst}
//| otherwise					= {tst & hst = deleteIData (subtasksids tasknr) hst}
where
	subtasksids tasknr formid
	# prefix 	= iTaskId userId tasknr ""
	# lprefix 	= size prefix
	# lformid	= size formid
	| lformid < lprefix	= False
	= debug "deleted " (debug "prefix " prefix == debug "formid " formid%(0,lprefix-1))

// *** wrappers, to be used in combination with an iData wrapper...

startTask :: !Int !Bool !(Task a) !*HSt -> (a,[BodyTag],!*HSt) | iCreate a 
startTask thisUser traceOn taska hst 
# (a,body,tst) = startTstTask thisUser traceOn taska tst
= (a,body,tst.hst)
where
	tst	=	{ tasknr		= [-1]
			, activated 	= True
			, currentUserId	= thisUser 
			, userId		= if (thisUser >= 0) defaultUser thisUser
			, html 			= BT []
			, trace			= Nothing
			, hst 			= hst
			, options 		= initialOptions
			}

startNewTask :: !Int !Bool !(Task a) -> Task a | iCreateAndPrint a 
startNewTask newUser traceOn taska = mkTask "startNewTask" startNewTask`
where
	startNewTask` tst=:{html} 
	# (a,body,tst) 	= startTstTask newUser traceOn taska {tst & html = BT [], currentUserId = newUser, userId = defaultUser, tasknr = [-1]}
	= (a,{tst & html = html +|+ BT body})

singleUserTask :: !Int !Bool !(Task a) !*HSt -> (Html,*HSt) | iCreate a 
singleUserTask userId traceOn task hst 
# (_,html,hst) = startTask userId traceOn task hst
= mkHtml "stest" html hst

multiUserTask :: !Int !Bool !(Task a) !*HSt -> (Html,*HSt) | iCreate a 
multiUserTask nusers traceOn task  hst 
# (idform,hst) 	= FuncMenu (Init,nFormId "User_Selected" 
						(0,[("User " +++ toString i,\_ -> i) \\ i<-[0..nusers - 1] ])) hst
# currentWorker	= snd idform.value
# (_,html,hst) 	= startTask currentWorker traceOn task hst
= mkHtml "mtest" (ifTraceOn idform.form ++ html) hst
where
	ifTraceOn form = if traceOn form []

multiUserTask2 :: !(!Int,!Int) !Int !Bool !(Task a) !*HSt -> (Html,*HSt) | iCreate a 
multiUserTask2 (minutes,seconds) nusers traceOn task  hst 
# (idform,hst) 	= FuncMenu (Init,nFormId "User_Selected" 
						(0,[("User " +++ toString i,\_ -> i) \\ i<-[0..nusers - 1] ])) hst
# currentWorker	= snd idform.value
# (_,html,hst) 	= startTask currentWorker traceOn task hst
= mkxHtml "mtest" (idform.form ++ html) hst
where
	mkxHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [Hd_Script [] (autoRefresh minutes seconds)]
	body tags 			= Body [onloadBody] tags
	onloadBody 			= `Batt_Events [OnLoad (SScript scriptName)]
	scriptName 			= "beginrefresh()"


startTstTask :: !Int !Bool !(Task a) !*TSt -> (a,[BodyTag],!*TSt) | iCreate a 
startTstTask thisUser traceOn taska tst=:{hst}
| thisUser < 0 
	# (a,tst=:{html}) 	= taska {tst & hst = hst}
	= (a, noFilter html, {tst & html = html})
# userVersionNr			= "User" <+++ thisUser <+++ "_VersionPNr"
# sessionVersionNr		= "User" <+++ thisUser <+++ "_VersionSNr" 
# traceId				= "User" <+++ thisUser <+++ "_Trace" 
# (pversion,hst)	 	= mkStoreForm (Init, pFormId userVersionNr 0) id hst
# (refresh,hst) 		= simpleButton userVersionNr "Refresh" id hst
# (traceAsked,hst) 		= simpleButton traceId "ShowTrace" (\_ -> True) hst
# doTrace				= traceAsked.value False
# (sversion,hst)	 	= mkStoreForm (Init, nFormId sessionVersionNr pversion.value) (if refresh.changed (\_ -> pversion.value) id) hst
| sversion.value < pversion.value	= (createDefault,  refresh.form ++ [Br,Br, Hr [],Br] <|.|>
														[Font [Fnt_Color (`Colorname Yellow)]
													   [B [] "Sorry, cannot apply command.",Br, 
													    B [] "Your page is not up-to date!",Br]],{tst & hst = hst})
# (a,tst=:{html,hst,trace}) = taska {tst & hst = hst, trace = if doTrace (Just []) Nothing}
# (pversion,hst)	 	= mkStoreForm (Init, pFormId userVersionNr 0) inc hst
# (sversion,hst)	 	= mkStoreForm (Init, nFormId sessionVersionNr pversion.value) inc hst
# (selbuts,selname,seltask,hst)	= Filter thisUser defaultUser ((defaultUser,"Main") @@: html) hst
= 	(a,	refresh.form ++ ifTraceOn traceAsked.form ++
		[Br,Hr [],showUser thisUser,Br,Br] ++ 
		if (doTrace && traceOn)
			[ printTrace2 trace ]
			[ STable []	[ [BodyTag  selbuts, selname <||>  seltask ]
						]
			] 
	,{tst & hst = hst})
where
	ifTraceOn form = if traceOn form []

	mkSTable2 :: [[BodyTag]] -> BodyTag
	mkSTable2 table
	= Table []	(mktable table)
	where
		mktable table 	= [Tr [] (mkrow rows) \\ rows <- table]	
		mkrow rows 		= [Td [Td_VAlign Alo_Top] [row] \\ row <- rows] 
	
	Filter thisuser taskuser tree hst
	# (_,accu) 		= Collect thisuser taskuser [] tree
	| isNil accu	= ([],[],[],hst)
	# (names,tasks) = unzip accu
	# info			= initialOptions
	# ((selected,buttons,chosenname),hst) = mkTaskButtons "Main Tasks:" ("User " <+++ thisuser) thisuser [] info names hst 
	= (buttons,chosenname,tasks!!if (selected > length accu) 0 selected,hst)

	Collect thisuser taskuser accu (BT bdtg)
	| thisuser == taskuser	= (bdtg,accu)
	| otherwise				= ([],accu)
	Collect thisuser taskuser accu (tree1 +|+ tree2)
	# (lhtml,accu)	= Collect thisuser taskuser accu tree1
	# (rhtml,accu)	= Collect thisuser taskuser accu tree2
	= (lhtml <|.|> rhtml,accu)
	Collect thisuser taskuser accu (tree1 +-+ tree2)
	# (lhtml,accu)	= Collect thisuser taskuser accu tree1
	# (rhtml,accu)	= Collect thisuser taskuser accu tree2
	= ([lhtml <=> rhtml],accu)
	Collect thisuser taskuser accu ((ntaskuser,taskname) @@: tree) 	// Collect returns the wanted code, and the remaining code
	# (myhtml,accu)	= Collect thisuser ntaskuser accu tree			// Collect all code of this user belonging to this task
	| thisuser == ntaskuser && not (isNil myhtml)
							= ([],[(taskname,myhtml):accu])
	| otherwise				= ([],accu)
	Collect thisuser taskuser accu (nuser -@: tree)
	| thisuser == nuser 	= ([],accu)
	| otherwise				= Collect thisuser taskuser accu tree

	isNil [] = True
	isNil _ = False

	noFilter (BT body) 			= body
	noFilter (_ @@: html) 		= noFilter html
	noFilter (_ -@: html) 		= noFilter html
	noFilter (htmlL +-+ htmlR) 	= [noFilter htmlL  <=>  noFilter htmlR]
	noFilter (htmlL +|+ htmlR) 	=  noFilter htmlL <|.|> noFilter htmlR

mkTaskButtons :: !String !String !Int !TaskNr !Options ![String] *HSt -> ((Int,[BodyTag],[BodyTag]),*HSt)
mkTaskButtons header myid userId tasknr info btnnames hst
# btnsId			= iTaskId userId tasknr (myid <+++ "genBtns")
# myidx				= length btnnames
# (chosen,hst)		= SelectStore (myid,myidx) tasknr info id hst						// which choice was made in the past
# (buttons,hst)		= SelectButtons Init btnsId info (chosen,btnnames) hst				// create buttons
# (chosen,hst)		= SelectStore (myid,myidx) tasknr info  buttons.value hst			// maybe a new button was pressed
# (buttons,hst)		= SelectButtons Set btnsId info (chosen,btnnames) hst				// adjust look of that button
= ((chosen,[red header, Br: buttons.form],[yellow (btnnames!!chosen),Br,Br]),hst)
where
	SelectButtons init id info (idx,btnnames) hst = TableFuncBut2 (init,cFormId info id 
															[[(mode idx n, but txt,\_ -> n)] \\ txt <- btnnames & n <- [0..]] <@ Page) hst
	but i = LButton defpixel i

	mode i j
	| i==j = Display

	= Edit

	SelectStore :: !(String,Int) !TaskNr !Options (Int -> Int) *HSt -> (Int,*HSt)
	SelectStore (myid,idx) tasknr info fun hst 
	# storeId 			= iTaskId userId tasknr (myid <+++ "BtnsS" <+++ idx)
	# (storeform,hst)	= mkStoreForm (Init,cFormId info storeId 0) fun hst
	= (storeform.value,hst)


// make an iTask editor

editTask :: String a -> (Task a) | iData a 
editTask prompt a = mkTask "editTask" (editTask` prompt a)

editTask` prompt a tst=:{tasknr,html,hst,userId}
# taskId			= iTaskId userId tasknr "EdFin"
# editId			= iTaskId userId tasknr "EdVal"
# buttonId			= iTaskId userId tasknr "EdBut"
# (taskdone,hst) 	= mkStoreForm (Init,cFormId tst.options taskId False) id hst  			// remember if the task has been done
| taskdone.value																			// test if task has completed
	# (editor,hst) 	= (mkEditForm  (Init,cdFormId tst.options editId a <@ Display) hst)		// yes, read out current value, make editor passive
	= (editor.value,{tst & activated = True, html = html +|+ BT editor.form, hst = hst})	// return result task
# (editor,hst) 		= mkEditForm  (Init,cFormId tst.options editId a) hst					// no, read out current value from active editor
# (finbut,hst)  	= simpleButton buttonId prompt (\_ -> True) hst							// add button for marking task as done
# (taskdone,hst) 	= mkStoreForm (Init,cFormId tst.options taskId False) finbut.value hst 	// remember task status for next time
| taskdone.value	= editTask` prompt a {tst & hst = hst}									// task is now completed, handle as previously
= (editor.value,{tst & activated = taskdone.value, html = html +|+ BT (editor.form ++ finbut.form), hst = hst})

// monads for combining itasks

(=>>) infix 1 :: (Task a) (a -> Task b) -> Task b | iCreateAndPrint b
(=>>) taska taskb = mybind
where
	mybind tst=:{options}
	# (a,tst=:{activated}) = taska tst
	= taskb a {tst & options = options}
/*	| activated	= taskb a {tst & options = options}
	= (createDefault,tst)*/

(#>>) infixl 1 :: (Task a) (Task b) -> Task b
(#>>) taska taskb = mybind
where
	mybind tst=:{options}
	# (a,tst) = taska tst
	= taskb {tst & options = options}

return_V :: a -> (Task a) | iCreateAndPrint a
return_V a  = mkTask "return_V" (return a) 

// some simple variants

return_D :: a -> (Task a) | gForm {|*|}, iCreateAndPrint a
return_D a = mkTask "return_D" return_Display`
where
	return_Display` tst
	= (a,{tst & html = tst.html +|+ BT [toHtml a ]})		// return result task

return_VF :: a [BodyTag] -> (Task a) | iCreateAndPrint a
return_VF a bodytag = mkTask "return_VF" return_VF`
where
	return_VF` tst
	= (a,{tst & html = tst.html +|+ BT bodytag})

(<|) infix 6 :: (Task a) (a -> .Bool, a -> [BodyTag]) -> Task a | iCreate a
(<|) taska (pred,message) = doTask
where
	doTask tst=:{html = ohtml,activated}
	| not activated 					= (createDefault,tst)
	# (a,tst=:{activated,html= nhtml}) 	= taska {tst & html = BT []}
	| not activated || pred a			= (a,{tst & html = ohtml +|+ nhtml})
	= doTask {tst & html = ohtml +|+ BT (message a)}

(?>>) infix 5 :: [BodyTag] (Task a) -> (Task a) | iCreate a
(?>>) prompt task = doTask
where
	doTask tst=:{html=ohtml,activated}
	| not activated						= (createDefault,tst)
	# (a,tst=:{activated,html=nhtml}) 	= task {tst & html = BT []}
	| activated 						= (a,{tst & html = ohtml})
	= (a,{tst & html = ohtml +|+ BT prompt +|+ nhtml})

(!>>) infix 5 :: [BodyTag] (Task a) -> (Task a) | iCreate a
(!>>) prompt task = doTask
where
	doTask tst=:{html=ohtml,activated=myturn}
	| not myturn			= (createDefault,tst)
	# (a,tst=:{html=nhtml}) = task {tst & html = BT []}
	= (a,{tst & html = ohtml +|+ BT prompt +|+ nhtml})

// Task makers are wrappers which take care of
//		- deciding whether a task should be called (activated) or not
//		- adding trace information
//		- generating task numbers in a systematic way
// It is very important that the numbering of the tasks is done systematically
// Every task should have a unique number
// Every sequential task should increase the task number
// If a task j is a subtask of task i, than it will get number i.j in reverse order
	
mkTask :: !String (Task a) -> (Task a) | iCreateAndPrint a
mkTask taskname mytask = mkTaskNoInc taskname mytask o incTaskNr

mkTaskNoInc :: !String (Task a) -> (Task a) | iCreateAndPrint a			// common second part of task wrappers
mkTaskNoInc taskname mytask = mkTaskNoInc`
where
	mkTaskNoInc` tst=:{activated,tasknr,userId}		
	| not activated							= (createDefault,tst)	// not active, don't call task, return default value
	# (val,tst=:{activated,trace})			= mytask tst			// active, so perform task and get its result
	| isNothing trace || taskname == ""		= (val,tst)				// no trace, just return value
	= (val,{tst & tasknr = tasknr
				, trace = Just (InsertTrace activated tasknr userId taskname (printToString val) (fromJust trace))}) // adjust trace

incTaskNr tst 		= {tst & tasknr = incNr tst.tasknr}
newSubTaskNr tst	= {tst & tasknr = [-1:tst.tasknr]}

incNr [] = [0]
incNr [i:is] = [i+1:is]

addTasknr [] j = [j]
addTasknr [i:is] j = [i+j:is]

internEditSTask tracename prompt task = \tst -> mkTask tracename ((editTask` prompt task <<@ Page) <<@ Edit) tst

/////////////////////////////////////

// the following definitions are the simple versions
// they work, but are NOT suited for big applications
// I will kick them out some day....

repeatTask_Std :: (a -> Task a) (a -> Bool) -> a -> Task a | iCreateAndPrint a
repeatTask_Std task pred = \a -> mkTask "repeatTask_Std" (dorepeatTask_Std a)
where
	dorepeatTask_Std a tst
	# (na,tst)	= task a (newSubTaskNr tst)
	| pred na	= (na,tst)
	= dorepeatTask_Std na (incTaskNr tst)


foreverTask_Std :: (Task a) -> Task a | iCreateAndPrint a
foreverTask_Std task = mkTask "foreverTask_Std" doforeverTask_Std
where
	doforeverTask_Std tst		
	# (_,tst)	= task (newSubTaskNr tst)		
	= foreverTask_Std task tst						

newTask_Std :: !String (Task a) -> (Task a) | iCreateAndPrint a
newTask_Std taskname mytask = mkTask taskname (mytask o newSubTaskNr)

// same, but by remembering task results stack space can be saved
newTask :: !String (Task a) -> (Task a) 	| iData a 
newTask taskname mytask = mkTask taskname newTask`
where
	newTask` tst=:{tasknr,userId,options}		
	# taskId					= iTaskId userId tasknr taskname
	# (taskval,tst) 			= LiftHst (mkStoreForm (Init,cFormId options taskId (False,createDefault)) id) tst  // remember if the task has been done
	# (taskdone,taskvalue)		= taskval.value										// select values
	| taskdone					= (taskvalue,tst)									// if rewritten return stored value
	# (val,tst=:{activated})	= mytask {tst & tasknr = [-1:tasknr]} 				// do task, first shift tasknr
	| not activated				= (val,{tst & tasknr = tasknr, options = options})	// subtask not ready, return value of subtasks
	# tst						= deleteSubTasks tasknr tst							// task ready, garbage collect it
	# (_,tst) 					= LiftHst (mkStoreForm (Init,cFormId options taskId (False,createDefault)) (\_ -> (True,val))) tst  // remember if the task has been done
	= (val,{tst & tasknr = tasknr, options = options})

// foreverTask is a loop
// when gc option set and task finished, it will throw away all subtasks and start all over
// otherwise, when task finshed it will remember the new tasknr to prevent checking of previously finished tasks

foreverTask :: (Task a) -> Task a | iData a
foreverTask task = mkTask "foreverTask" foreverTask`
where
	foreverTask` tst=:{tasknr,activated,userId,options} 
	| options.gc == Collect																				// garbace collect everything when task finsihed
		# (val,tst=:{activated})= task {tst & tasknr = [-1:tasknr]}										// shift tasknr
		| activated 			= foreverTask` (deleteSubTasks tasknr {tst & tasknr = tasknr, options = options}) 			// loop
		= (val,tst)					
	# taskId					= iTaskId userId tasknr "ForSt"											// create store id
	# (currtasknr,tst)			= LiftHst (mkStoreForm (Init,cFormId options taskId tasknr) id) tst		// fetch actual tasknr
	# (val,tst=:{activated})	= task {tst & tasknr = [-1:currtasknr.value]}
	| activated 																						// task is completed	
		# ntasknr				= incNr currtasknr.value												// incr tasknr
		# (currtasknr,tst)		= LiftHst (mkStoreForm (Init,cFormId options taskId tasknr) (\_ -> ntasknr)) tst // store next task nr
		= foreverTask` {tst & tasknr = tasknr, options = options}										// initialize new task
	= (val,tst)					

// parallel subtask creation utility

mkParSubTask :: !String !Int (Task a) -> (Task a)  | iCreateAndPrint a		// two shifts are needed
mkParSubTask name i task = mkParSubTask`
where
	mkParSubTask` tst=:{tasknr}
	# (v,tst) = mkTaskNoInc (name <+++ "." <+++ i) mysubtask {tst & tasknr = [i:tasknr],activated = True} // shift task
	= (v,{tst & tasknr = tasknr})
	where
		mysubtask tst=:{tasknr} = task {tst & tasknr = [-1:tasknr], activated = True}	// shift once again!

// assigning tasks to users, each user is identified by a number

(@:) infix 3 :: !(!String,!Int) (Task a)	-> (Task a)			| iCreateAndPrint a
(@:) (taskname,nuserId) taska = \tst=:{userId} -> assignTask` userId {tst & userId = nuserId}
where
	assignTask` userId tst=:{html=ohtml,activated,userId = nuserId}
	| not activated						= (createDefault,tst)
	# (a,tst=:{html=nhtml,activated})	= taska {tst & html = BT [],userId = nuserId}		// activate task of indicated user
	| activated 						= (a,{tst & activated = True						// work is done	
												  ,	userId = userId							// restore previous user id						
												  ,	html = ohtml +|+ 						// show old code						
														   ((nuserId,taskname) @@: nhtml)})	// plus new one tagged
	= (a,{tst & userId = userId																// restore user Id
			  , html = 	ohtml +|+ 															// show old code
						BT [Br, Txt ("Waiting for Task "), yellow taskname, Txt " from ", showUser nuserId,Br] +|+  // show waiting for
						((nuserId,taskname) @@: BT [Txt "Requested by ", showUser userId,Br,Br] +|+ // show requested by
												 nhtml)})									// plus new one tagged				

(@::) infix 3 :: !Int (Task a)	-> (Task a)			| iCreate  a
(@::) nuserId taska = \tst=:{userId} -> assignTask` userId {tst & userId = nuserId}
where
	assignTask` userId tst=:{html,activated}
	| not activated						= (createDefault,tst)
	# (a,tst=:{html=nhtml,activated})	= taska {tst & html = BT [],userId = nuserId}		// activate task of indicated user
	| activated 						= (a,{tst & userId = userId							// work is done						
												  ,	html = html})	
	= (a,{tst & userId = userId																// restore user Id
			  , html = 	html +|+  
			  			BT [Br, Txt "Waiting for ", yellow ("Task " <+++ userId), Txt " from ", showUser nuserId,Br] +|+ 
						((nuserId,"Task " <+++ userId) @@: 
							BT [Txt "Requested by ", showUser userId,Br,Br] +|+ nhtml)})				// combine html code, filter later					

// sequential tasks

seqTasks :: [(String,Task a)] -> (Task [a])| iCreateAndPrint a
seqTasks options = mkTask "seqTasks" seqTasks`
where
	seqTasks` tst=:{tasknr}
	# (val,tst)	 = doseqTasks options [] {tst & tasknr = [-1:tasknr]}
	= (val,{tst & tasknr = tasknr})

	doseqTasks [] accu tst 		= (reverse accu,{tst & activated = True})
	doseqTasks [(taskname,task):ts] accu tst=:{html,options} 
	# (a,tst=:{activated=adone,html=ahtml}) 
									= task {tst & activated = True, html = BT []}
	| not adone						= (reverse accu,{tst & html = html +|+ BT [yellow taskname,Br,Br] +|+ ahtml})
	= doseqTasks ts [a:accu] {tst & html = html +|+ ahtml, options = options}

// choose one or more tasks out of a collection
buttonTask :: String (Task a) -> (Task a) | iCreateAndPrint a
buttonTask s task = iCTask_button "buttonTask" [(s,task)]

iCTask_button tracename options = mkTask tracename (dochooseTask True options)

chooseTask :: [(String,Task a)] -> (Task a) | iCreateAndPrint a
chooseTask options = mkTask "chooseTask" (dochooseTask True options)

chooseTaskV :: [(String,Task a)] -> (Task a) | iCreateAndPrint a
chooseTaskV options = mkTask "chooseTask" (dochooseTask False options)

dochooseTask _ [] tst			= return createDefault tst				
dochooseTask horizontal taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
# taskId						= iTaskId userId tasknr ("ChoSt" <+++ length taskOptions)
# buttonId						= iTaskId userId tasknr "ChoBut"
# (chosen,tst)					= LiftHst (mkStoreForm  (Init,cFormId options taskId -1) id) tst
| chosen.value == -1			// no choice made yet
	# allButtons				= if horizontal 
										[[(but txt,\_ -> n) \\ txt <- map fst taskOptions & n <- [0..]]]
										[[(but txt,\_ -> n)] \\ txt <- map fst taskOptions & n <- [0..]]
	# (choice,tst)				= LiftHst (TableFuncBut (Init,cFormId options buttonId allButtons <@ Page)) tst
	# (chosen,tst)				= LiftHst (mkStoreForm  (Init,cFormId options taskId -1) choice.value) tst
	| chosen.value == -1		= (createDefault,{tst & activated =False,html = html +|+ BT choice.form})
	# chosenTask				= snd (taskOptions!!chosen.value)
	# (a,tst=:{activated=adone,html=ahtml}) = chosenTask {tst & tasknr = [-1:tasknr], activated = True, html = BT []}
	= (a,{tst & tasknr = tasknr, activated = adone, html = html +|+ ahtml})
# chosenTask					= snd (taskOptions!!chosen.value)
# (a,tst=:{activated=adone,html=ahtml}) = chosenTask {tst & tasknr = [-1:tasknr], activated = True, html = BT []}
= (a,{tst & tasknr = tasknr, activated = adone, html = html +|+ ahtml})

but i = LButton defpixel i

chooseTask_pdm :: [(String,Task a)] -> (Task a) |iCreateAndPrint a
chooseTask_pdm taskOptions = mkTask "chooseTask_pdm" (dochooseTask_pdm taskOptions)
where
	dochooseTask_pdm [] tst			= (createDefault,{tst& activated = True})	
	dochooseTask_pdm taskOptions tst=:{tasknr,html,userId,options}								// choose one subtask out of the list
	# taskId						= iTaskId userId tasknr ("ChoPdm" <+++ length taskOptions)
	# (choice,tst)					= LiftHst (FuncMenu  (Init,cFormId options taskId (0,[(txt,id) \\ txt <- map fst taskOptions]))) tst
	# (_,tst=:{activated=adone,html=ahtml})	
									= internEditSTask "" "Done" Void {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 	
	| not adone						= (createDefault,{tst & activated = False, html = html +|+ BT choice.form +|+ ahtml, tasknr = tasknr})
	# chosenIdx						= snd choice.value
	# chosenTask					= snd (taskOptions!!chosenIdx)
	# (a,tst=:{activated=bdone,html=bhtml}) 
									= chosenTask {tst & activated = True, html = BT [], tasknr = [0:tasknr]}
	= (a,{tst & activated = adone&&bdone, html = html +|+ bhtml, tasknr = tasknr})

mchoiceTasks :: [(String,Task a)] -> (Task [a]) | iCreateAndPrint a
mchoiceTasks taskOptions = mkTask "mchoiceTask" (domchoiceTasks taskOptions)
where
	domchoiceTasks [] tst	= ([],{tst& activated = True})
	domchoiceTasks taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
	# seltaskId				= iTaskId userId tasknr ("MtpChSel" <+++ length taskOptions)
	# donetaskId			= iTaskId userId tasknr "MtpChSt"
	# (cboxes,tst)			= LiftHst (ListFuncCheckBox (Init,cFormId options seltaskId initCheckboxes)) tst
	# (done,tst)			= LiftHst (mkStoreForm      (Init,cFormId options donetaskId False) id) tst
	# optionsform			= cboxes.form <=|> [Txt text \\ (text,_) <- taskOptions]
	| done.value			= seqTasks [option \\ option <- taskOptions & True <- snd cboxes.value] {tst & tasknr = [0:tasknr]}
	# (_,tst=:{html=ahtml,activated = adone})
							= (internEditSTask "" "OK" Void) {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 
	| not adone				= ([],{tst & html = html +|+ BT [optionsform] +|+ ahtml})
	# (_,tst)				= LiftHst (mkStoreForm      (Init,cFormId options donetaskId False) (\_ -> True)) tst
	= domchoiceTasks taskOptions {tst & tasknr = tasknr, html = html, options = options, userId =userId, activated = True}									// choose one subtask out of the list

	initCheckboxes  = 
		[(CBNotChecked  text,  \ b bs id -> id) \\ (text,_) <- taskOptions]

// tasks ending as soon as one of its subtasks completes

(-||-) infixr 3 :: (Task a) (Task a) -> (Task a) | iCreateAndPrint a
(-||-) taska taskb = mkTask "-||-" (doOrTask (taska,taskb))

orTask :: (Task a,Task a) -> (Task a) | iCreateAndPrint a
orTask (taska,taskb) = mkTask "orTask" (doOrTask (taska,taskb))

doOrTask (taska,taskb) tst=:{tasknr,options,html,userId}
# taskId								= iTaskId userId tasknr "orTaskSt"
# (chosen,tst)							= LiftHst (mkStoreForm  (Init,cFormId options taskId -1) id) tst
| chosen.value == 0						// task a was finished first in the past
	# (a,tst)							= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT []}
	= (a,{tst & html = html})
| chosen.value == 1						// task b was finished first in the past
	# (b,tst)							= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
	= (b,{tst & html = html})
# (a,tst=:{activated=adone,html=ahtml})	= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT [],options = options}
# (b,tst=:{activated=bdone,html=bhtml})	= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT [],options = options}
| adone
	# tst 								= deleteSubTasks [1:tasknr] {tst & tasknr = tasknr}
	# (chosen,tst)						= LiftHst (mkStoreForm  (Init,cFormId options taskId -1) (\_ -> 0)) {tst & html = BT []}
	= (a,{tst & html = html, activated = True})
| bdone
	# tst 								= deleteSubTasks [0:tasknr] {tst & tasknr = tasknr}
	# (chosen,tst)						= LiftHst (mkStoreForm  (Init,cFormId options taskId -1) (\_ -> 1)) {tst & html = BT []}
	= (b,{tst & html = html, activated = True})
= (a,{tst & activated = False, html = html +|+ ahtml +|+ bhtml})
 

orTask2 :: (Task a,Task b) -> (Task (EITHER a b)) | iCreateAndPrint a & iCreateAndPrint b
orTask2 (taska,taskb) = mkTask "orTask2" (doorTask2 (taska,taskb))
where
	doorTask2 (taska,taskb) tst=:{tasknr,html,options,userId}
	# taskId								= iTaskId userId tasknr "orTask2St"
	# (chosen,tst)							= LiftHst (mkStoreForm  (Init,cFormId options taskId -1) id) tst
	| chosen.value == 0						// task a was finished first in the past
		# (a,tst=:{html=ahtml})				= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT []}
		= (LEFT a,{tst & html = html})
	| chosen.value == 1						// task b was finished first in the past
		# (b,tst=:{html=bhtml})				= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
		= (RIGHT b,{tst & html = html})
	# (a,tst=:{activated=adone,html=ahtml})	= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT []}
	# (b,tst=:{activated=bdone,html=bhtml})	= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
	| adone
		# tst 								= deleteSubTasks [1:tasknr] {tst & tasknr = tasknr}
		# (chosen,tst)						= LiftHst (mkStoreForm  (Init,cFormId options taskId -1) (\_ -> 0)) {tst & html = BT []}
		= (LEFT a,{tst & html = html, activated = True})
	| bdone
		# tst 								= deleteSubTasks [0:tasknr] {tst & tasknr = tasknr}
		# (chosen,tst)						= LiftHst (mkStoreForm  (Init,cFormId tst.options taskId -1) (\_ -> 1)) {tst & html = BT []}
		= (RIGHT b,{tst & html = html, activated = True})
	= (LEFT a,{tst & activated = False, html = html +|+ ahtml +|+ bhtml})

checkAnyTasks traceid taskoptions (ctasknr,skipnr) (bool,which) tst=:{tasknr}
| ctasknr == length taskoptions	= (bool,which,tst)
| ctasknr == skipnr				= checkAnyTasks traceid taskoptions (inc ctasknr,skipnr) (bool,which) tst
# task							= taskoptions!!ctasknr
# (a,tst=:{activated = adone})	= mkParSubTask traceid ctasknr task {tst & tasknr = tasknr, activated = True}
= checkAnyTasks traceid taskoptions (inc ctasknr,skipnr) (bool||adone,if adone ctasknr which) {tst & tasknr = tasknr, activated = True}

orTasks :: [(String,Task a)] -> (Task a) | iData a
orTasks taskOptions = mkTask "orTasks" (doorTasks taskOptions)
where
	doorTasks [] tst	= return createDefault tst
	doorTasks tasks tst=:{tasknr,html,options,userId}
	# taskId			= iTaskId userId tasknr "orTasksChosen"
	# (chosenS,tst)		= LiftHst (mkStoreForm  (Init,cFormId options taskId (-1,createDefault)) id) tst
	# (pchosen,avalue)	= chosenS.value
	| pchosen <> -1		// one of the tasks has been finished already
		= (avalue,{tst & activated = True, html = html}) 
	# ((chosen,buttons,chosenname),tst) 
						= LiftHst (mkTaskButtons "or Tasks:" "or" userId tasknr options (map fst taskOptions)) {tst & html = BT []}
	# (finished,which,tst=:{html=allhtml})= checkAnyTasks "orTasks" (map snd taskOptions) (0,chosen) (False,0) {tst & html = BT [], activated = True}
	# chosenvalue		= if finished which chosen			// it can be the case that someone else has finshed one of the tasks
	# chosenTaskName	= fst (taskOptions!!chosenvalue)
	# chosenTask		= snd (taskOptions!!chosenvalue)
	# (a,tst=:{activated=adone,html=ahtml}) 
						= mkParSubTask "orTasks" chosenvalue chosenTask {tst & tasknr = tasknr, activated = True, html = BT []}
	| not adone			= (a,{tst 	& activated = adone
									, html =	html +|+ 
												BT buttons +-+ 	(BT chosenname +|+ ahtml) +|+ 
												(userId -@: allhtml)
							})
	# tst 				= deleteSubTasks tasknr {tst & tasknr = tasknr}
	# (_,tst)			= LiftHst (mkStoreForm  (Init,cFormId options taskId (-1,createDefault)) (\_ -> (chosenvalue,a)))  {tst & html = BT []} // remember finished task for next tim
	= (a,{tst & activated = adone, html = html, tasknr = tasknr}) 

// Parallel tasks ending if all complete

(-&&-) infixr 4 ::  (Task a) (Task b) -> (Task (a,b)) | iCreateAndPrint a & iCreateAndPrint b
(-&&-) taska taskb = mkTask "-&&-" (doAndTask (taska,taskb))

andTask :: (Task a,Task b) -> (Task (a,b)) | iCreateAndPrint a & iCreateAndPrint b
andTask (taska,taskb) = mkTask "andTask" (doAndTask (taska,taskb))

doAndTask (taska,taskb) tst=:{tasknr,html}
# (a,tst=:{activated=adone,html=ahtml})	= mkParSubTask "andTask" 0 taska {tst & html = BT []}
# (b,tst=:{activated=bdone,html=bhtml})	= mkParSubTask "andTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
= ((a,b),{tst & activated = adone&&bdone, html = html +|+ ahtml +|+ bhtml})

andTasks :: [(String,Task a)] -> (Task [a]) | iCreateAndPrint a
andTasks taskOptions = mkTask "andTasks" (doandTasks taskOptions)
where
	doandTasks [] tst	= return [] tst
	doandTasks taskOptions tst=:{tasknr,html,options,userId}
	# (alist,tst=:{activated=finished})		
						= checkAllTasks "andTasks" taskOptions (0,-1) True [] {tst & html = BT [], activated = True}
	| finished			= (map snd alist,{tst & html = html}) 		// all andTasks are finished
	# ((chosen,buttons,chosenname),tst) 							// user can select one of the tasks to work on
						= LiftHst (mkTaskButtons "and Tasks:" "and" userId tasknr options (map fst taskOptions)) tst
	# chosenTask		= snd (taskOptions!!chosen)
	# chosenTaskName	= fst (taskOptions!!chosen)
	# (a,tst=:{activated=adone,html=ahtml}) 						// enable the selected task (finished or not)
						= mkParSubTask "andTasks" chosen chosenTask {tst & tasknr = tasknr, activated = True, html = BT []}
	# (alist,tst=:{activated=finished,html=allhtml})				// check again whether all other tasks are now finished, collect their code		
						= checkAllTasks "andTasks" taskOptions (0,chosen) True [] {tst & tasknr = tasknr, html = BT [], activated = True}
	| not adone			= ([a],{tst &	activated = False 			// not done, since chosen task not finished
									, 	html = 	html +|+ 
												BT buttons +-+ 	(BT chosenname +|+ ahtml) +|+ 
												(userId -@: allhtml) // code for non selected alternatives are not shown for the owner of this task
							})
	# (alist,tst=:{activated=finished,html=allhtml})		
						= checkAllTasks "andTasks" taskOptions (0,-1) True [] {tst & html = BT [],activated = True} 
	| finished			= (map snd alist,{tst & activated = finished, html = html}) // since selected task and others are finished
	= (map snd alist,{tst 	& activated = finished
							, html = 	html +|+ 
										BT buttons +-+ 	(BT chosenname +|+ ahtml) +|+ 
										(userId -@: allhtml)
						})

checkAllTasks traceid options (ctasknr,skipnr) bool alist tst=:{tasknr}
| ctasknr == length options 	= (reverse alist,{tst & activated = bool})
| ctasknr == skipnr				= checkAllTasks traceid options (inc ctasknr,skipnr) bool alist tst
# (taskname,task)				= options!!ctasknr
# (a,tst=:{activated = adone})	= mkParSubTask traceid ctasknr task {tst & tasknr = tasknr, activated = True}
= checkAllTasks traceid options (inc ctasknr,skipnr) (bool&&adone) [(taskname,a):alist] {tst & tasknr = tasknr, activated = True}

/* Do use this one!!
andTasks_mstone :: do all iTasks in any order (interleaved), task completed when all done
					but continue with next task as soon as one of the tasks is completed
					string indicates which task delivered what
*/

andTasks_mstone :: [(String,Task a)] -> (Task [(String,a)]) | iCreateAndPrint a
andTasks_mstone taskOptions = mkTask "andTasks_mstone" (PMilestoneTasks` taskOptions)
where
	PMilestoneTasks` [] tst	= return [] tst
	PMilestoneTasks` taskOptions tst=:{tasknr,html,options,userId}
	# (alist,tst=:{activated=finished,html=allhtml})		
						= checkAllTasks "andTasks" taskOptions (0,-1) True [] {tst & html = BT [], activated = True}
	| finished			= (alist,{tst & html = html}) 
	# ((chosen,buttons,chosenname),tst) 
						= LiftHst (mkTaskButtons "and Tasks:" "and" userId tasknr options (map fst taskOptions)) tst
	# chosenTask		= snd (taskOptions!!chosen)
	# chosenTaskName	= fst (taskOptions!!chosen)
	# (a,tst=:{activated=adone,html=ahtml}) 
						= mkParSubTask "andTasks" chosen chosenTask {tst & tasknr = tasknr, activated = True, html = BT []}
	# (milestoneReached,_,tst)	
						= checkAnyTasks "andTasks_mstone" (map snd taskOptions) (0,-1) (False,-1) {tst & html = BT []}
	| not adone			= (alist,{tst & activated = adone || milestoneReached
									, 	html = 	html +|+ 
												BT buttons +-+ 	(BT chosenname +|+ ahtml) +|+ 
												(userId -@: allhtml) 
							})
	# (alist,tst=:{activated=finished,html=allhtml})		
						= checkAllTasks "andTasks" taskOptions (0,chosen) True [] {tst & html = BT []}
	| finished			= (alist,{tst & activated = finished, html = html })
	= (alist,{tst 	& 	activated = finished || milestoneReached
					, html = 	html +|+ 
								BT buttons +-+ 	(BT chosenname +|+ ahtml) +|+ 
								(userId -@: allhtml)
					})

andTasks_mu :: String [(Int,Task a)] -> (Task [a]) | iData a
andTasks_mu taskid tasks = newTask "andTasks_mu" (domu_andTasks tasks)
where
	domu_andTasks list = andTasks [(taskid <+++ " " <+++ i, i @:: task) \\ (i,task) <- list] 

// very experimental higher order lazy task stuf

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
	= (val,{tst & html = html +|+ BT [Txt ("Waiting for completion of "<+++ name)], tasknr = tasknr})

closureTask  :: String (Task a) -> (Task (TCl a)) | iCreateAndPrint a
closureTask name task = mkTask ("closure " +++ name) mkClosure
where
	mkClosure tst=:{tasknr,options,userId}
	# ((TCl sa,ra),tst) 	= doSplit name task tst
	# (_,tst)     				= sa tst
	= (ra, {tst & activated = True})

closureLzTask  :: String (Task a) -> (Task (TCl a)) | iCreateAndPrint a
closureLzTask name task = mkTask ("closure " +++ name) mkClosure
where
	mkClosure tst=:{tasknr,options,userId}
	# ((TCl sa,ra),tst) 	= doSplit name task tst
	# (_,tst)     				= sa tst
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
		= (val,{tst & html = html +|+ BT [Txt ("Waiting for completion of "<+++ name)], tasknr = tasknr})

		sharedStoreId	= iTaskId userId tasknr "Shared_Store"
		sharedMem fun	= LiftHst (mkStoreForm (Init,cFormId options sharedStoreId False) fun)

waitForTimeTask:: HtmlTime	-> (Task HtmlTime)
waitForTimeTask time = mkTask "waitForTimeTask" waitForTimeTask`
where
	waitForTimeTask` tst=:{tasknr,userId,hst}
	# taskId				= iTaskId userId tasknr "Time_"
	# (stime,hst) 			= mkStoreForm (Init,cFormId tst.options taskId time) id hst  			// remember time
	# ((currtime,_),hst)	= getTimeAndDate hst
	| currtime < stime.value= (stime.value,{tst & activated = False,hst = hst})
	= (currtime - stime.value,{tst & hst = hst})

waitForTimerTask:: HtmlTime	-> (Task HtmlTime)
waitForTimerTask time  = waitForTimerTask`
where
	waitForTimerTask` tst=:{hst}
	# ((ctime,_),hst)	= getTimeAndDate hst
	= waitForTimeTask (ctime + time) {tst & hst = hst}

waitForDateTask:: HtmlDate	-> (Task HtmlDate)
waitForDateTask date = mkTask "waitForDateTask" waitForDateTask`
where
	waitForDateTask` tst=:{tasknr,userId,hst}
	# taskId				= iTaskId userId tasknr "Date_"
	# (taskdone,hst) 		= mkStoreForm (Init,cFormId tst.options taskId (False,date)) id hst  			// remember date
	# ((_,currdate),hst) 	= getTimeAndDate hst
	| currdate < date		= (date,{tst & activated = False, hst = hst})
	= (date,{tst & hst = hst})

// functions on TSt

taskId :: TSt -> (Int,TSt)
taskId tst=:{userId} = (userId,tst)

userId :: TSt -> (Int,TSt)
userId tst=:{currentUserId} = (currentUserId,tst)

addHtml :: [BodyTag] TSt -> TSt
addHtml bodytag  tst=:{activated, html}  
| not activated = tst						// not active, return default value
= {tst & html = html +|+ BT bodytag}		// active, so perform task or get its result

// lifters to iTask state
(*>>) infix 4 :: (TSt -> (a,TSt)) (a -> Task b) -> (Task b)
(*>>) ftst b = doit
where
	doit tst
	# (a,tst) = ftst tst
	= b a tst

(@>>) infix 4 :: (TSt -> TSt) (Task a) -> Task a
(@>>) ftst b = doit
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

appHSt2 :: (HSt -> (a,HSt)) -> (Task a) | iData a
appHSt2 fun = mkTask "appHSt" doit
where
	doit tst=:{hst}
	# (value,hst)		= fun hst
	= (value,{tst & hst = hst, activated = True})													// task is now completed, handle as previously

appHSt :: (HSt -> (a,HSt)) -> (Task a) | iData a
appHSt fun = mkTask "appHSt" doit
where
	doit tst=:{activated,html,tasknr,hst,userId,options}
	# taskId			= iTaskId userId tasknr "appHst"
	# (store,hst) 		= mkStoreForm (Init,cFormId options taskId (False,createDefault)) id hst  			
	# (done,value)		= store.value
	| done 				= (value,{tst & hst = hst})													// if task has completed, don't do it again
	# (value,hst)		= fun hst
	# (store,hst) 		= mkStoreForm (Init,cFormId options taskId (False,createDefault)) (\_ -> (True,value)) hst 	// remember task status for next time
	# (done,value)		= store.value
	= (value,{tst & activated = done, hst = hst})													// task is now completed, handle as previously
	
Once :: (Task a) -> (Task a) | iData a
Once fun = mkTask "Once" doit
where
	doit tst=:{activated,html,tasknr,hst,userId,options}
	# taskId			= iTaskId userId tasknr "Once_"
	# (store,hst) 		= mkStoreForm (Init,cFormId options taskId (False,createDefault)) id hst  			
	# (done,value)		= store.value
	| done 				= (value,{tst & hst = hst})													// if task has completed, don't do it again
	# (value,tst=:{hst})= fun {tst & hst = hst}
	# (store,hst) 		= mkStoreForm (Init,cFormId options taskId (False,createDefault)) (\_ -> (True,value)) hst 	// remember task status for next time
	# (done,value)		= store.value
	= (value,{tst & activated = done, hst = hst})													// task is now completed, handle as previously

// Notice that when combining tasks the context restrictions on certain types will get stronger
// It can vary from : no restriction on a -> iTrace a -> iData a
// In most cases the user can simply ask Clean to derive the corresponding generic functions
// For the type Task this will not work since it is a higher order type
// Therefore when yielding a task as result of a task,
// the type Task need to be wrapped into TCl for which the generic functions are defined below
// Tested for iTrace, will not work for iData

gPrint{|TCl|} gpa a ps = ps <<- "Task Closure"

gUpd{|TCl|} gc (UpdSearch _ 0)	  	 c		= (UpdDone, c)								
gUpd{|TCl|} gc (UpdSearch val cnt)  c		= (UpdSearch val (cnt - 2),c)						
gUpd{|TCl|} gc (UpdCreate l)        _		
# (mode,default)	= gc (UpdCreate l) undef
= (UpdCreate l, TCl (\tst -> (default,tst)))			
gUpd{|TCl|} gc mode                 b		= (mode, b)										

gForm{|TCl|} gfa (init,formid) hst
= ({value=formid.ival,changed=False,form=[]},hst)

/*
convertTask task 	= dynamic_to_string (dynamic task::*TSt -> *(a^,*TSt))
string_to_dynamic` s = string_to_dynamic ( {s` \\ s` <-: s})
*/


// *** utility section ***

// editors


// simple html code generation utilities

showUser nr
= yellow ("User " <+++ nr)

yellow message
= Font [Fnt_Color (`Colorname Yellow)] [B [] message]

silver message
= Font [Fnt_Color (`Colorname Silver)] [B [] message]

red message
= Font [Fnt_Color (`Colorname Red)] [B [] message]

// task number generation

showTaskNr [] 		= ""
showTaskNr [i] 		= toString i
showTaskNr [i:is] 	= showTaskNr is <+++ "." <+++ toString i 

iTaskId :: !Int !TaskNr String -> String
iTaskId userid tasknr postfix 
| postfix == ""
	| userid < 0	= "iLog_"  <+++ (showTaskNr tasknr) 
	| otherwise		= "iTask_" <+++ (showTaskNr tasknr) 
| userid < 0		= "iLog_"  <+++ (showTaskNr tasknr) <+++ "-" <+++ postfix
| otherwise			= "iTask_" <+++ (showTaskNr tasknr) <+++ "-" <+++ postfix <+++ "+" <+++ userid

InsertTrace :: !Bool !TaskNr !Int String !String ![Trace] -> [Trace]
InsertTrace finished idx who taskname val trace = InsertTrace` ridx who val trace
where
	InsertTrace` :: !TaskNr !Int !String ![Trace] -> [Trace]
	InsertTrace` [i] 	who str traces
	| i < 0					= abort ("negative task numbers:" <+++ showTaskNr idx <+++ "," <+++ who <+++ "," <+++ taskname)
	# (Trace _ itraces)		= select i traces
	= updateAt` i (Trace (Just (finished,(who,show,taskname,str))) itraces)  traces
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
printTrace2 (Just a)  	= STable emptyBackground (print False a)
where
	print _ []		= []
	print b trace	= [pr b x ++ [STable emptyBackground (print (isDone x||b) xs)]\\ (Trace x xs) <- trace] 

	pr _ Nothing 			= []
	pr dprev (Just (dtask,(w,i,tn,s)))	
	| dprev && (not dtask)	= pr False Nothing	// subtask not important anymore (assume no milestone tasks)
	| not dtask				= showTask cellattr1b White Navy Maroon Silver (w,i,tn,s)
	= showTask cellattr1a White Yellow Red White (w,i,tn,s)
	
	showTask2 attr1 c1 c2 c3 c4 (w,i,tn,s)
	= [Table doneBackground 	[ Tr [] [Td attr1 [font c1 (toString (last (reverse i)))],	Td cellattr2 [font c2 tn]]
								, Tr [] [Td attr1 [font c3 (toString w)], 					Td cellattr2 [font c4 s]]
								]
	  ,Br]

	showTask att c1 c2 c3 c4 (w,i,tn,s)
	= [STable doneBackground 	
		[ [font c1 (toString w),font c2 ("T" <+++ showTaskNr i)]
		, [EmptyBody, font c3 tn]
		, [EmptyBody, font c4 s]
		]
		]
	isDone Nothing = False
	isDone (Just (b,(w,i,tn,s))) = b


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
