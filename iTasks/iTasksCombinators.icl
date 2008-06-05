implementation module iTasksCombinators

// *********************************************************************************************************************************
// This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
// with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdList, StdFunc, StdTuple
import iDataTrivial, iDataFormlib
import iTasksBasicCombinators, iTasksHtmlSupport, iTasksTimeAndDateHandling, iTasksLiftingCombinators, iTasksSettings, iTasksEditors

derive gForm 	[]
derive gUpd  	[]
derive gUpd 	Maybe
derive gForm 	Maybe
derive gPrint 	Maybe
derive gParse 	Maybe
derive read		Maybe
derive write	Maybe
// ******************************************************************************************************
// monads for combining iTasks

(#>>) infixl 1 :: !(Task a) !(Task b) -> Task b | iCreateAndPrint b
(#>>) taska taskb 
=				taska
	=>> \_ ->	taskb 

(=>>?) infixl 1 	:: !(Task (Maybe a)) !(a -> Task (Maybe b)) -> Task (Maybe b) | iCreateAndPrint a & iCreateAndPrint b
(=>>?) t1 t2 
= 				t1 
	=>> \r1 -> 	case r1 of 
					Nothing 	-> return_V Nothing
					Just r`1 	-> t2 r`1
return_VF :: !HtmlCode !a -> (Task a) | iCreateAndPrint a
return_VF bodytag a = return_V a <<! bodytag

return_D :: !a -> (Task a) | gForm {|*|}, iCreateAndPrint a
return_D a = return_V a <<! [toHtml a ]

// ******************************************************************************************************
// repetition

repeatTask :: !(a -> Task a) !(a -> Bool) a -> Task a | iData a
repeatTask task pred a = dorepeatTask a
where
	dorepeatTask a 
	= newTask "repeatTask" dorepeatTask`
	where
		dorepeatTask` tst
		| pred a	= (a,tst) 
		# (na,tst)	= task a tst	
		= dorepeatTask na tst

(<|) infixl 6 :: !(Task a) !(a -> (Bool, HtmlCode)) -> Task a | iData a
(<|) taska pred = newTask "repeatTest" doTask
where
	doTask
	=				taska
		=>> \r -> 		case pred r of
						(True,_) -> return_V r
//						(False,msg) -> msg ?>> doTask
						(False,msg) -> msg ?>> (taska <| pred)


// ******************************************************************************************************
// Assigning tasks to users, each user has to be identified by an unique number >= 0

(@:) infix 3 :: !UserId !(LabeledTask a) -> Task a | iData a
(@:) nuserId ltask=:(taskname,task) = assigntask
where
	assigntask tst=:{userId} 
	= 				([showText ("Waiting for Task "), showLabel taskname, showText " from ", showUser nuserId,Br]
					?>> assignTaskTo nuserId (taskname,[showText "Requested by ", showUser userId,Br,Br] ?>> task)) tst

	showUser nr = showLabel ("User " <+++ nr)

(@::) infix 3 :: !UserId !(Task a)	-> (Task a) | iData  a												
(@::) nuserId taska = nuserId @: ("Task for " <+++ nuserId,taska)

(@:>) infix 3 :: !UserId !(LabeledTask a) -> Task a | iData a
(@:>) nuserId ltask = assignTaskTo nuserId ltask

(@::>) infix 3 :: !UserId !(Task a) -> (Task a) | iData  a												
(@::>) nuserId taska = nuserId @:> ("Task for " <+++ nuserId,taska)

// ******************************************************************************************************
// choose one or more tasks on forehand out of a set

chooseTask_btn 	:: !HtmlCode !Bool![LabeledTask a] -> Task a | iData a
chooseTask_btn prompt horizontal ltasks
= selectTasks (\lt -> prompt ?>> selectTask_btn horizontal lt) ltasks =>> \la -> return_V (hd la)		
where
	selectTask_btn direction ltasks = newTask "selectTask_btn" (selectTask_btn` direction ltasks)		

	selectTask_btn` _ [] tst		= return [] tst				
	selectTask_btn` horizontal taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
	# taskId						= iTaskId userId tasknr ("ChoSt" <+++ length taskOptions)
	# (chosen,tst)					= liftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.value == -1			// no choice made yet
		# buttonId					= iTaskId userId tasknr "ChoBut"
		# allButtons				= if horizontal 
											[[(iTaskButton txt,\_ -> n)  \\ txt <- map fst taskOptions & n <- [0..]]]
											[[(iTaskButton txt,\_ -> n)] \\ txt <- map fst taskOptions & n <- [0..]]
		# (choice,tst)				= liftHst (TableFuncBut (Init,pageFormId options buttonId allButtons)) tst
		# (chosen,tst)				= liftHst (mkStoreForm  (Init,storageFormId options taskId -1) choice.value) tst
		| chosen.value == -1		= ([],{tst & activated = False,html = html +|+ BT choice.form})
		= ([chosen.value],{tst & tasknr = tasknr, activated = True, html = html})
	= ([chosen.value],{tst & tasknr = tasknr, activated = True, html = html})


chooseTask_pdm 	:: !HtmlCode !Int ![LabeledTask a] -> Task a | iData a
chooseTask_pdm prompt initial ltasks
= selectTasks (\lt -> prompt ?>> selectTask_pdm initial lt) ltasks =>> \la -> return_V (hd la)		
where
	selectTask_pdm _ [] tst			= return createDefault tst	
	selectTask_pdm defaultOn taskOptions tst=:{tasknr,html,userId,options}													// choose one subtask out of  a pulldown menu
	# taskId						= iTaskId userId tasknr ("ChoStPdm" <+++ length taskOptions)
	# (chosen,tst)					= liftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.value == -1			// no choice made yet
		# numberOfItems					= length taskOptions
		# defaultOn						= if (defaultOn >= 0 && defaultOn <= numberOfItems  - 1) defaultOn 0 		
		# taskPdMenuId					= iTaskId userId tasknr ("ChoPdm" <+++ numberOfItems)
		# (choice,tst)					= liftHst (FuncMenu  (Init,sessionFormId options taskPdMenuId (defaultOn,[(txt,id) \\ txt <- map fst taskOptions]))) tst
		# (_,tst=:{activated=adone,html=ahtml})	
										= editTaskLabel "" "Done" Void {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 	
		| not adone						= ([],{tst & activated = False, html = html +|+ BT prompt +|+ BT choice.form +|+ ahtml, tasknr = tasknr})
		# chosenIdx						= snd choice.value
		# chosenTask					= snd (taskOptions!!chosenIdx)
		# (chosen,tst)					= liftHst (mkStoreForm  (Init,storageFormId options taskId -1) (\_ -> chosenIdx)) tst
		= ([chosen.value],{tst & tasknr = tasknr, activated = True, html = html})
	= ([chosen.value],{tst & activated = True, html = html, tasknr = tasknr})
/*
	selectTask_pdm :: !Int ![LabeledTask a] -> Task [Int]
	selectTask_pdm defaultOn taskOptions = dochooseTask_pdm taskOptions
	where
		dochooseTask_pdm [] tst			= return createDefault tst	
		dochooseTask_pdm taskOptions tst=:{tasknr,html,userId,options}													// choose one subtask out of  a pulldown menu
		# numberOfItems					= length taskOptions
		# defaultOn						= if (defaultOn >= 0 && defaultOn <= numberOfItems  - 1) defaultOn 0 		
		# taskPdMenuId					= iTaskId userId tasknr ("ChoPdm" <+++ numberOfItems)
		# (choice,tst)					= liftHst (FuncMenu  (Init,sessionFormId options taskPdMenuId (defaultOn,[(txt,id) \\ txt <- map fst taskOptions]))) tst
		# (_,tst=:{activated=adone,html=ahtml})	
										= editTaskLabel "" "Done" Void {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 	
		| not adone						= (createDefault,{tst & activated = False, html = html +|+ BT choice.form +|+ ahtml, tasknr = tasknr})
		= ([snd choice.value],{tst & activated = True, html = html, tasknr = tasknr})
*/
chooseTask_radio:: !HtmlCode !Int ![(HtmlCode,LabeledTask a)] -> Task a | iData a
chooseTask_radio prompt initial code_ltasks
= selectTasks (\lt -> prompt ?>> selectTask_radio initial (map fst code_ltasks) lt) (map snd code_ltasks) =>> \la -> return_V (hd la)		
where
	selectTask_radio :: !Int ![HtmlCode] ![LabeledTask a] -> Task [Int]
	selectTask_radio defaultOn htmlcodes taskOptions = dochooseTask_pdm taskOptions
	where
		dochooseTask_pdm [] tst			= return createDefault tst	
		dochooseTask_pdm taskOptions tst=:{tasknr,html,userId,options}													// choose one subtask out of  a pulldown menu
		# numberOfButtons				= length taskOptions
		# defaultOn						= if (defaultOn >= 0 && defaultOn <= numberOfButtons - 1) defaultOn 0 		
		# taskId						= iTaskId userId tasknr ("ChoStRadio" <+++ numberOfButtons)
		# taskRadioMenuId				= iTaskId userId tasknr ("ChoRadio" <+++ numberOfButtons)
		# (chosen,tst)					= liftHst (mkStoreForm  (Init,storageFormId options taskId defaultOn) id) tst
		# (nradio,tst)					= liftHst (ListFuncRadio (Init,sessionFormId options taskRadioMenuId (defaultOn,[\i a -> i \\ j <- [0 .. numberOfButtons - 1]]))) tst
		# choice						= if nradio.changed (snd nradio.value) chosen.value
		# (nradio,tst)					= liftHst (ListFuncRadio (Set, sessionFormId options taskRadioMenuId (choice,[\i a -> i \\ j <- [0 .. numberOfButtons - 1]]))) tst
		# (_,tst=:{activated=adone,html=ahtml})	
										= editTaskLabel "" "Done" Void {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 	
		| not adone
			# (chosen,tst)					= liftHst (mkStoreForm  (Set,storageFormId options taskId choice) id) {tst & activated = adone, html = BT []}
			# radioform						= nradio.form <=|>  [[showLabel label] <||> htmlcode \\ htmlcode <- htmlcodes & (label,_) <- taskOptions]
			= (createDefault,{tst & activated = False, html = html +|+ BT [radioform] +|+ ahtml, tasknr = tasknr})
		= ([choice],{tst & activated = True, html = html, tasknr = tasknr})

chooseTask_cbox	:: !([LabeledTask a] -> Task [a]) !HtmlCode ![((!Bool,!ChoiceUpdate,!HtmlCode),LabeledTask a)] -> Task [a] | iData a

chooseTask_cbox whatisthis prompt code_ltasks
= selectTasks (\lt -> prompt ?>> selectTask_cbox (map fst code_ltasks) lt) (map snd code_ltasks)		
where
	selectTask_cbox :: ![(!Bool,!ChoiceUpdate,!HtmlCode)] ![LabeledTask a] -> Task [Int]
	selectTask_cbox htmlcodes taskOptions = domchoiceTasks taskOptions
	where
		domchoiceTasks [] tst	= ([],{tst& activated = True})
		domchoiceTasks taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
		# seltaskId				= iTaskId userId tasknr ("MtpChSel" <+++ length taskOptions)
		# donetaskId			= iTaskId userId tasknr "MtpChSt"
		# (cboxes,tst)			= liftHst (ListFuncCheckBox (Init,cFormId options seltaskId initCheckboxes)) tst
		# (fun,nblist)			= cboxes.value
		# nsettings				= fun nblist
		# (cboxes,tst)			= liftHst (ListFuncCheckBox (Set ,cFormId options seltaskId (setCheckboxes nsettings))) tst
		# (done,tst)			= liftHst (mkStoreForm      (Init,storageFormId options donetaskId False) id) tst
	
		# (_,tst=:{html=ahtml,activated = adone})
								= (editTaskLabel "" "OK" Void) {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 
		| not adone	
			# optionsform		= cboxes.form <=|> [[showLabel label] <||> htmlcode \\ (_,_,htmlcode) <- htmlcodes & (label,_) <- taskOptions]
			= ([],{tst & html = html +|+  BT [optionsform] +|+ ahtml})
		# (_,tst)				= liftHst (mkStoreForm      (Init,storageFormId options donetaskId False) (\_ -> True)) tst
		= ([i \\ True <- snd cboxes.value & i <- [0..]],{tst & tasknr = tasknr, html = html, options = options, userId =userId, activated = True})									// choose one subtask out of the list
	
		initCheckboxes  = 
			[(if set CBChecked CBNotChecked  label,  \b bs _ -> setfun b bs) \\ (set,setfun,_) <- htmlcodes & (label,_) <- taskOptions & i <- [0..]]
	
		setCheckboxes  boollist = 
			[(if set CBChecked CBNotChecked  label,  \b bs _ -> setfun b bs) \\ (_,setfun,_) <- htmlcodes & (label,_) <- taskOptions 
																		& i <- [0..] & set <- boollist]

// ******************************************************************************************************
// choose one or more tasks on forehand out of a set

button :: !String !a -> (Task a) | iData a
button s a = newTask "button" (chooseTask_btn [] True [(s,return_V a)])

buttonTask :: !String !(Task a) -> (Task a) | iData a
buttonTask s task = newTask "buttonTask" (chooseTask_btn [] True [(s,task)])

chooseTask :: !HtmlCode ![LabeledTask a] -> (Task a) | iData a
chooseTask prompt options = newTask "chooseTask" (chooseTask_btn prompt True options)

chooseTaskV :: !HtmlCode ![LabeledTask a] -> (Task a) | iData a
chooseTaskV prompt options = newTask "chooseTaskV" (chooseTask_btn prompt False options)

mchoiceTasks :: !HtmlCode ![LabeledTask a] -> (Task [a]) | iData a
mchoiceTasks prompt taskOptions 
= chooseTask_cbox seqTasks prompt [((False,\b bs -> bs,[]),labeltask) \\ labeltask <- taskOptions]

mchoiceTasks2 :: !HtmlCode ![(!Bool,LabeledTask a)] -> Task [a] | iData a
mchoiceTasks2 prompt taskOptions 
= chooseTask_cbox seqTasks prompt [((set,\b bs -> bs,[]),labeltask) \\ (set,labeltask) <- taskOptions]

mchoiceTasks3 :: !HtmlCode ![((!Bool,!ChoiceUpdate,!HtmlCode),LabeledTask a)] -> Task [a] | iData a
mchoiceTasks3 prompt taskOptions 
= chooseTask_cbox seqTasks prompt taskOptions

mchoiceAndTasks :: !HtmlCode ![LabeledTask a] -> (Task [a]) | iData a
mchoiceAndTasks prompt taskOptions 
= chooseTask_cbox andTasks prompt [((False,\b bs -> bs,[]),labeltask) \\ labeltask <- taskOptions]

mchoiceAndTasks2 :: !HtmlCode ![(!Bool,LabeledTask a)] -> Task [a] | iData a
mchoiceAndTasks2 prompt taskOptions 
= chooseTask_cbox andTasks prompt [((set,\b bs -> bs,[]),labeltask) \\ (set,labeltask) <- taskOptions]

mchoiceAndTasks3 :: !HtmlCode ![((!Bool,!ChoiceUpdate,!HtmlCode),LabeledTask a)] -> Task [a] | iData a
mchoiceAndTasks3 prompt taskOptions 
= chooseTask_cbox andTasks prompt taskOptions

// ******************************************************************************************************
// Speculative OR-tasks: task ends as soon as one of its subtasks completes

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iData a
(-||-) taska taskb =  newTask "-||-" (doOrTask (taska,taskb))
where
	doOrTask :: !(Task a,Task a) -> (Task a) | iCreateAndPrint a
	doOrTask (taska,taskb)
	= 			orTask2 (taska,taskb)
		=>> \at ->  case at of
						(LEFT a)  -> return_V a
						(RIGHT b) -> return_V b

(-&&-) infixr 4 ::  !(Task a) !(Task b) -> (Task (a,b)) | iData a & iData b
(-&&-) taska taskb = newTask "-&&-" (andTask2 (taska,taskb))

orTasks :: ![LabeledTask a] -> (Task a) | iData a
orTasks []				= return createDefault
orTasks taskCollection	= newTask "orTasks" (andTasksCond "or Tasks" (\list -> length list >= 1) taskCollection)
							=>> \list -> return  (hd list)

andTasks :: ![LabeledTask a] -> (Task [a]) | iData a
andTasks taskCollection = newTask "andTasks" (andTasksCond "and Tasks" (\_ -> False) taskCollection)

(-&&-?) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iData a & iData b
(-&&-?) t1 t2 
= 		andTasksCond "Maybe Task" noNothing [("Maybe 1",left),("Maybe 2",right)]
  =>>	combineResult
where
	left 	= t1 =>> \tres -> return_V (LEFT tres) 
	right	= t2 =>> \tres -> return_V (RIGHT tres) 

	combineResult	[LEFT (Just r1),RIGHT (Just r2)]	= return_V (Just (r1,r2))
	combineResult	_									= return_V Nothing

	noNothing []					= False
	noNothing [LEFT  Nothing:xs]	= True
	noNothing [RIGHT Nothing:xs]	= True
	noNothing [x:xs]				= noNothing xs	

andTasks_mu :: !String ![(Int,Task a)] -> (Task [a]) | iData a
andTasks_mu label tasks = newTask "andTasks_mu" (domu_andTasks tasks)
where
	domu_andTasks list = andTasks [(label  <+++ " " <+++ i, i @:: task) \\ (i,task) <- list] 

// ******************************************************************************************************
// Timer Tasks ending when timed out

waitForTimerTask:: !HtmlTime	-> (Task HtmlTime)
waitForTimerTask time  = waitForTimerTask`
where
	waitForTimerTask`
	=						appHStOnce "getTimeAndDate" getTimeAndDate
		=>> \(ctime,_) ->  	waitForTimeTask (ctime + time)

