implementation module iTasksCombinators

// *********************************************************************************************************************************
// This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
// with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdList, StdFunc, StdTuple
import iDataTrivial, iDataFormlib, StdBimap
import iTasksBasicCombinators, iTasksHtmlSupport, iTasksTimeAndDateHandling, iTasksLiftingCombinators, iTasksSettings, iTasksEditors

derive gForm 	[]
derive gUpd  	[]

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
return_VF :: ![HtmlTag] !a -> (Task a) | iCreateAndPrint a
return_VF bodytag a = return_V a <<! bodytag

return_D :: !a -> (Task a) | gForm {|*|}, iCreateAndPrint a
return_D a = return_V a <<! [toHtml a ]

// ******************************************************************************************************
// repetition

repeatTask :: !(a -> Task a) !(a -> Bool) a -> Task a | iData a
repeatTask task pred a = dorepeatTask a
where
	dorepeatTask a 
	= newTask "repeatTask" (Task dorepeatTask`)
	where
		dorepeatTask` tst
		| pred a	= (a,tst) 
		# (na,tst)	= appTaskTSt (task a) tst	
		= appTaskTSt (dorepeatTask na) tst

(<|) infixl 6 :: !(Task a) !(a -> (Bool, [HtmlTag])) -> Task a | iData a
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
(@:) nuserId ltask=:(taskname,task) = Task assigntask
where
	assigntask tst=:{userId} 
	= 				appTaskTSt ([showText ("Waiting for Task "), showLabel taskname, showText " from ", showUser nuserId,BrTag []]
					?>> assignTaskTo nuserId (taskname,[showText "Requested by ", showUser userId,BrTag [] ,BrTag []] ?>> task)) tst

	showUser nr = showLabel ("User " <+++ nr)

(@::) infix 3 :: !UserId !(Task a)	-> (Task a) | iData  a												
(@::) nuserId taska = nuserId @: ("Task for " <+++ nuserId,taska)

(@:>) infix 3 :: !UserId !(LabeledTask a) -> Task a | iData a
(@:>) nuserId ltask = assignTaskTo nuserId ltask

(@::>) infix 3 :: !UserId !(Task a) -> (Task a) | iData  a												
(@::>) nuserId taska = nuserId @:> ("Task for " <+++ nuserId,taska)

// ******************************************************************************************************
// choose one or more tasks on forehand out of a set

chooseTask_btn 	:: ![HtmlTag] !Bool![LabeledTask a] -> Task a | iData a
chooseTask_btn prompt horizontal ltasks
= selectTasks (\lt -> prompt ?>> selectTask_btn horizontal lt) seqTasks ltasks =>> \la -> return_V (hd la)		
where
	selectTask_btn direction ltasks = newTask "selectTask_btn" (Task (selectTask_btn` direction ltasks))	

	selectTask_btn` _ [] tst		= return [] tst				
	selectTask_btn` horizontal taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
	# taskId						= iTaskId userId tasknr ("ChoSt" <+++ length taskOptions)
	# (chosen,tst)					= liftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.Form.value == -1		// no choice made yet
		# buttonId					= iTaskId userId tasknr "ChoBut"
		# allButtons				= if horizontal 
											[[(iTaskButton txt,\_ -> n)  \\ txt <- map fst taskOptions & n <- [0..]]]
											[[(iTaskButton txt,\_ -> n)] \\ txt <- map fst taskOptions & n <- [0..]]
		# (choice,tst)				= liftHst (TableFuncBut (Init,pageFormId options buttonId allButtons)) tst
		# (chosen,tst)				= liftHst (mkStoreForm  (Init,storageFormId options taskId -1) choice.Form.value) tst
		| chosen.Form.value == -1		= ([],{tst & activated = False,html = html +|+ BT choice.form choice.inputs})
		= ([chosen.Form.value],{tst & activated = True})
	= ([chosen.Form.value],{tst & activated = True})

chooseTask_pdm 	:: ![HtmlTag] !Int ![LabeledTask a] -> Task a | iData a
chooseTask_pdm prompt initial ltasks
= selectTasks (\lt -> prompt ?>> selectTask_pdm initial lt) seqTasks ltasks =>> \la -> return_V (hd la)		
where
	selectTask_pdm initial ltasks = newTask "selectTask_pdm" (Task (selectTask_pdm` initial ltasks))

	selectTask_pdm` _ [] tst			= return createDefault tst	
	selectTask_pdm` defaultOn taskOptions tst=:{tasknr,html,userId,options}													// choose one subtask out of  a pulldown menu
	# taskId						= iTaskId userId tasknr ("ChoStPdm" <+++ length taskOptions)
	# (chosen,tst)					= liftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.Form.value == -1			// no choice made yet
		# numberOfItems					= length taskOptions
		# defaultOn						= if (defaultOn >= 0 && defaultOn <= numberOfItems  - 1) defaultOn 0 		
		# taskPdMenuId					= iTaskId userId tasknr ("ChoPdm" <+++ numberOfItems)
		# (choice,tst)					= liftHst (FuncMenu  (Init,sessionFormId options taskPdMenuId (defaultOn,[(txt,id) \\ txt <- map fst taskOptions]))) tst
		# (_,tst=:{activated=adone,html=ahtml})	
										= appTaskTSt (editTaskLabel "" "Done" Void) {tst & activated = True, html = BT [] [], tasknr = [-1:tasknr]} 	
		| not adone						= ([],{tst & activated = False, html = html +|+ BT prompt [] +|+ BT choice.form choice.inputs +|+ ahtml, tasknr = tasknr})
		# chosenIdx						= snd choice.Form.value
		# chosenTask					= snd (taskOptions!!chosenIdx)
		# (chosen,tst)					= liftHst (mkStoreForm  (Init,storageFormId options taskId -1) (\_ -> chosenIdx)) tst
		= ([chosen.Form.value],{tst & tasknr = tasknr, activated = True, html = html})
	= ([chosen.Form.value],{tst & activated = True, html = html, tasknr = tasknr})


chooseTask_cbox	:: !([LabeledTask a] -> Task [a]) ![HtmlTag] ![((!Bool,!ChoiceUpdate,![HtmlTag]),LabeledTask a)] -> Task [a] | iData a
chooseTask_cbox order prompt code_ltasks
= selectTasks (\lt -> prompt ?>> selectTask_cbox (map fst code_ltasks) lt) order (map snd code_ltasks)		
where
	selectTask_cbox :: ![(!Bool,!ChoiceUpdate,![HtmlTag])] ![LabeledTask a] -> Task [Int]
	selectTask_cbox htmlcodes taskOptions = newTask "selectTask_cbox" (Task (selectTask_cbox` taskOptions))
	where
		selectTask_cbox` [] tst	= ([],{tst& activated = True})
		selectTask_cbox` taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
		# seltaskId				= iTaskId userId tasknr ("MtpChSel" <+++ length taskOptions)
		# donetaskId			= iTaskId userId tasknr "MtpChSt"
		# (cboxes,tst)			= liftHst (ListFuncCheckBox (Init,cFormId options seltaskId initCheckboxes)) tst
		# (fun,nblist)			= cboxes.Form.value
		# nsettings				= fun nblist
		# (cboxes,tst)			= liftHst (ListFuncCheckBox (Set ,cFormId options seltaskId (setCheckboxes nsettings))) tst
		# (done,tst)			= liftHst (mkStoreForm      (Init,storageFormId options donetaskId False) id) tst
	
		# (_,tst=:{html=ahtml,activated = adone})
								= appTaskTSt (editTaskLabel "" "OK" Void) {tst & activated = True, html = BT [] [], tasknr = [-1:tasknr]} 
		| not adone	
			# optionsform		= cboxes.form <=|> [DivTag [] ([showLabel label] <||> htmlcode) \\ (_,_,htmlcode) <- htmlcodes & (label,_) <- taskOptions]
			= ([],{tst & html = html +|+  BT optionsform cboxes.inputs +|+ ahtml})
		# (_,tst)				= liftHst (mkStoreForm      (Init,storageFormId options donetaskId False) (\_ -> True)) tst
		= ([i \\ True <- snd cboxes.Form.value & i <- [0..]],{tst & tasknr = tasknr, html = html, options = options, userId =userId, activated = True})									// choose one subtask out of the list
	
		initCheckboxes  = 
			[(HtmlCheckbox [Text label] set,  \b bs _ -> setfun b bs) \\ (set,setfun,_) <- htmlcodes & (label,_) <- taskOptions & i <- [0..]]
	
		setCheckboxes  boollist = 
			[(HtmlCheckbox [Text label] set,  \b bs _ -> setfun b bs) \\ (_,setfun,_) <- htmlcodes & (label,_) <- taskOptions 
																		& i <- [0..] & set <- boollist]

// ******************************************************************************************************
// choose one or more tasks on forehand out of a set

button :: !String !a -> (Task a) | iData a
button s a = newTask "button" (chooseTask_btn [] True [(s,return_V a)])

buttonTask :: !String !(Task a) -> (Task a) | iData a
buttonTask s task = newTask "buttonTask" (chooseTask_btn [] True [(s,task)])

chooseTask :: ![HtmlTag] ![LabeledTask a] -> (Task a) | iData a
chooseTask prompt options = newTask "chooseTask" (chooseTask_btn prompt True options)

chooseTaskV :: ![HtmlTag] ![LabeledTask a] -> (Task a) | iData a
chooseTaskV prompt options = newTask "chooseTaskV" (chooseTask_btn prompt False options)

mchoiceTasks :: ![HtmlTag] ![LabeledTask a] -> (Task [a]) | iData a
mchoiceTasks prompt taskOptions 
= chooseTask_cbox seqTasks prompt [((False,\b bs -> bs,[]),labeltask) \\ labeltask <- taskOptions]

mchoiceTasks2 :: ![HtmlTag] ![(!Bool,LabeledTask a)] -> Task [a] | iData a
mchoiceTasks2 prompt taskOptions 
= chooseTask_cbox seqTasks prompt [((set,\b bs -> bs,[]),labeltask) \\ (set,labeltask) <- taskOptions]

mchoiceTasks3 :: ![HtmlTag] ![((!Bool,!ChoiceUpdate,![HtmlTag]),LabeledTask a)] -> Task [a] | iData a
mchoiceTasks3 prompt taskOptions 
= chooseTask_cbox seqTasks prompt taskOptions

mchoiceAndTasks :: ![HtmlTag] ![LabeledTask a] -> (Task [a]) | iData a
mchoiceAndTasks prompt taskOptions 
= chooseTask_cbox andTasks prompt [((False,\b bs -> bs,[]),labeltask) \\ labeltask <- taskOptions]

mchoiceAndTasks2 :: ![HtmlTag] ![(!Bool,LabeledTask a)] -> Task [a] | iData a
mchoiceAndTasks2 prompt taskOptions 
= chooseTask_cbox andTasks prompt [((set,\b bs -> bs,[]),labeltask) \\ (set,labeltask) <- taskOptions]

mchoiceAndTasks3 :: ![HtmlTag] ![((!Bool,!ChoiceUpdate,![HtmlTag]),LabeledTask a)] -> Task [a] | iData a
mchoiceAndTasks3 prompt taskOptions 
= chooseTask_cbox andTasks prompt taskOptions

// ******************************************************************************************************
// Speculative OR-tasks: task ends as soon as one of its subtasks completes

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iData a
(-||-) taska taskb =  newTask "-||-" (doOrTask (taska,taskb))
where
	doOrTask :: !(Task a,Task a) -> (Task a) | iData a
	doOrTask (taska,taskb)
	= 			orTask2 (taska,taskb)
		=>> \at ->  case at of
						(LEFT a)  -> return_V a
						(RIGHT b) -> return_V b

(-&&-) infixr 4 ::  !(Task a) !(Task b) -> (Task (a,b)) | iData a & iData b
(-&&-) taska taskb = newTask "-&&-" (andTask2 (taska,taskb))

orTasks :: ![LabeledTask a] -> (Task a) | iData a
orTasks []				= Task (return createDefault)
orTasks taskCollection	= newTask "orTasks" (andTasksCond "or Tasks" (\list -> length list >= 1) taskCollection)
							=>> \list -> (Task (return  (hd list)))

orTask2 :: !(Task a,Task b) -> Task (EITHER a b) | iData a & iData b
orTask2 (taska,taskb) 
=					allTasksCond "orTask2" showBoth (\list -> length list > 0) [("orTask.0",taska =>> \a -> return_V (LEFT a)),("orTask.0",taskb =>> \b -> return_V (RIGHT b))]
	=>> \res -> 	return_V (hd res) 

showBoth id list tst=:{hst}
# (sel,hst)	= mkEditForm (Init,nFormId id [0,1] <@ NoForm) hst
= ((sel.Form.value,sel.form),{tst & hst = hst})

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

andTask2 :: !(Task a,Task b) -> Task (a,b) | iData a & iData b
andTask2 (taska,taskb) 
=								allTasksCond "andTask2" showBoth (\l -> False) [("andTask.0",taska =>> \a -> return_V (LEFT a)),("andTask.0",taskb =>> \b -> return_V (RIGHT b))]
	=>> \[LEFT a, RIGHT b] -> 	return_V (a,b) 

andTasks_mu :: !String ![(Int,Task a)] -> (Task [a]) | iData a
andTasks_mu label tasks = newTask "andTasks_mu" (domu_andTasks tasks)
where
	domu_andTasks list = andTasks [(label  <+++ " " <+++ i, i @:: task) \\ (i,task) <- list] 

andTasksCond 	:: !String !([a] -> Bool) ![LabeledTask a] -> (Task [a]) 	| iData a 
andTasksCond label pred taskCollection 
= allTasksCond label selectButtons pred taskCollection 
where
	selectButtons id list tst=:{hst,userId,tasknr,options}
	# ((i,buttons,chosenname),hst) = mkTaskButtons True "mybut" userId tasknr options (map fst list) hst
	= (([i],mkbuttons buttons chosenname),{tst & hst = hst})	
	where
		mkbuttons buttons chosenname = if (length list > 1) 
											[showMainLabel label,showTrace " / ",showLabel chosenname: buttons] // PK "and"->label
											[]

andTasksCond_pdm 	:: !String !([a] -> Bool) ![LabeledTask a] -> Task [a]	| iData a 
andTasksCond_pdm label pred taskCollection 
= allTasksCond label selectButtons pred taskCollection 
where
	selectButtons ident list tst=:{hst,options}
	# (result,hst) = FuncMenu (Init,applyoptions (nFormId (ident +++ "andTaskCond_pdm") (0,[(name,id) \\ (name,_) <- list]))) hst
	= (([snd result.Form.value],[showLabel label] <||> result.form),{tst & hst = hst})	
	where
		applyoptions nformid = nformid <@ options.tasklife <@ options.taskstorage <@ options.taskmode



// ******************************************************************************************************
// Timer Tasks ending when timed out

waitForTimerTask:: !HtmlTime	-> (Task HtmlTime)
waitForTimerTask time  = waitForTimerTask`
where
	waitForTimerTask`
	=						appHStOnce "getTimeAndDate" getTimeAndDate
		=>> \(ctime,_) ->  	waitForTimeTask (ctime + time)

