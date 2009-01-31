implementation module CommonCombinators
/**
* This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdList, StdFunc, StdTuple, GenBimap
import iDataTrivial, iDataFormlib
import EditTasks, UserTasks, TimeAndDateTasks, BasicCombinators, PromptingCombinators, LiftingCombinators
import InternaliTasksCommon

derive gForm 	[]
derive gUpd  	[]

derive read		Maybe
derive write	Maybe

showText   		text :== Text text
showLabel  		text :== ITag [] [Text text]

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
		# (na,tst)	= accTaskTSt (task a) tst	
		= accTaskTSt (dorepeatTask na) tst

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
	assigntask tst=:{TSt| userId}
	= 				accTaskTSt (
					getDisplayNamesTask [nuserId] =>> \displayNames ->
					[showText ("Waiting for Task "), showLabel taskname, showText " from ", showLabel (hd displayNames),BrTag []]
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

chooseTask_btn 	:: ![HtmlTag] !Bool ![LabeledTask a] -> Task a | iData a
chooseTask_btn prompt horizontal ltasks
	= newTask "chooseTask_btn" (selectTasks (\lt -> prompt ?>> selectTask_btn horizontal lt) seqTasks ltasks 
		=>> \la -> return_V (hd la))		
where
	selectTask_btn direction ltasks = mkBasicTask "selectTask_btn" (Task (selectTask_btn` direction ltasks))	

	selectTask_btn` _ [] tst		= return [] tst				
	selectTask_btn` horizontal taskOptions tst=:{taskNr,html,options,userId}									// choose one subtask out of the list
	# taskId						= iTaskId userId taskNr ("ChoSt" <+++ length taskOptions)
	# (chosen,tst)					= accHStTSt (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.Form.value == -1		// no choice made yet
		# buttonId					= iTaskId userId taskNr "ChoBut"
		# allButtons				= if horizontal 
											[[(HtmlButton txt False,\_ -> n)  \\ txt <- map fst taskOptions & n <- [0..]]]
											[[(HtmlButton txt False,\_ -> n)] \\ txt <- map fst taskOptions & n <- [0..]]
		# (choice,tst)				= accHStTSt (TableFuncBut (Init,pageFormId options buttonId allButtons)) tst
		# (chosen,tst)				= accHStTSt (mkStoreForm  (Init,storageFormId options taskId -1) choice.Form.value) tst
		| chosen.Form.value == -1
			# tst = setOutput choice.form tst
			# tst = setInputs choice.inputs tst
			= ([],{tst & activated = False})
		| otherwise
			= ([chosen.Form.value],{tst & activated = True})
	= ([chosen.Form.value],{tst & activated = True})

chooseTask_pdm 	:: ![HtmlTag] !Int ![LabeledTask a] -> Task a | iData a
chooseTask_pdm prompt initial ltasks
	= newTask "chooseTask_pdm" (selectTasks (\lt -> prompt ?>> selectTask_pdm initial lt) seqTasks ltasks =>> \la -> return_V (hd la))		
where
	selectTask_pdm initial ltasks		=  mkBasicTask "selectTask_pdm" (Task (selectTask_pdm` initial ltasks))
	
	selectTask_pdm` _ [] tst			= return createDefault tst
	selectTask_pdm` defaultOn taskOptions tst=:{taskNr,html,userId,options}									// choose one subtask out of  a pulldown menu
		# taskId							= iTaskId userId taskNr ("ChoStPdm")
		# (chosen,tst)						= accHStTSt (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
		| chosen.Form.value == -1			// no choice made yet	
			# pulldownId					= iTaskId userId taskNr "ChoPdm"
			# buttonId						= iTaskId userId taskNr "ChoBut"
			# (choice,tst)					= accHStTSt (mkEditForm (Init, pageFormId options pulldownId (mkSelect taskOptions defaultOn))) tst
			# (done,tst)					= accHStTSt (mkEditForm (Init, pageFormId options buttonId mkButton )) tst
			| fromButton done.Form.value
				# chosenId						= fromSelect choice.Form.value
				# (chosen,tst)					= liftHst (mkStoreForm (Init,storageFormId options taskId -1) (\_ -> chosenId)) tst
				= ([chosen.Form.value],{tst & activated = True})
			| otherwise
				# tst = setOutput (choice.form ++ done.form) tst
				# tst = setInputs (choice.inputs ++ done.inputs) tst
				= ([],{tst & activated = False})
		= ([chosen.Form.value],{tst & activated = True})

	mkButton				= HtmlButton "Ok" False
	mkSelect tasks cur 		= HtmlSelect [(label,toString i) \\ (label,_) <- tasks & i <- [0..] ] (toString cur)
	
	fromButton (HtmlButton _ val) = val
	fromSelect (HtmlSelect _ val) = toInt val
	
chooseTask_cbox	:: !([LabeledTask a] -> Task [a]) ![HtmlTag] ![((!Bool,!(Bool [Bool] -> [Bool]),![HtmlTag]),LabeledTask a)] -> Task [a] | iData a
chooseTask_cbox order prompt code_ltasks
	= newTask "chooseTask_cbox" (selectTasks (\_ -> prompt ?>> selectTask_cbox (map fst code_ltasks) ) order (map snd code_ltasks))		

// ******************************************************************************************************
// choose one or more tasks on forehand out of a set

button :: !String !a -> (Task a) | iData a
button s a = newTaskTrace "button" (chooseTask_btn [] True [(s,return_V a)])

buttonTask :: !String !(Task a) -> (Task a) | iData a
buttonTask s task = newTaskTrace "buttonTask" (chooseTask_btn [] True [(s,task)])

chooseTask :: ![HtmlTag] ![LabeledTask a] -> (Task a) | iData a
chooseTask prompt options = newTaskTrace "chooseTask" (chooseTask_btn prompt True options)

chooseTaskV :: ![HtmlTag] ![LabeledTask a] -> (Task a) | iData a
chooseTaskV prompt options = newTaskTrace "chooseTaskV" (chooseTask_btn prompt False options)

mchoiceTasks :: ![HtmlTag] ![LabeledTask a] -> (Task [a]) | iData a
mchoiceTasks prompt taskOptions 
= chooseTask_cbox seqTasks prompt [((False,\b bs -> bs,[Text label]),(label,task)) \\ (label,task) <- taskOptions]

mchoiceTasks2 :: ![HtmlTag] ![(!Bool,LabeledTask a)] -> Task [a] | iData a
mchoiceTasks2 prompt taskOptions 
= chooseTask_cbox seqTasks prompt [((set,\b bs -> bs,[Text label]),(label,task)) \\ (set,(label,task)) <- taskOptions]

mchoiceTasks3 :: ![HtmlTag] ![((!Bool,!(Bool [Bool] -> [Bool]),![HtmlTag]),LabeledTask a)] -> Task [a] | iData a
mchoiceTasks3 prompt taskOptions 
= chooseTask_cbox seqTasks prompt taskOptions

mchoiceAndTasks :: ![HtmlTag] ![LabeledTask a] -> (Task [a]) | iData a
mchoiceAndTasks prompt taskOptions 
= chooseTask_cbox andTasks prompt [((False,\b bs -> bs,[Text label]),(label,task)) \\ (label,task) <- taskOptions]

mchoiceAndTasks2 :: ![HtmlTag] ![(!Bool,LabeledTask a)] -> Task [a] | iData a
mchoiceAndTasks2 prompt taskOptions 
= chooseTask_cbox andTasks prompt [((set,\b bs -> bs,[Text label]),(label,task)) \\ (set,(label,task)) <- taskOptions]

mchoiceAndTasks3 :: ![HtmlTag] ![((!Bool,!(Bool [Bool] -> [Bool]),![HtmlTag]),LabeledTask a)] -> Task [a] | iData a
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
(-&&-) taska taskb = newTaskTrace "-&&-" (andTask2 (taska,taskb))

orTasks :: ![LabeledTask a] -> (Task a) | iData a
orTasks []				= Task (return createDefault)
orTasks taskCollection	= newTask "orTasks" (allTasksCond "orTask" TTHorizontal (\list -> length list >= 1) taskCollection)
							=>> \list -> (Task (return  (hd list)))

orTask2 :: !(Task a,Task b) -> Task (EITHER a b) | iData a & iData b
orTask2 (taska,taskb) 
=	newTask "orTask2" 	( allTasksCond "orTask" TTHorizontal (\list -> length list > 0) 
								[ ("orTask.0",taska =>> \a -> return_V (LEFT a))
								, ("orTask.0",taskb =>> \b -> return_V (RIGHT b))
								]
						 =>> \res -> 	return_V (hd res)
						) 


andTasks :: ![LabeledTask a] -> (Task [a]) | iData a
andTasks taskCollection = newTaskTrace "andTasks" (allTasksCond "andTask" (TTSplit msg) (\_ -> False) taskCollection)
where
	msg = [Text "All of the following tasks need to be completed before this task can continue."]
	
(-&&-?) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iData a & iData b
(-&&-?) t1 t2 
= 		newTask "maybeTask" (andTasksCond "maybeTask" noNothing [("Maybe 1",left),("Maybe 2",right)]
  							=>>	combineResult)
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
=	newTask "andTask2"	(allTasksCond "andTask" TTHorizontal (\l -> False) 
							[ ("andTask.0",taska =>> \a -> return_V (LEFT a))
							, ("andTask.0",taskb =>> \b -> return_V (RIGHT b))
							]
						=>> \[LEFT a, RIGHT b] -> 	return_V (a,b) 
						)

andTasks_mu :: !String ![(Int,Task a)] -> (Task [a]) | iData a
andTasks_mu label tasks = newTaskTrace "andTaskMU" (domu_andTasks tasks)
where
	domu_andTasks list = andTasks [(label  <+++ " " <+++ i, i @:: task) \\ (i,task) <- list] 

andTasksCond 	:: !String !([a] -> Bool) ![LabeledTask a] -> (Task [a]) 	| iData a 
andTasksCond label pred taskCollection 
= allTasksCond label (TTSplit []) pred taskCollection 




// ******************************************************************************************************
// Timer Tasks ending when timed out

waitForTimerTask:: !HtmlTime	-> (Task HtmlTime)
waitForTimerTask time  = waitForTimerTask`
where
	waitForTimerTask`
	=						appHStOnce "getTimeAndDate" getTimeAndDate
		=>> \(ctime,_) ->  	waitForTimeTask (ctime + time)

