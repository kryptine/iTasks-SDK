implementation module iTasksCombinators

// *********************************************************************************************************************************
// This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
// with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdList, StdFunc
import iDataTrivial
import iTasksBasicCombinators, iTasksHtmlSupport, iTasksTimeAndDateHandling, iTasksLiftingCombinators

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
	= newTask "doReapeatTask" dorepeatTask`
	where
		dorepeatTask` tst
		| pred a	= (a,tst) 
		# (na,tst)	= task a tst	
		= dorepeatTask na tst

(<|) infixl 6 :: !(Task a) !(a -> (Bool, HtmlCode)) -> Task a | iData a
(<|) taska pred = mkTask "repeatTest" doTask
where
	doTask
	=				taska
		=>> \r -> 		case pred r of
						(True,_) -> return_V r
						(False,msg) -> msg ?>> doTask


// ******************************************************************************************************
// Assigning tasks to users, each user has to be identified by an unique number >= 0

(@:) infix 3 :: !UserId !(LabeledTask a) -> Task a | iData a
(@:) nuserId ltask = assignTaskTo True nuserId ltask

(@::) infix 3 :: !UserId !(Task a)	-> (Task a) | iData  a												
(@::) nuserId taska = assignTaskTo True nuserId ("Task for " <+++ nuserId,taska)

(@:>) infix 3 :: !UserId !(LabeledTask a) -> Task a | iData a
(@:>) nuserId ltask = assignTaskTo False nuserId ltask

(@::>) infix 3 :: !UserId !(Task a) -> (Task a) | iData  a												
(@::>) nuserId taska = assignTaskTo False nuserId ("Task for " <+++ nuserId,taska)

// ******************************************************************************************************
// choose one or more tasks on forehand out of a set

button :: !String !a -> (Task a) | iCreateAndPrint a
button s a = mkTask "button" (chooseTask_btn [] True [(s,return_V a)])

buttonTask :: !String !(Task a) -> (Task a) | iCreateAndPrint a
buttonTask s task = mkTask "buttonTask" (chooseTask_btn [] True [(s,task)])

chooseTask :: !HtmlCode ![LabeledTask a] -> (Task a) | iCreateAndPrint a
chooseTask prompt options = mkTask "chooseTask" (chooseTask_btn prompt True options)

chooseTaskV :: !HtmlCode ![LabeledTask a] -> (Task a) | iCreateAndPrint a
chooseTaskV prompt options = mkTask "chooseTask" (chooseTask_btn prompt False options)

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

