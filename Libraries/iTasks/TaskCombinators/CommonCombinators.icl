implementation module CommonCombinators
/**
* This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdList, StdTuple
import iDataFormlib, iDataTrivial
from StdFunc	import id
from TSt		import :: Task(..), :: TSt{..}, :: StaticInfo{..}, :: Workflow
from TSt		import accTaskTSt, mkSequenceTask, mkParallelTask, mkBasicTask, setOutput, setInputs
from Types		import :: ProcessId, :: TaskId
from SessionDB	import :: Session
from TaskTree	import :: TaskTree, :: TaskCombination(..)

import UITasks, UserTasks, TimeAndDateTasks, BasicCombinators, TuningCombinators, PromptingCombinators, LiftingCombinators
import Util

//Task composition
(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iData a
(-||-) taska taskb  
=	parallel "-||-" (\list -> length list >= 1) (\[x:_] -> case x of (LEFT a) = a; (RIGHT b) = b)
			[("Left",	taska >>= \a -> return (LEFT a))
			,("Right",	taskb >>= \b -> return (RIGHT b))
			] <<@ TTHorizontal
			
(-&&-) infixr 4 ::  !(Task a) !(Task b) -> (Task (a,b)) | iData a & iData b
(-&&-) taska taskb
=	parallel "-&&-" (\_ -> False) (\[LEFT a, RIGHT b] -> (a,b))
			[("Left",	taska >>= \a -> return (LEFT a))
			,("Right",	taskb >>= \b -> return (RIGHT b))
			] <<@ TTHorizontal

orTasks :: ![LabeledTask a] -> (Task a) | iData a
orTasks []		= return createDefault
orTasks tasks	= parallel "orTasks"  (\list -> length list >= 1) hd tasks

andTasks :: ![LabeledTask a] -> Task [a] | iData a
andTasks tasks = parallel "andTasks"  (\_ -> False) id tasks <<@ (TTSplit msg)
where
	msg = [Text "All of the following tasks need to be completed before this task can continue."]				

eitherTask :: !(Task a) !(Task b) -> Task (EITHER a b) | iData a & iData b
eitherTask taska taskb 
=	parallel "eitherTask" (\list -> length list > 0) hd
			[ ("Left",	taska >>= \a -> return (LEFT a))
			, ("Right",	taskb >>= \b -> return (RIGHT b))
			] <<@ TTHorizontal


// ******************************************************************************************************
// monads for combining iTasks
(>>?) infixl 1 	:: !(Task (Maybe a)) !(a -> Task (Maybe b)) -> Task (Maybe b) | iData a & iData b
(>>?) t1 t2 
= 				t1 
	>>= \r1 -> 	case r1 of 
					Nothing 	-> return Nothing
					Just r`1 	-> t2 r`1

// ******************************************************************************************************
// repetition

repeatTask :: !(a -> Task a) !(a -> Bool) a -> Task a | iData a
repeatTask task pred a = dorepeatTask a
where
	dorepeatTask a 
	= compound "repeatTask" (Task dorepeatTask`)
	where
		dorepeatTask` tst
		| pred a	= (a,tst) 
		# (na,tst)	= accTaskTSt (task a) tst	
		= accTaskTSt (dorepeatTask na) tst

(<|) infixl 6 :: !(Task a) !(a -> (Bool, [HtmlTag])) -> Task a | iData a
(<|) taska pred = compound "repeatTest" doTask
where
	doTask
	=				taska
		>>= \r -> 		case pred r of
						(True,_) -> return r
						(False,msg) -> msg ?>> (taska <| pred)


// ******************************************************************************************************
// Assigning tasks to users, each user has to be identified by an unique number >= 0

(@:) infix 3 :: !UserId !(LabeledTask a) -> Task a | iData a
(@:) nuserId ltask=:(taskname,task) = mkSequenceTask "@:" assigntask
where
	assigntask tst =:{TSt|userId} = accTaskTSt (
			getUser nuserId >>= \(_,displayName) ->
			
			[Text "Waiting for Task ", ITag [] [Text taskname], Text " from ", ITag [] [Text displayName], BrTag []]
			?>>
			delegate nuserId (taskname,task)
		) tst

//(@::) infix 3 :: !UserId !(Task a)	-> (Task a) | iData  a												
//(@::) nuserId taska = nuserId @: ("Task for " +++ toString nuserId, taska)

(@:>) infix 3 :: !UserId !(LabeledTask a) -> Task a | iData a
(@:>) nuserId ltask = delegate nuserId ltask

//(@::>) infix 3 :: !UserId !(Task a) -> (Task a) | iData  a												
//(@::>) nuserId taska = nuserId @:> ("Task for " +++ toString nuserId, taska)

// ******************************************************************************************************
// choose one or more tasks on forehand out of a set

selection :: !([LabeledTask a] -> Task [Int]) !([LabeledTask a] -> Task [a]) ![LabeledTask a] -> Task [a] | iData a
selection chooser executer tasks = chooser tasks >>= \chosen -> executer [tasks!!i \\ i <- chosen | i >=0 && i < length tasks]

chooseTask_btn 	:: ![HtmlTag] ![LabeledTask a] -> Task a | iData a
chooseTask_btn prompt ltasks
	= (prompt ?>> selectWithButtons (map fst ltasks)) >>= \chosen -> (snd (ltasks!!chosen))

chooseTask_pdm 	:: ![HtmlTag] !Int ![LabeledTask a] -> Task a | iData a
chooseTask_pdm prompt initial ltasks
	= (prompt ?>> selectWithPulldown (map fst ltasks) initial) >>= \chosen -> (snd (ltasks!!chosen))
	
chooseTask_cbox	:: !([LabeledTask a] -> Task [a]) ![HtmlTag] ![((!Bool,!(Bool [Bool] -> [Bool]),![HtmlTag]),LabeledTask a)] -> Task [a] | iData a
chooseTask_cbox order prompt code_ltasks
	= selection (\_ -> prompt ?>> selectWithCheckboxes [(html,set,setfun) \\ ((set,setfun,html),_) <- code_ltasks] ) order (map snd code_ltasks)		

// ******************************************************************************************************
// choose one or more tasks on forehand out of a set

button :: !String !a -> (Task a) | iData a
button s a = chooseTask_btn [] [(s,return a)]

buttonTask :: !String !(Task a) -> (Task a) | iData a
buttonTask s task = chooseTask_btn [] [(s,task)]

chooseTask :: ![HtmlTag] ![LabeledTask a] -> (Task a) | iData a
chooseTask prompt options = chooseTask_btn prompt options

mchoiceTasks :: ![HtmlTag] ![LabeledTask a] -> (Task [a]) | iData a
mchoiceTasks prompt taskOptions 
= chooseTask_cbox (sequence "mchoiceTasks") prompt [((False,\b bs -> bs,[Text label]),(label,task)) \\ (label,task) <- taskOptions]

mchoiceTasks2 :: ![HtmlTag] ![(!Bool,LabeledTask a)] -> Task [a] | iData a
mchoiceTasks2 prompt taskOptions 
= chooseTask_cbox (sequence "mchoiceTasks2") prompt [((set,\b bs -> bs,[Text label]),(label,task)) \\ (set,(label,task)) <- taskOptions]

mchoiceTasks3 :: ![HtmlTag] ![((!Bool,!(Bool [Bool] -> [Bool]),![HtmlTag]),LabeledTask a)] -> Task [a] | iData a
mchoiceTasks3 prompt taskOptions 
= chooseTask_cbox (sequence "mchoiceTasks3") prompt taskOptions

mchoiceAndTasks :: ![HtmlTag] ![LabeledTask a] -> (Task [a]) | iData a
mchoiceAndTasks prompt taskOptions 
= chooseTask_cbox andTasks prompt [((False,\b bs -> bs,[Text label]),(label,task)) \\ (label,task) <- taskOptions]

mchoiceAndTasks2 :: ![HtmlTag] ![(!Bool,LabeledTask a)] -> Task [a] | iData a
mchoiceAndTasks2 prompt taskOptions 
= chooseTask_cbox andTasks prompt [((set,\b bs -> bs,[Text label]),(label,task)) \\ (set,(label,task)) <- taskOptions]

mchoiceAndTasks3 :: ![HtmlTag] ![((!Bool,!(Bool [Bool] -> [Bool]),![HtmlTag]),LabeledTask a)] -> Task [a] | iData a
mchoiceAndTasks3 prompt taskOptions 
= chooseTask_cbox andTasks prompt taskOptions


(-&?&-) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iData a & iData b
(-&?&-) t1 t2 
= 	parallel "maybeTask" noNothing combineResult
			[("Left",	t1 >>= \tres -> return (LEFT tres))
			,("Right",	t2 >>= \tres -> return (RIGHT tres))
			] <<@ TTHorizontal
where
	noNothing []					= False
	noNothing [LEFT  Nothing:xs]	= True
	noNothing [RIGHT Nothing:xs]	= True
	noNothing [x:xs]				= noNothing xs	

	combineResult	[LEFT (Just r1),RIGHT (Just r2)]	= Just (r1,r2)
	combineResult	_									= Nothing

andTasks_mu :: !String ![(Int,Task a)] -> (Task [a]) | iData a
andTasks_mu label tasks = domu_andTasks tasks
where
	domu_andTasks list = andTasks [(label  +++ " " +++ toString i, i @: (toString i,task)) \\ (i,task) <- list] 


// ******************************************************************************************************
// Timer Tasks ending when timed out

waitForTimerTask:: !HtmlTime	-> (Task HtmlTime)
waitForTimerTask time  = waitForTimerTask`
where
	waitForTimerTask`
	=						appHSt "getTimeAndDate" getTimeAndDate
		>>= \(ctime,_) ->  	waitForTimeTask (ctime + time)

//Misc
ok :: Task Void
ok = button "Ok" Void

yes	:: Task Bool
yes = button "Yes" True

no :: Task Bool
no = button "No" False

transform :: (a -> b) a -> Task b | iData b
transform f x = return (f x)

edit :: (Task a) ((a,b) -> c) b -> Task c | iData a & iData b & iData c
edit finishTask endTransform val = mkParallelTask "edit" edit`
where
	edit` tst
		# (b,tst)	= accTaskTSt (mkSequenceTask "edit-form" form) {tst & activated = True}
		# (a,tst)	= accTaskTSt (mkSequenceTask "edit-finish" finish) {tst & activated = True}
		= (endTransform (a,b), tst)
	
	form tst
		= accTaskTSt (mkBasicTask "edit-editor" editor) tst
	
	editor tst =:{taskNr,hst}
		# editorId		= iTaskId taskNr "editor"
		# (editor,hst) 	= mkEditForm  (Init, cFormId tst.options editorId val) hst
		# tst			= setOutput editor.form {tst & hst = hst}
		# tst			= setInputs editor.inputs tst
		= (editor.Form.value, {tst & activated = False})
	
	finish tst
		= accTaskTSt finishTask tst
