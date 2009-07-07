implementation module CommonCombinators
/**
* This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdList, StdTuple, StdGeneric, StdMisc, GenBimap
import iDataForms, iDataFormlib, iDataTrivial

from StdFunc	import id, const
from TSt		import :: Task(..), :: TSt{..}, :: TaskInfo{..}, :: StaticInfo{..}, :: Workflow, :: ChangeLifeTime
from TSt		import applyTask, mkSequenceTask, mkParallelTask, mkBasicTask, setOutput, setInputs
from Types		import :: ProcessId, :: DynamicId, :: TaskId, :: TaskPriority(..)
from SessionDB	import :: Session
from TaskTree	import :: TaskTree, :: TaskCombination(..)

import UITasks, UserTasks, TimeAndDateTasks, CoreCombinators, TuningCombinators, PromptingCombinators, LiftingCombinators
import Util, Either

derive gForm	Either
derive gUpd		Either
derive gPrint	Either
derive gParse	Either

//Task composition
(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iData a
(-||-) taska taskb  
=	parallel "-||-" (\list -> length list >= 1) (\[x:_] -> case x of (Left a) = a; (Right b) = b) undef
			[(taska >>= \a -> return (Left a)) <<@ "Left"
			,(taskb >>= \b -> return (Right b)) <<@ "Right"
			] <<@ TTHorizontal
			
(-&&-) infixr 4 ::  !(Task a) !(Task b) -> (Task (a,b)) | iData a & iData b
(-&&-) taska taskb
=	parallel "-&&-" (\_ -> False) undef (\[Left a, Right b] -> (a,b))
			[(taska >>= \a -> return (Left a)) <<@ "Left"
			,(taskb >>= \b -> return (Right b)) <<@ "Right"
			] <<@ TTHorizontal

anyTask	:: ![Task a] -> Task a | iData a
anyTask []		= return createDefault
anyTask tasks	= parallel "any" (\list -> length list >= 1) hd undef tasks

orTasks :: ![LabeledTask a] -> (Task a) | iData a
orTasks []		= return createDefault
orTasks tasks	= parallel "orTasks"  (\list -> length list >= 1) hd undef [t <<@ l \\ (l,t) <- tasks]

andTasks :: ![LabeledTask a] -> Task [a] | iData a
andTasks tasks = parallel "andTasks"  (\_ -> False) undef id [t <<@ l \\ (l,t) <- tasks] <<@ (TTSplit msg)
where
	msg = [Text "All of the following tasks need to be completed before this task can continue."]				

allTasks :: ![Task a] -> Task [a] | iData a
allTasks tasks = parallel "all" (\_ -> False) undef id tasks

eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iData a & iData b
eitherTask taska taskb 
=	parallel "eitherTask" (\list -> length list > 0) hd undef
			[ (taska >>= \a -> return (Left a)) <<@ "Left"
			, (taskb >>= \b -> return (Right b)) <<@ "Right"
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
repeatTask task pred a =
	task a >>= \na -> if (pred na) (return na) (repeatTask task pred na)

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
(@:) nuserId (label,task) = assign nuserId NormalPriority Nothing (task <<@ label)

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


buttonTask :: !String !(Task a) -> (Task a) | iData a
buttonTask s task = chooseTask_btn [] [(s,task)]

chooseTask :: ![HtmlTag] ![LabeledTask a] -> (Task a) | iData a
chooseTask prompt options = chooseTask_btn prompt options

mchoiceTasks :: ![HtmlTag] ![LabeledTask a] -> (Task [a]) | iData a
mchoiceTasks prompt taskOptions 
= chooseTask_cbox (\tasks -> sequence "mchoiceTasks" [ t <<@ l \\ (l,t) <- tasks]) prompt [((False,\b bs -> bs,[Text label]),(label,task)) \\ (label,task) <- taskOptions]

mchoiceTasks2 :: ![HtmlTag] ![(!Bool,LabeledTask a)] -> Task [a] | iData a
mchoiceTasks2 prompt taskOptions 
= chooseTask_cbox (\tasks -> sequence "mchoiceTasks2" [ t <<@ l \\ (l,t) <- tasks]) prompt [((set,\b bs -> bs,[Text label]),(label,task)) \\ (set,(label,task)) <- taskOptions]

mchoiceTasks3 :: ![HtmlTag] ![((!Bool,!(Bool [Bool] -> [Bool]),![HtmlTag]),LabeledTask a)] -> Task [a] | iData a
mchoiceTasks3 prompt taskOptions 
= chooseTask_cbox (\tasks -> sequence "mchoiceTasks3" [ t <<@ l \\ (l,t) <- tasks]) prompt taskOptions

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
= 	parallel "maybeTask" noNothing combineResult combineResult
			[(t1 >>= \tres -> return (Left tres)) <<@ "Left"
			,(t2 >>= \tres -> return (Right tres)) <<@ "Right"
			] <<@ TTHorizontal
where
	noNothing []					= False
	noNothing [Left  Nothing:xs]	= True
	noNothing [Right Nothing:xs]	= True
	noNothing [x:xs]				= noNothing xs	

	combineResult	[Left (Just r1),Right (Just r2)]	= Just (r1,r2)
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
transform :: (a -> b) a -> Task b | iData b
transform f x = return (f x)

edit :: (Task a) ((a,b) -> c) b -> Task c | iData a & iData b & iData c
edit finishTask endTransform val = mkParallelTask "edit" edit`
where
	edit` tst
		# (b,tst)	= applyTask (mkSequenceTask "edit-form" form) {tst & activated = True}
		# (a,tst)	= applyTask (mkSequenceTask "edit-finish" finish) {tst & activated = True}
		= (endTransform (a,b), tst)
	
	form tst
		= applyTask (mkBasicTask "edit-editor" editor) tst
	
	editor tst =:{taskNr,hst}
		# editorId		= iTaskId taskNr "editor"
		# (editor,hst) 	= mkEditForm  (Init, cFormId tst.TSt.options editorId val) hst
		# tst			= setOutput editor.form {tst & hst = hst}
		# tst			= setInputs editor.inputs tst
		= (editor.Form.value, {tst & activated = False})
	
	finish tst
		= applyTask finishTask tst

