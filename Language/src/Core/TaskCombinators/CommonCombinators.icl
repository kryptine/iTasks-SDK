implementation module CommonCombinators
/**
* This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc

from StdFunc	import id, const
from TSt		import :: Task(..), :: TaskDescription(..), :: TSt{..}, :: TaskInfo{..}, :: StaticInfo{..}, :: Workflow, :: ChangeLifeTime,:: HTTPRequest, :: Config
from TSt		import applyTask, mkSequenceTask, mkParallelTask
from Types		import :: ProcessId, :: TaskId, :: TaskPriority(..), :: User(..)
from Store		import :: Store
from SessionDB	import :: Session
from TaskTree	import :: TaskTree, :: TaskParallelInfo{..}, :: TaskParallelType{..}
from CommonDomain	import :: Note

import TaskTree
import SystemTasks, InteractionTasks, UserDBTasks, CoreCombinators, TuningCombinators, LiftingCombinators
import Util, Either
import GenVisualize, GenUpdate

derive gPrint Either
derive gParse Either

derive bimap	Maybe

//Grouping combinators
(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb = group "-||-" "Done when either subtask is finished." orfunc hd [] [taska,taskb] 
where
	orfunc (val,_) [] = ([val],Stop)
	orfunc (val,_) _  = abort "Multiple results in OR"

(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= group "||-" "Done when the second subtask is finished." rorfunc hd [] [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)]
where
	rorfunc (Right val,_) [] = ([val],Stop)
	rorfunc (Left val, _) [] = ([],Continue)
	rorfunc _ _				 = abort "Illegal result in ||-"

(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= group "||-" "Done when the first subtask is finished" lorfunc hd [] [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)]
where
	lorfunc (Right val,_) [] = ([],Continue)
	lorfunc (Left val, _) [] = ([val],Stop)
	lorfunc _ _				 = abort "Illegal result in -||"

(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb = group "-&&-" "Done when both subtasks are finished" andfunc parseresult (Nothing,Nothing) [(taska >>= \a -> return (Left a)),(taskb >>= \b -> return (Right b))]
where
	andfunc :: ((Either a b),Int) (Maybe a, Maybe b) -> ((Maybe a, Maybe b),PAction (Task (Either a b)))
	andfunc (val,_) (left,right)
	= case val of
		(Left a)
			# state = (Just a,right)
			= case state of
				(Just l, Just r) = (state,Stop)
				_				 = (state,Continue)
		(Right b)
			# state = (left,Just b)
			= case state of
				(Just l, Just r) = (state,Stop)
				_				 = (state,Continue)		
	
	parseresult (Just a,Just b) = (a,b)
	parseresult _							   = abort "AND not finished"

anyTask :: ![Task a] -> Task a | iTask a
anyTask [] 		= getDefaultValue
anyTask tasks 	= group "any" "Done when any subtask is finished" anyfunc hd [] tasks
where
	anyfunc (val,_) [] = ([val],Stop)
	anyfunc (val,_) _  = abort "Multiple results in ANY"

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks = group "all" "Done when all subtasks are finished" (allfunc(length tasks)) sortByIndex [] tasks
where
	allfunc tlen (val,idx) st 
		# st = st ++ [(idx,val)]
		| length st == tlen = (st,Stop)
		| otherwise = (st,Continue)
		
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = group "either" "Done when either subtask is finished" eitherfunc hd [] [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)]
where
	eitherfunc (val,idx) [] = ([val],Stop)
	eitherfunc (val,idx) _  = abort "Multiple results in Either"

//Parallel composition
orProc :: !(AssignedTask a) !(AssignedTask a) !TaskParallelType -> Task a | iTask a
orProc taska taskb type = parallel type "-|@|-" "Done if either subtask is finished." orfunc hd [] [taska,taskb] 
where
	orfunc (val,_) [] = ([val],Stop)
	orfunc (val,_) _  = abort "Multiple results in -|@|-"

andProc :: !(AssignedTask a) !(AssignedTask b) !TaskParallelType -> Task (a,b) | iTask a & iTask b
andProc taska taskb type = parallel type "AndProc" "Done if both subtasks are finished." andfunc parseresult (Nothing,Nothing) [{AssignedTask | taska & task = taska.task >>= \a -> return (Left a)},{AssignedTask | taskb & task = taskb.task >>= \b -> return (Right b)}]
where
	andfunc :: ((Either a b),Int) (Maybe a, Maybe b) -> ((Maybe a, Maybe b),PAction (AssignedTask (Either a b)))
	andfunc (val,_) (left,right)
	= case val of
		(Left a)
			# state = (Just a,right)
			= case state of
				(Just l, Just r) = (state,Stop)
				_				 = (state,Continue)
		(Right b)
			# state = (left,Just b)
			= case state of
				(Just l, Just r) = (state,Stop)
				_				 = (state,Continue)		
	
	parseresult (Just a,Just b) = (a,b)
	parseresult _	 			= abort "AND not finished"

anyProc :: ![AssignedTask a] !TaskParallelType -> Task a | iTask a
anyProc [] 	  type = getDefaultValue
anyProc tasks type = parallel type "any" "Done if any subtask is finished." anyfunc hd [] tasks
where
	anyfunc (val,_) [] = ([val],Stop)
	anyfunc (val,_) _  = abort "Multiple results in ANY"

allProc :: ![AssignedTask a] !TaskParallelType -> Task [a] | iTask a
allProc tasks type = parallel type "all" "Done if all subtasks are finished." (allfunc (length tasks)) sortByIndex [] tasks 
where
	allfunc tlen (val,idx) st 
		# st = st ++ [(idx,val)]
		| length st == tlen = (st,Stop)
		| otherwise = (st,Continue)

//===== LEGACY ====
/**
* The behaviour of the 'old' parallel combinator expressed in terms of the 'new' parallel combinator*
**/

//derive bimap (,)

oldParallel :: !String !([a] -> Bool) ([a] -> b) ([a] -> b) ![Task a] -> Task b | iTask a & iTask b
oldParallel label pred f_pred f_all tasks = group label label aggregate finalize (False,[]) tasks
where
	aggregate x (match,xs) = let xs` = [x:xs] in
		if (length xs` == length tasks)
			((False,xs`),Stop)			//All tasks are finished, let's stop :)
			(if (pred (map fst xs`))
				((True,xs`),Stop)		//The predicate matched, let's stop
				((False,xs`),Continue)	//Keep on working
			)
	finalize (match,xs) = (if match f_pred f_all) (map fst (sortBy (\a b -> snd a < snd b) xs))
//===================

//Task composition for optional values
(>>?) infixl 1 	:: !(Task (Maybe a)) !(a -> Task (Maybe b)) -> Task (Maybe b) | iTask a & iTask b
(>>?) t1 t2 
= 	t1 
	>>= \r1 -> 	case r1 of 
					Nothing 	-> return Nothing
					Just r`1 	-> t2 r`1

(-&?&-) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iTask a & iTask b
(-&?&-) taska taskb 
= group "-&?&-" "Done when both subtasks are finished. Yields only a result of both subtasks have a result" mbandfunc parsefunc (Nothing,Nothing) [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)]
where
	mbandfunc (val,_) (left,right)
		= case val of
			Left v
				# state = (Just v,right)
				= case state of
					(Just a, Just b) = (state,Stop)
					_				 = (state,Continue)				
			Right v
				# state = (left,Just v)
				= case state of
					(Just a, Just b) = (state,Stop)
					_				 = (state,Continue)
	parsefunc (Just (Just a), Just (Just b)) = Just (a,b)
	parsefunc _								 = Nothing

//Post processing of results
ignoreResult :: !(Task a) -> Task Void | iTask a
ignoreResult task = "ignoreResult" @>> (task >>| return Void)

transformResult :: !(a -> b) !(Task a) -> Task b | iTask a & iTask b
transformResult fun task = "transformResult" @>> (task >>= \a -> return (fun a))

stop :: Task Void
stop = "stop" @>> return Void

sortByIndex :: ![(Int,a)] -> [a]
sortByIndex [] = []
sortByIndex [(i,v):ps] = sortByIndex [(is,vs) \\ (is,vs) <- ps | is < i] ++ [v] ++ sortByIndex [(is,vs) \\ (is,vs) <- ps | is > i]
// ******************************************************************************************************
// repetition

repeatTask :: !(a -> Task a) !(a -> Bool) a -> Task a | iTask a
repeatTask task pred a =
	task a >>= \na -> if (pred na) (return na) (repeatTask task pred na)

(<|) infixl 6 :: !(Task a) !(a -> (Bool, [HtmlTag])) -> Task a | iTask a
(<|) taska pred 
		=			taska
		>>= \r -> 	case pred r of
						(True,_) -> return r
						(False,msg) -> showStickyMessage msg ||- (taska <| pred)


// ******************************************************************************************************
// Assigning tasks to users, each user has to be identified by an unique number >= 0

instance @: String
where
	(@:) :: !String !(LabeledTask a) -> Task a | iTask a
	(@:) s (label,task) = assign (toUserName s) NormalPriority Nothing (task <<@ label)
	
instance @: UserName
where
	(@:) :: !UserName !(LabeledTask a) -> Task a | iTask a
	(@:) username (label,task) = assign username NormalPriority Nothing (task <<@ label)

instance @: User
where
	(@:) :: !User !(LabeledTask a) -> Task a | iTask a
	(@:) user task = (toUserName user) @: task

// ******************************************************************************************************
