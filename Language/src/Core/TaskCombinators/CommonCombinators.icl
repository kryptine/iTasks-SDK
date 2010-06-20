implementation module CommonCombinators
/**
* This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass

from StdFunc	import id, const
from TSt		import :: Task(..), :: TSt{..}, :: TaskInfo{..}, :: StaticInfo{..}, :: Workflow, :: ChangeLifeTime,:: HTTPRequest, :: Config
from TSt		import applyTask, mkSequenceTask, mkParallelTask
from Types		import :: ProcessId, :: TaskId, :: TaskPriority(..), :: User(..)
from Store		import :: Store
from SessionDB	import :: Session
from TaskTree	import :: TaskTree, :: TaskParallelInfo{..}, :: TaskParallelType{..}
from CommonDomain	import :: Note

import TaskTree
import SystemTasks, InteractionTasks, UserDBTasks, CoreCombinators, TuningCombinators, LiftingCombinators, ExceptionCombinators
import Util, Either
import GenVisualize, GenUpdate

derive gPrint Either
derive gParse Either
derive gParse		GAction, GOnlyAction, GroupedBehaviour
derive gPrint		GAction, GOnlyAction, GroupedBehaviour
derive gVisualize	GAction, GOnlyAction, GroupedBehaviour
derive gUpdate		GAction, GOnlyAction, GroupedBehaviour
derive gError		GAction, GOnlyAction, GroupedBehaviour
derive gHint		GAction, GOnlyAction, GroupedBehaviour
derive bimap Maybe, (,)

//Grouping combinators
emptyGActionL :: [GroupAction a b Void]
emptyGActionL = []

dynamicGroup :: ![Task GAction] -> Task Void
dynamicGroup initTasks = dynamicGroupA initTasks emptyGActionL

dynamicGroupA :: ![Task GAction] ![GroupAction GAction Void s] -> Task Void | iTask s
dynamicGroupA initTasks gActions =  group "dynamicGroup" "A simple group with dynamically added tasks" procfun id Void initTasks gActions
where
	procfun (action,_) _ = case action of
		GStop			= (Void, Stop)
		GContinue		= (Void, Continue)
		GExtend tasks	= (Void, Extend tasks)
		
dynamicGroupAOnly :: ![Task Void] ![GroupAction GOnlyAction Void s] -> Task Void | iTask s
dynamicGroupAOnly initTasks gActions = group "dynamicGroup" "A simple group with dynamically added tasks" procfun id Void (changeTasksType initTasks) gActions
where
	procfun (action,_) _ = case action of
		GOStop			= (Void, Stop)
		GOContinue		= (Void, Continue)
		GOExtend tasks	= (Void, Extend (changeTasksType tasks))
	changeTasksType tasks = map (\t -> (t >>| return GOContinue) <<@ getGroupedBehaviour t) tasks
	getGroupedBehaviour (Task _ {GroupedProperties | groupedBehaviour} _ _) = groupedBehaviour
		
(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb = group "-||-" "Done when either subtask is finished." orfunc hd [] [taska,taskb] emptyGActionL
where
	orfunc (val,_) [] = ([val],Stop)
	orfunc (val,_) _  = abort "Multiple results in OR"

(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= group "||-" "Done when the second subtask is finished." rorfunc hd [] [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)] emptyGActionL
where
	rorfunc (Right val,_) [] = ([val],Stop)
	rorfunc (Left val, _) [] = ([],Continue)
	rorfunc _ _				 = abort "Illegal result in ||-"

(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= group "||-" "Done when the first subtask is finished" lorfunc hd [] [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)] emptyGActionL
where
	lorfunc (Right val,_) [] = ([],Continue)
	lorfunc (Left val, _) [] = ([val],Stop)
	lorfunc _ _				 = abort "Illegal result in -||"

(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb = group "-&&-" "Done when both subtasks are finished" andfunc parseresult (Nothing,Nothing) [(taska >>= \a -> return (Left a)),(taskb >>= \b -> return (Right b))] emptyGActionL
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
anyTask tasks 	= group "any" "Done when any subtask is finished" anyfunc hd [] tasks emptyGActionL
where
	anyfunc (val,_) [] = ([val],Stop)
	anyfunc (val,_) _  = abort "Multiple results in ANY"

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks = group "all" "Done when all subtasks are finished" (allfunc(length tasks)) sortByIndex [] tasks emptyGActionL
where
	allfunc tlen (val,idx) st 
		# st = st ++ [(idx,val)]
		| length st == tlen = (st,Stop)
		| otherwise = (st,Continue)
		
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = group "either" "Done when either subtask is finished" eitherfunc hd [] [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)] emptyGActionL
where
	eitherfunc (val,idx) [] = ([val],Stop)
	eitherfunc (val,idx) _  = abort "Multiple results in Either"

//Parallel composition
orProc :: !(Task a) !(Task a) !TaskParallelType -> Task a | iTask a
orProc taska taskb type = parallel type "-|@|-" "Done if either subtask is finished." orfunc hd [] [taska,taskb] 
where
	orfunc (val,_) [] = ([val],Stop)
	orfunc (val,_) _  = abort "Multiple results in -|@|-"

andProc :: !(Task a) !(Task b) !TaskParallelType -> Task (a,b) | iTask a & iTask b
andProc taska taskb type = parallel type "AndProc" "Done if both subtasks are finished." andfunc parseresult (Nothing,Nothing) [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)]
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
	parseresult _	 			= abort "AND not finished"

anyProc :: ![Task a] !TaskParallelType -> Task a | iTask a
anyProc [] 	  type = getDefaultValue
anyProc tasks type = parallel type "any" "Done when any subtask is finished." anyfunc hd [] tasks
where
	anyfunc (val,_) [] = ([val],Stop)
	anyfunc (val,_) _  = abort "Multiple results in ANY"

allProc :: ![Task a] !TaskParallelType -> Task [a] | iTask a
allProc tasks type = parallel type "all" "Done when all subtasks are finished." (allfunc (length tasks)) sortByIndex [] tasks 
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
oldParallel label pred f_pred f_all tasks = group label label aggregate finalize (False,[]) tasks emptyGActionL
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
= group "-&?&-" "Done when both subtasks are finished. Yields only a result of both subtasks have a result" mbandfunc parsefunc (Nothing,Nothing) [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)] emptyGActionL
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
ignoreResult task = Subject "ignoreResult" @>> (task >>| return Void)

transformResult :: !(a -> b) !(Task a) -> Task b | iTask a & iTask b
transformResult fun task = Subject "transformResult" @>> (task >>= \a -> return (fun a))

stop :: Task Void
stop = Subject "stop" @>> return Void

randomChoice :: ![a] -> Task a | iTask a
randomChoice [] = throw "Cannot make a choice from an empty list"
randomChoice list = getRandomInt >>= \i -> return (list !! (i rem (length list)))

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

(@:) infix 3 :: !User !(Task a) -> Task a | iTask a
(@:) user task = assign user task

// ******************************************************************************************************
