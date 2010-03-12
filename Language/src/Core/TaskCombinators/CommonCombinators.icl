implementation module CommonCombinators
/**
* This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdBool, StdList, StdTuple, StdGeneric, StdMisc

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

//Task composition
(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb
=
	parallel "-||-" "Done if either subtask is finished." orfunc hd [] [taska,taskb] 
where
	orfunc (val,_) [] = ([val],Stop)
	orfunc (val,_) _  = abort "Multiple results in OR"

(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb =
	parallel "-&&-" "Done if both subtasks are finished." andfunc parseresult (Nothing,Nothing) [(taska >>= \a -> return (Left a)),(taskb >>= \b -> return (Right b))]
where
	andfunc :: ((Either a b),Int) (Maybe a, Maybe b) -> ((Maybe a, Maybe b),ParallelAction (Either a b))
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
anyTask tasks 	= parallel "any" "Done if any subtask is finished." anyfunc hd [] tasks

anyTaskExt :: ![(UserId,Task a)] !TaskParallelType -> Task a | iTask a
anyTaskExt tasks type = parallelU "any" "Done if any subtask is finished." type anyfunc hd [] tasks

anyfunc (val,_) [] = ([val],Stop)
anyfunc (val,_) _  = abort "Multiple results in ANY"

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks = parallel "all" "Done if all subtasks are finished" (allfunc(length tasks)) sortByIndex [] tasks

allTasksExt :: ![(UserId,Task a)] !TaskParallelType -> Task [a] | iTask a 
allTasksExt tasks type = parallelU "all" "Done if all subtasks are finished." type (allfunc (length tasks)) sortByIndex [] tasks 

allfunc :: !Int !(a,Int) ![(Int,a)] -> ([(Int,a)],ParallelAction a)
allfunc tlen (val,idx) st 
		# st = st ++ [(idx,val)]
		| length st == tlen = (st,Stop)
		| otherwise = (st,Continue)
		
sortByIndex :: ![(Int,a)] -> [a]
sortByIndex [] = []
sortByIndex [(i,v):ps] = sortByIndex [(is,vs) \\ (is,vs) <- ps | is < i] ++ [v] ++ sortByIndex [(is,vs) \\ (is,vs) <- ps | is > i]

eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = parallel "either" "Done if either subtask is finished. The next step depends on which subtask is finished." eitherfunc hd [] [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)]

eitherTaskExt :: !(UserId,Task a) !(UserId,Task b) !TaskParallelType -> Task (Either a b) | iTask a & iTask b
eitherTaskExt (usera,taska) (userb,taskb) type = parallelU "either" "Done if either subtask is finished. The next step depends on which subtask is finished." type eitherfunc hd [] [(usera,(taska >>= \a -> return (Left a))),(userb,(taskb >>= \b -> return (Right b)))]

eitherfunc :: !((Either a b),Int) ![(Either a b)] -> ([(Either a b)],ParallelAction (Either a b))
eitherfunc (val,idx) [] = ([val],Stop)
eitherfunc (val,idx) _  = abort "Multiple results in Either"

(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= parallel "||-" "Done if the second subtask is finished. Only the result of this task is used." rorfunc hd [] [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)]
where
	rorfunt (Right val,_) [] = ([val],Stop)
	rorfunc (Left val, _) [] = ([],Continue)
	rorfunc _ _				 = abort "Illegal result in ||-"

(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel "||-" "Done if the first subtask is finished. Only the result of this task is used." lorfunc hd [] [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)]
where
	lorfunt (Right val,_) [] = ([],Continue)
	lorfunc (Left val, _) [] = ([val],Stop)
	lorfunc _ _				 = abort "Illegal result in -||"

(>>?) infixl 1 	:: !(Task (Maybe a)) !(a -> Task (Maybe b)) -> Task (Maybe b) | iTask a & iTask b
(>>?) t1 t2 
= 	t1 
	>>= \r1 -> 	case r1 of 
					Nothing 	-> return Nothing
					Just r`1 	-> t2 r`1

(-&?&-) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iTask a & iTask b
(-&?&-) taska taskb 
= parallel "-&?&-" "Done if the both subtasks are finished. There is only a result if neither subtask has been left blank." mbandfunc parsefunc (Nothing,Nothing) [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)]
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

instance @: UserId
where
	(@:) :: !UserId !(LabeledTask a) -> Task a | iTask a
	(@:) username (label,task) = assign username NormalPriority Nothing (task <<@ label)
	
instance @: UserName
where
	(@:) :: !UserName !(LabeledTask a) -> Task a | iTask a
	(@:) (UserName username) (label,task) = assign username NormalPriority Nothing (task <<@ label)

instance @: User
where
	(@:) :: !User !(LabeledTask a) -> Task a | iTask a
	(@:) user task = user.User.userName @: task

// ******************************************************************************************************
