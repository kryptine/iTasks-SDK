implementation module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass, GenRecord, Text, Time, Tuple, List
import Util, Either, GenVisualize, GenUpdate
from StdFunc			import id, const, o
from SystemTypes		import :: ProcessId, :: User(..), :: Note(..)
from TaskContext		import :: TaskContextTree(..), :: SubTaskContext, :: ParallelMeta
from SharedCombinators	import mapShared, :: Shared, :: ReadWriteShared
from SystemData			import randomInt, topLevelTasks
from Map				import qualified newMap
import CoreTasks, CoreCombinators, ExceptionCombinators, TuningCombinators, ProcessDBTasks, InteractionTasks

// use string instances of generic function for Tag values 
gVisualizeText{|Tag|} mode (Tag t) = gVisualizeText{|*|} mode (toString t)
gVisualizeHtml{|Tag|} mode (Tag t) = gVisualizeHtml{|*|} mode (toString t)
gVisualizeEditor{|Tag|} val vst = gVisualizeEditor{|*|} (toStr val) vst
where
	toStr Nothing			= Nothing
	toStr (Just (Tag t))	= Just (toString t)

gUpdate{|Tag|} mode ust = basicUpdateSimple mode (Tag "") ust
gDefaultMask{|Tag|} _ = [Touched []]

gVerify{|Tag|} _ vst = simpleVerify "Enter a tag" vst
	
JSONEncode{|Tag|} (Tag t) = JSONEncode{|*|} (toString t)
JSONDecode{|Tag|} nodes = case nodes of
	[JSONString str]	= (Just (Tag str), [])
	_					= (Nothing, nodes)
gEq{|Tag|} (Tag x) (Tag y) = (toString x) == (toString y)

derive bimap Maybe, (,)

(>>*) infixl 1 :: !(Task a) !(TermFunc a (Task b)) -> Task b | iTask a & iTask b
(>>*) task termF = task >>+ termF >>= id

(>?*) infixl 1 :: !(Task a) ![(!Action,!TaskContinuation a b)] -> Task b | iTask a & iTask b
(>?*) task continuations = task >>* \st-> UserActions (map (appSnd (mapContinuation st)) continuations)
where
	mapContinuation _						(Always task)	= Just task
	mapContinuation {localValid,modelValue}	(IfValid taskF)	= if localValid (Just (taskF modelValue)) Nothing
	mapContinuation st						(Sometimes f)	= f st
	
(>?) infixl 1 :: !(Task a) !(a -> Bool) -> Task a | iTask a
(>?) task pred = task >>+ \{modelValue} -> if (pred modelValue) (StopInteraction modelValue) (UserActions [])

//Helper function for tasks in a parallel set
accu :: (a acc -> (acc,Bool)) (Task a) (TaskList acc) -> Task ParallelControl | iTask a & iTask acc
accu accufun task tlist
	=	task
	>>= \result ->
		get (taskListState tlist)
	>>= \state ->
		let (nstate,stop) =  accufun result state in
				set nstate (taskListState tlist) 
			>>| return (if stop Stop Continue)
			
transform :: !(a -> b) !a -> Task b | iTask b
transform f x = mkInstantTask ("Value transformation", "Value transformation with a custom function") eval
where
	eval taskNr iworld = (TaskFinished (f x), iworld)
	
/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/

assign :: !ManagementMeta !(Task a) -> Task a | iTask a
assign props task = parallel ("Assign","Manage a task assigned to another user.") Nothing (\_ (Just r) -> r)
									[(Embedded, processControl),(Detached props, accu accJust task)] <<@ defaultParallelLayout
where
	processControl :: !(TaskList a) -> Task ParallelControl
	processControl tlist =
		(updateSharedInformation (taskTitle task,"Waiting for " +++ taskTitle task) [UpdateView (GetShared toView, SetShared fromView)] control Void >>+ noActions)
	where
		control = taskListProperties tlist
		
	accJust r _ = (Just r,True)
			
	toView [_,{ParallelTaskInfo|properties=Right {systemProperties=s=:{issuedAt,issuedBy,firstEvent,latestEvent},managerProperties=m=:{worker}}}]=
		{ mapRecord m
		& assignedTo	= worker
		, issuedAt		= Display (formatTimestamp issuedAt)
		, issuedBy		= Display issuedBy
		, firstWorkedOn	= Display (fmap formatTimestamp firstEvent)
		, lastWorkedOn	= Display (fmap formatTimestamp latestEvent)
		}
	toView [_,{ParallelTaskInfo|properties=Left p}]=
		{ assignedTo	= Just AnyUser
		, priority		= NormalPriority
		, issuedBy		= Display AnyUser
		, issuedAt		= Display (formatTimestamp (Timestamp 0))
		, firstWorkedOn = Display Nothing
		, lastWorkedOn	= Display Nothing
		, deadline		= Nothing
		}
		
	fromView view=:{ProcessControlView|assignedTo} _ _
		= []// [UpdateProperties 1 {mapRecord view & worker = assignedTo}]
		
	formatTimestamp timestamp = timestampToGmDateTime timestamp
	
:: ProcessControlView =	{ assignedTo	:: !Maybe User
						, priority		:: !TaskPriority
						, issuedAt		:: !Display DateTime
						, issuedBy		:: !Display User
						, firstWorkedOn	:: !Display (Maybe DateTime)
						, lastWorkedOn	:: !Display (Maybe DateTime)
						, deadline		:: !Maybe DateTime
						}
derive class iTask ProcessControlView
derive class GenRecord ProcessControlView, ManagementMeta, TaskPriority

(@:) infix 3 :: !User !(Task a) -> Task a | iTask a
(@:) user task = assign {noMeta & worker = Just user} task

(>>^) infixl 1 :: !(Task a) (Task b) -> Task a | iTask a & iTask b
(>>^) taska taskb = taska >>= \x -> taskb >>= \_ -> return x

(>>?) infixl 1 	:: !(Task (Maybe a)) !(a -> Task (Maybe b)) -> Task (Maybe b) | iTask a & iTask b
(>>?) t1 t2 
= 	t1 
	>>= \r1 -> 	case r1 of 
					Nothing 	-> return Nothing
					Just r`1 	-> t2 r`1

justdo :: !(Task (Maybe a)) -> Task a | iTask a
justdo task
= task >>= \r -> case r of
	Just x	= return x
	Nothing	= throw ("The task " +++ taskTitle task +++ " returned nothing.")

sequence :: !String ![Task a]  -> Task [a] | iTask a
sequence label tasks = Description label @>> (seqTasks tasks)
where
	seqTasks []		= return []
	seqTasks [t:ts]	= t >>= \a -> seqTasks ts >>= \as -> return [a:as]


(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iTask a
(<!) task pred = parallel (taskMeta task) Nothing (\_ (Just a) -> a) [(Embedded, checked pred task 0)] <<@ layout
where
	checked pred task i tlist
		=	task
		>>= \a -> if (pred a)
			(set (Just a) (taskListState tlist) 											>>| return Stop)
			(removeTask i tlist >>| appendTask (Embedded, checked pred task (i + 1)) tlist	>>| return Continue)
			 
	layout :: TUIParallel -> (TUIDef,[TaskAction])
	layout {TUIParallel|items=[(_,tui,actions):_]} = (fromJust tui, actions)

forever :: !(Task a) -> Task b | iTask a & iTask b	
forever	t = (<!) t (\_ -> False) >>| return defaultValue

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb = parallel ("-||-", "Done when either subtask is finished.") Nothing (\_ (Just a) -> a)
						[(Embedded, accu orfun taska), (Embedded, accu orfun taskb)]
where
	orfun a _ = (Just a,True)
	
(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= parallel (taskTitle taskb) Nothing (\_ (Just b) -> b)
		[(Embedded, \_ -> taska >>| return Continue), (Embedded, accu orfun taskb)]
where
	orfun b _ = (Just b,True)
	
(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel (taskTitle taska) Nothing (\_ (Just a) -> a)
		[(Embedded, accu orfun taska), (Embedded, \_ -> taskb >>| return Continue)]				
where
	orfun a _ = (Just a,True)
	
(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb = parallel ("-&&-", "Done when both subtasks are finished") (Nothing,Nothing) resfun
	[(Embedded, accu (\a (_,b) -> ((Just a,b),False)) taska),(Embedded, accu (\b (a,_) -> ((a,Just b),False)) taskb)]
where
	resfun _ (Just a,Just b)	= (a,b)
	resfun _ _					= abort "AND not finished"

(-&?&-) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iTask a & iTask b
(-&?&-) taska taskb = parallel ("-&?&-", "Done when both subtasks are finished. Yields only a result of both subtasks have a result") (Nothing,Nothing) resfun
	[(Embedded, accu (\a (_,b) -> ((a,b),False)) taska),(Embedded, accu (\b (a,_) -> ((a,b),False)) taskb)]
where				
	resfun _ (Just a,Just b)	= Just (a,b)
	resfun _ _					= Nothing

			
:: ProcessOverviewView =	{ index			:: !Hidden Int
							, subject		:: !Display String
							, assignedTo	:: !User
							}

derive class iTask ProcessOverviewView

anyTask :: ![Task a] -> Task a | iTask a
anyTask [] 		= return defaultValue
anyTask tasks 	= parallel ("any", "Done when any subtask is finished") Nothing (\_ (Just a) -> a) (map (\t -> (Embedded, accu anyfun t)) tasks)
where
	anyfun a _ = (Just a, True)

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks = parallel ("all", "Done when all subtasks are finished") [] (\_ l -> sortByIndex l) [(Embedded, accu (allfun i) t) \\ t <- tasks & i <- [0..]] 
where
	allfun i a acc = ([(i,a):acc],False)
			
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = parallel ("either", "Done when either subtask is finished") Nothing (\_ (Just a) -> a)
	[(Embedded, accu afun taska), (Embedded, accu bfun taskb)]
where
	afun a _ = (Just (Left a),True)
	bfun b _ = (Just (Right b),True)

randomChoice :: ![a] -> Task a | iTask a
randomChoice [] = throw "Cannot make a choice from an empty list"
randomChoice list = get randomInt >>= \i -> return (list !! ((abs i) rem (length list)))

repeatTask :: !(a -> Task a) !(a -> Bool) a -> Task a | iTask a
repeatTask task pred a =
	task a >>= \na -> if (pred na) (return na) (repeatTask task pred na)

(<|) infixl 6 :: !(Task a) !(a -> (Bool, String)) -> Task a | iTask a
(<|) taska pred 
		=			taska
		>>= \r -> 	case pred r of
						(True,_) -> return r
						(False,msg) -> (viewInformation "Feedback" []  msg >>+ noActions`) ||- (taska <| pred)
where
	noActions` :: (TermFunc a Void) | iTask a
	noActions` = noActions
	
appendTopLevelTask :: !ManagementMeta !(Task a) -> Task ProcessId | iTask a
appendTopLevelTask props task = appendTask (Detached props, \_ -> task >>| return Continue) topLevelTasks >>= transform WorkflowProcess 