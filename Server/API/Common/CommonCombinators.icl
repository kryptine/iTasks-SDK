implementation module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass, GenRecord, Text, Time
import Util, Either, GenVisualize, GenUpdate
from StdFunc			import id, const, o
from SystemTypes		import :: ProcessId, :: User(..), :: Note(..)
from SessionDB			import :: Session
from TaskContext		import :: TaskContextTree(..), :: SubTaskContext, :: ParallelMeta
from SharedCombinators	import mapShared, :: Shared, :: ReadWriteShared
from SystemData			import randomInt
from Map				import qualified newMap
import CoreTasks, CoreCombinators, ExceptionCombinators, TuningCombinators, ProcessDBTasks, InteractionTasks

// use string instances of generic function for Tag values 
gVisualize{|Tag|} val vst = gVisualize{|*|} (toStr val) vst
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
accu :: (a acc -> (acc,Bool)) (Task a) (Shared acc) (ParallelInfo acc) -> Task a | iTask a & iTask acc
accu accufun task pstate pcontrol
	=	task
	>>= \result ->
		get pstate
	>>= \state ->
		let (nstate,stop) =  accufun result state in
				set pstate nstate
			>>| if stop
				(set pcontrol [StopParallel] >>| return result)
				(return result)
			
transform :: !(a -> b) !a -> Task b | iTask b
transform f x = mkInstantTask ("Value transformation", "Value transformation with a custom function") eval
where
	eval taskNr iworld = (TaskFinished (f x), iworld)
	
/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/

assign :: !ManagerProperties !(Task a) -> Task a | iTask a
assign props task = parallel ("Assign","Manage a task assigned to another user.") Nothing (\_ (Just r) -> r)
									[ShowAs BodyTask processControl, ShowAs (DetachedTask props) (accu accJust task)] <<@ minimalParallelLayout
where
	processControl :: state !(ReadWriteShared [ParallelTaskInfo] [Control c]) -> Task Void | iTask c
	processControl _ control =
		(updateSharedInformation (taskTitle task,"Waiting for " +++ taskTitle task) [UpdateView (GetShared toView, PutbackShared fromView)] control Void >>+ noActions) <<@ ControlTask
	
	accJust r _ = (Just r,True)
			
	toView [_,{ParallelTaskInfo|properties=Right {systemProperties=s=:{issuedAt,firstEvent,latestEvent},managerProperties=m=:{worker}}}]=
		{ mapRecord m
		& assignedTo	= worker
		, issuedAt		= Display (formatTimestamp issuedAt)
		, firstWorkedOn	= Display (fmap formatTimestamp firstEvent)
		, lastWorkedOn	= Display (fmap formatTimestamp latestEvent)
		}
	toView [_,{ParallelTaskInfo|properties=Left p}]=
		{ assignedTo = NamedUser "root"
		, priority = NormalPriority
		, status = Suspended
		, issuedAt = Display (formatTimestamp (Timestamp 0))
		, firstWorkedOn = Display Nothing
		, lastWorkedOn = Display Nothing
		, deadline = Nothing
		}
		
	fromView view=:{ProcessControlView|assignedTo} _ _
		= [UpdateProperties 1 {mapRecord view & worker = assignedTo}]
		
	formatTimestamp timestamp = timestampToGmDateTime timestamp
	
:: ProcessControlView =	{ assignedTo	:: !User
						, priority		:: !TaskPriority
						, status		:: !RunningTaskStatus
						, issuedAt		:: !Display DateTime
						, firstWorkedOn	:: !Display (Maybe DateTime)
						, lastWorkedOn	:: !Display (Maybe DateTime)
						, deadline		:: !Maybe DateTime
						}
derive class iTask ProcessControlView
derive class GenRecord ProcessControlView, ManagerProperties, TaskPriority, RunningTaskStatus

(@:) infix 3 :: !User !(Task a) -> Task a | iTask a
(@:) user task = assign {initManagerProperties & worker = user} task

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
sequence label tasks = parallel (label,RawText description) [] (\_ acc -> reverse acc) (seqTasks tasks)
where
	description = "Do the following tasks one at a time:<br /><ul><li>" +++ (join "</li><li>" (map taskTitle tasks)) +++ "</li></ul>"
	
	seqTasks []		= []
	seqTasks [t:ts]	= [ShowAs BodyTask \pstate pinfo -> t >>= accResult pstate >>= startNext pinfo ts]
	
	accResult pstate a 		= update (\acc -> [a:acc]) pstate >>| return a
	
	startNext pinfo [] a	= return a
	startNext pinfo ts a	= set pinfo [AppendTask t \\ t <- seqTasks ts] >>| return a
		

(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iTask a
(<!) task pred = parallel (taskTitle task,taskDescription task) Nothing (\_ (Just a) -> a) [ShowAs BodyTask (checked pred task)] <<@ layout
where
	checked pred task pstate pinfo
		= task >>= \a -> if (pred a)
			(set pstate (Just a) >>|return a)
			(update (\[{ParallelTaskInfo|index}] -> [RemoveTask index, AppendTask (ShowAs BodyTask (checked pred task))]) pinfo >>| return a)

	layout :: TUIParallel -> (TUIDef,[TaskAction])
	layout {TUIParallel|items} = appFst fromJust (hd items)

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb = parallel ("-||-", "Done when either subtask is finished.") Nothing (\_ (Just a) -> a)
						[ShowAs BodyTask (accu orfun taska), ShowAs BodyTask (accu orfun taskb)]
where
	orfun a _ = (Just a,True)
	
(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= parallel ("||-", "Done when the second subtask is finished.") Nothing (\_ (Just b) -> b)
		[ShowAs BodyTask (\_ _ -> taska), ShowAs BodyTask (accu orfun taskb)]
where
	orfun b _ = (Just b,True)
	
(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel ("-||", "Done when the first subtask is finished") Nothing (\_ (Just a) -> a)
		[ShowAs BodyTask (accu orfun taska), ShowAs BodyTask (\_ _ -> taskb)]				
where
	orfun a _ = (Just a,True)
	
(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb = parallel ("-&&-", "Done when both subtasks are finished") (Nothing,Nothing) resfun
	[ShowAs BodyTask (accu (\a (_,b) -> ((Just a,b),False)) taska), ShowAs BodyTask (accu (\b (a,_) -> ((a,Just b),False)) taskb)]
where
	resfun _ (Just a,Just b)	= (a,b)
	resfun _ _					= abort "AND not finished"

(-&?&-) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iTask a & iTask b
(-&?&-) taska taskb = parallel ("-&?&-", "Done when both subtasks are finished. Yields only a result of both subtasks have a result") (Nothing,Nothing) resfun
	[ShowAs BodyTask (accu (\a (_,b) -> ((a,b),False)) taska),ShowAs BodyTask (accu (\b (a,_) -> ((a,b),False)) taskb)]
where				
	resfun _ (Just a,Just b)	= Just (a,b)
	resfun _ _					= Nothing

			
:: ProcessOverviewView =	{ index			:: !Hidden TaskIndex
							, subject		:: !Display String
							, assignedTo	:: !User
							}

derive class iTask ProcessOverviewView

anyTask :: ![Task a] -> Task a | iTask a
anyTask [] 		= return defaultValue
anyTask tasks 	= parallel ("any", "Done when any subtask is finished") Nothing (\_ (Just a) -> a) (map (\t -> (ShowAs BodyTask (accu anyfun t))) tasks)
where
	anyfun a _ = (Just a, True)

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks = parallel ("all", "Done when all subtasks are finished") [] (\_ l -> sortByIndex l) [ShowAs BodyTask (accu (allfun i) t) \\ t <- tasks & i <- [0..]] 
where
	allfun i a acc = ([(i,a):acc],False)
			
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = parallel ("either", "Done when either subtask is finished") Nothing (\_ (Just a) -> a)
	[ShowAs BodyTask (accu afun taska), ShowAs BodyTask (accu bfun taskb)]
where
	afun a _ = (Just (Left a),True)
	bfun b _ = (Just (Right b),True)

stop :: Task Void
stop = return Void

randomChoice :: ![a] -> Task a | iTask a
randomChoice [] = throw "Cannot make a choice from an empty list"
randomChoice list = get randomInt >>= \i -> return (list !! ((abs i) rem (length list)))

repeatTask :: !(a -> Task a) !(a -> Bool) a -> Task a | iTask a
repeatTask task pred a =
	task a >>= \na -> if (pred na) (return na) (repeatTask task pred na)

(<|) infixl 6 :: !(Task a) !(a -> (Bool, [HtmlTag])) -> Task a | iTask a
(<|) taska pred 
		=			taska
		>>= \r -> 	case pred r of
						(True,_) -> return r
						(False,msg) -> (showInformation "Feedback" [] (toHtmlDisplay msg) >>+ noActions) ||- (taska <| pred)