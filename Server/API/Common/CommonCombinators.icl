implementation module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass, GenRecord, Text, Time, Tuple, List
import Util, Either, GenVisualize, GenUpdate
from StdFunc			import id, const, o
from SystemTypes		import :: ProcessId, :: User(..), :: Note(..)
from TaskContext		import :: TaskContextTree(..), :: SubTaskId, :: SubTaskOrder, :: SubTaskContext, :: ParallelMeta
from SharedCombinators	import mapShared, :: Shared, :: ReadWriteShared
from SystemData			import randomInt, topLevelTasks
from Map				import qualified newMap
import CoreTasks, CoreCombinators, TuningCombinators, InteractionTasks

(>>*) infixl 1 :: !(Task a) ![TaskStep a b] -> Task b | iTask a & iTask b
(>>*) task steps = step task steps 

(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>=) taska taskbf = step taska [WhenStable taskbf]

(>>!) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>!) taska taskbf = step taska [WithResult ActionOk (const True) taskbf]

(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>|) taska taskb = step taska [WhenStable (const taskb)]

(>>^) infixl 1 :: !(Task a) (Task b) -> Task a | iTask a & iTask b
(>>^) taska taskb = taska >>= \x -> taskb >>| return x

(@?) infixl 1 :: !(Task a) !((Maybe a) -> Maybe b) -> Task b | iTask a & iTask b
(@?) task f = transform f task

(@) infixl 1 :: !(Task a) !(a -> b) -> Task b | iTask a & iTask b
(@) task f = transform (fmap f) task

(@>) infixl 1 :: !(Task a) !((Maybe a) r -> Maybe w, ReadWriteShared r w) -> Task a | iTask a
(@>) task (f,share) = project f share task

try :: !(Task a) (e -> Task a) -> Task a | iTask a & iTask, toString e
try task handler = step task [WhenStable return, Catch handler]

catchAll :: !(Task a) (String -> Task a) -> Task a | iTask a
catchAll task handler = step task [WhenStable return, CatchAll handler]

//Helper functions for projections
justResult :: (Maybe (Maybe a)) -> Maybe a
justResult (Just (Just a))	= Just a
justResult _				= Nothing

projectJust :: (Maybe a) r -> Maybe (Maybe a)
projectJust mba _ = Just mba
/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
assign :: !ManagementMeta !(Task a) -> Task a | iTask a
assign props task
	=	parallel ("Assign","Manage a task assigned to another user.") Nothing
			[(Embedded, processControl),(Detached props, \s -> (task @> (\mba _ -> Just mba, taskListState s) ) @ const Keep)]
	@?	justResult
where
	//processControl :: !(TaskList (Maybe a)) -> Task ParallelResult
	processControl tlist =
		(enterSharedChoice (taskTitle task,"Waiting for " +++ taskTitle task) [] control @ const Keep)
	where
		control = taskListMeta tlist
					
	toView [_,{ParallelTaskMeta|taskMeta,progressMeta=Just p,managementMeta=Just m}]=
		{ assignedTo	= m.ManagementMeta.worker
		, priority		= m.ManagementMeta.priority
		, issuedAt		= Display (Just p.ProgressMeta.issuedAt)
		, issuedBy		= Display (Just p.ProgressMeta.issuedBy)
		, firstWorkedOn	= Display p.ProgressMeta.firstEvent
		, lastWorkedOn	= Display p.ProgressMeta.latestEvent
		}
	toView [_,_]=
		{ assignedTo	= Nothing
		, priority		= NormalPriority
		, issuedAt		= Display Nothing
		, issuedBy		= Display Nothing
		, firstWorkedOn	= Display Nothing
		, lastWorkedOn	= Display Nothing
		}	
	fromView view=:{ProcessControlView|assignedTo} _ _
		= []// [UpdateProperties 1 {mapRecord view & worker = assignedTo}]
		
	formatTimestamp timestamp = timestampToGmDateTime timestamp
	
:: ProcessControlView =	{ assignedTo	:: !Maybe User
						, priority		:: !TaskPriority
						, issuedAt		:: !Display (Maybe DateTime)
						, issuedBy		:: !Display (Maybe User)
						, firstWorkedOn	:: !Display (Maybe DateTime)
						, lastWorkedOn	:: !Display (Maybe DateTime)
						}
derive class iTask ProcessControlView
derive class GenRecord ProcessControlView, ManagementMeta, TaskPriority

(@:) infix 3 :: !User !(Task a) -> Task a | iTask a
(@:) user task = assign {noMeta & worker = Just user} task

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
(<!) task pred = (parallel (taskMeta task) Nothing [(Embedded, checked pred task 0)] <<@ layout) @? res
where
	checked pred task i tlist
		=	task
		>>= \a -> if (pred a)
			(set (Just a) (taskListState tlist) 											>>| return Stop)
			(removeTask i tlist >>| appendTask (Embedded, checked pred task (i + 1)) tlist	>>| return Keep)
	
	res (Just (Just a)) = Just a
	res _				= Nothing

	layout :: TUIParallel -> (TUIDef,[TaskAction])
	layout {TUIParallel|items=[(_,_,_,tui,actions):_]} = (fromJust tui, actions)

forever :: !(Task a) -> Task b | iTask a & iTask b	
forever	t = (<!) t (\_ -> False) >>| return defaultValue

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb
	= parallel ("-||-", "Done when either subtask is finished.") (Nothing,Nothing)
		[(Embedded, \s -> (taska @> (\mbl (_,mbr) -> Just (mbl,mbr),taskListState s)) @ const Stop)
		,(Embedded, \s -> (taskb @> (\mbr (mbl,_) -> Just (mbl,mbr),taskListState s)) @ const Stop)
		] @? res
where
	res (Just (Just a,_)) 		= Just a
	res (Just (Nothing,Just a))	= Just a
	res _						= Nothing
	
(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= parallel (taskTitle taskb) Nothing //DON'T COPY ALL META DATA (this will also copy things like 'Hide'
		[(Embedded, \_ -> taska @ const Keep)
		,(Embedded, \s -> (taskb @> (\mbb _ -> Just mbb,taskListState s)) @ const Stop)
		] @? res
where
	res	(Just (Just b)) = Just b
	res _				= Nothing
	
(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel (taskTitle taska) Nothing //DON'T COPY ALL META DATA (this will also copy things like 'Hide'
		[(Embedded, \s -> (taska @> (\mba _ -> Just mba,taskListState s)) @ const Stop)
		,(Embedded, \_ -> taskb @ const Keep)
		] @? res			
where
	res	(Just (Just a)) = Just a
	res _				= Nothing
	
(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb
	= parallel ("-&&-", "Done when both subtasks are finished") (Nothing,Nothing)
		[(Embedded, \s -> (taska @> (\mbl (_,mbr) -> Just (mbl,mbr),taskListState s)) @ const Keep)
		,(Embedded, \s -> (taskb @> (\mbr (mbl,_) -> Just (mbl,mbr),taskListState s)) @ const Keep)
		] @? res
where
	res (Just (Just a,Just b))	= Just (a,b)
	res _						= Nothing
				
:: ProcessOverviewView =	{ index			:: !Hidden Int
							, subject		:: !Display String
							, assignedTo	:: !User
							}

derive class iTask ProcessOverviewView

anyTask :: ![Task a] -> Task a | iTask a
anyTask tasks
	= parallel ("any", "Done when any subtask is finished") [Nothing \\ _ <- tasks]
		[(Embedded, \s -> (t @> (\mba r -> Just (updateAt i mba r),taskListState s)) @ const Keep ) \\ t <- tasks & i <- [0..]] @? res
where	
	res (Just ([Just a:_]))		= Just a
	res (Just ([Nothing:as]))	= res (Just as)
	res _ 						= Nothing

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks
	= parallel ("all", "Done when all subtasks are finished") [Nothing \\ _ <- tasks]
		[(Embedded, \s -> (t @> (\mba r -> Just (updateAt i mba r),taskListState s)) @ const Keep ) \\ t <- tasks & i <- [0..]] @? res
where
	res (Just [])				= Just []
	res (Just [Just a:mbas])	= case res (Just mbas) of
		Just as = Just [a:as]
		Nothing	= Nothing
	res _						= Nothing
				
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb
	= parallel ("either", "Done when either subtask is finished") (Nothing,Nothing)
		[(Embedded, \s -> (taska @> (\mbl (_,mbr) -> Just (mbl,mbr),taskListState s)) @ const Keep)
		,(Embedded, \s -> (taskb @> (\mbr (mbl,_) -> Just (mbl,mbr),taskListState s)) @ const Keep)
		] @? res
where
	res (Just (Just a,_))	= Just (Left a)
	res (Just (_,Just b))	= Just (Right b)
	res _					= Nothing 

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
						(False,msg) -> (viewInformation "Feedback" []  msg) ||- (taska <| pred)

whileUnchanged :: (ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask w & iTask b
whileUnchanged share task
	= (get share >>= \val -> (task val @ Just) -||- (Hide @>> wait "watching share change" ((=!=) val) share @ const Nothing)) <! isJust @ fromJust
	
appendTopLevelTask :: !ManagementMeta !(Task a) -> Task ProcessId | iTask a
appendTopLevelTask props task = appendTask (Detached props, \_ -> task >>| return Keep) topLevelTasks @ WorkflowProcess 