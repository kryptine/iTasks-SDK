implementation module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass, GenRecord, Text, Time, Tuple, List
import Util, Either, GenVisualize, GenUpdate
from StdFunc			import id, const, o
from SystemTypes		import :: User(..), :: Note(..)
from TaskContext		import :: TaskState(..), :: ParallelMeta, :: ParallelContext, :: ParallelItem
from SystemData			import randomInt, topLevelTasks

import CoreTasks, CoreCombinators, InteractionTasks, LayoutCombinators

(>>*) infixl 1 :: !(Task a) ![TaskStep a b] -> Task b | iTask a & iTask b
(>>*) task steps = step task steps 

(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>=) taska taskbf = step taska [WhenStable taskbf]

(>>!) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>!) taska taskbf = step taska [WithResult ActionContinue (const True) taskbf]

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

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t

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
	=	parallel Void Nothing
			[(Embedded, processControl),(Detached props, \s -> (task @> (\mba _ -> Just mba, taskListState s) ) @ const Keep)]
	@?	justResult
where
	//processControl :: !(TaskList (Maybe a)) -> Task ParallelResult
	processControl tlist =
		(enterSharedChoice ("Waiting","Waiting for " <+++ task) [] control @ const Keep)
	where
		control = taskListMeta tlist
					
	toView [_,{TaskListItem|progressMeta=Just p,managementMeta=Just m}]=
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
	Nothing	= throw ("The task returned nothing.")

sequence :: !String ![Task a]  -> Task [a] | iTask a
sequence label tasks = Title label @>> (seqTasks tasks)
where
	seqTasks []		= return []
	seqTasks [t:ts]	= t >>= \a -> seqTasks ts >>= \as -> return [a:as]

//Repeat task until the predicate holds (loops if the predicate is false)
(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iTask a
(<!) task pred
	= parallel Void Nothing [(Embedded, checked pred task)] @? res
where
	checked pred task tlist
		=	task
		@> (prj,taskListState tlist)
		>>= \a -> if (pred a)
			(return Remove)
			((appendTask Embedded (checked pred task) tlist) @ const Remove)
			
	prj mba _ 			= Just mba
	res (Just (Just a)) = Just a
	res _				= Nothing

forever :: !(Task a) -> Task b | iTask a & iTask b	
forever	t = (t <! (const False)) >>| return defaultValue

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb
	= parallel "Do one of the following." (Nothing,Nothing)
		[(Embedded, \s -> (taska @> (\mbl (_,mbr) -> Just (mbl,mbr),taskListState s)) @ const Stop)
		,(Embedded, \s -> (taskb @> (\mbr (mbl,_) -> Just (mbl,mbr),taskListState s)) @ const Stop)
		] @? res
where
	res (Just (Just a,_)) 		= Just a
	res (Just (Nothing,Just a))	= Just a
	res _						= Nothing
	
(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= parallel Void Nothing 
		[(Embedded, \_ -> taska @ const Keep)
		,(Embedded, \s -> (taskb @> (\mbb _ -> Just mbb,taskListState s)) @ const Stop)
		] @? res
where
	res	(Just (Just b)) = Just b
	res _				= Nothing
	
(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel Void Nothing
		[(Embedded, \s -> (taska @> (\mba _ -> Just mba,taskListState s)) @ const Stop)
		,(Embedded, \_ -> taskb @ const Keep)
		] @? res			
where
	res	(Just (Just a)) = Just a
	res _				= Nothing
	
(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb
	= parallel Void (Nothing,Nothing)
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
	= parallel Void [Nothing \\ _ <- tasks]
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

whileUnchanged :: !(ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask w & iTask b
whileUnchanged share task
	= ((	get share	
		>>= \val ->
			(task val >>= \res -> return (Just res))
			-||-
			(wait "watching share change" ((=!=) val) share >>| return Nothing)
		)
	<! isJust) >>= \(Just r) -> return r
	
appendTopLevelTask :: !ManagementMeta !(Task a) -> Task TaskId | iTask a
appendTopLevelTask props task = appendTask (Detached props) (\_ -> task @ const Remove) topLevelTasks @ \topNo -> (TaskId topNo 0)

appendTopLevelTaskFor :: !User !(Task a) -> Task TaskId | iTask a
appendTopLevelTaskFor user task = appendTopLevelTask {noMeta & worker = Just user} task

instance tune BeforeLayout
where tune (BeforeLayout f) task = tune (ModifyLayout (\l pa0 ac0 at0 -> let (pa1,ac1,at1) = f (pa0,ac0,at0) in l pa1 ac1 at1)) task
		
instance tune AfterLayout
where tune (AfterLayout f) task	= tune (ModifyLayout (\l -> (\pa ac at -> (f (l pa ac at))))) task

instance tune Title
where tune (Title t) task = tune (BeforeLayout (appThd3 (kvSet TITLE_ATTRIBUTE t))) task
instance tune Icon 
where tune (Icon i) task = tune (BeforeLayout (appThd3 (kvSet ICON_ATTRIBUTE i))) task
instance tune Attribute
where tune (Attribute k v) task = tune (BeforeLayout (appThd3 (kvSet k v))) task
instance tune Window
where tune Window task = task