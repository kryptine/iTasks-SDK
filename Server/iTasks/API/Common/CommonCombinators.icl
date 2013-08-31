implementation module iTasks.API.Common.CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass
import Text, System.Time, Data.Tuple, Data.List, Data.Either, Data.Functor
import iTasks.Framework.Util
from StdFunc			import id, const, o
from iTasks.API.Core.SystemTypes		import :: User(..), :: Note(..)
from iTasks.Framework.TaskState			import :: TaskTree(..), :: DeferredJSON
from iTasks.API.Core.SystemData			import randomInt, topLevelTasks
from Data.Map				import qualified put

import iTasks.API.Core.CoreTasks, iTasks.API.Core.CoreCombinators, iTasks.API.Common.InteractionTasks, iTasks.API.Core.LayoutCombinators

(>>*) infixl 1 :: !(Task a) ![TaskStep a b] -> Task b | iTask a & iTask b
(>>*) task steps = step task steps 

(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>=) taska taskbf = step taska [OnAction ActionContinue (hasValue taskbf), OnValue (ifStable taskbf)]

(>>!) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>!) taska taskbf = step taska [OnAction ActionContinue (hasValue taskbf)]

(>>-) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>-) taska taskbf = step taska [OnValue (ifStable taskbf)]

(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>|) taska taskb = step taska [OnAction ActionContinue (hasValue (const taskb)), OnValue (ifStable (const taskb))]

(>>^) infixl 1 :: !(Task a) (Task b) -> Task a | iTask a & iTask b
(>>^) taska taskb = taska >>= \x -> taskb >>| return x

(@?) infixl 1 :: !(Task a) !((TaskValue a) -> TaskValue b) -> Task b | iTask a & iTask b
(@?) task f = transform f task

(@) infixl 1 :: !(Task a) !(a -> b) -> Task b | iTask a & iTask b
(@) task f = transform (fmap f) task

(@>) infixl 1 :: !(Task a) !((TaskValue a) r -> Maybe w, ReadWriteShared r w) -> Task a | iTask a
(@>) task (f,share) = project f share task

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t

(<@@) infixl 2 :: !(Task a) !(b a) -> Task a | tunev b a & iTask a
(<@@) t a = tunev a t

(@@>) infixr 2 :: !(b a) !(Task a) -> Task a | tunev b a & iTask a
(@@>) a t = tunev a t

try :: !(Task a) (e -> Task a) -> Task a | iTask a & iTask, toString e
try task handler = step task [OnValue (ifStable return), OnException handler]

catchAll :: !(Task a) (String -> Task a) -> Task a | iTask a
catchAll task handler = step task [OnValue (ifStable return), OnAllExceptions handler]

(>^*) infixl 1 :: !(Task a) ![TaskStep a b] -> Task a | iTask a & iTask b
(>^*) task steps = sideStep task steps

sideStep :: !(Task a) ![TaskStep a b] -> Task a | iTask a & iTask b
sideStep ta steps = parallel Void [(Embedded,const ta),(Embedded,stepper)] @ (map snd) @? firstRes
where
    firstRes (Value [v:_] _) = v
    stepper l = forever (watch (taskListState l) @? firstRes >>* steps) @? const NoValue

//Helper functions for projections
projectJust :: (Maybe a) r -> Maybe (Maybe a)
projectJust mba _ = Just mba
/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
assign :: !ManagementMeta !(Task a) -> Task a | iTask a
assign props task
	=	parallel Void
			[(Embedded, \s -> processControl s),(Detached props False, \_ -> task)]
	@?	result
where
	processControl tlist
		= viewSharedInformation (Title "Waiting for result") [ViewWith toView] (taskListMeta tlist) @? const NoValue
					
	toView [_,{TaskListItem|progressMeta=Just p,managementMeta=Just m}]=
		{ assignedTo	= toString m.ManagementMeta.worker
		, issuedBy		= toString p.ProgressMeta.issuedBy
		, issuedAt		= p.ProgressMeta.issuedAt
		, priority		= m.ManagementMeta.priority
		, firstWorkedOn	= p.ProgressMeta.firstEvent
		, lastWorkedOn	= p.ProgressMeta.latestEvent
		}	
	result (Value [_,(_,v)] _)	= v
	result _					= NoValue

instance toString UserConstraint
where
	toString AnyUser				= "Anybody"
	toString (UserWithId uid)		= uid
	toString (UserWithRole role)	= "Any user with role " +++ role

:: ProcessControlView =	{ assignedTo	:: !String
						, issuedBy		:: !String
						, issuedAt		:: !DateTime
						, priority		:: !TaskPriority
						, firstWorkedOn	:: !Maybe DateTime
						, lastWorkedOn	:: !Maybe DateTime
						}
derive class iTask ProcessControlView

(@:) infix 3 :: !worker !(Task a) -> Task a | iTask a & toUserConstraint worker
(@:) worker task = assign {defaultValue & worker = toUserConstraint worker} task

justdo :: !(Task (Maybe a)) -> Task a | iTask a
justdo task
= task >>= \r -> case r of
	Just x	= return x
	Nothing	= throw ("The task returned nothing.")

sequence :: !String ![Task a]  -> Task [a] | iTask a
sequence _ tasks = seqTasks tasks
where
	seqTasks []		= return []
	seqTasks [t:ts]	= t >>- \a -> seqTasks ts >>- \as -> return [a:as]

//Repeat task until the predicate holds (loops if the predicate is false)
(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iTask a
(<!) task pred
	= parallel Void [(Embedded,const task),(Embedded,restarter)] @? res
where
    restarter tlist = ((watch (taskListState tlist) @ hd) >>* [OnValue (check (restart tlist))]) <<@ NoUserInterface

    check t (Value (Value x stable) _)  = if (stable && not (pred x)) (Just t) Nothing
    check t _                           = Nothing
	
	restart tlist
		=	get (taskListMeta tlist)
		>>- \l=:[{TaskListItem|taskId=t1},{TaskListItem|taskId=t2}:_] ->
		    removeTask t1 tlist
		>>|	removeTask t2 tlist
		>>| appendTask Embedded (const task) tlist
		>>| appendTask Embedded restarter tlist
        @?  const NoValue

	res (Value [(_,value):_] _)	= value
	res _					    = NoValue

forever :: !(Task a) -> Task a | iTask a	
forever	t = (t <! (const False))

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb = anyTask [taska,taskb]

(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= parallel Void
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] @? res
where
	res	(Value [_,(_,Value (Right b) s)] _)	= Value b s
	res _									= NoValue
	
(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel Void
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] @? res			
where
	res	(Value [(_,Value (Left a) s),_] _)	= Value a s
	res _									= NoValue
	
(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb
	= parallel Void
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] @? res
where
	res (Value [(_,Value (Left a) sa),(_,Value (Right b) sb)] _)	= Value (a,b) (sa && sb)
	res _														    = NoValue

feedForward :: !d (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task b | descr d & iTask a & iTask b
feedForward desc taska taskbf = parallel desc
	[(Embedded, \s -> taska @ Left)
	,(Embedded, \s -> taskbf (mapRead prj (toReadOnly (taskListState s))) @ Right)
	] @? res
where
	prj [Value (Left a) _,_]		= Just a
	prj _							= Nothing
	
	res (Value [_,(_,Value (Right b) s)] _)	= Value b s
	res _									= NoValue
	
(>&>) infixl 1  :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task b | iTask a & iTask b
(>&>) taska taskbf = feedForward Void taska taskbf
				
feedSideways :: !d (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task a | descr d & iTask a & iTask b
feedSideways desc taska taskbf = parallel desc
    [(Embedded, \s -> taska)
	,(Embedded, \s -> taskbf (mapRead prj (toReadOnly (taskListState s))) @? const NoValue)
    ] @? res
where
	prj [Value a _:_]	= Just a
	prj _				= Nothing

    res (Value [(_,v):_] _) = v
    res _                   = NoValue

(>&^) infixl 1 :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task a | iTask a & iTask b
(>&^) taska taskbf = feedSideways Void taska taskbf

:: ProcessOverviewView =	{ index			:: !Hidden Int
							, subject		:: !Display String
							, assignedTo	:: !User
							}

derive class iTask ProcessOverviewView

anyTask :: ![Task a] -> Task a | iTask a
anyTask tasks
	= parallel Void [(Embedded,const t) \\ t <- tasks] @? res
where
	res (Value l _) = let sl = sortBy (\a b -> fst a > fst b) l in
                        hd ([v \\ (_,v=:(Value _ True)) <- sl] ++ [v \\ (_,v=:(Value _ False)) <- sl] ++ [NoValue])
	res _			= NoValue

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks
	= parallel Void
		[(Embedded,const t) \\ t <- tasks] @? res
where
	res (Value l _)	= Value [v \\ (_,Value v _) <- l] (foldl allStable True l)

    allStable cur (_,Value _ s) = cur && s
    allStable cur _             = False
				
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = (taska @ Left) -||- (taskb @ Right)

randomChoice :: ![a] -> Task a | iTask a
randomChoice [] = throw "Cannot make a choice from an empty list"
randomChoice list = get randomInt >>= \i -> return (list !! ((abs i) rem (length list)))

repeatTask :: !(a -> Task a) !(a -> Bool) a -> Task a | iTask a
repeatTask task pred a =
	task a >>= \na -> if (pred na) (return na) (repeatTask task pred na)

whileUnchanged :: !(ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask w & iTask b
whileUnchanged share task
	= 	( (get share >>- \val -> (wait Void ((=!=) val) share <<@ NoUserInterface @ const Nothing)
          -||-
          (task val @ Just)
        ) <! isJust)
	@?	onlyJust

onlyJust (Value (Just x) s) = Value x s
onlyJust _                  = NoValue

whileUnchangedWith :: !(r r -> Bool) !(ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask w & iTask b
whileUnchangedWith eq share task
	= 	((get share >>= \val -> (wait Void (eq val) share <<@ NoUserInterface @ const Nothing) -||- (task val @ Just)) <! isJust)
	@?	onlyJust

appendTopLevelTask :: !ManagementMeta !Bool !(Task a) -> Task TaskId | iTask a
appendTopLevelTask props evalDirect task = appendTask (Detached props evalDirect) (\_ -> task @ const Void) topLevelTasks

appendTopLevelTaskFor :: !worker !Bool !(Task a) -> Task TaskId | iTask a & toUserConstraint worker
appendTopLevelTaskFor worker evalDirect task = appendTopLevelTask {defaultValue & worker = toUserConstraint worker} evalDirect task
			
valToMaybe (Value v _)  = Just v
valToMaybe NoValue		= Nothing

always :: (Task b) (TaskValue a) -> Maybe (Task b)
always taskb val = Just taskb

never :: (Task b) (TaskValue a) -> Maybe (Task b)
never taskb val	= Nothing

ifValue :: (a -> Bool) (a -> Task b) (TaskValue a) -> Maybe (Task b)
ifValue pred ataskb (Value a _) 
    | pred a 	= Just (ataskb a)
    | otherwise = Nothing
ifValue _ _ _ = Nothing

hasValue	:: (a -> Task b) 				(TaskValue a) -> Maybe (Task b)
hasValue ataskb (Value a _) = Just (ataskb a)
hasValue _ _ = Nothing

ifCond :: Bool (Task b) (TaskValue a) -> Maybe (Task b)
ifCond True taskb _ = Just taskb
ifCond False taskb _ = Nothing

ifStable :: (a -> Task b) (TaskValue a) -> Maybe (Task b)
ifStable ataskb (Value a True) = Just (ataskb a)
ifStable _ _ 				   = Nothing

ifUnstable :: (a -> Task b) (TaskValue a) -> Maybe (Task b)
ifUnstable ataskb (Value a False) = Just (ataskb a)
ifUnstable _ _ 				   = Nothing
