implementation module iTasks.API.Common.TaskCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass
import Text, System.Time, Data.Tuple, Data.List, Data.Either, Data.Functor
import iTasks.Framework.Util
from StdFunc			import id, const, o
from iTasks.API.Core.Types		    import :: User(..), :: Note(..)
from iTasks.API.Core.SDSs           import randomInt, topLevelTasks, currentUser, currentDateTime
from iTasks.Framework.TaskState		import :: TaskTree(..), :: DeferredJSON
import qualified Data.Map as DM

import iTasks.API.Core.Tasks, iTasks.API.Core.TaskCombinators, iTasks.API.Common.InteractionTasks, iTasks.API.Core.LayoutCombinators
import iTasks.API.Common.SDSCombinators

(>>*) infixl 1 :: !(Task a) ![TaskCont a (Task b)] -> Task b | iTask a & iTask b
(>>*) task steps = step task (const Nothing) steps

(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>=) taska taskbf = step taska (const Nothing) [OnAction ActionContinue (hasValue taskbf), OnValue (ifStable taskbf)]

(>>!) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>!) taska taskbf = step taska (const Nothing) [OnAction ActionContinue (hasValue taskbf)]

(>>-) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>-) taska taskbf = step taska (const Nothing) [OnValue (ifStable taskbf)]

(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>|) taska taskb = step taska (const Nothing) [OnAction ActionContinue (hasValue (const taskb)), OnValue (ifStable (const taskb))]

(>>^) infixl 1 :: !(Task a) (Task b) -> Task a | iTask a & iTask b
(>>^) taska taskb = taska >>= \x -> taskb >>| return x

(@?) infixl 1 :: !(Task a) !((TaskValue a) -> TaskValue b) -> Task b | iTask a & iTask b
(@?) task f = transform f task

(@) infixl 1 :: !(Task a) !(a -> b) -> Task b | iTask a & iTask b
(@) task f = transform (fmap f) task

(@!) infixl 1 :: !(Task a) !b -> Task b | iTask a & iTask b
(@!) task b = transform (fmap (const b)) task

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
try task handler = step task id [OnValue (ifStable return), OnException handler]

catchAll :: !(Task a) (String -> Task a) -> Task a | iTask a
catchAll task handler = step task id [OnValue (ifStable return), OnAllExceptions handler]

(>^*) infixl 1 :: !(Task a) ![TaskCont a (Task b)] -> Task a | iTask a & iTask b
(>^*) task steps = sideStep task steps

sideStep :: !(Task a) ![TaskCont a (Task b)] -> Task a | iTask a & iTask b
sideStep ta conts = parallel [(Embedded,const ta)] (map pcont conts) @ (map snd) @? firstRes
where
    pcont (OnValue tfun)         = OnValue (vfun tfun)
    pcont (OnAction action tfun) = OnAction action (vfun tfun)
    pcont (OnException tfun)     = OnException (efun tfun)
    pcont (OnAllExceptions tfun) = OnAllExceptions (efun tfun)

    vfun tfun (Value [(t,v):_] _) = fmap (\t -> (Embedded,removeWhenStable t)) (tfun v)
    efun tfun e = (\t -> (Embedded,removeWhenStable t)) (tfun e)

    firstRes (Value [v:_] _) = v

removeWhenStable :: (Task a) -> ParallelTask b | iTask a & iTask b
removeWhenStable t
    = \l -> t >>* [OnValue (ifStable (const (get (taskListSelfId l) >>- \id -> removeTask id l @? const NoValue)))]

//Helper functions for projections
projectJust :: (Maybe a) r -> Maybe (Maybe a)
projectJust mba _ = Just mba

/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
assign :: !TaskAttributes !(Task a) -> Task a | iTask a
assign attr task
	=	parallel [(Embedded, \s -> processControl s),(Detached attr False, \_ -> task)] []
	@?	result
where
	processControl tlist
		= viewSharedInformation () [ViewWith toView] (sdsFocus filter tlist) @? const NoValue
    where
        filter = {TaskListFilter|onlySelf=False,onlyTaskId = Nothing, onlyIndex = Just [1]
                 ,includeValue=False,includeAttributes=True,includeProgress=True}
					
	toView (_,[{TaskListItem|progress=Just p,attributes}:_]) =
		{ assignedTo	= toSingleLineText (case ('DM'.get TAUser attributes, 'DM'.get TARole attributes) of
                                              (Just u, _) -> Just (toString u)
                                              (_, Just r) -> Just (toString r)
                                              _           -> Nothing)
		, firstWorkedOn	= p.InstanceProgress.firstEvent
		, lastWorkedOn	= p.InstanceProgress.lastEvent
        , taskStatus    = case p.InstanceProgress.value of
                            None      -> "No results so far..."
                            Unstable  -> "In progres..."
                            Stable    -> "Task done"
                            Exception -> "Something went wrong"
        }

	result (Value [_,(_,v)] _)	= v
	result _					= NoValue

:: ProcessControlView =	{ assignedTo	:: !String
						, firstWorkedOn	:: !Maybe DateTime
						, lastWorkedOn	:: !Maybe DateTime
                        , taskStatus    :: !String
						}
derive class iTask ProcessControlView

workerAttributes :: worker [(TaskAttrKey, TaskAttrValue)] -> TaskAttributes | toUserConstraint worker
workerAttributes worker attr = case toUserConstraint worker of
    AnyUser = 'DM'.newMap
    u=:(UserWithId _) = 'DM'.fromList [(TAUser, TAUserVal u):attr]
    UserWithRole role = 'DM'.fromList [(TARole, TAStringVal role):attr]
    
(@:) infix 3 :: !worker !(Task a) -> Task a | iTask a & toUserConstraint worker
(@:) worker task
  =                get currentUser -&&- get currentDateTime
  >>= \(me,now) -> assign (workerAttributes worker
                             [ (TATitle,      TAStringVal (toTitle worker))
                             , (TACreatedBy,  TAUserVal (toUserConstraint me))
                             , (TACreatedAt,  TADateTimeVal now)
                             , (TAPriority,   TAIntVal 5)
                             , (TACreatedFor, TAUserVal (toUserConstraint worker))
                             ])
                          task

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
	= parallel [(Embedded,const task),(Embedded,restarter)] [] @? res
where
    restarter tlist = (watch (sdsFocus (Left 0) (taskListItemValue tlist)) >>* [OnValue (check (restart tlist))]) <<@ NoUserInterface

    check t (Value (Value x stable) _)  = if (stable && not (pred x)) (Just t) Nothing
    check t _                           = Nothing
	
	restart tlist
		=   get (taskListIds tlist)
		>>- \[t1,t2:_] ->
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
	= parallel
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] [] @? res
where
	res	(Value [_,(_,Value (Right b) s)] _)	= Value b s
	res _									= NoValue
	
(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] [] @? res
where
	res	(Value [(_,Value (Left a) s),_] _)	= Value a s
	res _									= NoValue
	
(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb
	= parallel
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] [] @? res
where
	res (Value [(_,Value (Left a) sa),(_,Value (Right b) sb)] _)	= Value (a,b) (sa && sb)
	res _														    = NoValue

feedForward :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task b | iTask a & iTask b
feedForward taska taskbf = parallel
	[(Embedded, \s -> taska @ Left)
	,(Embedded, \s -> taskbf (mapRead prj (toReadOnly (sdsFocus (Left 0) (taskListItemValue s)))) @ Right)
	] [] @? res
where
	prj (Value (Left a) _)  = Just a
	prj _					= Nothing
	
	res (Value [_,(_,Value (Right b) s)] _)	= Value b s
	res _									= NoValue
	
(>&>) infixl 1  :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task b | iTask a & iTask b
(>&>) taska taskbf = feedForward taska taskbf
				
feedSideways :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task a | iTask a & iTask b
feedSideways taska taskbf = parallel
    [(Embedded, \s -> taska)
	,(Embedded, \s -> taskbf (mapRead prj (toReadOnly (sdsFocus (Left 0) (taskListItemValue s)))) @? const NoValue)
    ] [] @? res
where
	prj (Value a _)	= Just a
	prj _			= Nothing

    res (Value [(_,v):_] _) = v
    res _                   = NoValue

(>&^) infixl 1 :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task a | iTask a & iTask b
(>&^) taska taskbf = feedSideways taska taskbf

:: ProcessOverviewView =	{ index			:: !Hidden Int
							, subject		:: !Display String
							, assignedTo	:: !User
							}

derive class iTask ProcessOverviewView

anyTask :: ![Task a] -> Task a | iTask a
anyTask tasks
	= parallel [(Embedded,const t) \\ t <- tasks] [] @? res
where
	res (Value l _) = let sl = sortBy (\a b -> fst a > fst b) l in
                        hd ([v \\ (_,v=:(Value _ True)) <- sl] ++ [v \\ (_,v=:(Value _ False)) <- sl] ++ [NoValue])
	res _			= NoValue

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks
	= parallel
		[(Embedded,const t) \\ t <- tasks] [] @? res
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

//We throw an exception when the share changes to make sure that the right hand side of
//the -||- combinator is not evaluated anymore (because it was created from the 'old' share value)
whileUnchanged :: !(ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask b
whileUnchanged share task
	= 	( (get share >>- \val ->
            try ((watch share >>* [OnValue (ifValue ((=!=) val) (\_ -> throw ShareChanged))]) -||- (task val @ Just))
                (\ShareChanged -> (return Nothing) )
          ) <! isJust
        )
	@?	onlyJust

:: ShareChanged = ShareChanged
derive class iTask ShareChanged
instance toString ShareChanged where toString ShareChanged = "Share changed exception"

onlyJust (Value (Just x) s) = Value x s
onlyJust _                  = NoValue

whileUnchangedWith :: !(r r -> Bool) !(ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask w & iTask b
whileUnchangedWith eq share task
	= 	((get share >>= \val -> (wait Void (eq val) share <<@ NoUserInterface @ const Nothing) -||- (task val @ Just)) <! isJust)
	@?	onlyJust

withSelection :: (Task c) (a -> Task b) (ReadOnlyShared (Maybe a)) -> Task b | iTask a & iTask b & iTask c
withSelection def tfun s = whileUnchanged s (maybe (def @? const NoValue) tfun)

appendTopLevelTask :: !TaskAttributes !Bool !(Task a) -> Task TaskId | iTask a
appendTopLevelTask attr evalDirect task = appendTask (Detached attr evalDirect) (\_ -> task @ const Void) topLevelTasks

appendTopLevelTaskFor :: !worker !Bool !(Task a) -> Task TaskId | iTask a & toUserConstraint worker
appendTopLevelTaskFor worker evalDirect task = appendTopLevelTask (workerAttributes worker []) evalDirect task
			
valToMaybe (Value v _)  = Just v
valToMaybe NoValue		= Nothing

always :: b (TaskValue a) -> Maybe b
always taskb val = Just taskb

never :: b (TaskValue a) -> Maybe b
never taskb val	= Nothing

ifValue :: (a -> Bool) (a -> b) (TaskValue a) -> Maybe b
ifValue pred ataskb (Value a _) 
    | pred a 	= Just (ataskb a)
    | otherwise = Nothing
ifValue _ _ _ = Nothing

hasValue	:: (a -> b) (TaskValue a) -> Maybe b
hasValue ataskb (Value a _) = Just (ataskb a)
hasValue _ _ = Nothing

ifCond :: Bool b (TaskValue a) -> Maybe b
ifCond True taskb _ = Just taskb
ifCond False taskb _ = Nothing

ifStable :: (a -> b) (TaskValue a) -> Maybe b
ifStable ataskb (Value a True) = Just (ataskb a)
ifStable _ _ 				   = Nothing

ifUnstable :: (a -> b) (TaskValue a) -> Maybe b
ifUnstable ataskb (Value a False) = Just (ataskb a)
ifUnstable _ _ 				   = Nothing
