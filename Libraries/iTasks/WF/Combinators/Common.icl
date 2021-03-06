implementation module iTasks.WF.Combinators.Common
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
*/
import StdEnv
import Data.Functor
import Data.Func
import qualified Data.Map as DM

import iTasks.Engine
import iTasks.Internal.SDS
import iTasks.SDS.Combinators.Common
import iTasks.SDS.Sources.Core
import iTasks.SDS.Sources.System
import iTasks.UI.Definition
import iTasks.UI.Editor.Controls
import iTasks.UI.Layout.Common, iTasks.UI.Layout.Default
import iTasks.UI.Tune
import iTasks.WF.Combinators.Core
import iTasks.WF.Combinators.SDS
import iTasks.WF.Derives
import iTasks.WF.Tasks.Core
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Tasks.SDS

instance Functor Task where
	fmap f t = t @ f

(>>*) infixl 1 :: !(Task a) ![TaskCont a (Task b)] -> Task b | TC, JSONEncode{|*|} a
(>>*) task steps = step task (const Nothing) steps

(>>?) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | TC, JSONEncode{|*|} a
(>>?) taska taskbf = step taska (const Nothing) [OnAction ActionContinue (hasValue taskbf), OnValue (ifStable taskbf)]

(>?|) infixl 1 :: !(Task a) !(Task b) -> Task b | TC, JSONEncode{|*|} a
(>?|) l r = l >>* [OnAction ActionContinue (always r), OnValue (ifStable (\_->r))]

(>>!) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | TC, JSONEncode{|*|} a
(>>!) taska taskbf = step taska (const Nothing) [OnAction ActionContinue (hasValue taskbf)]

(>!|) infixl 1 :: !(Task a) !(Task b) -> Task b | TC, JSONEncode{|*|} a
(>!|) taska taskb = taska >>! \_ -> taskb

(>>-) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | TC, JSONEncode{|*|} a
(>>-) taska taskbf = step taska (const Nothing) [OnValue (ifStable taskbf)]

(>>~) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | TC, JSONEncode{|*|} a
(>>~) taska taskbf = step taska (const Nothing) [OnValue (hasValue taskbf)]

(>>^) infixl 1 :: !(Task a) (Task b) -> Task a | TC, JSONEncode{|*|} a & TC, JSONEncode{|*|} b
(>>^) taska taskb = taska >>? \x -> taskb >?| return x

(@?) infixl 1 :: !(Task a) !((TaskValue a) -> TaskValue b) -> Task b
(@?) task f = transform f task

(@) infixl 1 :: !(Task a) !(a -> b) -> Task b
(@) task f = transform (fmap f) task

(@!) infixl 1 :: !(Task a) !b -> Task b
(@!) task b = transform (fmap (const b)) task

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

justdo :: !(Task (Maybe a)) -> Task a | iTask a
justdo task
= task >>- \r -> case r of
	Just x	= return x
	Nothing	= throw ("The task returned nothing.")

sequence :: ![Task a]  -> Task [a] | iTask a
sequence tasks = foldr (\t ts->t >>- \tv->ts >>- \tvs->return [tv:tvs]) (return []) tasks

foreverStIf :: (a -> Bool) a !(a -> Task a) -> Task a | iTask a
foreverStIf pred st t
	= step (t st) id [OnValue $ withStable \st->Just (if (pred st) (foreverStIf pred st t) (return st))]

(<!) infixl 6 :: (Task a) (a -> Bool) -> Task a | iTask a
(<!) task pred = foreverIf (not o pred) task

foreverIf :: (a -> Bool) !(Task a) -> Task a | iTask a
foreverIf pred task = step task id [OnValue $ withStable \v->Just (if (pred v) (foreverIf pred task) (return v))]

foreverSt :: a !(a -> Task a) -> Task a | iTask a
foreverSt st t = step (t st) id [OnValue $ withStable $ \st -> Just $ foreverSt st t]

forever :: (Task a) -> Task a | iTask a
forever t = step t id [OnValue $ withStable \_->Just $ forever t]

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

feedForward :: (Task a) ((SDSLens () (Maybe a) ()) -> Task b) -> Task b | iTask a & iTask b
feedForward taska taskbf = parallel
	[(Embedded, \s -> taska @ Left)
	,(Embedded, \s -> taskbf (mapRead prj (sdsFocus (Left 0) (taskListItemValue s))) @ Right)
	] [] @? res
where
	prj (Value (Left a) _)  = Just a
	prj _					= Nothing

	res (Value [_,(_,Value (Right b) s)] _)	= Value b s
	res _									= NoValue

(>&>) infixl 1  :: (Task a) ((SDSLens () (Maybe a) ()) -> Task b) -> Task b | iTask a & iTask b
(>&>) taska taskbf = feedForward taska taskbf

feedSideways :: (Task a) ((SDSLens () (Maybe a) ()) -> Task b) -> Task a | iTask a & iTask b
feedSideways taska taskbf = parallel
    [(Embedded, \s -> taska)
	,(Embedded, \s -> taskbf (mapRead prj (sdsFocus (Left 0) (taskListItemValue s))) @? const NoValue)
    ] [] @? res
where
	prj (Value a _)	= Just a
	prj _			= Nothing

    res (Value [(_,v):_] _) = v
    res _                   = NoValue

(>&^) infixl 1 :: (Task a) ((SDSLens () (Maybe a) ()) -> Task b) -> Task a | iTask a & iTask b
(>&^) taska taskbf = feedSideways taska taskbf

feedBidirectionally :: !((SDSLens () (Maybe b) ()) -> Task a) !((SDSLens () (Maybe a) ()) -> Task b)
                    -> Task (a, b) | iTask a & iTask b
feedBidirectionally taskaf taskbf = parallel
	[(Embedded, \s -> taskaf (mapRead prjR (toReadOnly (sdsFocus (Left 1) (taskListItemValue s)))) @ Left)
	,(Embedded, \s -> taskbf (mapRead prjL (toReadOnly (sdsFocus (Left 0) (taskListItemValue s)))) @ Right)
	] [] @? res
where
	prjL (Value (Left a) _)  = Just a
	prjL _                   = Nothing

	prjR (Value (Right a) _) = Just a
	prjR _                   = Nothing

	res (Value [(_,Value (Left a) sa),(_,Value (Right b) sb)] _) = Value (a,b) (sa && sb)
	res _                                                        = NoValue

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
randomChoice list = get randomInt >>- \i -> return (list !! ((abs i) rem (length list)))

//We throw an exception when the share changes to make sure that the right hand side of
//the -||- combinator is not evaluated anymore (because it was created from the 'old' share value)
whileUnchanged :: !(sds () r w) (r -> Task b) -> Task b | iTask r & iTask b & Registrable sds & TC w
whileUnchanged share task
	= 	( (get share >>- \val ->
            try (
					((watch share >>* [OnValue (ifValue ((=!=) val) (\_ -> throw ShareChanged))])
					  -||- (task val @ Just)
					 ) <<@ ApplyLayout (sequenceLayouts [removeSubUIs (SelectByPath [0]), unwrapUI]))
                (\ShareChanged -> (return Nothing) )
          ) <! isJust
        )
	@?	onlyJust

:: ShareChanged = ShareChanged
derive class iTask ShareChanged
instance toString ShareChanged where toString ShareChanged = "Share changed exception"

onlyJust (Value (Just x) s) = Value x s
onlyJust _                  = NoValue

whileUnchangedWith :: !(r r -> Bool) !(sds () r w) (r -> Task b) -> Task b | iTask r & TC w & iTask b & Registrable sds
whileUnchangedWith eq share task
	=  ((get share >>- \val -> (wait (eq val) share <<@ NoUserInterface @ const Nothing) -||- (task val @ Just)) <! isJust)
	@? onlyJust

withSelection :: (Task c) (a -> Task b) (sds () (Maybe a) ()) -> Task b | iTask a & iTask b & iTask c & RWShared sds
withSelection def tfun s = whileUnchanged s (maybe (def @? const NoValue) tfun)

workOnChosenTask :: !(((String, Int) -> String) -> ChoiceOption (String, Int)) ![(String, Task a)] -> Task a | iTask a
workOnChosenTask choiceOption options =
	withShared 0 \sharedChoice -> workOnChosenTaskWithSharedChoice choiceOption sharedChoice options

workOnChosenTaskWithSharedChoice ::
	!(((String, Int) -> String) -> ChoiceOption (String, Int)) !(Shared sds Int) ![(String, Task a)] -> Task a
	| iTask a & RWShared sds
workOnChosenTaskWithSharedChoice choiceOption sharedChoice options =
	editSharedChoiceAs [choiceOption fst] [(label,n) \\ (label,_) <- options & n <- [1..]] snd sharedChoice`
	>&> \choiceIdx ->
		( parallel
			[(Embedded, managementTask choiceIdx): [(Embedded, \_ -> task) \\ (_, task) <- options]]
			[]
			<<@ ApplyLayout
				( layoutSubUIs
					( SelectAND
						(SelectByDepth 1)
						(SelectByAttribute "visible" $ \val -> val =: (JSONBool False))
					)
					(addCSSClass "itasks-hidden")
				)
		)
	-&&-
		(watch choiceIdx <<@ ApplyLayout hideUI)
	@? \taskValue ->
		case taskValue of
			Value (values, Just choiceIdx) _ -> snd $ values !! choiceIdx
			_                                -> NoValue
where
	managementTask choiceIdx taskList = whileUnchanged choiceIdx updateVisible
	where
		updateVisible choice = upd (setVisible choice) (sdsFocus fullTaskListFilter taskList) @? const NoValue
		setVisible choice (_,items) =
			[ (taskId,'DM'.put "visible" (JSONBool (Just idx == choice)) managementAttributes)
			\\ {TaskListItem| taskId, managementAttributes} <- items & idx <- [0..]
			]

	// Increment/decrement indexes to make shared choice index starting at 0.
	sharedChoice` = mapReadWrite (Just o inc, \idx _ -> dec <$> idx) Nothing sharedChoice

appendTopLevelTask :: !TaskAttributes !Bool !(Task a) -> Task TaskId | iTask a
appendTopLevelTask attr evalDirect task = get applicationOptions
	>>- \eo->appendTask (Detached evalDirect attr) (\_->mtune eo task @! ()) topLevelTasks
where
	mtune eo task = if eo.autoLayout (task <<@ ApplyLayout defaultSessionLayout) task

compute :: !String a -> Task a | iTask a
compute s a = Hint s @>> enterInformation [EnterUsing id ed] >>~ \_->return a
where
	ed :: Editor Bool
	ed = fieldComponent UILoader Nothing (\_ _ -> True)

valToMaybe :: (TaskValue a) -> Maybe a
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

withoutValue :: (Maybe b) (TaskValue a) -> Maybe b
withoutValue b NoValue = b
withoutValue _ _       = Nothing

withValue :: (a -> Maybe b) (TaskValue a) -> Maybe b
withValue a2mb (Value tv _) = a2mb tv
withValue _    _            = Nothing

withStable :: (a -> Maybe b) (TaskValue a) -> Maybe b
withStable a2mb (Value tv True) = a2mb tv
withStable _    _               = Nothing

withUnstable :: (a -> Maybe b) (TaskValue a) -> Maybe b
withUnstable a2mb (Value tv False) = a2mb tv
withUnstable _    _                = Nothing

tvHd :: (TaskValue [a]) -> TaskValue a
tvHd (Value [x] s) = Value x s
tvHd _             = NoValue

tvFst :: (TaskValue (a,b)) -> TaskValue a
tvFst tv = fmap fst tv

tvSnd :: (TaskValue (a,b)) -> TaskValue b
tvSnd tv = fmap snd tv

tvFromMaybe :: (TaskValue (Maybe a)) -> TaskValue a
tvFromMaybe (Value (Just a) s) = Value a s
tvFromMaybe _                  = NoValue

tvToMaybe :: (TaskValue a) -> TaskValue (Maybe a)
tvToMaybe (Value a s) = Value (Just a) s
tvToMaybe NoValue     = Value Nothing False
