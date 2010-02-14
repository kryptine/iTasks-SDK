implementation module CommonCombinators
/**
* This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdBool, StdList, StdTuple, StdGeneric, StdMisc

from StdFunc	import id, const
from TSt		import :: Task(..), :: TaskDescription(..), :: TSt{..}, :: TaskInfo{..}, :: StaticInfo{..}, :: Workflow, :: ChangeLifeTime, :: Options, :: HTTPRequest, :: Config
from TSt		import applyTask, mkSequenceTask, mkParallelTask
from Types		import :: ProcessId, :: DynamicId, :: TaskId, :: TaskPriority(..), :: User(..)
from Store		import :: Store
from SessionDB	import :: Session
from TaskTree	import :: TaskTree
from CommonDomain	import :: Note

import SystemTasks, InteractionTasks, UserDBTasks, CoreCombinators, TuningCombinators, LiftingCombinators
import Util, Either
import GenVisualize, GenUpdate

derive gPrint Either
derive gParse Either

derive bimap	Maybe

//Task composition
(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb  
=	parallel "-||-" (\list -> length list >= 1) (\[x:_] -> case x of (Left a) = a; (Right b) = b) (abort "-||- both parts finished??")
			[taska >>= \a -> return (Left a)
			,taskb >>= \b -> return (Right b)
			]
			
(-&&-) infixr 4 ::  !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb
=	parallel "-&&-" (\_ -> False) (abort "-&&- predicate became true??") (\[Left a, Right b] -> (a,b))
			[taska >>= \a -> return (Left a)
			,taskb >>= \b -> return (Right b)
			]

anyTask	:: ![Task a] -> Task a | iTask a
anyTask []		= getDefaultValue
anyTask tasks	= parallel "any" (\list -> length list >= 1) hd (abort "anyTask all parts finished??") tasks

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks = parallel "all" (\_ -> False) (abort "allTasks predicate became true") id tasks

eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb 
=	parallel "eitherTask" (\list -> length list > 0) hd (abort "eitherTask all parts finished??")
			[ (taska >>= \a -> return (Left a)) <<@ "Left"
			, (taskb >>= \b -> return (Right b)) <<@ "Right"
			]

(||-) infixr 3		:: !(Task a) !(Task b)						-> Task b				| iTask a & iTask b
(||-) taska taskb
	= parallel "||-" rightDone takeRight takeRight
		[ (taska >>= \a -> return (Left a)) <<@ "Left"
		, (taskb >>= \b -> return (Right b)) <<@ "Right"
		]
where
	rightDone [Right x] = True
	rightDone _			= False
	
	takeRight l			= hd [ x \\ (Right x) <- l] 

(-||) infixl 3		:: !(Task a) !(Task b)						-> Task a				| iTask a & iTask b
(-||) taska taskb
	= parallel "-||" leftDone takeLeft takeLeft
		[ (taska >>= \a -> return (Left a)) <<@ "Left"
		, (taskb >>= \b -> return (Right b)) <<@ "Right"
		]
where
	leftDone [Left x] 	= True
	leftDone _			= False
	
	takeLeft l			= hd [ x \\ (Left x) <- l] 

(>>?) infixl 1 	:: !(Task (Maybe a)) !(a -> Task (Maybe b)) -> Task (Maybe b) | iTask a & iTask b
(>>?) t1 t2 
= 				t1 
	>>= \r1 -> 	case r1 of 
					Nothing 	-> return Nothing
					Just r`1 	-> t2 r`1

(-&?&-) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iTask a & iTask b
(-&?&-) t1 t2 
= 	parallel "maybeTask" noNothing combineResult combineResult
			[(t1 >>= \tres -> return (Left tres)) <<@ "Left"
			,(t2 >>= \tres -> return (Right tres)) <<@ "Right"
			]
where
	noNothing []					= False
	noNothing [Left  Nothing:xs]	= True
	noNothing [Right Nothing:xs]	= True
	noNothing [x:xs]				= noNothing xs	

	combineResult	[Left (Just r1),Right (Just r2)]	= Just (r1,r2)
	combineResult	_									= Nothing

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
	(@:) nuserId (label,task) = assign nuserId NormalPriority Nothing (task <<@ label)

instance @: User
where
	(@:) :: !User !(LabeledTask a) -> Task a | iTask a
	(@:) user task = user.User.userId @: task

instance @: String
where
	(@:) :: String !(LabeledTask a) -> Task a | iTask a
	(@:) name task
		 = getUserByName name
		 >>= \user -> user.User.userId @: task

assignByName :: !String !String !TaskPriority !(Maybe Timestamp) (Task a) -> Task a | iTask a
assignByName name subject priority deadline task
	=	getUserByName name
	>>= \user ->
		assign user.User.userId priority deadline (task <<@ subject)
// ******************************************************************************************************
