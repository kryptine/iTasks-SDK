implementation module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass

from StdFunc	import id, const
from TSt		import :: Task(..), :: TSt{..}, :: IWorld(..), :: TaskInfo{..}, :: StaticInfo{..}, :: Workflow, :: ChangeLifeTime,:: HTTPRequest, :: Config
from TSt		import applyTask, mkSequenceTask, mkParallelTask, mkInstantTask
from Types		import :: ProcessId, :: TaskId, :: TaskPriority(..), :: User(..)
from Store		import :: Store
from SessionDB	import :: Session
from TaskTree	import :: TaskTree, :: TaskParallelType{..}
from CommonDomain	import :: Note

import TaskTree
import SystemTasks, InteractionTasks, UserDBTasks, CoreCombinators, TuningCombinators, LiftingCombinators, ExceptionCombinators
import Util, Either
import GenVisualize, GenUpdate

derive gVisualize	GAction, GOnlyAction, GroupedBehaviour
derive gUpdate		GAction, GOnlyAction, GroupedBehaviour
derive gVerify		GAction, GOnlyAction, GroupedBehaviour

derive JSONEncode	GAction, GOnlyAction, GroupedBehaviour
derive JSONDecode	GAction, GOnlyAction, GroupedBehaviour

derive bimap Maybe, (,)

// Collection of references to all editor states of an MDI application
:: MDIAppState editorState :== [DBId editorState]

transform :: !(a -> b) !a -> Task b | iTask b
transform f x = mkInstantTask "Value transformation" "Value transformation with a custom function" (\tst -> (TaskFinished (f x),tst))

(@:) infix 3 :: !User !(Task a) -> Task a | iTask a
(@:) user task = assign user task

(>>?) infixl 1 	:: !(Task (Maybe a)) !(a -> Task (Maybe b)) -> Task (Maybe b) | iTask a & iTask b
(>>?) t1 t2 
= 	t1 
	>>= \r1 -> 	case r1 of 
					Nothing 	-> return Nothing
					Just r`1 	-> t2 r`1
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
	andfunc :: ((Either a b),Int) (Maybe a, Maybe b) -> ((Maybe a, Maybe b),PAction (Task (Either a b)) tag)
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

orProc :: !(Task a) !(Task a) !TaskParallelType -> Task a | iTask a
orProc taska taskb type = parallel type "-|@|-" "Done if either subtask is finished." orfunc hd [] [taska,taskb] 
where
	orfunc (val,_) [] = ([val],Stop)
	orfunc (val,_) _  = abort "Multiple results in -|@|-"

andProc :: !(Task a) !(Task b) !TaskParallelType -> Task (a,b) | iTask a & iTask b
andProc taska taskb type = parallel type "AndProc" "Done if both subtasks are finished." andfunc parseresult (Nothing,Nothing) [taska >>= \a -> return (Left a),taskb >>= \b -> return (Right b)]
where
	andfunc :: ((Either a b),Int) (Maybe a, Maybe b) -> ((Maybe a, Maybe b),PAction (Task (Either a b)) tag)
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

stop :: Task Void
stop = return Void

randomChoice :: ![a] -> Task a | iTask a
randomChoice [] = throw "Cannot make a choice from an empty list"
randomChoice list = getRandomInt >>= \i -> return (list !! (i rem (length list)))

repeatTask :: !(a -> Task a) !(a -> Bool) a -> Task a | iTask a
repeatTask task pred a =
	task a >>= \na -> if (pred na) (return na) (repeatTask task pred na)

(<|) infixl 6 :: !(Task a) !(a -> (Bool, [HtmlTag])) -> Task a | iTask a
(<|) taska pred 
		=			taska
		>>= \r -> 	case pred r of
						(True,_) -> return r
						(False,msg) -> showStickyMessage "Feedback" msg r -||- (taska <| pred)					

dynamicGroup :: ![Task GAction] -> Task Void
dynamicGroup initTasks = dynamicGroupA initTasks emptyGActionL

dynamicGroupA :: ![Task GAction] ![GroupAction GAction Void s] -> Task Void | iTask s
dynamicGroupA initTasks gActions =  group "dynamicGroup" "A simple group with dynamically added tasks" procfun id Void initTasks gActions
where
	procfun (action,_) _ = case action of
		GStop			= (Void, Stop)
		GContinue		= (Void, Continue)
		GExtend tasks	= (Void, Extend tasks)
		GFocus tag		= (Void, Focus (Tag tag))
		
dynamicGroupAOnly :: ![Task Void] ![GroupAction GOnlyAction Void s] -> Task Void | iTask s
dynamicGroupAOnly initTasks gActions = group "dynamicGroup" "A simple group with dynamically added tasks" procfun id Void (changeTasksType initTasks) gActions
where
	procfun (action,_) _ = case action of
		GOStop			= (Void, Stop)
		GOContinue		= (Void, Continue)
		GOExtend tasks	= (Void, Extend (changeTasksType tasks))
		GOFocus tag		= (Void, Focus (Tag tag))
	changeTasksType tasks = map (\t -> (t >>| return GOContinue) <<@ getGroupedBehaviour t) tasks
	getGroupedBehaviour (Task _ {GroupedProperties | groupedBehaviour} _ _) = groupedBehaviour
		
mdiApplication :: !globalState !((DBId globalState) (MDITasks editorState iterationState) -> [GroupAction GAction Void globalState]) -> Task Void | iTask, SharedVariable globalState & iTask, SharedVariable editorState & iTask iterationState
mdiApplication initAppState gActions =
				createDB initAppState
	>>= \aid.	createDB initMDIState
	>>= \sid.	dynamicGroupA [] (gActions aid (globalTasks sid))
	>>|			deleteDB aid
	>>|			deleteDB sid
	>>|			return Void
where
	//initMDIState :: [DBId editorState] | iTask editorState
	initMDIState = []

	//globalTasks :: !(DBId (MDIAppState editorState)) -> MDITasks editorState iterationState | iTask, SharedVariable editorState & iTask iterationState
	globalTasks sid =
		{ createEditor		= createEditor
		, iterateEditors	= iterateEditors
		, existsEditor		= existsEditor
		}
	where
		//createEditor :: !editorState ((DBId editorState) -> Task Void) -> Task GAction | iTask, SharedVariable editorState
		createEditor initState editorTask =
						createDB initState
			>>= \esid.	modifyDB sid (\editors -> [esid:editors])
			>>|			editorTask esid
			>>|			deleteDB esid
			>>|			modifyDB sid (\editors -> filter (\id -> id <> esid) editors)
			>>|			return GContinue
			
		//iterateEditors	:: !iterationState !(iterationState (DBId editorState) -> Task iterationState) -> Task iterationState | iTask, SharedVariable editorState & iTask iterationState
		iterateEditors v f =
							readDB sid
			>>= \editors.	iterate v editors
		where
			iterate v [] = return v
			iterate v [editor:editors] =
						f v editor
				>>= \v.	iterate v editors
			
		//existsEditor :: !(editorState -> Bool) -> Task (Maybe (DBId editorState)) | iTask, SharedVariable editorState
		existsEditor pred =
							readDB sid
			>>= \editors.	check editors
		where
			check [] = return Nothing
			check [eid:eids] =
								readDB eid
				>>= \editor.	if (pred editor)
									(return (Just eid))
									(check eids)

//Utility functions
emptyGActionL :: [GroupAction a b Void]
emptyGActionL = []

sortByIndex :: ![(Int,a)] -> [a]
sortByIndex [] = []
sortByIndex [(i,v):ps] = sortByIndex [(is,vs) \\ (is,vs) <- ps | is < i] ++ [v] ++ sortByIndex [(is,vs) \\ (is,vs) <- ps | is > i]
