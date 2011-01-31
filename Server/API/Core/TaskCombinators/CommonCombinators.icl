implementation module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass
import Util, Either, TSt, GenVisualize, GenUpdate, Map
from StdFunc	import id, const, o
from Types		import :: ProcessId, :: User(..), :: Note(..)
from Store		import :: Store
from SessionDB	import :: Session
from TaskTree	import :: TaskTree, :: TaskParallelType{..}
import StoreTasks, CoreCombinators, ExceptionCombinators, TuningCombinators, SystemTasks, InteractionTasks

derive class iTask GAction, GOnlyAction

// use string instances of generic function for Tag values 
gVisualize{|Tag|} val vst = gVisualize{|*|} (toStr val) vst
where
	toStr Nothing			= Nothing
	toStr (Just (Tag t))	= Just (toString t)
	
gUpdate{|Tag|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (Tag "", {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Tag|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (Tag update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om}) 
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|Tag|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])}) 
gVerify{|Tag|} mbTag vst = gVerify{|*|} (toStr mbTag) vst
where
	toStr Nothing			= Nothing
	toStr (Just (Tag t))	= Just (toString t)
JSONEncode{|Tag|} (Tag t) = JSONEncode{|*|} (toString t)
JSONDecode{|Tag|} nodes = case nodes of
	[JSONString str]	= (Just (Tag str), [])
	_					= (Nothing, nodes)
gEq{|Tag|} (Tag x) (Tag y) = (toString x) == (toString y)

derive bimap Maybe, (,)

// Collection of references to all editor states of an MDI application
:: MDIAppState editorState :== [Shared editorState]

transform :: !(a -> b) !a -> Task b | iTask b
transform f x = mkInstantTask ("Value transformation", "Value transformation with a custom function") (\tst -> (TaskFinished (f x),tst))

(@:) infix 3 :: !User !(Task a) -> Task a | iTask a
(@:) user task = assign user task

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

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb = group ("-||-", "Done when either subtask is finished.") orfunc hd [] [taska,taskb] [] undef
where
	orfunc (val,_) [] = ([val],Stop)
	orfunc (val,_) _  = abort "Multiple results in OR"

(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= group ("||-", "Done when the second subtask is finished.") rorfunc hd [] [(taska >>= \a -> return (Left a)) <<@ taska.groupedProperties.GroupedProperties.groupedBehaviour, (taskb >>= \b -> return (Right b)) <<@ taskb.groupedProperties.GroupedProperties.groupedBehaviour] [] undef
where
	rorfunc (Right val,_) [] = ([val],Stop)
	rorfunc (Left val, _) [] = ([],Continue)
	rorfunc _ _				 = abort "Illegal result in ||-"

(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= group ("||-", "Done when the first subtask is finished") lorfunc hd [] [(taska >>= \a -> return (Left a)) <<@ taska.groupedProperties.GroupedProperties.groupedBehaviour,(taskb >>= \b -> return (Right b)) <<@ taskb.groupedProperties.GroupedProperties.groupedBehaviour] [] undef
where
	lorfunc (Right val,_) [] = ([],Continue)
	lorfunc (Left val, _) [] = ([val],Stop)
	lorfunc _ _				 = abort "Illegal result in -||"					

(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb = group ("-&&-", "Done when both subtasks are finished") andfunc parseresult (Nothing,Nothing) [(taska >>= \a -> return (Left a)) <<@ taska.groupedProperties.GroupedProperties.groupedBehaviour, (taskb >>= \b -> return (Right b)) <<@ taskb.groupedProperties.GroupedProperties.groupedBehaviour] [] undef
where
	andfunc :: ((Either a b),Int) (Maybe a, Maybe b) -> ((Maybe a, Maybe b),PAction (Task (Either a b)))
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
	
	parseresult (Just a,Just b)	= (a,b)
	parseresult _				= abort "AND not finished"

(-&?&-) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iTask a & iTask b
(-&?&-) taska taskb 
= group ("-&?&-", "Done when both subtasks are finished. Yields only a result of both subtasks have a result") mbandfunc parsefunc (Nothing,Nothing) [(taska >>= \a -> return (Left a)) <<@ taska.groupedProperties.GroupedProperties.groupedBehaviour, (taskb >>= \b -> return (Right b)) <<@ taskb.groupedProperties.GroupedProperties.groupedBehaviour] [] undef
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
anyTask tasks 	= group ("any", "Done when any subtask is finished") anyfunc hd [] tasks [] undef
where
	anyfunc (val,_) [] = ([val],Stop)
	anyfunc (val,_) _  = abort "Multiple results in ANY"

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks = group ("all", "Done when all subtasks are finished") (allfunc(length tasks)) sortByIndex [] tasks [] undef
where
	allfunc tlen (val,idx) st 
		# st = st ++ [(idx,val)]
		| length st == tlen = (st,Stop)
		| otherwise = (st,Continue)
			
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = group ("either", "Done when either subtask is finished") eitherfunc hd [] [(taska >>= \a -> return (Left a)) <<@ taska.groupedProperties.GroupedProperties.groupedBehaviour, (taskb >>= \b -> return (Right b)) <<@ taskb.groupedProperties.GroupedProperties.groupedBehaviour] [] undef
where
	eitherfunc (val,idx) [] = ([val],Stop)
	eitherfunc (val,idx) _  = abort "Multiple results in Either"

orProc :: !(Task a) !(Task a) !TaskParallelType -> Task a | iTask a
orProc taska taskb type = parallel type ("-|@|-", "Done if either subtask is finished.") orfunc hd [] [taska,taskb] 
where
	orfunc (val,_) [] = ([val],Stop)
	orfunc (val,_) _  = abort "Multiple results in -|@|-"

andProc :: !(Task a) !(Task b) !TaskParallelType -> Task (a,b) | iTask a & iTask b
andProc taska taskb type = parallel type ("AndProc", "Done if both subtasks are finished.") andfunc parseresult (Nothing,Nothing) [(taska >>= \a -> return (Left a)) <<@ taska.groupedProperties.GroupedProperties.groupedBehaviour, (taskb >>= \b -> return (Right b)) <<@ taskb.groupedProperties.GroupedProperties.groupedBehaviour]
where
	andfunc :: ((Either a b),Int) (Maybe a, Maybe b) -> ((Maybe a, Maybe b),PAction (Task (Either a b)))
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
anyProc tasks type = parallel type ("any", "Done when any subtask is finished.") anyfunc hd [] tasks
where
	anyfunc (val,_) [] = ([val],Stop)
	anyfunc (val,_) _  = abort "Multiple results in ANY"

allProc :: ![Task a] !TaskParallelType -> Task [a] | iTask a
allProc tasks type = parallel type ("all", "Done when all subtasks are finished.") (allfunc (length tasks)) sortByIndex [] tasks 
where
	allfunc tlen (val,idx) st 
		# st = st ++ [(idx,val)]
		| length st == tlen = (st,Stop)
		| otherwise = (st,Continue)

stop :: Task Void
stop = return Void

randomChoice :: ![a] -> Task a | iTask a
randomChoice [] = throw "Cannot make a choice from an empty list"
randomChoice list = getRandomInt >>= \i -> return (list !! ((abs i) rem (length list)))

repeatTask :: !(a -> Task a) !(a -> Bool) a -> Task a | iTask a
repeatTask task pred a =
	task a >>= \na -> if (pred na) (return na) (repeatTask task pred na)

(<|) infixl 6 :: !(Task a) !(a -> (Bool, [HtmlTag])) -> Task a | iTask a
(<|) taska pred 
		=			taska
		>>= \r -> 	case pred r of
						(True,_) -> return r
						(False,msg) -> showStickyMessage ("Feedback",msg) r -||- (taska <| pred)					

dynamicGroup :: ![Task GAction] -> Task Void
dynamicGroup initTasks = dynamicGroupA initTasks [] (\_ -> GStop)

dynamicGroupA :: ![Task GAction] ![GroupAction Void] !(GroupActionGenFunc GAction) -> Task Void
dynamicGroupA initTasks gActions genFunc = group ("dynamicGroup", "A simple group with dynamically added tasks") procfun id Void initTasks gActions genFunc
where
	procfun (action,_) _ = case action of
		GStop			= (Void, Stop)
		GContinue		= (Void, Continue)
		GExtend tasks	= (Void, Extend tasks)
		GFocus tag		= (Void, Focus tag)
		
dynamicGroupAOnly :: ![Task Void] ![GroupAction Void] !(GroupActionGenFunc GOnlyAction) -> Task Void
dynamicGroupAOnly initTasks gActions genFunc = group ("dynamicGroup", "A simple group with dynamically added tasks") procfun id Void (changeTasksType initTasks) gActions genFunc
where
	procfun (action,_) _ = case action of
		GOStop			= (Void, Stop)
		GOExtend tasks	= (Void, Extend (changeTasksType tasks))
		GOFocus tag		= (Void, Focus tag)
	changeTasksType tasks = map (\t -> (t >>| return (GOExtend [])) <<@ t.groupedProperties.GroupedProperties.groupedBehaviour) tasks
		
mdiApplication :: !globalState ![GroupAction Void] !((Shared (globalState,EditorCollection editorState)) (MDITasks editorState iterationState) -> (GroupActionGenFunc GAction)) !((globalState,EditorCollection editorState) -> Menus) -> Task Void | iTask globalState & iTask editorState & iTask iterationState
mdiApplication initAppState gActions gActionsGenFunc menuGenFunc =
				createDB (initAppState,newMap)
	>>= \ref.	createDB 0
	>>= \idref.	dynamicGroupA [] gActions (gActionsGenFunc ref (globalTasks ref idref)) <<@ DynamicMenus ref menuGenFunc
	>>|			deleteDB ref
	>>|			deleteDB idref
	>>|			stop
where
	globalTasks ref idref =
		{ createEditor		= createEditor
		, iterateEditors	= iterateEditors
		, existsEditor		= existsEditor
		}
	where
		createEditor initState editorTask =
						updateDB idref inc
			>>= \eid.	updateDB ref (putInEditorStates eid initState)
			>>|			editorTask eid (sharedForEditor eid)
			>>|			updateDB ref (updateEditorStates (del eid))
			>>|			stop
			
		iterateEditors initAcc f =
						readDB ref
			>>= \st.	iterate initAcc (map fst (toList (snd st))) f ref
		where
			iterate acc [] _ _= return acc
			iterate acc [eid:eids] f ref =
							f acc (sharedForEditor eid)
				>>= \acc.	iterate acc eids f ref
			
		existsEditor pred =
						readDB ref
			>>= \st.	return (check (toList (snd st)))
		where
			check [] = Nothing
			check [(eid,editor):ests]
				| pred editor	= Just eid
				| otherwise		= check ests
				
		sharedForEditor eid = mapShared (fromJust o (get eid) o snd, putInEditorStates eid) ref		
		putInEditorStates eid est st = updateEditorStates (put eid est) st
		updateEditorStates f st = app2 (id,f) st

//Utility functions
sortByIndex :: ![(Int,a)] -> [a]
sortByIndex [] = []
sortByIndex [(i,v):ps] = sortByIndex [(is,vs) \\ (is,vs) <- ps | is < i] ++ [v] ++ sortByIndex [(is,vs) \\ (is,vs) <- ps | is > i]