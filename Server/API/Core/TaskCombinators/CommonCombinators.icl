implementation module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass, GenRecord
import Util, Either, TSt, GenVisualize, GenUpdate, Map
from StdFunc	import id, const, o
from Types		import :: ProcessId, :: User(..), :: Note(..)
from Store		import :: Store
from SessionDB	import :: Session
from TaskTree	import :: TaskTree
from Shared		import mapShared, :: SymmetricShared
import CoreCombinators, ExceptionCombinators, TuningCombinators, SystemTasks, InteractionTasks, SharedTasks, ProcessDBTasks

derive class iTask GAction, GOnlyAction

// use string instances of generic function for Tag values 
gVisualize{|Tag|} val vst = gVisualize{|*|} (toStr val) vst
where
	toStr Nothing			= Nothing
	toStr (Just (Tag t))	= Just (toString t)

gUpdate{|Tag|} mode ust = basicUpdateSimple mode Tag (Tag "") ust
gDefaultMask{|Tag|} _ = [Touched []]

gVerify{|Tag|} _ vst = simpleVerify "Enter a tag" vst
	
JSONEncode{|Tag|} (Tag t) = JSONEncode{|*|} (toString t)
JSONDecode{|Tag|} nodes = case nodes of
	[JSONString str]	= (Just (Tag str), [])
	_					= (Nothing, nodes)
gEq{|Tag|} (Tag x) (Tag y) = (toString x) == (toString y)

derive bimap Maybe, (,)

transform :: !(a -> b) !a -> Task b | iTask b
transform f x = mkInstantTask ("Value transformation", "Value transformation with a custom function") (\tst -> (TaskFinished (f x),tst))

/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
assign :: !ManagerProperties !ActionMenu !(Task a) -> Task a | iTask a
assign props actionMenu task = parallel ("Assign","Manage a task assigned to another user.") (Nothing,\_ r _ -> (Just r,Just StopParallel), \_ (Just r) -> r) [InBodyPTask processControl] [DetachedTask props actionMenu task]
where
	processControl shared =
			updateSharedInformationA (taskTitle task,"Waiting for " +++ taskTitle task) (toView,fromView) [] shared
		>>|	return undef
		
	toView (_,[_,{processProperties=p=:Just {progress,systemProperties=s=:{issuedAt,firstEvent,latestEvent},managerProperties=m=:{worker}}}])=
		{ mapRecord m
		& assignedTo	= worker
		, progress		= formatProgress progress
		, issuedAt		= Display issuedAt
		, firstWorkedOn	= Display firstEvent
		, lastWorkedOn	= Display latestEvent
		}
	where
		formatProgress TPActive		= coloredLabel "Active" "green"
		formatProgress TPStuck		= coloredLabel "Stuck" "purple"
		formatProgress TPWaiting	= coloredLabel "Waiting" "blue"
		formatProgress TPReject		= coloredLabel "Reject" "red"
	
		coloredLabel label color = toHtmlDisplay [SpanTag [StyleAttr ("color:" +++ color)] [Text label]]
		
	fromView view=:{ProcessControlView|assignedTo} _
		= [(1,{mapRecord view & worker = assignedTo})]
	
:: ProcessControlView =	{ assignedTo	:: !User
						, priority		:: !TaskPriority
						, status		:: !RunningTaskStatus
						, progress		:: !HtmlDisplay
						, issuedAt		:: !Display Timestamp
						, firstWorkedOn	:: !Display (Maybe Timestamp)
						, lastWorkedOn	:: !Display (Maybe Timestamp)
						, deadline		:: !Maybe DateTime
						}
derive class iTask ProcessControlView
derive class GenRecord ProcessControlView, ManagerProperties, TaskPriority, RunningTaskStatus

(@:) infix 3 :: !User !(Task a) -> Task a | iTask a
(@:) user task = assign {initManagerProperties & worker = user} noMenu task

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
(-||-) taska taskb = parallel ("-||-", "Done when either subtask is finished.") ([],orfunc,\_ l -> hd l) [] [InBodyTask taska,InBodyTask taskb]
where
	orfunc _ val [] = ([val],Just StopParallel)
	orfunc _ val _  = abort "Multiple results in OR"

(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= parallel ("||-", "Done when the second subtask is finished.") ([],rorfunc,\_ l -> hd l) [] [InBodyTask (taska >>= \a -> return (Left a)),InBodyTask (taskb >>= \b -> return (Right b))]
where
	rorfunc _ (Right val) []	= ([val],Just StopParallel)
	rorfunc _ (Left val) []		= ([],Nothing)
	rorfunc _ _ _				= abort "Illegal result in ||-"

(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel ("-||", "Done when the first subtask is finished") ([],lorfunc,\_ l -> hd l) [] [InBodyTask (taska >>= \a -> return (Left a)),InBodyTask (taskb >>= \b -> return (Right b))]
where
	lorfunc _ (Right val) []	= ([],Nothing)
	lorfunc _ (Left val) []		= ([val],Just StopParallel)
	lorfunc _ _ _				= abort "Illegal result in -||"					

(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb = parallel ("-&&-", "Done when both subtasks are finished") ((Nothing,Nothing),andfunc,parseresult) [] [InBodyTask (taska >>= \a -> return (Left a)),InBodyTask (taskb >>= \b -> return (Right b))]
where
	andfunc :: !Int !(Either a b) (Maybe a, Maybe b) -> ((Maybe a, Maybe b),Maybe (Control (Either a b) (Maybe a,Maybe b)))
	andfunc _ val (left,right) = case val of
		(Left a)
			# state = (Just a,right)
			= case state of
				(Just l, Just r) = (state,Just StopParallel)
				_				 = (state,Nothing)
		(Right b)
			# state = (left,Just b)
			= case state of
				(Just l, Just r) = (state,Just StopParallel)
				_				 = (state,Nothing)		
	
	parseresult _ (Just a,Just b)	= (a,b)
	parseresult _ _					= abort "AND not finished"

(-&?&-) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iTask a & iTask b
(-&?&-) taska taskb 
= parallel ("-&?&-", "Done when both subtasks are finished. Yields only a result of both subtasks have a result") ((Nothing,Nothing),mbandfunc,parsefunc) [] [InBodyTask (taska >>= \a -> return (Left a)),InBodyTask (taskb >>= \b -> return (Right b))]
where
	mbandfunc _ val (left,right) = case val of
		Left v
			# state = (Just v,right)
			= case state of
				(Just a, Just b) = (state,Just StopParallel)
				_				 = (state,Nothing)				
		Right v
			# state = (left,Just v)
			= case state of
				(Just a, Just b) = (state,Just StopParallel)
				_				 = (state,Nothing)
				
	parsefunc _ (Just (Just a), Just (Just b))	= Just (a,b)
	parsefunc _ _								= Nothing
	
oldParallel :: !d !(ValueMerger taskResult pState pResult) ![TaskContainer taskResult] -> Task pResult | iTask taskResult & iTask pState & iTask pResult & descr d
oldParallel d valueMerger initTasks = parallel d valueMerger [InBodyPTask overviewControl] initTasks
where
	overviewControl shared =
			updateSharedInformationA "" (toView,fromView) [] shared
		>>|	return undef
	
	toView (_,infos) = Table (map toView` (filter isProc infos))
	toView` {index,processProperties=p=:Just {managerProperties=m=:{worker},taskProperties=t=:{taskDescription}}} =
		{ ProcessOverviewView
		| index			= Hidden index
		, subject		= Display taskDescription.TaskDescription.title
		, assignedTo	= worker
		}
		
	isProc {processProperties} = isJust processProperties
	
	fromView (Table viewList) (_,infos) = map fromView` viewList
	where
		fromView` ({ProcessOverviewView|index=i=:Hidden index,assignedTo})
			= case filter (\info -> info.ParallelTaskInfo.index == index) infos of
				[{processProperties=p=:Just {managerProperties}}] = (index,{managerProperties & worker = assignedTo})
				_ = abort "old parallel: no manager properties"
			
:: ProcessOverviewView =	{ index			:: !Hidden TaskIndex
							, subject		:: !Display String
							, assignedTo	:: !User
							}
derive class iTask ProcessOverviewView

anyTask :: ![Task a] -> Task a | iTask a
anyTask [] 		= getDefaultValue
anyTask tasks 	= parallel ("any", "Done when any subtask is finished") ([],anyfunc,\_ l -> hd l) [] (map InBodyTask tasks)
where
	anyfunc _ val [] = ([val],Just StopParallel)
	anyfunc _ val _  = abort "Multiple results in ANY"

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks = parallel ("all", "Done when all subtasks are finished") ([],allfunc (length tasks),\_ l -> sortByIndex l) [] (map InBodyTask tasks)
where
	allfunc tlen idx val st 
		# st = st ++ [(idx,val)]
		| length st == tlen = (st,Just StopParallel)
		| otherwise = (st,Nothing)
			
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = parallel ("either", "Done when either subtask is finished") ([],eitherfunc,\_ l -> hd l) [] [InBodyTask (taska >>= \a -> return (Left a)),InBodyTask (taskb >>= \b -> return (Right b))]
where
	eitherfunc _ val [] = ([val],Just StopParallel)
	eitherfunc _ val _  = abort "Multiple results in Either"

orProc :: !(ProcTask a) !(ProcTask a) -> Task a | iTask a
orProc taska taskb = oldParallel ("-|@|-", "Done if either subtask is finished.") (Nothing,orfunc,\_ (Just r) -> r) [mkDetachedTask taska,mkDetachedTask taskb]
where
	orfunc _ val Nothing	= (Just val,Just StopParallel)
	orfunc _ val _ 			= abort "Multiple results in -|@|-"

andProc :: !(ProcTask a) !(ProcTask b) -> Task (a,b) | iTask a & iTask b
andProc (taska,propa,menua) (taskb,propb,menub) = oldParallel ("AndProc", "Done if both subtasks are finished.") ((Nothing,Nothing),andfunc,parseresult) [DetachedTask propa menua (taska >>= \a -> return (Left a)),DetachedTask propb menub (taskb >>= \b -> return (Right b))]
where
	andfunc :: !Int !(Either a b) !(!Maybe a,!Maybe b) -> (!(!Maybe a,!Maybe b),!Maybe (Control (Either a b) (!Maybe a,!Maybe b)))
	andfunc _ val (left,right)
	= case val of
		(Left a)
			# state = (Just a,right)
			= case state of
				(Just l, Just r) = (state,Just StopParallel)
				_				 = (state,Nothing)
		(Right b)
			# state = (left,Just b)
			= case state of
				(Just l, Just r) = (state,Just StopParallel)
				_				 = (state,Nothing)		
	
	parseresult _ (Just a,Just b)	= (a,b)
	parseresult _ _	 				= abort "AND not finished"

anyProc :: ![ProcTask a] -> Task a | iTask a
anyProc [] 	  = getDefaultValue
anyProc tasks = oldParallel ("any", "Done when any subtask is finished.") (Nothing,anyfunc,\_ (Just r) -> r) (map mkDetachedTask tasks)
where
	anyfunc _ val Nothing	= (Just val,Just StopParallel)
	anyfunc _ val _			= abort "Multiple results in ANY"

allProc :: ![ProcTask a] -> Task [a] | iTask a
allProc tasks = oldParallel ("all", "Done when all subtasks are finished.") ([],allfunc (length tasks),\_ r -> sortByIndex r) (map mkDetachedTask tasks) 
where
	allfunc tlen idx val st 
		# st = st ++ [(idx,val)]
		| length st == tlen = (st,Just StopParallel)
		| otherwise = (st,Nothing)
		
mkDetachedTask :: !(!Task a,!ManagerProperties,!ActionMenu) -> TaskContainer a
mkDetachedTask (task,prop,menu) = DetachedTask prop menu task

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

/*dynamicGroup :: ![Task GAction] -> Task Void
dynamicGroup initTasks = dynamicGroupA initTasks [] (\_ -> GStop)

dynamicGroupA :: ![Task GAction] ![GroupAction Void] !(GroupActionGenFunc GAction) -> Task Void
dynamicGroupA initTasks gActions genFunc = parallel ("dynamicGroup", "A simple parallel with dynamically added tasks") procfun id Void initTasks gActions genFunc
where
	procfun (action,_) _ = case action of
		GStop			= (Void, Just StopParallel)
		GContinue		= (Void, Nothing)
		GExtend tasks	= (Void, Just (Extend tasks))
		GFocus tag		= (Void, Just (Focus tag))
		
dynamicGroupAOnly :: ![Task Void] ![GroupAction Void] !(GroupActionGenFunc GOnlyAction) -> Task Void
dynamicGroupAOnly initTasks gActions genFunc = parallel ("dynamicGroup", "A simple parallel with dynamically added tasks") procfun id Void (changeTasksType initTasks) gActions genFunc
where
	procfun (action,_) _ = case action of
		GOStop			= (Void, Just StopParallel)
		GOExtend tasks	= (Void, Just (Extend (changeTasksType tasks)))
		GOFocus tag		= (Void, Just (Focus tag))
	changeTasksType tasks = map (\t -> (t >>| return (GOExtend []))) tasks
		
mdiApplication :: !globalState ![GroupAction Void] !((SymmetricShared (globalState,EditorCollection editorState)) (MDITasks editorState iterationState) -> (GroupActionGenFunc GAction)) !((globalState,EditorCollection editorState) -> MenuDefinition) -> Task Void | iTask globalState & iTask editorState & iTask iterationState
mdiApplication initAppState gActions gActionsGenFunc menuGenFunc =
				createSharedStore (initAppState,newMap)
	>>= \ref.	createSharedStore 0
	>>= \idref.	dynamicGroupA [] gActions (gActionsGenFunc ref (globalTasks ref idref)) <<@ DynamicMenus ref menuGenFunc
	>>|			stop
where
	globalTasks ref idref =
		{ createEditor		= createEditor
		, iterateEditors	= iterateEditors
		, existsEditor		= existsEditor
		}
	where
		createEditor initState editorTask =
						updateShared inc idref
			>>= \eid.	updateShared (putInEditorStates eid initState) ref
			>>|			editorTask eid (sharedForEditor eid)
			>>|			updateShared (updateEditorStates (del eid)) ref
			>>|			stop
			
		iterateEditors initAcc f =
						readShared ref
			>>= \st.	iterate initAcc (map fst (toList (snd st))) f ref
		where
			iterate acc [] _ _= return acc
			iterate acc [eid:eids] f ref =
							f acc (sharedForEditor eid)
				>>= \acc.	iterate acc eids f ref
			
		existsEditor pred =
						readShared ref
			>>= \st.	return (check (toList (snd st)))
		where
			check [] = Nothing
			check [(eid,editor):ests]
				| pred editor	= Just eid
				| otherwise		= check ests
				
		sharedForEditor eid = mapShared (fromJust o (get eid) o snd, putInEditorStates eid) ref		
		putInEditorStates eid est st = updateEditorStates (put eid est) st
		updateEditorStates f st = appSnd f st*/
