implementation module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass, GenRecord, Text
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

gUpdate{|Tag|} mode ust = basicUpdateSimple mode (Tag "") ust
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
assign props actionMenu task = parallel ("Assign","Manage a task assigned to another user.") Nothing (\_ (Just r) -> r) [InBodyCTask processControl] [DetachedTask props actionMenu task (\(Just r) _ -> (Just r,[StopParallel]))]
where
	processControl shared =
			updateSharedInformationA (taskTitle task,"Waiting for " +++ taskTitle task) (toView,fromView) [] shared
		>>|	return undef
		
	toView (_,[_,{ParallelTaskInfo|properties=Right {progress,systemProperties=s=:{issuedAt,firstEvent,latestEvent},managerProperties=m=:{worker}}}])=
		{ mapRecord m
		& assignedTo	= worker
		, progress		= formatProgress progress
		, issuedAt		= Display issuedAt
		, firstWorkedOn	= Display firstEvent
		, lastWorkedOn	= Display latestEvent
		}
		
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

sequence :: !String ![Task a]  -> Task [a] | iTask a
sequence _ [] = return []
sequence label tasks=:[first:rest] = parallel (label,RawText description) (rest,[]) (\_ (_,acc) -> reverse acc) [] [seqTask first]
where
	description = "Do the following tasks one at a time:<br /><ul><li>" +++ (join "</li><li>" (map taskTitle tasks)) +++ "</li></ul>"
	seqTask t = InBodyTask t accuFun
	accuFun r (rest,acc)
		# acc = [r:acc]
		= case rest of
			[]			= (([],acc),[])
			[next:rest]	= ((rest,acc),[AppendTasks [seqTask next]])
	
(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iTask a
(<!) task pred = parallel (taskTitle task,taskDescription task) Nothing (\_ (Just a) -> a) [] [InBodyTask task checkResult]
where
	checkResult a _
		| pred a	= (Just a,[])
		| otherwise	= (Nothing,[AppendTasks [InBodyTask task checkResult]])

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb = parallel ("-||-", "Done when either subtask is finished.") [] (\_ l -> hd l) [] [InBodyTask taska orfunc,InBodyTask taskb orfunc]
where
	orfunc val [] = ([val],[StopParallel])
	orfunc val _  = abort "Multiple results in OR"

(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= parallel ("||-", "Done when the second subtask is finished.") Nothing (\_ v -> fromJust v) [] [InBodyTask taska (\_ _ -> (Nothing,[])),InBodyTask taskb (\v _ -> (Just v,[StopParallel]))]

(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel ("-||", "Done when the first subtask is finished") Nothing (\_ v -> fromJust v) [] [InBodyTask taska (\v _ -> (Just v,[StopParallel])),InBodyTask taskb (\_ _ -> (Nothing,[]))]				

(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb = parallel ("-&&-", "Done when both subtasks are finished") (Nothing,Nothing) parseresult [] [InBodyTask taska (\a (_,b) -> ((Just a,b),[])),InBodyTask taskb (\b (a,_) -> ((a,Just b),[]))]
where
	parseresult _ (Just a,Just b)	= (a,b)
	parseresult _ _					= abort "AND not finished"

(-&?&-) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iTask a & iTask b
(-&?&-) taska taskb = parallel ("-&?&-", "Done when both subtasks are finished. Yields only a result of both subtasks have a result") (Nothing,Nothing) parsefunc [] [InBodyTask taska (\a (_,b) -> ((a,b),[])),InBodyTask taskb (\b (a,_) -> ((a,b),[]))]
where			
	parsefunc _ (Just a,Just b)	= Just (a,b)
	parsefunc _ _				= Nothing
	
oldParallel :: !d !pState !(ResultFun pState pResult) ![TaskContainer pState] -> Task pResult | iTask pState & iTask pResult & descr d
oldParallel d initState resultFun initTasks = parallel d initState resultFun [InBodyCTask overviewControl] initTasks
where
	overviewControl shared =
			updateSharedInformationA "" (toView,fromView) [] shared
		>>|	return undef
	
	toView (_,infos) = Table (map toView` (filter isProc infos))
	toView` {ParallelTaskInfo|index,properties=Right {ProcessProperties|managerProperties=m=:{worker},taskProperties=t=:{taskDescription}}} =
		{ ProcessOverviewView
		| index			= Hidden index
		, subject		= Display taskDescription.TaskDescription.title
		, assignedTo	= worker
		}
		
	isProc {ParallelTaskInfo|properties = Right _} = True
	isProc _ = False
	
	fromView (Table viewList) (_,infos) = map fromView` viewList
	where
		fromView` ({ProcessOverviewView|index=i=:Hidden index,assignedTo})
			= case filter (\info -> info.ParallelTaskInfo.index == index) infos of
				[{ParallelTaskInfo|properties=Right {ProcessProperties|managerProperties}}] = (index,{managerProperties & worker = assignedTo})
				_ = abort "old parallel: no manager properties"
			
:: ProcessOverviewView =	{ index			:: !Hidden TaskIndex
							, subject		:: !Display String
							, assignedTo	:: !User
							}
derive class iTask ProcessOverviewView

anyTask :: ![Task a] -> Task a | iTask a
anyTask [] 		= getDefaultValue
anyTask tasks 	= parallel ("any", "Done when any subtask is finished") [] (\_ l -> hd l) [] (map (\t -> (InBodyTask t anyfunc)) tasks)
where
	anyfunc val [] = ([val],[StopParallel])
	anyfunc val _  = abort "Multiple results in ANY"

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks = parallel ("all", "Done when all subtasks are finished") [] (\_ l -> sortByIndex l) [] (map (\(idx,t) -> (InBodyTask t (allfunc (length tasks) idx))) (zip (indexList tasks,tasks)))
where
	allfunc tlen idx val st 
		# st = st ++ [(idx,val)]
		| length st == tlen	= (st,[StopParallel])
		| otherwise			= (st,[])
			
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = parallel ("either", "Done when either subtask is finished") [] (\_ l -> hd l) [] [InBodyTask (taska >>= \a -> return (Left a)) eitherfunc,InBodyTask (taskb >>= \b -> return (Right b)) eitherfunc]
where
	eitherfunc val [] = ([val],[StopParallel])
	eitherfunc val _  = abort "Multiple results in Either"

orProc :: !(ProcTask a) !(ProcTask a) -> Task a | iTask a
orProc taska taskb = oldParallel ("-|@|-", "Done if either subtask is finished.") Nothing (\_ (Just r) -> r) [mkDetachedTask taska orfunc,mkDetachedTask taskb orfunc]
where
	orfunc (Just val) Nothing	= (Just val,[StopParallel])
	orfunc val _ 				= abort "Multiple results in -|@|-"

andProc :: !(ProcTask a) !(ProcTask b) -> Task (a,b) | iTask a & iTask b
andProc (taska,propa,menua) (taskb,propb,menub) = oldParallel ("AndProc", "Done if both subtasks are finished.") (Nothing,Nothing) parseresult [DetachedTask propa menua (taska >>= \a -> return (Left a)) andfunc,DetachedTask propb menub (taskb >>= \b -> return (Right b)) andfunc]
where
	andfunc :: !(Maybe (Either a b)) !(!Maybe a,!Maybe b) -> (!(!Maybe a,!Maybe b),![Control (!Maybe a,!Maybe b)])
	andfunc (Just val) (left,right)
	= case val of
		(Left a)
			# state = (Just a,right)
			= case state of
				(Just l, Just r) = (state,[StopParallel])
				_				 = (state,[])
		(Right b)
			# state = (left,Just b)
			= case state of
				(Just l, Just r) = (state,[StopParallel])
				_				 = (state,[])		
	
	parseresult _ (Just a,Just b)	= (a,b)
	parseresult _ _	 				= abort "AND not finished"

anyProc :: ![ProcTask a] -> Task a | iTask a
anyProc [] 	  = getDefaultValue
anyProc tasks = oldParallel ("any", "Done when any subtask is finished.") Nothing (\_ (Just r) -> r) (map (\t -> (mkDetachedTask t anyfunc)) tasks)
where
	anyfunc (Just val) Nothing	= (Just val,[StopParallel])
	anyfunc val _				= abort "Multiple results in ANY"

allProc :: ![ProcTask a] -> Task [a] | iTask a
allProc tasks = oldParallel ("all", "Done when all subtasks are finished.") [] (\_ r -> sortByIndex r) (map (\(idx,t) -> (mkDetachedTask t (allfunc (length tasks) idx))) (zip (indexList tasks,tasks)))
where
	allfunc tlen idx (Just val) st 
		# st = st ++ [(idx,val)]
		| length st == tlen	= (st,[StopParallel])
		| otherwise			= (st,[])
		
mkDetachedTask :: !(!Task a,!ManagerProperties,!ActionMenu) !(AccuFunDetached a acc) -> TaskContainer acc | iTask a
mkDetachedTask (task,prop,menu) fun = DetachedTask prop menu task fun

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
