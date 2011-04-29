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
import CoreCombinators, ExceptionCombinators, TuningCombinators, SystemTasks, SharedTasks, ProcessDBTasks, UpdateTasks, OutputTasks

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

//Helper function for tasks in a parallel set
accu :: (a acc -> (acc,Bool)) (Task a) (SymmetricShared acc) (ParallelInfo acc) -> Task a | iTask a & iTask acc
accu accufun task pstate pcontrol
	=	task
	>>= \result ->
		readShared pstate
	>>= \state ->
		let (nstate,stop) =  accufun result state in
				writeShared pstate nstate
			>>| if stop
				(writeShared pcontrol [StopParallel] >>| return result)
				(return result)
			
transform :: !(a -> b) !a -> Task b | iTask b
transform f x = mkInstantTask ("Value transformation", "Value transformation with a custom function") (\tst -> (TaskFinished (f x),tst))

/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
assign :: !ManagerProperties !ActionMenu !(Task a) -> Task a | iTask a
assign props actionMenu task = parallel ("Assign","Manage a task assigned to another user.") Nothing (\_ (Just r) -> r)
									[InBodyTask processControl, DetachedTask props actionMenu (accu accJust task)] <<@ minimalParallelLayout
where
	processControl state control =
			updateSharedInformationA (taskTitle task,"Waiting for " +++ taskTitle task) (toView,fromView) [] control
		>>|	return 42
	
	accJust r _ = (Just r,True)
			
	toView [_,{ParallelTaskInfo|properties=Right {progress,systemProperties=s=:{issuedAt,firstEvent,latestEvent},managerProperties=m=:{worker}}}]=
		{ mapRecord m
		& assignedTo	= worker
		, progress		= formatProgress progress
		, issuedAt		= Display issuedAt
		, firstWorkedOn	= Display firstEvent
		, lastWorkedOn	= Display latestEvent
		}
		
	fromView view=:{ProcessControlView|assignedTo} _
		= [UpdateProperties 1 {mapRecord view & worker = assignedTo}]
	
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
sequence label tasks = parallel (label,RawText description) [] (\_ acc -> reverse acc) (seqTasks tasks)
where
	description = "Do the following tasks one at a time:<br /><ul><li>" +++ (join "</li><li>" (map taskTitle tasks)) +++ "</li></ul>"
	
	seqTasks []		= []
	seqTasks [t:ts]	= [InBodyTask \pstate pinfo -> t >>= accResult pstate >>= startNext pinfo ts]
	
	accResult pstate a 		= updateShared (\acc -> [a:acc]) pstate >>| return a
	
	startNext pinfo [] a	= return a
	startNext pinfo ts a	= writeShared pinfo [AppendTask t \\ t <- seqTasks ts] >>| return a
		

(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iTask a
(<!) task pred = parallel (taskTitle task,taskDescription task) Nothing (\_ (Just a) -> a) [InBodyTask (checked pred task)]
where
	checked pred task pstate pinfo
		= task >>= \a -> if (pred a)
			(writeShared pstate (Just a) >>|return a)
			(writeShared pinfo [AppendTask (InBodyTask (checked pred task))] >>| return a)
			
(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb = parallel ("-||-", "Done when either subtask is finished.") Nothing (\_ (Just a) -> a)
						[InBodyTask (accu orfun taska), InBodyTask (accu orfun taskb)]
where
	orfun a _ = (Just a,True)
	
(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= parallel ("||-", "Done when the second subtask is finished.") Nothing (\_ (Just b) -> b)
		[InBodyTask (\_ _ -> taska), InBodyTask (accu orfun taskb)]
where
	orfun b _ = (Just b,True)
	
(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel ("-||", "Done when the first subtask is finished") Nothing (\_ (Just a) -> a)
		[InBodyTask (accu orfun taska), InBodyTask (\_ _ -> taskb)]				
where
	orfun a _ = (Just a,True)
	
(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb = parallel ("-&&-", "Done when both subtasks are finished") (Nothing,Nothing) resfun
	[InBodyTask (accu (\a (_,b) -> ((Just a,b),False)) taska), InBodyTask (accu (\b (a,_) -> ((a,Just b),False)) taskb)]
where
	resfun _ (Just a,Just b)	= (a,b)
	resfun _ _					= abort "AND not finished"

(-&?&-) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iTask a & iTask b
(-&?&-) taska taskb = parallel ("-&?&-", "Done when both subtasks are finished. Yields only a result of both subtasks have a result") (Nothing,Nothing) resfun
	[InBodyTask (accu (\a (_,b) -> ((a,b),False)) taska),InBodyTask (accu (\b (a,_) -> ((a,b),False)) taskb)]
where				
	resfun _ (Just a,Just b)	= Just (a,b)
	resfun _ _					= Nothing

			
:: ProcessOverviewView =	{ index			:: !Hidden TaskIndex
							, subject		:: !Display String
							, assignedTo	:: !User
							}

derive class iTask ProcessOverviewView

anyTask :: ![Task a] -> Task a | iTask a
anyTask [] 		= getDefaultValue
anyTask tasks 	= parallel ("any", "Done when any subtask is finished") Nothing (\_ (Just a) -> a) (map (\t -> (InBodyTask (accu anyfun t))) tasks)
where
	anyfun a _ = (Just a, True)

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks = parallel ("all", "Done when all subtasks are finished") [] (\_ l -> sortByIndex l) [InBodyTask (accu (allfun i) t) \\ t <- tasks & i <- [0..]] 
where
	allfun i a acc = ([(i,a):acc],False)
			
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = parallel ("either", "Done when either subtask is finished") Nothing (\_ (Just a) -> a)
	[InBodyTask (accu afun taska), InBodyTask (accu bfun taskb)]
where
	afun a _ = (Just (Left a),True)
	bfun b _ = (Just (Right b),True)

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
						(False,msg) -> (showMessageA ("Feedback",msg) [] r >>= transform snd) -||- (taska <| pred)					

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
