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
from Shared		import mapShared, :: SymmetricShared
import CoreCombinators, ExceptionCombinators, TuningCombinators, SystemTasks, InteractionTasks, SharedTasks

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
assign :: !User !(Task a) -> Task a | iTask a	
assign user task = parallel ("Assign","Manage a task assigned to another user.") (Nothing,\_ r _ -> (Just r,Just Stop), \_ (Just r) -> r) [processControl] [task <<@ user]
where
	processControl shared =
			updateSharedInformationA (taskTitle task,"Waiting for " +++ taskTitle task) (toView,fromView) [] shared
		>>|	return undef
		
	toView (_,[{progress,systemProperties=s=:{issuedAt,firstEvent,latestEvent},managerProperties=m=:{worker,priority,deadline,context,tags}}:_])=
		{ assignedTo	= worker
		, priority		= priority
		, progress		= formatProgress progress
		, issuedAt		= Display issuedAt
		, firstWorkedOn	= Display firstEvent
		, lastWorkedOn	= Display latestEvent
		, deadline		= deadline
		, context		= fmap Note context
		, tags			= list2mb tags
		}
	where
		formatProgress TPActive		= coloredLabel "Active" "green"
		formatProgress TPStuck		= coloredLabel "Stuck" "purple"
		formatProgress TPWaiting	= coloredLabel "Waiting" "blue"
		formatProgress TPReject		= coloredLabel "Reject" "red"
		
		coloredLabel label color = toHtmlDisplay [SpanTag [StyleAttr ("color:" +++ color)] [Text label]]
		
	fromView {ProcessControlView|assignedTo,context,priority,deadline,tags} (_,[{managerProperties}:rest])
		# newManagerProperties =	{ managerProperties
									& worker	= assignedTo
									, context	= fmap toString context
									, priority	= priority
									, deadline	= deadline
									, tags		= mb2list tags
									}
		= [newManagerProperties:map (\{managerProperties} -> managerProperties) rest]
	
:: ProcessControlView =	{ assignedTo	:: !User
						, priority		:: !TaskPriority
						, progress		:: !HtmlDisplay
						, issuedAt		:: !Display Timestamp
						, firstWorkedOn	:: !Display (Maybe Timestamp)
						, lastWorkedOn	:: !Display (Maybe Timestamp)
						, deadline		:: !Maybe DateTime
						, context		:: !Maybe Note
						, tags			:: !Maybe [String]
						}
derive class iTask ProcessControlView

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
	orfunc (val,_) [] = ([val],Just Stop)
	orfunc (val,_) _  = abort "Multiple results in OR"

(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= group ("||-", "Done when the second subtask is finished.") rorfunc hd [] [(taska >>= \a -> return (Left a)), (taskb >>= \b -> return (Right b))] [] undef
where
	rorfunc (Right val,_) [] = ([val],Just Stop)
	rorfunc (Left val, _) [] = ([],Nothing)
	rorfunc _ _				 = abort "Illegal result in ||-"

(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= group ("-||", "Done when the first subtask is finished") lorfunc hd [] [(taska >>= \a -> return (Left a)),(taskb >>= \b -> return (Right b))] [] undef
where
	lorfunc (Right val,_) [] = ([],Nothing)
	lorfunc (Left val, _) [] = ([val],Just Stop)
	lorfunc _ _				 = abort "Illegal result in -||"					

(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb = group ("-&&-", "Done when both subtasks are finished") andfunc parseresult (Nothing,Nothing) [(taska >>= \a -> return (Left a)), (taskb >>= \b -> return (Right b))] [] undef
where
	andfunc :: ((Either a b),Int) (Maybe a, Maybe b) -> ((Maybe a, Maybe b),Maybe (PAction (Either a b) (Maybe a,Maybe b)))
	andfunc (val,_) (left,right)
	= case val of
		(Left a)
			# state = (Just a,right)
			= case state of
				(Just l, Just r) = (state,Just Stop)
				_				 = (state,Nothing)
		(Right b)
			# state = (left,Just b)
			= case state of
				(Just l, Just r) = (state,Just Stop)
				_				 = (state,Nothing)		
	
	parseresult (Just a,Just b)	= (a,b)
	parseresult _				= abort "AND not finished"

(-&?&-) infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) -> Task (Maybe (a,b)) | iTask a & iTask b
(-&?&-) taska taskb 
= group ("-&?&-", "Done when both subtasks are finished. Yields only a result of both subtasks have a result") mbandfunc parsefunc (Nothing,Nothing) [(taska >>= \a -> return (Left a)), (taskb >>= \b -> return (Right b))] [] undef
where
	mbandfunc (val,_) (left,right)
		= case val of
			Left v
				# state = (Just v,right)
				= case state of
					(Just a, Just b) = (state,Just Stop)
					_				 = (state,Nothing)				
			Right v
				# state = (left,Just v)
				= case state of
					(Just a, Just b) = (state,Just Stop)
					_				 = (state,Nothing)
	parsefunc (Just (Just a), Just (Just b)) = Just (a,b)
	parsefunc _								 = Nothing
	
oldParallel :: !TaskParallelType !d !(ValueMerger taskResult pState pResult) ![Task taskResult] -> Task pResult | iTask taskResult & iTask pState & iTask pResult & descr d
oldParallel parType d valueMerger initTasks = parallel d valueMerger [overviewControl] (map (container (DetachedTask noMenu)) initTasks)
where
	overviewControl shared =
			updateSharedInformationA "" (toView,fromView) [] shared
		>>|	return undef
	
	toView (_,props) = Table (map toView` props)
	toView` {managerProperties=m=:{worker,taskDescription}} =
		{ ProcessOverviewView
		| subject		= Display taskDescription.TaskDescription.title
		, assignedTo	= worker
		}
	
	fromView (Table viewList) (_,props) = map fromView` (zip2 viewList props)
	fromView` ({ProcessOverviewView|assignedTo},{managerProperties})
		=	{ managerProperties
			& worker = assignedTo				
			}
		
:: ProcessOverviewView =	{ subject		:: !Display String
							, assignedTo	:: !User
							}
derive class iTask ProcessOverviewView

anyTask :: ![Task a] -> Task a | iTask a
anyTask [] 		= getDefaultValue
anyTask tasks 	= group ("any", "Done when any subtask is finished") anyfunc hd [] tasks [] undef
where
	anyfunc (val,_) [] = ([val],Just Stop)
	anyfunc (val,_) _  = abort "Multiple results in ANY"

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks = group ("all", "Done when all subtasks are finished") (allfunc(length tasks)) sortByIndex [] tasks [] undef
where
	allfunc tlen (val,idx) st 
		# st = st ++ [(idx,val)]
		| length st == tlen = (st,Just Stop)
		| otherwise = (st,Nothing)
			
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = group ("either", "Done when either subtask is finished") eitherfunc hd [] [(taska >>= \a -> return (Left a)), (taskb >>= \b -> return (Right b))] [] undef
where
	eitherfunc (val,idx) [] = ([val],Just Stop)
	eitherfunc (val,idx) _  = abort "Multiple results in Either"

orProc :: !(Task a) !(Task a) !TaskParallelType -> Task a | iTask a
orProc taska taskb type = oldParallel type ("-|@|-", "Done if either subtask is finished.") (Nothing,orfunc,\_ (Just r) -> r) [taska,taskb] 
where
	orfunc _ val Nothing	= (Just val,Just Stop)
	orfunc _ val _ 			= abort "Multiple results in -|@|-"

andProc :: !(Task a) !(Task b) !TaskParallelType -> Task (a,b) | iTask a & iTask b
andProc taska taskb type = oldParallel type ("AndProc", "Done if both subtasks are finished.") ((Nothing,Nothing),andfunc,parseresult) [(taska >>= \a -> return (Left a)), (taskb >>= \b -> return (Right b))]
where
	andfunc :: !Int !(Either a b) !(!Maybe a,!Maybe b) -> (!(!Maybe a,!Maybe b),!Maybe (PAction (Either a b) (!Maybe a,!Maybe b)))
	andfunc _ val (left,right)
	= case val of
		(Left a)
			# state = (Just a,right)
			= case state of
				(Just l, Just r) = (state,Just Stop)
				_				 = (state,Nothing)
		(Right b)
			# state = (left,Just b)
			= case state of
				(Just l, Just r) = (state,Just Stop)
				_				 = (state,Nothing)		
	
	parseresult _ (Just a,Just b)	= (a,b)
	parseresult _ _	 				= abort "AND not finished"

anyProc :: ![Task a] !TaskParallelType -> Task a | iTask a
anyProc [] 	  type = getDefaultValue
anyProc tasks type = oldParallel type ("any", "Done when any subtask is finished.") (Nothing,anyfunc,\_ (Just r) -> r) tasks
where
	anyfunc _ val Nothing	= (Just val,Just Stop)
	anyfunc _ val _			= abort "Multiple results in ANY"

allProc :: ![Task a] !TaskParallelType -> Task [a] | iTask a
allProc tasks type = oldParallel type ("all", "Done when all subtasks are finished.") ([],allfunc (length tasks),\_ r -> sortByIndex r) tasks 
where
	allfunc tlen idx val st 
		# st = st ++ [(idx,val)]
		| length st == tlen = (st,Just Stop)
		| otherwise = (st,Nothing)

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
		GStop			= (Void, Just Stop)
		GContinue		= (Void, Nothing)
		GExtend tasks	= (Void, Just (Extend tasks))
		GFocus tag		= (Void, Just (Focus tag))
		
dynamicGroupAOnly :: ![Task Void] ![GroupAction Void] !(GroupActionGenFunc GOnlyAction) -> Task Void
dynamicGroupAOnly initTasks gActions genFunc = group ("dynamicGroup", "A simple group with dynamically added tasks") procfun id Void (changeTasksType initTasks) gActions genFunc
where
	procfun (action,_) _ = case action of
		GOStop			= (Void, Just Stop)
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
		updateEditorStates f st = appSnd f st
