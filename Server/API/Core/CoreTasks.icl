implementation module CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Util, HtmlUtil, Time, Error, OSError, Map, Tuple, List
import iTaskClass, Task, TaskContext, TaskEval, TaskStore, TuningCombinators, TUIDefinition
from SharedCombinators		import :: Shared
from Shared					import qualified readShared, writeShared, isSharedChanged, updateShared
from Shared					import :: SharedGetTimestamp, :: SharedWrite, :: SharedRead, :: SharedId, :: ReadWriteShared(..)
from StdFunc				import o, id
from IWorld					import :: IWorld(..), :: Control(..)
from iTasks					import dynamicJSONEncode, dynamicJSONDecode
from ExceptionCombinators	import :: SharedException(..), instance toString SharedException, :: OSException(..), instance toString OSException, :: WorkOnException(..), instance toString WorkOnException
from SystemData				import topLevelTasks
from Map					import qualified get

derive class iTask WorkOnProcessState

PARTS_STORE				:== "parts"
LOCAL_STORE				:== "local"
LAST_EDIT_STORE			:== "lastEdit"
EVENT_STORE				:== "event"
EDIT_CONFLICT_STORE		:== "editConflict"
EDIT_CONFLICT_WARNING	:== "An edit conflict occurred. The form was refreshed with the most recent value."

derive JSONEncode StoredPart, UpdateMask
derive JSONDecode StoredPart, UpdateMask
JSONEncode{|StoredPutback|} _ _ p				= [dynamicJSONEncode p]
JSONDecode{|StoredPutback|} _ _ [json:r]		= (dynamicJSONDecode json,r)
JSONDecode{|StoredPutback|} _ _ c				= (Nothing,c)

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask ("return", "Return a value") (\_ iworld -> (TaskFinished a,iworld))

sharedStore :: !SharedStoreId !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV = ReadWriteShared
	["sharedStore_" +++ storeId]
	(get (loadValue NS_APPLICATION_SHARES) defaultV)
	write
	(get (getStoreTimestamp NS_APPLICATION_SHARES) (Timestamp 0))
where	
	get f defaultV iworld
		# (mbV,iworld) = f storeId iworld
		# res = case mbV of
			Nothing	= Ok defaultV
			Just v	= Ok v
		= (res,iworld)
		
	write v iworld = (Ok Void,storeValue NS_APPLICATION_SHARES storeId v iworld)

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask ("Read shared", "Reads a shared value") eval
where
	eval taskNr iworld
		# (val,iworld) = 'Shared'.readShared shared iworld
		# res = case val of
			Ok val	= TaskFinished val
			Error e	= taskException (SharedException e)
		= (res, iworld)

set :: !a !(ReadWriteShared r a)  -> Task a | iTask a
set val shared = mkInstantTask ("Write shared", "Writes a shared value") eval
where
	eval taskNr iworld
		# (res,iworld)	='Shared'.writeShared shared val iworld
		# res = case res of
			Ok _	= TaskFinished val
			Error e	= taskException (SharedException e)
		= (res, iworld)

update :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
update fun shared = mkInstantTask ("Update shared", "Updates a shared value") eval
where
	eval taskNr iworld
		# (val,iworld)	= 'Shared'.updateShared shared fun iworld
		| isError val	= (taskException (SharedException (fromError val)), iworld)
		= (TaskFinished (fromOk val), iworld)

interact :: !d !(l r Bool -> [InteractionPart l w]) l !(ReadWriteShared r w) -> Task (l,r) | descr d & iTask l & iTask r & iTask w
interact description partFunc initLocal shared = mkActionTask description (\termFunc -> {initFun = init, editFun = edit, evalFun = eval termFunc})
where
	init taskNr iworld
		= (TCBasic newMap, iworld)
	
	//Rewrite lucky events to events that target this task
	edit taskNr (LuckyEvent e) context iworld = (edit taskNr (TaskEvent [] e) context iworld)
	//There is an edit event for this task (because the location part of the event is the empty list)
	edit taskNr (TaskEvent [] (dps,editV)) context iworld=:{IWorld|timestamp,latestEvent}	
		//Read latest versions of states
		# (model,iworld) 			= 'Shared'.readShared shared iworld
		| isError model				= (context, iworld)
		# local						= getLocalState context
		//Split datapath into datapath & part index
		# (idx,dp)					= splitDataPath dps
		# parts						= getParts context	
		| idx >= length parts		= (context, iworld) 
		//Save event for use in the visualization of the task
		# context					= setEvent (dps,editV) context
		//Check if the share has changed since the last event
		/*# (changed,iworld) = case latestEvent of
			Nothing			= (Ok False,iworld)
			Just lastEvent	= 'Shared'.isSharedChanged shared lastEvent iworld
		*/
		//TODO: Fix detection by checking against local latest event
		# changed = Ok False
		= case changed of
			Ok True	
				//Edit conflict
				= (setLocalVar EDIT_CONFLICT_STORE True context, iworld) 
			_
				# context = delLocalVar EDIT_CONFLICT_STORE context 
				= case parts !! idx of
					StoredUpdateView jsonV umask (StoredPutback putback)
						# mbValue				= fromJSON jsonV
						| isNothing mbValue		= (context,iworld)
						# value					= fromJust mbValue
						// update value & masks
						
						//TEST
						# (value,umask,iworld)	= if(dataPathLevel dp == 0)
							(case fromJSON editV of
								Just newV	= (newV,defaultMask newV, iworld)
								Nothing 	= (value,umask,iworld)
							)
							(updateValueAndMask dp editV value umask iworld)
						
						//TEST
						//# (value,umask,iworld)	= updateValueAndMask dp editV value umask iworld
						# vmask					= verifyForm value umask
						# parts					= updateAt idx (StoredUpdateView (toJSON value) umask (StoredPutback putback)) parts
						# context				= setLocalVar PARTS_STORE parts context
						// calculate new local & model value
						# (local,mbModel)		= putback (if (isValidValue vmask) (Just value) Nothing)
						# context				= setLocalState initLocal local context
						# context				= setLocalVar LAST_EDIT_STORE timestamp context
						= case mbModel of
							Just model
								# (_,iworld) 	= 'Shared'.writeShared shared model iworld
								= (context,iworld)
							Nothing	
								= (context, iworld)
					StoredUpdate (local,mbModel)			
						# context				= setLocalState initLocal local context
						= case mbModel of
							Just model	= (context, snd ('Shared'.writeShared shared model iworld))
							Nothing		= (context, iworld)
					_
						= (context,iworld)
				
	edit _ _ context iworld = (context,iworld)
	
	eval termFunc taskNr props event tuiTaskNr repAs context iworld=:{IWorld|timestamp}
		# (model,iworld) 				= 'Shared'.readShared shared iworld
		| isError model					= (sharedException model, iworld)
		# (localTimestamp,iworld)		= getLocalTimestamp context iworld
		# (changed,iworld)				= 'Shared'.isSharedChanged shared localTimestamp iworld
		| isError changed				= (sharedException changed, iworld)
		# local							= getLocalState context
		# parts							= partFunc local (fromOk model) (fromOk changed)
		# storedParts					= getParts context
		# (mbEvent,context)				= getEvent context
		# (reps,newParts,valid,iworld)	= visualizeParts taskNr repAs parts storedParts mbEvent iworld
		# context 						= setLocalVar PARTS_STORE newParts context
		= case termFunc {modelValue = (local,fromOk model), localValid = valid} of
			StopInteraction result		= (TaskFinished result,iworld)
			UserActions actions
				= case getActionResult event actions of
					Just result			= (TaskFinished result, iworld)
					Nothing
						# warning = case (getLocalVar EDIT_CONFLICT_STORE context) of
							Just True	= Just EDIT_CONFLICT_WARNING
							_			= Nothing
						# taskId		= taskNrToString taskNr
						# tactions		= [(taskId,action,isJust val) \\ (action,val) <- actions]
						# (rep,actions) = case repAs of
							(RepAsTUI ilayout _)
								= appFst TUIRep (mergeTUI props ilayout [tui \\ TUIRep tui <- reps] warning tactions)
							_
								= (ServiceRep (flatten [part \\ (ServiceRep part) <- reps]), tactions)
						= (TaskBusy rep actions context, iworld)
						
	getLocalTimestamp context iworld=:{IWorld|timestamp}
		= case getLocalVar LAST_EDIT_STORE context of
			Just ts	= (ts,iworld)
			Nothing	= (Timestamp 0,iworld)
	
	getLocalState context
		= fromMaybe initLocal (getLocalVar LOCAL_STORE context)

	setLocalState :: l !l !TaskContextTree -> TaskContextTree | JSONEncode{|*|} l
	setLocalState _ state context
		= setLocalVar LOCAL_STORE state context
	
	splitDataPath dp
		= (hd dplist, dataPathFromList (reverse (tl dplist)))
	where
		dplist = reverse (dataPathList (s2dp dp))
	
	getParts context 
		= case getLocalVar PARTS_STORE context of
			Just parts	= parts
			Nothing		= []

	setEvent event context
		= setLocalVar EVENT_STORE event context
		
	getEvent context
		= case getLocalVar EVENT_STORE context of
			Just (dp,val)	= (Just (s2dp dp, val), delLocalVar EVENT_STORE context)
			Nothing			= (Nothing, context)
	
	
	mergeTUI meta ilayout parts warning actions
		= ilayout	{ title = meta.TaskMeta.title
					, instruction = meta.TaskMeta.instruction
					, editorParts = parts
					, actions = actions
					, type = meta.interactionType
					, localInteraction = meta.TaskMeta.localInteraction
					, warning = warning
					}
		
		
:: StoredPart l w	= StoredUpdateView	!JSONNode !UpdateMask !(StoredPutback l w)
					| StoredDisplayView
					| StoredUpdate		!(!l,!Maybe w)
:: StoredPutback l w = E.v: StoredPutback !((Maybe v) -> (!l,!Maybe w)) & iTask v

visualizeParts :: !TaskNr !TaskRepInput ![InteractionPart l w] ![StoredPart l w] !(Maybe (!DataPath,!JSONNode)) !*IWorld -> (![TaskRep],![StoredPart l w],!Bool,!*IWorld)
visualizeParts taskNr repAs parts oldParts mbEdit iworld = visualizeParts` parts oldParts 0 iworld
where
	visualizeParts` [] _ idx iworld
		= ([],[],True,iworld)
	visualizeParts` [p:ps] [] idx iworld
		# (rep,part,valid,iworld)	= visualizePart p Nothing idx iworld
		# (reps,parts,valids,iworld)= visualizeParts` ps [] (inc idx) iworld
		= ([rep:reps],[part:parts],valid && valids, iworld)
	visualizeParts` [p:ps] [o:os] idx iworld
		# (rep,part,valid,iworld)	= visualizePart p (Just o) idx iworld
		# (reps,parts,valids,iworld)= visualizeParts` ps os (inc idx) iworld
		= ([rep:reps],[part:parts],valid && valids, iworld)
		
	visualizePart part mbV idx iworld
		= case part of
			FormPart formView putback = case formView of
				FormValue value
					# umask				= defaultMask value
					# vmask				= verifyForm value umask
					# (rep,iworld)		= case repAs of
											(RepAsTUI _ _)
												# (editor,iworld) = visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit iworld
												= (mbToTUIRep editor, iworld)
											_	
												= (ServiceRep [(taskNrToString taskNr, idx, toJSON value)], iworld)
					= (rep,StoredUpdateView (toJSON value) umask (StoredPutback putback), isValidValue vmask, iworld)
				Unchanged init = case mbV of
					Just (StoredUpdateView jsonV umask _) = case fromJSON` formView jsonV of
						Just value
										# vmask = verifyForm value umask
										# (rep,iworld)	= case repAs of
											(RepAsTUI _ _)
												# (editor,iworld)	= visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit iworld
												= (mbToTUIRep editor, iworld)
											_				
												= (ServiceRep [(taskNrToString taskNr, idx, toJSON value)], iworld)
										= (rep, StoredUpdateView jsonV umask (StoredPutback putback), isValidValue vmask, iworld)
						Nothing			= visualizePart (FormPart init putback) Nothing idx iworld
					_					= visualizePart (FormPart init putback) Nothing idx iworld
				Blank					= blankForm repAs formView putback mbEdit iworld
			
			DisplayPart v				= case repAs of
											(RepAsTUI _ _)	
												# (editor,iworld) = visualizeAsDisplay v iworld
												= (mbToTUIRep editor, StoredDisplayView, True, iworld)
											_	
												= (ServiceRep [(taskNrToString taskNr,idx,toJSON v)], StoredDisplayView, True, iworld)
			
			UpdatePart label w			= case repAs of
											(RepAsTUI _ _)	= (TUIRep (defaultDef (TUIButton	{ TUIButton
																	| name			= toString idx
																	, taskId		= taskNrToString taskNr
																	, text			= label
																	, disabled		= False
																	, iconCls		= ""
																	, actionButton	= False
																	})),StoredUpdate w, True,iworld)
											_				= (ServiceRep [(taskNrToString taskNr,idx,JSONString label)], StoredUpdate w, True, iworld)
	where
		fromJSON` :: !(FormView v) !JSONNode -> (Maybe v) | JSONDecode{|*|} v
		fromJSON` _ json = fromJSON json

		blankForm repAs formView putback mbEdit iworld
			# value	= defaultValue` formView
			# umask	= Untouched
			# vmask	= verifyForm value umask
			# (rep,iworld)	= case repAs of
				(RepAsTUI _ _)
					# (editor,iworld) = visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit iworld
					= (mbToTUIRep editor,iworld)
				_				
					= (ServiceRep [(taskNrToString taskNr, idx, toJSON value)], iworld)
			= (rep, StoredUpdateView (toJSON value) umask (StoredPutback putback), isValidValue vmask, iworld)
		
		defaultValue` :: !(FormView v) -> v | gUpdate{|*|} v
		defaultValue` _ = defaultValue

		mbToTUIRep :: (Maybe TUIDef) -> TaskRep
		mbToTUIRep Nothing		= NoRep
		mbToTUIRep (Just def)	= TUIRep def 

	
sharedException :: !(MaybeErrorString a) -> (TaskResult b)
sharedException err = taskException (SharedException (fromError err))

workOn :: !ProcessId -> Task WorkOnProcessState
workOn (SessionProcess sessionId)
	= abort "workOn applied to session process"
workOn processId
	= mkActionTask ("Work on","Work on another top-level instance.") (\termFunc -> {initFun = init, editFun = edit, evalFun = eval termFunc})
where
	init taskNr iworld = (TCEmpty, iworld)
	
	edit taskNr event _ iworld
		//Load instance
		# (mbContext,iworld)	= loadTaskInstance processId iworld
		| isError mbContext		= (TCEmpty, iworld)
		//Apply event to instance
		# (mbContext,iworld)	= editInstance (Just event) (fromOk mbContext) iworld
		//Store instance
		| isError mbContext		= (TCEmpty, iworld)
		# iworld				= storeTaskInstance (fromOk mbContext) iworld
		= (TCEmpty, iworld)
		
	eval termFunc taskNr props event tuiTaskNr (RepAsTUI ilayout playout) _ iworld=:{evalStack}
		//Check for cycles
		| isMember processId evalStack
			=(taskException WorkOnDependencyCycle, iworld)
		//Load instance
		# (mbContext,iworld)		= loadTaskInstance processId iworld
		| isError mbContext	
			//If the instance can not be found, check if it was only just added by an
			//appendTask in the same session. If so, create a temporary result and trigger
			//reevaluation.
			# (found,iworld)	= checkIfAddedGlobally processId iworld
			| found
				= (TaskBusy (TUIRep (stringDisplay "Task finished")) [] TCEmpty, {iworld & readShares = Nothing})
			| otherwise
				= (taskException WorkOnNotFound ,iworld)
		//Eval instance
		# target = case processId of
			(WorkflowProcess procNo)	= [procNo,changeNo (fromOk mbContext)]
			(EmbeddedProcess _ taskId)	= reverse (taskNrFromString taskId)
		# (mbResult,context,iworld)	= evalInstance target event True (fromOk mbContext) iworld
		= case mbResult of
			Error e				= (taskException WorkOnEvalError, iworld)
			Ok result
				//Store context
				# iworld		= storeTaskInstance context iworld
				# (state,tui,sactions,iworld) = case result of
					(TaskBusy tui actions _)		= (WOActive, tui, actions, iworld)
					(TaskFinished _)				= (WOFinished, TUIRep (stringDisplay "Task finished"), [], iworld)
					(TaskException _ err)			= (WOExcepted, TUIRep (stringDisplay ("Task excepted: " +++ err)), [], iworld)
				//Check trigger
				= case termFunc {localValid = True, modelValue = state} of
					StopInteraction result
						= (TaskFinished result,iworld)
					UserActions uactions	
						= case getActionResult event uactions of
							Just result
								= (TaskFinished result, iworld)
							Nothing
								# taskId			= taskNrToString taskNr
								# tactions			= [(taskId,action,isJust val) \\ (action,val) <- uactions]
								= (TaskBusy tui (sactions ++ tactions) TCEmpty,iworld)

	changeNo (TaskContext _ _ _ _ n _) = n

	checkIfAddedGlobally (WorkflowProcess procNo) iworld=:{parallelControls,currentUser}
		= case 'Map'.get (toString topLevelTasks) parallelControls of
			Just (_,controls)
				= (isMember procNo [i \\ AppendTask i currentUser _ <- controls], iworld)
			_
				= (False,iworld)
	checkIfAddedGlobally _ iworld = (False,iworld)

getActionResult (Just (LuckyEvent _)) [(action,result):_]
	= result
getActionResult (Just (TaskEvent [] name)) actions
	= listToMaybe (catMaybes [result \\ (action,result) <- actions | actionName action == name])
getActionResult _ actions
	= Nothing

applyChangeToProcess :: !ProcessId !ChangeDyn !ChangeLifeTime  -> Task Void
applyChangeToProcess pid change lifetime
	= mkInstantTask ("Apply a change to a process", ("Apply a " +++ lt +++ " change to task " +++ toString pid)) eval
where
	eval taskNr iworld = (TaskException (dynamic "TODO") "TODO", iworld)

//id (\tst -> (TaskFinished Void, applyChangeToTaskTree pid (lifetime,change) tst))
//Interesting one, we need the tst somehow :)
	lt = case lifetime of
		CLTransient = "transient"
		CLPersistent _	= "persistent"
	
appWorld :: !(*World -> *World) -> Task Void
appWorld fun = mkInstantTask ("Run world function", "Run a world function.") eval
where
	eval taskNr iworld=:{IWorld|world}
		= (TaskFinished Void, {IWorld|iworld & world = fun world})
		
accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask ("Run world function", "Run a world function and get result.") eval
where
	eval taskNr iworld=:{IWorld|world}
		# (res,world) = fun world
		= (TaskFinished res, {IWorld|iworld & world = world})
	
accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask ("Run a world function", "Run a world function with error handling.") eval
where
	eval taskNr iworld=:{IWorld|world}
		# (res,world)	= fun world
		= case res of
			Error e		= (taskException (errf e),{IWorld|iworld & world = world})
			Ok v		= (TaskFinished v,{IWorld|iworld & world = world})
	
accWorldOSError :: !(*World -> (!MaybeOSError a, !*World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException
