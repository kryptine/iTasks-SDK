implementation module CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Util, HtmlUtil, Time, Error, OSError, Map
import iTaskClass, Task, TaskContext
from Shared			import ::ReadWriteShared(..), :: Shared, :: SharedGetTimestamp, :: SharedWrite, :: SharedRead, :: SharedId
from Shared			import qualified readShared, writeShared, isSharedChanged, updateShared
from StdFunc		import o, id
from iTasks			import dynamicJSONEncode, dynamicJSONDecode
from ExceptionCombinators	import :: SharedException(..), instance toString SharedException, :: OSException(..), instance toString OSException
from WorkflowDB		import qualified class WorkflowDB(..), instance WorkflowDB IWorld

PARTS_STORE				:== "parts"
LOCAL_STORE				:== "local"
LAST_EDIT_STORE			:== "lastEdit"
EVENT_STORE				:== "event"
EDIT_CONFLICT_STORE		:== "editConflict"
EDIT_CONFLICT_WARNING	:== "An edit conflict occurred. The form was refreshed with the most recent value."

derive JSONEncode StoredPart, UpdateMask
derive JSONDecode StoredPart, UpdateMask
JSONEncode{|StoredPutback|} _ _ p			= dynamicJSONEncode p
JSONDecode{|StoredPutback|} _ _ [json:r]	= (dynamicJSONDecode json,r)
JSONDecode{|StoredPutback|} _ _ _			= (Nothing,[])
derive bimap Maybe,(,)

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask ("return", "Return a value") (\_ iworld -> (TaskFinished a,iworld))

sharedStore :: !SharedStoreId !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV = ReadWriteShared
	["sharedStore_" +++ storeId]
	(get loadValue defaultV)
	write
	(get getStoreTimestamp (Timestamp 0))
where	
	get f defaultV iworld
		# (mbV,iworld) = f storeId iworld
		# res = case mbV of
			Nothing	= Ok defaultV
			Just v	= Ok v
		= (res,iworld)
		
	write v iworld = (Ok Void,storeValue storeId v iworld)

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask ("Read shared", "Reads a shared value") eval
where
	eval taskNr iworld
		# (val,iworld) = 'Shared'.readShared shared iworld
		# res = case val of
			Ok val	= TaskFinished val
			Error e	= taskException (SharedException e)
		= (res, iworld)

set :: !(ReadWriteShared r a) !a -> Task a | iTask a
set shared val = mkInstantTask ("Write shared", "Writes a shared value") eval
where
	eval taskNr iworld
		# (res,iworld)	='Shared'.writeShared shared val iworld
		# res = case res of
			Ok _	= TaskFinished val
			Error e	= taskException (SharedException e)
		= (res, iworld)

update :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
update f shared = mkInstantTask ("Update shared", "Updates a shared value") eval
where
	eval taskNr iworld
		# (val,iworld)	= 'Shared'.updateShared shared f iworld
		| isError val	= (taskException (SharedException (fromError val)), iworld)
		= (TaskFinished (fromOk val), iworld)

interact :: !d !(l r Bool -> (![InteractionPart (!l,!Maybe w)],!l)) l !(ReadWriteShared r w) -> Task (l,r) | descr d & iTask l & iTask r & iTask w
interact description partFunc initLocal shared = mkActionTask description (\termFunc -> {initFun = init, editEventFun = edit, evalTaskFun = eval termFunc})
where
	init taskNr iworld
		= (TCBasic newMap, iworld)
	
	//There is an edit event for this task (because the location part of the event is the empty list)
	edit taskNr ([],dps,editV) context iworld=:{IWorld|timestamp,latestEvent}
		//Split datapath into datapath & part index
		# (idx,dp)					= splitDataPath dps
		//Read latest versions of states
		# (model,iworld) 			= 'Shared'.readShared shared iworld
		| isError model				= (context, iworld)
		# local						= getLocalState context
		# parts						= getParts context
		| idx >= length parts		= (context, iworld) 
		//Save event for use in the visualization of the task
		# context					= setEvent (dps,editV) context
		//Check if the share has changed since the last event
		# (changed,iworld) = case latestEvent of
			Nothing			= (Ok False,iworld)
			Just lastEvent	= 'Shared'.isSharedChanged shared lastEvent iworld
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
						# (value,umask,iworld)	= updateValueAndMask dp editV value umask iworld
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
				
	edit taskNr _ context iworld = (context,iworld)
	
	eval termFunc taskNr props event tuiTaskNr imerge pmerge mmerge context iworld=:{IWorld|timestamp}
		# (model,iworld) 				= 'Shared'.readShared shared iworld
		| isError model					= (sharedException model, iworld)
		# (localTimestamp,iworld)		= getLocalTimestamp context iworld
		# (changed,iworld)				= 'Shared'.isSharedChanged shared localTimestamp iworld
		| isError changed				= (sharedException changed, iworld)
		# local							= getLocalState context
		# (parts,local)					= partFunc local (fromOk model) (fromOk changed)
		# context						= setLocalState initLocal local context
		= case termFunc {modelValue = (local,fromOk model), localValid = fromOk changed} of
			StopInteraction result
				= (TaskFinished result,iworld)
			UserActions actions
				= case getActionResult event actions of
					Just result
						= (TaskFinished result, iworld)
					Nothing
						# (tui,actions,context,iworld) = renderTUI taskNr imerge parts actions context iworld
						= (TaskBusy (Just tui) actions context, iworld)
	where
		renderTUI taskNr imerge parts actions context iworld
			# taskId				= taskNrToString taskNr
			# storedParts			= getParts context
			# (mbEvent,context)		= getEvent context
			# (tuis,newParts)		= visualizeParts taskNr parts storedParts mbEvent
			# context 				= setLocalVar PARTS_STORE newParts context
			# tactions				= [(taskId,action,isJust val) \\ (action,val) <- actions]
			# warning				= case (getLocalVar EDIT_CONFLICT_STORE context) of
				Just True	= Just EDIT_CONFLICT_WARNING
				_			= Nothing
			# (tui,actions)			= mergeTUI imerge tuis warning tactions 
			= (tui,actions,context,iworld)
		
		mergeTUI imerge tuis warning actions
			= imerge { title = props.taskDescription.TaskDescription.title
					 , description = props.taskDescription.TaskDescription.description
					 , editorParts = tuis
					 , actions = actions
					 , type = props.interactionType
					 , isControlTask = props.controlTask
					 , localInteraction = props.TaskProperties.localInteraction
					 , warning = warning
					 }
						
	getLocalTimestamp context iworld=:{IWorld|timestamp}
		= case getLocalVar LAST_EDIT_STORE context of
			Just ts	= (ts,iworld)
			Nothing	= (Timestamp 0,iworld)
	
	getLocalState context
		= fromMaybe initLocal (getLocalVar LOCAL_STORE context)

	setLocalState :: l !l !TaskContext -> TaskContext | JSONEncode{|*|} l
	setLocalState _ state context
		= setLocalVar LOCAL_STORE state context
	
	splitDataPath dp
		= (hd dplist, dataPathFromList (reverse (tl dplist)))
	where
		dplist = reverse (dataPathList (s2dp dp))

	getActionResult (Just ([],name)) actions
		= listToMaybe (catMaybes [result \\ (action,result) <- actions | actionName action == name])
	getActionResult _ actions
		= Nothing
	
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
			
:: StoredPart l w	= StoredUpdateView	!JSONNode !UpdateMask !(StoredPutback l w)
					| StoredDisplayView
					| StoredUpdate		!(!l,!Maybe w)
:: StoredPutback l w = E.v: StoredPutback !((Maybe v) -> (!l,!Maybe w)) & iTask v

visualizeParts :: !TaskNr ![InteractionPart (!l,!Maybe w)] ![StoredPart l w] !(Maybe (!DataPath,!JSONNode)) -> (![TUIDef],![StoredPart l w])
visualizeParts taskNr parts oldParts mbEdit
	# res			= [visualizePart (part,mbV,idx) \\ part <- parts & mbV <- (map Just oldParts ++ repeat Nothing) & idx <- [0..]]
	= unzip res
where
	visualizePart (part,mbV,idx)
		= case part of
			UpdateView formView putback = case formView of
				FormValue value
					# umask				= defaultMask value
					# vmask				= verifyForm value umask
					# tui				= visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit
					= (tui,StoredUpdateView (toJSON value) umask (StoredPutback putback))
				Unchanged init = case mbV of
					Just (StoredUpdateView jsonV umask storedPutback) = case fromJSON` formView jsonV of
						Just value		= (visualizeAsEditor value (taskNrToString taskNr) idx (verifyForm value umask) mbEdit,StoredUpdateView jsonV umask storedPutback)
						Nothing			= visualizePart (UpdateView init putback,Nothing,idx)
					_					= visualizePart (UpdateView init putback,Nothing,idx)
				Blank					= blankForm formView putback mbEdit
			DisplayView v				= (htmlDisplay (toString (visualizeAsHtmlDisplay v)),StoredDisplayView)
			Update label w				=	({ content = TUIButton	{ TUIButton
																	| name			= toString idx
																	, taskId		= taskNrToString taskNr
																	, text			= label
																	, disabled		= False
																	, iconCls		= ""
																	, actionButton	= False
																	}
											, width = Auto, height = Auto, margins = Nothing},StoredUpdate w)
	where
		fromJSON` :: !(FormView v) !JSONNode -> (Maybe v) | JSONDecode{|*|} v
		fromJSON` _ json = fromJSON json

		blankForm formView putback mbEdit
			# value	= defaultValue` formView
			# umask	= Untouched
			# vmask	= verifyForm value umask
			= (visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit,StoredUpdateView (toJSON value) umask (StoredPutback putback))
		
		defaultValue` :: !(FormView v) -> v | gUpdate{|*|} v
		defaultValue` _ = defaultValue
	
sharedException :: !(MaybeErrorString a) -> (TaskResult b)
sharedException err = taskException (SharedException (fromError err))

addWorkflow :: !Workflow -> Task WorkflowDescription
addWorkflow workflow = mkInstantTask "Adds a workflow to the system" eval
where
	eval taskNr iworld = appFst TaskFinished ('WorkflowDB'.addWorkflow workflow iworld)

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
