implementation module CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Util, HtmlUtil, Time, Error, OSError, Map
import iTaskClass, Task, TaskContext
from Shared			import ::Shared(..), :: SymmetricShared, :: SharedGetTimestamp, :: SharedWrite, :: SharedRead
from Shared			import qualified readShared, writeShared, isSharedChanged
from StdFunc		import o, id
from iTasks			import dynamicJSONEncode, dynamicJSONDecode
from ExceptionCombinators	import :: SharedException(..), instance toString SharedException, :: OSException(..), instance toString OSException
from WorkflowDB		import qualified class WorkflowDB(..), instance WorkflowDB IWorld

VIEWS_STORE				:== "views"
LOCAL_STORE				:== "local"
LAST_EDIT_STORE			:== "lastEdit"
LAST_TUI_STORE			:== "lastTui"
EVENT_STORE				:== "event"
EDIT_CONFLICT_STORE		:== "editConflict"
EDIT_CONFLICT_WARNING	:== "An edit conflict occurred. The form was refreshed with the most recent value."

derive JSONEncode UpdateMask, VerifyMask, ErrorMessage
derive JSONDecode UpdateMask, VerifyMask, ErrorMessage
JSONEncode{|InteractionPart|} _ ip		= dynamicJSONEncode ip
JSONDecode{|InteractionPart|} _ [json:r]= (dynamicJSONDecode json,r)
JSONDecode{|InteractionPart|} _ _ 		= (Nothing,[])

derive bimap Maybe,(,)

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask ("return", "Return a value") (\_ iworld -> (TaskFinished a,iworld))

sharedStore :: !SharedStoreId !a -> SymmetricShared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV = Shared
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

get :: !(Shared a w) -> Task a | iTask a
get shared = mkInstantTask ("Read shared", "Reads a shared value") eval
where
	eval taskNr iworld
		# (val,iworld) = 'Shared'.readShared shared iworld
		# res = case val of
			Ok val	= TaskFinished val
			Error e	= taskException (SharedException e)
		= (res, iworld)

//TODO: Mark (smartly) that a particular share has been updated	
set :: !(Shared r a) !a -> Task a | iTask a
set shared val = mkInstantTask ("Write shared", "Writes a shared value") eval
where
	eval taskNr iworld
		# (res,iworld)	='Shared'.writeShared shared val iworld
		# res = case res of
			Ok _	= TaskFinished val
			Error e	= taskException (SharedException e)
		= (res, iworld)

//TODO: Mark (smartly) that a particular share has been updated	
update :: !(r -> w) !(Shared r w) -> Task w | iTask r & iTask w
update f shared = mkInstantTask ("Update shared", "Updates a shared value") eval
where
	eval taskNr iworld
		# (val,iworld)	= 'Shared'.readShared shared iworld
		| isError val	= (taskException (SharedException (fromError val)), iworld)
		# val			= f (fromOk val)
		# (wres,iworld)	= 'Shared'.writeShared shared val iworld
		| isError wres	= (taskException (SharedException (fromError wres)), iworld)
		= (TaskFinished val, iworld)

interact :: !d !(l r Bool -> [InteractionPart (!l,!Maybe w)]) l !(Shared r w) -> Task (l,r) | descr d & iTask l & iTask r & iTask w
interact description partFunc initLocal shared = mkActionTask description (\termFunc -> {initFun = init, editEventFun = edit, evalTaskFun = eval termFunc})
where
	init taskNr iworld
		= (TCBasic newMap, iworld)
	
	//There is an edit event for this task (because the location part of the event is the empty list)
	edit taskNr ([],dps,editV) context iworld=:{IWorld|timestamp}
		//Split datapath into datapath & part index
		# (idx,dp)					= splitDataPath dps
		//Read latest versions of states
		# (model,iworld) 			= 'Shared'.readShared shared iworld
		| isError model				= (context, iworld)
		# local						= getLocalState context
		# views						= getViews local (fromOk model) context
		| idx >= length views		= (context, iworld) 
		//Save event for use in the visualization of the task
		# context					= setEvent (dps,editV) context
		//Check if the share has changed since we last generated the visualization
		# lastTuiGen				= getLocalVar LAST_TUI_STORE context
		# (changed,iworld)			= 'Shared'.isSharedChanged shared (fromMaybe (Timestamp 0) lastTuiGen) iworld
		= case changed of
			Ok True
				//The share has changed since we last edited it
				= (setLocalVar EDIT_CONFLICT_STORE True context, iworld) 
			_
				# (mbV,part)	= views !! idx
				= case part of
					UpdateView _ putback
						| isNothing mbV			= (context,iworld)
						# (jsonV,umask,_)		= fromJust mbV
						# value					= fromJSON jsonV
						| isNothing value		= (context,iworld)
						// update value & masks
						# (value,umask,iworld)	= updateValueAndMask dp editV (fromJust value) umask iworld
						# vmask					= verifyForm value umask
						# views					= updateAt idx (Just (toJSON value,umask,vmask),part) views
						# context				= setViews views context
						// calculate new local & model value
						# (local,mbModel)		= putback (if (isValidValue vmask) (Just value) Nothing)
						# context				= setLocalState local context
						= case mbModel of
							Just model
								# (_,iworld) 	= 'Shared'.writeShared shared model iworld
								# context		= setLocalVar LAST_EDIT_STORE timestamp context
								= (context,iworld)
							Nothing	
								= (context, iworld)
					Update _ (local,mbModel)			
						# context				= setLocalState local context
						= case mbModel of
							Just model	= (context, snd ('Shared'.writeShared shared model iworld))
							Nothing		= (context, iworld)
					_
						= (context,iworld)
				
	edit taskNr _ context iworld = (context,iworld)
	
	eval termFunc taskNr event tuiTaskNr imerge pmerge context iworld=:{IWorld|timestamp}
		# (model,iworld) 				= 'Shared'.readShared shared iworld
		| isError model					= (sharedException model, iworld)
		# (localTimestamp,iworld)		= getLocalTimestamp context iworld
		# (changed,iworld)				= 'Shared'.isSharedChanged shared localTimestamp iworld
		| isError changed				= (sharedException changed, iworld)
		# local							= getLocalState context
		= case termFunc {modelValue = (local,fromOk model), localValid = fromOk changed} of
			StopInteraction result
				= (TaskFinished result,iworld)
			UserActions actions
				= case getActionResult event actions of
					Just result
						= (TaskFinished result, iworld)
					Nothing
						# context				= setLocalVar LAST_TUI_STORE timestamp context
						# (tui,context,iworld)	= renderTUI taskNr imerge local (fromOk model) (fromOk changed) actions context iworld
						= (TaskBusy (Just tui) context, iworld)
						
	getLocalTimestamp context iworld=:{IWorld|timestamp}
		= case getLocalVar LAST_EDIT_STORE context of
			Just ts	= (ts,iworld)
			Nothing	= (Timestamp 0,iworld)
	
	getLocalState context
		= fromMaybe initLocal (getLocalVar LOCAL_STORE context)

	setLocalState state context
		= setLocalVar LOCAL_STORE state context
	
	splitDataPath dp
		= (hd dplist, dataPathFromList (reverse (tl dplist)))
	where
		dplist = reverse (dataPathList (s2dp dp))

	getActionResult (Just ([],name)) actions
		= listToMaybe (catMaybes [result \\ (action,result) <- actions | actionName action == name])
	getActionResult _ actions
		= Nothing

	renderTUI taskNr imerge local model changed actions context iworld
		# parts					= partFunc local model changed
		# oldVs					= getViews local model context
		# (mbEvent,context)		= getEvent context
		# (tuis,newVs)			= visualizeParts taskNr parts oldVs mbEvent
		# context 				= setViews (zip2 newVs parts) context
		# buttons				= renderButtons taskNr (map (appSnd isJust) actions)
		# warning				= case (getLocalVar EDIT_CONFLICT_STORE context) of
			Just True	= Just EDIT_CONFLICT_WARNING
			_			= Nothing
		# tui					= mergeTUI imerge (toDescr description) tuis buttons warning
		= (tui,context,iworld)
	
	renderButtons taskNr actions
		# taskId = taskNrToString taskNr
		= [mkButton taskId action enabled \\ (action,enabled) <- actions]
	where
		mkButton taskId action enabled
			= { content	= TUIButton
					{ TUIButton
					| name = actionName action
					, taskId = taskId
					, disabled = not enabled
					, text = actionLabel action
					, iconCls = actionIcon action
					, actionButton = True
					}
			  , width		= Auto
			  , height	= Auto
			  , margins	= Nothing
			  }
				
	mergeTUI imerge {TaskDescription|title,description} tuis buttons warning
		= imerge { title = title
				 , description = description
				 , editorParts = tuis
				 , buttons = buttons
				 , type = Nothing //TODO get these types merged in here
				 , isControlTask = False
				 , localInteraction = False
				 , warning = warning
				 }
	
	getViews local model context 
		= case getLocalVar VIEWS_STORE context of
			Just views	= views
			Nothing		= [(Nothing,part) \\ part <- partFunc local model True]
			
	setViews views context
		= setLocalVar VIEWS_STORE views context

	setEvent event context
		= setLocalVar EVENT_STORE event context
		
	getEvent context
		= case getLocalVar EVENT_STORE context of
			Just (dp,val)	= (Just (s2dp dp, val), delLocalVar EVENT_STORE context)
			Nothing			= (Nothing, context)
			
:: Views a :== [(!Maybe (!JSONNode,!UpdateMask,!VerifyMask),!InteractionPart a)]

visualizeParts :: !TaskNr ![InteractionPart w] !(Views w) !(Maybe (!DataPath,!JSONNode)) -> (![TUIDef],![Maybe (!JSONNode,!UpdateMask,!VerifyMask)])
visualizeParts taskNr parts oldVs mbEdit
	# res			= [visualizePart (part,mbV,idx) \\ part <- parts & (mbV,_) <- (oldVs ++ repeat (Nothing,undef)) & idx <- [0..]]
	= unzip res
where
	visualizePart (part,mbV,idx)
		= case part of
			UpdateView formView putback = case formView of
				FormValue value
					# umask				= defaultMask value
					# vmask				= verifyForm value umask
					# tui				= visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit
					= (tui,Just (toJSON value,umask,vmask))
				Unchanged init = case mbV of
					Nothing				= visualizePart (UpdateView init putback,mbV,idx)
					Just (jsonV,_,vmask) = case fromJSON` formView jsonV of
						Nothing			= visualizePart (UpdateView init putback,mbV,idx)
						Just value		= (visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit,mbV)
				Blank					= blankForm formView mbEdit
			DisplayView v				= (htmlDisplay (toString (visualizeAsHtmlDisplay v)),Nothing)
			Update label _				=	({ content = TUIButton	{ TUIButton
																	| name			= toString idx
																	, taskId		= taskNrToString taskNr
																	, text			= label
																	, disabled		= False
																	, iconCls		= ""
																	, actionButton	= False
																	}
											, width = Auto, height = Auto, margins = Nothing},Nothing)
	where
		fromJSON` :: !(FormView v) !JSONNode -> (Maybe v) | JSONDecode{|*|} v
		fromJSON` _ json = fromJSON json
		
		blankForm :: !(FormView v) !(Maybe (!DataPath,!JSONNode)) -> (!TUIDef,!Maybe (!JSONNode,!UpdateMask,!VerifyMask)) | iTask v
		blankForm formView mbEdit
			# value	= defaultValue` formView
			# umask	= Untouched
			# vmask	= verifyForm value umask
			= (visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit,Just (toJSON value,umask,vmask))
		
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
