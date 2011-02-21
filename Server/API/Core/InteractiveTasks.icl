implementation module InteractiveTasks

import StdTuple, StdBool, StdList, StdMisc, Maybe, Types, Util, Shared, HtmlUtil
import iTaskClass, Task, TSt
from SharedTasks	import sharedStore, :: SharedStoreId
from StdFunc		import id, const, o

derive JSONEncode UpdateMask, VerifyMask, ErrorMessage
derive JSONDecode UpdateMask, VerifyMask, ErrorMessage
derive bimap Maybe,(,)

always :: (Verified a) -> Bool
always _ = True

ifvalid :: (Verified a) -> Bool
ifvalid (Valid _) 	= True
ifvalid _			= False 

ifinvalid :: (Verified a) -> Bool
ifinvalid Invalid	= True
ifinvalid _			= False

noAutoActionEvents :: AutoActionEvents a
noAutoActionEvents = const Nothing

makeInteractiveTask :: !(Maybe (About about)) !(about -> aboutV) !(View i v o) ![TaskAction i] !(AutoActionEvents i) !(InteractionTaskMode i o) !*TSt -> (!TaskResult (!ActionEvent,!Maybe i),!*TSt) | iTask i & iTask v & iTask o & iTask about & iTask aboutV
makeInteractiveTask mbAbout aboutView (bimapGet,bimapPutback) actions autoEventF informationTaskMode tst=:{taskNr, newTask, treeType}
	# tst						= if newTask (appIWorldTSt initTask tst) tst
	# (ovalue,tst)				= accIWorldTSt readValue tst
	# (oumask,tst)				= accIWorldTSt readUMask tst
	# (ovmask,tst)				= accIWorldTSt (readVMask ovalue oumask) tst
	# old						= (ovalue,oumask,ovmask)
	# (events,tst)				= getEvents tst
	# (localTimestamp,tst)		= accIWorldTSt getLocalTimestamp tst
	# (mbClientTimestamp,tst)	= clientTimestamp tst
	# (refresh,outdatedClient) = case mbClientTimestamp of
		Nothing
			// refresh if client did not sent timestamp
			= (True,False)
		Just clientTimestamp
			// refresh if client timestamp is older than local timestamp of the task or task is new
			# outdated = clientTimestamp < localTimestamp
			= (outdated || newTask,outdated)
	= case treeType of
		SpineTree
			// check auto event
			# ((modelV,_),tst) = accIWorldTSt readModelValue tst
			# mbAutoEvent = autoEventF (if (isValidValue ovmask) (Valid modelV) Invalid)
			| isJust mbAutoEvent = (TaskFinished (fromJust mbAutoEvent,(if (isValidValue ovmask) (Just modelV) Nothing)),tst)
			= (TaskBusy,tst)
		JSONTree
			// check for value event
			# (new=:(nvalue,_,vmask),tst) = case (valueEvent events,outdatedClient) of
				(Nothing,_)		// no value events
					= (old,tst)
				(_,True)		// ignore update events of outdated clients
					= (old,tst)
				(Just nvalue,_)
					// update view value
					# umask			= defaultMask nvalue
					# (vmask,tst)	= accIWorldTSt (verifyValue nvalue umask) tst
					# new			= (nvalue,umask,vmask)
					# tst			= appIWorldTSt (setStores new) tst
					// don't update model in enter mode
					# tst = case enterMode of
						False
							# ((oldModelValue,_),tst)	= accIWorldTSt readModelValue tst
							# newModelValue				= bimapPutback nvalue oldModelValue
							# tst						= appIWorldTSt (snd o writeShared shared newModelValue) tst
							= tst
						True
							= tst
					= (new,tst)
			// check auto event
			# ((modelV,_),tst) = accIWorldTSt readModelValue tst
			# mbAutoEvent = autoEventF (if (isValidValue vmask) (Valid modelV) Invalid)
			| isJust mbAutoEvent = (TaskFinished (fromJust mbAutoEvent,(if (isValidValue vmask) (Just modelV) Nothing)),tst)
			// check for action event
			# mbActionEvent	= actionEvent events actions
			= case mbActionEvent of
				Just event
					= handleActionEvent nvalue (isValidValue vmask) event tst
				Nothing
					// JSON representation is built after all possible changes of the model are done
					# tst = setJSONFunc (buildJSONValue new localTimestamp) tst
					= (TaskBusy,tst)
		UITree
			// check for edit events
			# edits = editEvents events
			# (rebuild,new=:(nvalue,numask,nvmask),errors,tst) = case (edits,outdatedClient) of
				(_,True)	// ignore update events of outdated clients & give error msg
					= (True,old,[(p,ErrorMessage "The client is outdated. The form was refreshed with the most recent value.") \\ (p,_) <- edits],tst)
				([],_)		// no edit events
					= (True,old,[],tst)
				_			// update edited view value
					# (nvalue,numask,tst)	= applyUpdates edits ovalue oumask tst
					# (nvmask,tst)			= accIWorldTSt (verifyValue nvalue numask) tst
					# new					= (nvalue,numask,nvmask)
					# tst					= appIWorldTSt (setStores new) tst
					# (conflict,tst)		= appFst fromOk (accIWorldTSt (isSharedChanged shared localTimestamp) tst)
					| not enterMode
						| not conflict
							| isValidValue nvmask
								# ((oldModelValue,_),tst)	= accIWorldTSt readModelValue tst
								# newModelValue				= bimapPutback nvalue oldModelValue
								# tst						= appIWorldTSt (snd o writeShared shared newModelValue) tst
								// rebuild value from model after also other possible changes are done
								= (True,new,[],tst)
							| otherwise
								// for updated invalid editors views are not rebuilt, updates are based on current value
								= (False,new,[],tst)
						| otherwise
							// task causes an edit conflict
							// don't update model, rebuild view based on current value of model and set errors
							= (True,old,[(p,ErrorMessage "An edit conflict occurred. The field was reset to the most recent value.") \\ (p,_) <- edits],tst)
					| otherwise
						// in enter mode views not rebuilt, updates are based on current value
						= (False,new,[],tst)
			// check auto event
			# ((modelV,_),tst) = accIWorldTSt readModelValue tst
			# mbAutoEvent = autoEventF (if (isValidValue nvmask) (Valid modelV) Invalid)
			| isJust mbAutoEvent = (TaskFinished (fromJust mbAutoEvent,(if (isValidValue nvmask) (Just modelV) Nothing)),tst)
			// check for action event
			# mbActionEvent	= actionEvent events actions
			= case mbActionEvent of
				Just event
					= handleActionEvent nvalue (isValidValue nvmask) event tst
				Nothing
					// UI is built after all possible changes of the model are done
					# tst = setTUIFunc (buildUI (ovalue,ovmask) new rebuild refresh localTimestamp errors (map fst edits)) tst
					= (TaskBusy,tst)
where
	// for local mode use auto generated store name, for shared mode use given store
	shared = case informationTaskMode of
		SharedUpdate shared	= shared
		_					= mapSharedRead w2r (sharedStore ("iTask_" +++ taskNrToString taskNr +++ "-model"))
	
	w2r = case informationTaskMode of
		LocalUpdateMode _ f	= f
		EnterMode f			= f
		_					= abort "no w2r function"
			
	// initialises the task the first time it is ran
	initTask iworld
		// auto generate model store if in local mode
		# iworld = case informationTaskMode of
			LocalUpdateMode initial	_	= snd (writeShared shared initial iworld)
			_							= iworld
		// determine initial view value based on model if not in enter mode
		| not enterMode
			# ((modelValue,modelTimestamp),iworld)	= readModelValue iworld
			# nvalue								= bimapGet modelValue
			# numask								= defaultMask nvalue
			# (nvmask,iworld)						= verifyValue nvalue numask iworld
			# iworld								= setStores (nvalue,numask,nvmask) iworld
			= iworld
		| otherwise
			= iworld
	
	handleActionEvent viewValue valid event tst
		# ((modelValue,_),tst) = accIWorldTSt readModelValue tst
		= (TaskFinished (event,if valid (Just modelValue) Nothing),tst)
	
	/**
	* Builds the user interface for an information task AFTER the entire task tree is built.
	* All changes to shared models have to be done before.
	*
	* @param The view value before the current request.
	* @param The view value possibly updated by events.
	* @param Determines if a new view value is build using the current model and the bimap get function.
	* @param Determines if a new UI definition is computed or the existing one is updated.
	* @param The timestamp of the local value before the current request.
	* @param Error messages added to the rebuilt value.
	* @param Datapaths of values updates by an event
	* @param IWorld
	*
	* @return A tree node containing the computed UI definition/updates.
	*/
	buildUI old new rebuild refresh localTimestamp errors updatedPaths iworld
		# ((modelValue,modelTimestamp), iworld)	= readModelValue iworld
		// check for changed model value
		# (modelChanged,iworld) = case enterMode of
			False								= appFst fromOk (isSharedChanged shared localTimestamp iworld)
			True								= (False,iworld)
		// determine new view value if model is changed, rebuild is requested & not in enter mode
		# ((rvalue,_,rvmask),iworld) = case modelChanged && rebuild && not enterMode of
			True								= updateViewValue bimapGet new modelValue modelTimestamp errors iworld
			False								= (appThd3 (setInvalid errors) new,iworld)
		# evalActions							= evaluateConditions actions (isValidValue rvmask) modelValue
		# editorId								= "tf-" +++ taskNrToString taskNr
		# iworld								= storeErrors errors iworld
		| refresh	// refresh UI, send new def instead of updates
			# form 								= visualizeAsEditor editorId rvalue rvmask
			# (mbContext,iworld) = case mbAbout of
				Nothing								= (Nothing,iworld)
				Just (AboutValue a)					= (Just (visualizeAsHtmlDisplay (aboutView a)),iworld)
				Just (SharedAbout ref)				= appFst (Just o visualizeAsHtmlDisplay o aboutView o fromOk) (readShared ref iworld)
			= (Definition (taskPanel (taskNrToString taskNr) mbContext (Just form)) evalActions,iworld)
		| otherwise	// update UI
			// get stored old errors
			# (oldErrors,iworld)				= getErrors taskNr iworld
			# old								= appSnd (setInvalid oldErrors) old
			# updates							= determineEditorUpdates editorId old (rvalue,rvmask) updatedPaths
			// update context if shared & changed
			# (updates,iworld) = case mbAbout of
				Just (SharedAbout shared)
					# (changed,iworld) = appFst fromOk (isSharedChanged shared localTimestamp iworld)
					| changed
						# (context,iworld) = appFst (visualizeAsHtmlDisplay o aboutView o fromOk) (readShared shared iworld)
						= ([TUIReplace contextId (taskContextPanel context):updates],iworld)
					| otherwise
						= (updates,iworld) 
				_
					= (updates,iworld)
			= (Updates updates evalActions,iworld)

	buildJSONValue new=:(nvalue,_,_) localTimestamp iworld
		# ((modelValue,modelTimestamp),iworld)	= readModelValue iworld
		// check for changed model value
		# (modelChanged,iworld)					= appFst fromOk (isSharedChanged shared localTimestamp iworld)
		// determine new view value if model is changed & not in enter mode
		# (rvalue,iworld) = case modelChanged && not enterMode of
			True								= appFst fst3 (updateViewValue bimapGet new modelValue modelTimestamp [] iworld)
			False								= (nvalue,iworld)
		= (toJSON rvalue,iworld)
					
	// determines a new view value from model
	updateViewValue :: !(a -> v) (!v,!UpdateMask,!VerifyMask) !a !Timestamp ![(!DataPath,!ErrorMessage)] !*IWorld -> (!(v,UpdateMask,VerifyMask),!*IWorld) | iTask a & iTask v
	updateViewValue bimapGet view=:(viewValue,_,_) modelValue modelTimestamp errors iworld
		# nvalue = bimapGet modelValue
		// only calculate new view value if 'get (put v m) <> v'
		| viewValue =!= nvalue
			# numask			= defaultMask nvalue
			# (nvmask,iworld)	= verifyValue nvalue numask iworld
			# nvmask			= setInvalid errors nvmask
			# new				= (nvalue,numask,nvmask)
			# iworld			= setStores new iworld
			= (new,iworld)
		| otherwise
			# iworld			= setTaskStoreFor taskNr "value" viewValue iworld // set value to update timestamp
			= (appThd3 (setInvalid errors) view,iworld)
					
	readValue iworld
		# (mbvalue,iworld)	= getTaskStoreFor taskNr "value" iworld
		= case mbvalue of
			Just v
				= (v,iworld)
			Nothing
				# (v,iworld)	= defaultValue iworld
				// store default value because store is used to determine local timestamp next time
				# iworld		= setTaskStoreFor taskNr "value" v iworld
				= (v,iworld)
							
	readUMask iworld
		# (mbmask,iworld) = getTaskStoreFor taskNr "umask" iworld
		= case mbmask of
			Just m	= (m,iworld)
			Nothing	= (Untouched,iworld)
			
	readVMask value umask iworld
		# (mbmask,iworld) = getTaskStoreFor taskNr "vmask" iworld
		= case mbmask of
			Just m
				= (m,iworld)
			Nothing
				# (vmask,iworld)	= verifyValue value umask iworld
				# iworld			= setTaskStoreFor taskNr "vmask" vmask iworld
				= (vmask,iworld)
				
	setStores(value,umask,vmask) iworld
		# iworld			= setTaskStoreFor taskNr "value" value iworld
		# iworld			= setTaskStoreFor taskNr "umask" umask iworld
		# iworld			= setTaskStoreFor taskNr "vmask" vmask iworld
		= iworld
			
	getLocalTimestamp iworld=:{IWorld|timestamp}
		# (mbTimestamp,iworld) = getTaskStoreTimestampFor taskNr "value" iworld
		= case mbTimestamp of
			Just timestamp	= (timestamp,iworld)
			Nothing			= (timestamp,iworld)
	
	readModelValue iworld
		| enterMode // don't read model in enter mode, but compute from view
			# (view,iworld)				= readValue iworld
			# (localTimestamp,iworld)	= getLocalTimestamp iworld
			= ((w2r (bimapPutback view undef),localTimestamp),iworld)
		| otherwise
			# (value,iworld) 	= appFst fromOk (readShared shared iworld)
			# (timest,iworld)	= appFst fromOk (getSharedTimestamp shared iworld)
			= ((value,timest),iworld)
	
	// Gets errors if stored (otherwise return empty error list)
	getErrors taskNr iworld
		# (mbErrors,iworld) = getTaskStoreFor taskNr "errors" iworld
		= case mbErrors of
			Just errors	= (map (appFst dataPathFromList) errors,iworld)
			Nothing		= ([],iworld)
			
	// Store errors if necessary
	storeErrors errors iworld
		// Only store error if store already exists or error are not empty
		# (mbErrors,iworld) = checkErrorStore iworld
		# store = case mbErrors of
			Just _	= True
			Nothing	= not (isEmpty errors)
		| store
			= setTaskStoreFor taskNr "errors" (map (appFst dataPathList) errors) iworld
		| otherwise
			= iworld
	where
		checkErrorStore :: !*IWorld -> (!Maybe [([Int],ErrorMessage)],!*IWorld)
		checkErrorStore tst = getTaskStoreFor taskNr "errors" tst
							
	applyUpdates [] val umask tst = (val,umask,tst)
	applyUpdates [(p,v):us] val umask tst=:{TSt|iworld}
		# (val,umask,iworld) = updateValueAndMask p v val umask iworld
		= applyUpdates us val umask {TSt|tst & iworld = iworld}
		
	clientTimestamp :: !*TSt -> (!Maybe Timestamp,!*TSt)
	clientTimestamp tst=:{request}
		# ts = paramValue "timestamp" request
		| ts <> ""	= (Just (Timestamp (toInt ts)),tst)
		| otherwise	= (Nothing,tst)
		
	enterMode = case informationTaskMode of
		EnterMode _	= True
		_			= False
		
	//Build TUI definition for task with given context/form	
	taskPanel :: String (Maybe HtmlTag) (Maybe [TUIDef]) -> [TUIDef]
	taskPanel taskid mbContext mbForm = maybeToList (fmap taskContextPanel mbContext) ++ maybe [] id mbForm
		
	taskContextPanel context = TUIHtmlContainer	{ TUIHtmlContainer
												| id = contextId
												, html = toString context
												, fieldLabel = Nothing
												}
											
	contextId = "context-" +++ taskNrToString taskNr

//Edit events of which the name is a datapath
editEvents :: [(String,JSONNode)] -> [(DataPath,String)]
editEvents events = [(s2dp name,value) \\ (name,JSONString value) <- events | isdps name]

//Check if there is a value event among the events
valueEvent :: ![(!String,!JSONNode)] -> Maybe a | JSONDecode{|*|} a
valueEvent events
	= case [value \\ (name,value) <- events | name == "value"] of
		[value]	= fromJSON value
		_		= Nothing

//Check if there is an action event among the events 
actionEvent :: ![(!String,!JSONNode)] ![TaskAction a] -> Maybe ActionEvent
actionEvent events actions	
	= case [value \\ (name,value) <- events | name == "action"] of
		[JSONString key]							= addData "" (mbAction key)
		[JSONArray [JSONString key,JSONString data]]= addData data (mbAction key)
		_											= Nothing
where
	mbAction key = case [action \\ (action,pred) <- actions | actionName action == key] of
		[action]	= Just action
		_			= Nothing
		
	addData data (Just action)	= Just (action,data)
	addData data Nothing		= Nothing
	
//Evaluate action's conditions
evaluateConditions :: ![(!Action, (Verified a) -> Bool)] !Bool a -> [(Action, Bool)]
evaluateConditions actions valid value = [(action,pred (if valid (Valid value) Invalid)) \\ (action,pred) <- actions]
