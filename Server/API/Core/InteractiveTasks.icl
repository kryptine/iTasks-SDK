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

makeInteractiveTask :: !(Maybe (About about)) !(about -> aboutV) !(View i v o) ![TaskAction i] !(Maybe (AutoActionEvents i)) !(InteractionTaskMode i o) -> TaskFunctions (!ActionEvent, !Maybe i) | iTask i & iTask v & iTask o & iTask about & iTask aboutV
makeInteractiveTask mbAbout aboutView (bimapGet,bimapPutback) actions mbAutoEventF informationTaskMode = (interactiveTaskE,interactiveTaskC)
where
	interactiveTaskE tst=:{taskNr}
		# (old=:(ovalue,oumask,ovmask),tst)	= accIWorldTSt (readStores taskNr) tst
		# tst								= appIWorldTSt (setCommitStoreOld taskNr old) tst
		# (localTimestamp,tst)				= accIWorldTSt (getLocalTimestamp taskNr) tst
		# (mbClientTimestamp,tst)			= clientTimestamp tst
		# outdatedClient					= maybe False (\clientTimestamp -> clientTimestamp < localTimestamp) mbClientTimestamp
		| outdatedClient					= tst // ignore edit events of outdated clients
		// check for edit/value event
		# (edits,tst)						= getEditEvents tst
		# (mbValueEvent,tst)				= getValueEvent tst
		# (mbNew,tst) = case (edits,mbValueEvent) of
			(edits,_) | not (isEmpty edits) // edit event
				# (nvalue,numask,tst)		= applyUpdates edits ovalue oumask tst
				= (Just (nvalue,numask),tst)
			(_,Just nvalue) // value event
				# numask					= defaultMask nvalue
				= (Just (nvalue,numask),tst)
			_ // no edit/value events
				= (Nothing,tst)
		# (conflict,tst) = case mbNew of
			Just (nvalue,numask)
				# (nvmask,tst)				= accIWorldTSt (verifyValue nvalue numask) tst
				# tst						= appIWorldTSt (setStores taskNr (nvalue,numask,nvmask)) tst
				| enterMode 				= (False,tst)
				# (conflict,tst)			= appFst fromOk (accIWorldTSt (isSharedChanged (shared taskNr) localTimestamp) tst)
				| not (isValidValue nvmask) || conflict = (conflict,tst)
				// update model if view is changed, not in enter mode, no edit conflict occurred & view is valid
				# ((oldModelValue,_),tst)	= accIWorldTSt (readModelValue taskNr) tst
				# newModelValue				= bimapPutback nvalue oldModelValue
				# tst						= appIWorldTSt (snd o writeShared (shared taskNr) newModelValue) tst
				= (conflict,tst)
			Nothing
				= (False,tst)
		= appIWorldTSt (setCommitStoreInfo taskNr (outdatedClient,conflict,map fst edits,localTimestamp,isJust mbClientTimestamp)) tst

	interactiveTaskC tst=:{taskNr,newTask,triggerPresent}
		// init task if it's new
		# tst								= if newTask (appIWorldTSt (initTask taskNr) tst) tst
		// read local value/masks & model value/timestamp
		# (nvmask,tst)						= accIWorldTSt (readVMask taskNr) tst
		# ((modelV,modelT),tst)				= accIWorldTSt (readModelValue taskNr) tst
		// check auto event
		# mbAutoEvent						= maybe Nothing (\autoEventF -> autoEventF (if (isValidValue nvmask) (Valid modelV) Invalid)) mbAutoEventF
		| isJust mbAutoEvent				= (TaskFinished (fromJust mbAutoEvent,(if (isValidValue nvmask) (Just modelV) Nothing)),tst)
		// check for action event
		# (mbActionEvent,tst)				= actionEvent actions tst
		| isJust mbActionEvent				= (TaskFinished (fromJust mbActionEvent,if (isValidValue nvmask) (Just modelV) Nothing),tst)
		// task is still busy, set trigger flag
		# tst								= {tst & triggerPresent = triggerPresent || isJust mbAutoEventF}
		# tst								= setInteractiveFuncs (tuiFunc,jsonFunc) tst
		= (TaskBusy,tst)
	where	
		jsonFunc iworld
			// read local value/masks & model value/timestamp
			# (new=:(nvalue,numask,nvmask),iworld)	= readStores taskNr iworld
			# ((modelV,modelT),iworld)				= readModelValue taskNr iworld
			# ((outdatedClient,conflict,updatedPaths,localTimestamp,_),iworld) = getCommitStoreInfo taskNr iworld
			# errors = case (outdatedClient,conflict) of
				(True,_)							= map (\p -> (p,ErrorMessage "The client is outdated. The form was refreshed with the most recent value.")) updatedPaths
				(_,True)							= map (\p -> (p,ErrorMessage "An edit conflict occurred. The field was reset to the most recent value.")) updatedPaths
				_									= []
			// rebuild view value from model if model is newer than local value, not in enter mode and no other errors occurred
			# ((new=:(nvalue,numask,nvmask)),iworld) = case localTimestamp < modelT && not (enterMode || conflict || outdatedClient || not (isValidValue nvmask)) of
				True	= updateViewValue taskNr bimapGet new modelV modelT errors iworld
				False	= (new,iworld)
			// delete commit stores
			# iworld	= deleteTaskStoreFor taskNr "commit-old" iworld
			# iworld	= deleteTaskStoreFor taskNr "commit-info" iworld	
			= (toJSON nvalue,iworld)
			
		tuiFunc iworld
			// read local value/masks & model value/timestamp
			# (new=:(nvalue,numask,nvmask),iworld)	= readStores taskNr iworld
			# ((modelV,modelT),iworld)				= readModelValue taskNr iworld
			# ((outdatedClient,conflict,updatedPaths,localTimestamp,clientTimestampSent),iworld) = getCommitStoreInfo taskNr iworld
			# errors = case (outdatedClient,conflict) of
				(True,_)							= map (\p -> (p,ErrorMessage "The client is outdated. The form was refreshed with the most recent value.")) updatedPaths
				(_,True)							= map (\p -> (p,ErrorMessage "An edit conflict occurred. The field was reset to the most recent value.")) updatedPaths
				_									= []
			// rebuild view value from model if model is newer than local value, not in enter mode and no other errors occurred
			# ((new=:(nvalue,numask,nvmask)),iworld) = case localTimestamp < modelT && not (enterMode || conflict || outdatedClient || not (isValidValue nvmask)) of
				True	= updateViewValue taskNr bimapGet new modelV modelT errors iworld
				False	= (new,iworld)
			# editorId								= "tf-" +++ taskNrToString taskNr
			# evalActions							= evaluateConditions actions (isValidValue nvmask) modelV
			# (mbOld,iworld)						= getCommitStoreOld taskNr iworld
			| not clientTimestampSent || outdatedClient || isNothing mbOld // refresh UI if client is outdated, no timestamp is send (refresh) or task is new for this request (no old value stored during edit phase)
				# form 								= visualizeAsEditor editorId nvalue nvmask
				# (mbContext,iworld) = case mbAbout of
					Nothing							= (Nothing,iworld)
					Just (AboutValue a)				= (Just (visualizeAsHtmlDisplay (aboutView a)),iworld)
					Just (SharedAbout ref)			= appFst (Just o visualizeAsHtmlDisplay o aboutView o fromOk) (readShared ref iworld)
				// delete commit stores
				# iworld	= deleteTaskStoreFor taskNr "commit-old" iworld
				# iworld	= deleteTaskStoreFor taskNr "commit-info" iworld
				= (Definition (taskPanel taskNr mbContext (Just form)) evalActions,iworld)
			| otherwise	// update UI
				// get stored old value, masks & errors
				# (ovalue,oumask,ovmask)			= fromJust mbOld
				# (oldErrors,iworld)				= getErrors taskNr iworld
				# nvmask							= setInvalid oldErrors nvmask
				# updates							= determineEditorUpdates editorId (ovalue,ovmask) (nvalue,nvmask) updatedPaths
				// update context if shared & changed
				# (updates,iworld) = case mbAbout of
					Just (SharedAbout shared)
						# (changed,iworld) = appFst fromOk (isSharedChanged shared localTimestamp iworld)
						| changed
							# (context,iworld) = appFst (visualizeAsHtmlDisplay o aboutView o fromOk) (readShared shared iworld)
							= ([TUIReplace (contextId taskNr) (taskContextPanel taskNr context):updates],iworld)
						| otherwise
							= (updates,iworld)
					_
						= (updates,iworld)
				// delete commit stores
				# iworld	= deleteTaskStoreFor taskNr "commit-old" iworld
				# iworld	= deleteTaskStoreFor taskNr "commit-info" iworld
				= (Updates updates evalActions,iworld)

	// for local mode use auto generated store name, for shared mode use given store
	shared taskNr = case informationTaskMode of
		SharedUpdate shared	= shared
		_					= mapSharedRead o2i (sharedStore ("iTask_" +++ taskNrToString taskNr +++ "-model"))
	
	o2i = case informationTaskMode of
		LocalUpdateMode _ f	= f
		EnterMode f			= f
		_					= abort "no o2i function"
		
	enterMode = case informationTaskMode of
		EnterMode _	= True
		_			= False
			
	// initialises the task the first time it is ran
	initTask taskNr iworld
		// auto generate model store if in local mode
		# iworld = case informationTaskMode of
			LocalUpdateMode initial	_	= snd (writeShared (shared taskNr) initial iworld)
			_							= iworld
		// determine initial view value based on model if not in enter mode
		| not enterMode
			# ((modelValue,modelTimestamp),iworld)	= readModelValue taskNr iworld
			# value									= bimapGet modelValue
			# umask									= defaultMask value
			# (vmask,iworld)						= verifyValue value umask iworld
			# iworld								= setStores taskNr (value,umask,vmask) iworld
			= iworld
		| otherwise
			# (value,iworld)						= defaultValue iworld
			# umask									= Untouched
			# (vmask,iworld)						= verifyValue value umask iworld
			# iworld								= setStores taskNr (value,umask,vmask) iworld
			// make use of 'bimapPutback' to help compiler determining the type of the default value
			# (defModel,iworld)						= defaultValue iworld
			# (_,iworld) 							= (bimapPutback value defModel,iworld)
			= iworld
			
	readModelValue taskNr iworld
		| enterMode // don't read model in enter mode, but compute from view
			# (view,iworld)				= readValue taskNr iworld
			# (localTimestamp,iworld)	= getLocalTimestamp taskNr iworld
			= ((o2i (bimapPutback view undef),localTimestamp),iworld)
		| otherwise
			# (value,iworld) 	= appFst fromOk (readShared (shared taskNr) iworld)
			# (timest,iworld)	= appFst fromOk (getSharedTimestamp (shared taskNr) iworld)
			= ((value,timest),iworld)

// store for information provided to the commit pass
setCommitStoreOld :: !TaskNr (!a,!UpdateMask,!VerifyMask) !*IWorld -> *IWorld | JSONEncode{|*|}, JSONDecode{|*|}, TC a
setCommitStoreOld taskNr old iworld = setTaskStoreFor taskNr "commit-old" old iworld
getCommitStoreOld :: !TaskNr !*IWorld -> (!Maybe (!a,!UpdateMask,!VerifyMask),!*IWorld) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
getCommitStoreOld taskNr iworld
	# (mbOld,iworld) = getTaskStoreFor taskNr "commit-old" iworld
	= (mbOld,iworld)
// outdated client & conflict flag & update paths
setCommitStoreInfo :: !TaskNr (!Bool,!Bool,![DataPath],!Timestamp,!Bool) !*IWorld -> *IWorld
setCommitStoreInfo taskNr info iworld = setTaskStoreFor taskNr "commit-info" ((\(o,c,dps,t,ct) -> (o,c,map dataPathList dps,t,ct)) info) iworld
getCommitStoreInfo :: !TaskNr !*IWorld -> (!(!Bool,!Bool,![DataPath],!Timestamp,!Bool),!*IWorld)
getCommitStoreInfo taskNr iworld=:{IWorld|timestamp}
	# (mbInfo,iworld) = getTaskStoreFor taskNr "commit-info" iworld
	= (fromMaybe (False,False,[],timestamp,True) (fmap (\(o,c,dps,t,ct) -> (o,c,map dataPathFromList dps,t,ct)) mbInfo),iworld)
		
// Gets errors if stored (otherwise return empty error list)
getErrors :: !TaskNr !*IWorld -> *(![(!DataPath,!a)],!*IWorld) | JSONEncode{|*|} a & JSONDecode{|*|} a & TC a
getErrors taskNr iworld
	# (mbErrors,iworld) = getTaskStoreFor taskNr "errors" iworld
	= (maybe [] (map (appFst dataPathFromList)) mbErrors,iworld)
storeErrors :: [(!DataPath,!a)] !TaskNr !*IWorld -> *IWorld | JSONEncode{|*|} a & JSONDecode{|*|} a & TC a
storeErrors errors taskNr iworld
	= setTaskStoreFor taskNr "errors" (map (appFst dataPathList) errors) iworld
					
// determines a new view value from model
updateViewValue :: !TaskNr !(a -> v) (!v,!UpdateMask,!VerifyMask) !a !Timestamp ![(!DataPath,!ErrorMessage)] !*IWorld -> (!(v,UpdateMask,VerifyMask),!*IWorld) | iTask a & iTask v
updateViewValue taskNr bimapGet view=:(viewValue,_,_) modelValue modelTimestamp errors iworld
	# nvalue = bimapGet modelValue
	// only calculate new view value if 'get (put v m) <> v'
	| viewValue =!= nvalue
		# numask			= defaultMask nvalue
		# (nvmask,iworld)	= verifyValue nvalue numask iworld
		# nvmask			= setInvalid errors nvmask
		# new				= (nvalue,numask,nvmask)
		# iworld			= setStores taskNr new iworld
		= (new,iworld)
	| otherwise
		# iworld			= setTaskStoreFor taskNr "value" viewValue iworld // set value to update timestamp
		= (appThd3 (setInvalid errors) view,iworld)

readStores :: !TaskNr !*IWorld -> *(!(!a,!UpdateMask,!VerifyMask),!*IWorld) | gVerify{|*|} a & gUpdate{|*|} a & JSONEncode{|*|} a & JSONDecode{|*|} a & TC a			
readStores taskNr iworld
	# (value,iworld)	= readValue taskNr iworld
	# (umask,iworld)	= readUMask taskNr iworld
	# (vmask,iworld)	= readVMask taskNr iworld
	= ((value,umask,vmask),iworld)

readValue :: !TaskNr !*IWorld -> *(!a,!*IWorld) | gUpdate{|*|} a & JSONEncode{|*|} a & JSONDecode{|*|} a & TC a
readValue taskNr iworld = appFst fromJust (getTaskStoreFor taskNr "value" iworld)

readUMask :: !TaskNr !*IWorld -> *(!UpdateMask,!*IWorld)						
readUMask taskNr iworld = appFst fromJust (getTaskStoreFor taskNr "umask" iworld)

readVMask :: !TaskNr !*IWorld -> *(!VerifyMask,!*IWorld)	
readVMask taskNr iworld = appFst fromJust (getTaskStoreFor taskNr "vmask" iworld)
		
setStores :: !TaskNr !(!a,!UpdateMask,!VerifyMask) !*IWorld -> *IWorld | JSONEncode{|*|} a & JSONDecode{|*|} a & TC a			
setStores taskNr (value,umask,vmask) iworld
	# iworld			= setTaskStoreFor taskNr "value" value iworld
	# iworld			= setTaskStoreFor taskNr "umask" umask iworld
	# iworld			= setTaskStoreFor taskNr "vmask" vmask iworld
	= iworld

getLocalTimestamp :: !TaskNr !*IWorld -> *(!Timestamp,!*IWorld)			
getLocalTimestamp taskNr iworld
	= appFst (fromMaybe (Timestamp 0)) (getTaskStoreTimestampFor taskNr "value" iworld)
	
applyUpdates :: ![(!DataPath,!String)] !a !UpdateMask !*TSt -> *(!a,!UpdateMask,!*TSt) | gUpdate{|*|} a							
applyUpdates [] val umask tst = (val,umask,tst)
applyUpdates [(p,v):us] val umask tst=:{TSt|iworld}
	# (val,umask,iworld) = updateValueAndMask p v val umask iworld
	= applyUpdates us val umask {TSt|tst & iworld = iworld}
		
clientTimestamp :: !*TSt -> (!Maybe Timestamp,!*TSt)
clientTimestamp tst=:{request}
	# ts = paramValue "timestamp" request
	| ts <> ""	= (Just (Timestamp (toInt ts)),tst)
	| otherwise	= (Nothing,tst)
		
//Build TUI definition for task with given context/form	
taskPanel :: !TaskNr !(Maybe HtmlTag) !(Maybe [TUIDef]) -> [TUIDef]
taskPanel taskNr mbContext mbForm = maybeToList (fmap (taskContextPanel taskNr) mbContext) ++ fromMaybe [] mbForm

taskContextPanel :: !TaskNr !a -> TUIDef | toString a		
taskContextPanel taskNr context = TUIHtmlContainer	{ TUIHtmlContainer
													| id = (contextId taskNr)
													, html = toString context
													, fieldLabel = Nothing
													}

contextId :: !TaskNr -> String											
contextId taskNr = "context-" +++ taskNrToString taskNr
	
//Evaluate action's conditions
evaluateConditions :: ![(!Action, (Verified a) -> Bool)] !Bool a -> [(Action, Bool)]
evaluateConditions actions valid value = [(action,pred (if valid (Valid value) Invalid)) \\ (action,pred) <- actions]

//Get action event for current task if present
actionEvent :: ![TaskAction a] !*TSt-> (!Maybe ActionEvent,!*TSt)
actionEvent actions	tst
	# (mbActionEvent,tst) = getActionEvent tst
	# mbEvent = case mbActionEvent of
		Just (JSONString key)								= addData "" (mbAction key)
		Just (JSONArray [JSONString key,JSONString data])	= addData data (mbAction key)
		_													= Nothing
	= (mbEvent,tst)
where
	mbAction key = listToMaybe [action \\ (action,pred) <- actions | actionName action == key]
	addData data mbAction = fmap (\a -> (a,data)) mbAction
