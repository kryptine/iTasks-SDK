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

makeInteractiveTask :: !(Maybe (About about)) !(about -> aboutV) !(View i v o) ![TaskAction i] !(AutoActionEvents i) !(InteractionTaskMode i o) -> TaskFunctions (!ActionEvent, !Maybe i) | iTask i & iTask v & iTask o & iTask about & iTask aboutV
makeInteractiveTask mbAbout aboutView (bimapGet,bimapPutback) actions autoEventF informationTaskMode = (interactiveTaskE,interactiveTaskC)
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
				# (conflict,tst)			= appFst fromOk (accIWorldTSt (isSharedChanged (shared taskNr) localTimestamp) tst)
				| enterMode || not (isValidValue nvmask) || conflict = (conflict,tst)
				# ((oldModelValue,_),tst)	= accIWorldTSt (readModelValue taskNr) tst
				# newModelValue				= bimapPutback nvalue oldModelValue
				# tst						= appIWorldTSt (snd o writeShared (shared taskNr) newModelValue) tst
				= (conflict,tst)
			Nothing
				= (False,tst)
		= appIWorldTSt (setCommitStoreInfo taskNr (outdatedClient,conflict,map fst edits,localTimestamp)) tst

	interactiveTaskC tst=:{taskNr,newTask,treeType}
		// init task if it's new
		# tst								= if newTask (appIWorldTSt (initTask taskNr) tst) tst
		// read local value/masks & model value/timestamp
		# (new=:(nvalue,numask,nvmask),tst)	= accIWorldTSt (readStores taskNr) tst
		# ((modelV,modelT),tst)				= accIWorldTSt (readModelValue taskNr) tst
		// check auto event
		# mbAutoEvent						= autoEventF (if (isValidValue nvmask) (Valid modelV) Invalid)
		| isJust mbAutoEvent				= (TaskFinished (fromJust mbAutoEvent,(if (isValidValue nvmask) (Just modelV) Nothing)),tst)
		# ((outdatedClient,conflict,updatedPaths,localTimestamp),tst) = accIWorldTSt (getCommitStoreInfo taskNr) tst
		# errors = case (outdatedClient,conflict) of
			(True,_)						= map (\p -> (p,ErrorMessage "The client is outdated. The form was refreshed with the most recent value.")) updatedPaths
			(_,True)						= map (\p -> (p,ErrorMessage "An edit conflict occurred. The field was reset to the most recent value.")) updatedPaths
			_								= []
		// rebuild value from model if model is newer than local value, not in enter mode and no other errors occurred
		# ((new=:(nvalue,numask,nvmask)),tst) = case localTimestamp < modelT && not (enterMode || conflict || outdatedClient || not (isValidValue nvmask)) of
			True	= accIWorldTSt (updateViewValue taskNr bimapGet new modelV modelT errors) tst
			False	= (new,tst)
		= case treeType of
			SpineTree
				= (TaskBusy,tst)
			JSONTree
				// check for action event
				# (mbActionEvent,tst) = actionEvent actions tst
				= case mbActionEvent of
					Just event
						= (TaskFinished (event,if (isValidValue nvmask) (Just modelV) Nothing),tst)
					Nothing
						# tst		= setJSONValue (toJSON nvalue) tst
						= (TaskBusy,tst)
			UITree
				// check for action event
				# (mbActionEvent,tst) = actionEvent actions tst
				# (res,tst) = case mbActionEvent of
					Just event
						= (TaskFinished (event,if (isValidValue nvmask) (Just modelV) Nothing),tst)
					Nothing
						# editorId                              = "tf-" +++ taskNrToString taskNr
						# evalActions                           = evaluateConditions actions (isValidValue nvmask) modelV
						# (mbClientTimestamp,tst)				= clientTimestamp tst
						| isNothing mbClientTimestamp || outdatedClient || newTask // refresh UI if client is outdated, no timestamp is send (refresh) or task is new
							# form 								= visualizeAsEditor editorId nvalue nvmask
							# (mbContext,tst) = case mbAbout of
								Nothing							= (Nothing,tst)
								Just (AboutValue a)				= (Just (visualizeAsHtmlDisplay (aboutView a)),tst)
								Just (SharedAbout ref)			= appFst (Just o visualizeAsHtmlDisplay o aboutView o fromOk) (accIWorldTSt (readShared ref) tst)
							# tst = setTUIDef (taskPanel taskNr mbContext (Just form)) evalActions tst
							= (TaskBusy,tst)
						| otherwise	// update UI
							// get stored old value, masks & errors
							# ((ovalue,oumask,ovmask),tst)		= accIWorldTSt (getCommitStoreOld taskNr) tst
							# (oldErrors,tst)					= accIWorldTSt (getErrors taskNr) tst
							# nvmask							= setInvalid oldErrors nvmask
							# updates							= determineEditorUpdates editorId (ovalue,ovmask) (nvalue,nvmask) updatedPaths
							// update context if shared & changed
							# (updates,tst) = case mbAbout of
								Just (SharedAbout shared)
									# (changed,tst) = appFst fromOk (accIWorldTSt (isSharedChanged shared localTimestamp) tst)
									| changed
										# (context,tst) = appFst (visualizeAsHtmlDisplay o aboutView o fromOk) (accIWorldTSt (readShared shared) tst)
										= ([TUIReplace (contextId taskNr) (taskContextPanel taskNr context):updates],tst)
									| otherwise
										= (updates,tst) 
								_
									= (updates,tst)
							# tst = setTUIUpdates updates evalActions tst
							= (TaskBusy,tst)
				// delete commit stores
				# tst	= deleteTaskStore "commit-old" tst
				# tst	= deleteTaskStore "commit-info" tst
				= (res,tst)

	// for local mode use auto generated store name, for shared mode use given store
	shared taskNr = case informationTaskMode of
		SharedUpdate shared	= shared
		_					= mapSharedRead w2r (sharedStore ("iTask_" +++ taskNrToString taskNr +++ "-model"))
	
	w2r = case informationTaskMode of
		LocalUpdateMode _ f	= f
		EnterMode f			= f
		_					= abort "no w2r function"
		
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
			# nvalue								= bimapGet modelValue
			# numask								= defaultMask nvalue
			# (nvmask,iworld)						= verifyValue nvalue numask iworld
			# iworld								= setStores taskNr (nvalue,numask,nvmask) iworld
			= iworld
		| otherwise
			= iworld
			
	readModelValue taskNr iworld
		| enterMode // don't read model in enter mode, but compute from view
			# (view,iworld)				= readValue taskNr iworld
			# (localTimestamp,iworld)	= getLocalTimestamp taskNr iworld
			= ((w2r (bimapPutback view undef),localTimestamp),iworld)
		| otherwise
			# (value,iworld) 	= appFst fromOk (readShared (shared taskNr) iworld)
			# (timest,iworld)	= appFst fromOk (getSharedTimestamp (shared taskNr) iworld)
			= ((value,timest),iworld)
	
// store for information provided to the commit pass
setCommitStoreOld :: !TaskNr (!a,!UpdateMask,!VerifyMask) !*IWorld -> *IWorld | JSONEncode{|*|}, JSONDecode{|*|}, TC a
setCommitStoreOld taskNr old iworld = setTaskStoreFor taskNr "commit-old" old iworld
getCommitStoreOld :: !TaskNr !*IWorld -> (!(!a,!UpdateMask,!VerifyMask),!*IWorld) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
getCommitStoreOld taskNr iworld
	# (mbOld,iworld)	= getTaskStoreFor taskNr "commit-old" iworld
	= (fromJust mbOld,iworld)
// outdated client & conflict flag & update paths
setCommitStoreInfo :: !TaskNr (!Bool,!Bool,![DataPath],!Timestamp) !*IWorld -> *IWorld
setCommitStoreInfo taskNr info iworld = setTaskStoreFor taskNr "commit-info" ((\(o,c,dps,t) -> (o,c,map dataPathList dps,t)) info) iworld
getCommitStoreInfo :: !TaskNr !*IWorld -> (!(!Bool,!Bool,![DataPath],!Timestamp),!*IWorld)
getCommitStoreInfo taskNr iworld=:{IWorld|timestamp}
	# (mbInfo,iworld) = getTaskStoreFor taskNr "commit-info" iworld
	= (fromMaybe (False,False,[],timestamp) (fmap (\(o,c,dps,t) -> (o,c,map dataPathFromList dps,t)) mbInfo),iworld)
		
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
	# (vmask,iworld)	= readVMask value umask taskNr iworld
	= ((value,umask,vmask),iworld)

readValue :: !TaskNr !*IWorld -> *(!a,!*IWorld) | gUpdate{|*|} a & JSONEncode{|*|} a & JSONDecode{|*|} a & TC a
readValue taskNr iworld
	# (mbvalue,iworld)	= getTaskStoreFor taskNr "value" iworld
	= case mbvalue of
		Just v	= (v,iworld)
		Nothing	= defaultValue iworld

readUMask :: !TaskNr !*IWorld -> *(!UpdateMask,!*IWorld)						
readUMask taskNr iworld
	# (mbMask,iworld) = getTaskStoreFor taskNr "umask" iworld
	= (fromMaybe Untouched mbMask,iworld)

readVMask :: !a !UpdateMask !TaskNr !*IWorld -> *(!VerifyMask,!*IWorld) | gVerify{|*|} a		
readVMask value umask taskNr iworld
	# (mbmask,iworld) = getTaskStoreFor taskNr "vmask" iworld
	= case mbmask of
		Just m
			= (m,iworld)
		Nothing
			# (vmask,iworld)	= verifyValue value umask iworld
			# iworld			= setTaskStoreFor taskNr "vmask" vmask iworld
			= (vmask,iworld)

setStores :: !TaskNr !(!a,!UpdateMask,!VerifyMask) !*IWorld -> *IWorld | JSONEncode{|*|} a & JSONDecode{|*|} a & TC a			
setStores taskNr (value,umask,vmask) iworld
	# iworld			= setTaskStoreFor taskNr "value" value iworld
	# iworld			= setTaskStoreFor taskNr "umask" umask iworld
	# iworld			= setTaskStoreFor taskNr "vmask" vmask iworld
	= iworld

getLocalTimestamp :: !TaskNr !*IWorld -> *(!Timestamp,!*IWorld)			
getLocalTimestamp taskNr iworld=:{IWorld|timestamp}
	= appFst (fromMaybe timestamp) (getTaskStoreTimestampFor taskNr "value" iworld)
	
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
