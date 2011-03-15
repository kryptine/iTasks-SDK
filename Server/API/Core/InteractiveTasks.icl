implementation module InteractiveTasks

import StdTuple, StdBool, StdList, StdMisc, Maybe, Types, Util, Shared, HtmlUtil
import iTaskClass, Task, TSt, TUIDiff
from SharedTasks			import sharedStore, :: SharedStoreId
from StdFunc				import id, const, o
from ExceptionCombinators	import :: SharedException(..), instance toString SharedException

derive JSONEncode UpdateMask, VerifyMask, ErrorMessage
derive JSONDecode UpdateMask, VerifyMask, ErrorMessage
derive JSONEncode TUIDef,TUIButton, TUIUpdate, TUIMenuButton, TUIMenu, TUIMenuItem, Key, Hotkey
derive JSONEncode TUIBasicControl, TUICurrencyControl, TUIDocumentControl, TUIConstructorControl
derive JSONEncode TUIButtonControl, TUIListItemControl, TUIChoiceControl, TUIAppletControl, TUIORYXControl
derive JSONEncode TUIStaticContainer, TUIRecordContainer, TUIListContainer, TUIHtmlContainer, TUIGridControl, TUIGridColumn, TUITreeControl, TUITree
derive JSONDecode TUIDef,TUIButton, TUIUpdate, TUIMenuButton, TUIMenu, TUIMenuItem, Key, Hotkey
derive JSONDecode TUIBasicControl, TUICurrencyControl, TUIDocumentControl, TUIConstructorControl
derive JSONDecode TUIButtonControl, TUIListItemControl, TUIChoiceControl, TUIAppletControl, TUIORYXControl
derive JSONDecode TUIStaticContainer, TUIRecordContainer, TUIListContainer, TUIHtmlContainer, TUIGridControl, TUIGridColumn, TUITreeControl, TUITree
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

makeInteractiveTask :: !(Maybe (About about)) !(about -> aboutV) !(View i v o) ![TaskAction i] !(Maybe (AutoActionEvents i)) !(InteractiveTaskMode i o) -> TaskFunctions (!Action, !Maybe i) | iTask i & iTask v & iTask o & iTask about & iTask aboutV
makeInteractiveTask mbAbout aboutView (bimapGet,bimapPutback) actions mbAutoEventF interactiveTaskMode = (interactiveTaskE,interactiveTaskC)
where
	interactiveTaskE tst=:{taskNr}
		# (edits,tst)						= getEditEvents tst
		# (mbValueEvent,tst)				= getValueEvent tst
		# (old=:(ovalue,oumask,ovmask),tst)	= accIWorldTSt (readStores taskNr) tst
		# (localTimestamp,tst)				= accIWorldTSt (getLocalTimestamp taskNr) tst
		# (mbClientTimestamp,tst)			= clientTimestamp tst
		# outdatedClient					= maybe False (\clientTimestamp -> clientTimestamp < localTimestamp) mbClientTimestamp
		| outdatedClient					= appIWorldTSt (setCommitStoreInfo taskNr (outdatedClient,False,map fst edits,localTimestamp,isJust mbClientTimestamp)) tst // ignore edit events of outdated clients
		// check for edit/value event
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
				# (conflict,tst)			= accIWorldTSt (isSharedChanged (shared taskNr) localTimestamp) tst
				| isError conflict			= (False,tst)
				# conflict					= fromOk conflict
				| not (isValidValue nvmask) || conflict = (conflict,tst)
				// update model if not in enter mode, no edit conflict occurred & view is valid
				# (oldModel,tst)			= accIWorldTSt (readModelValue taskNr) tst
				| isError oldModel			= (False,tst)
				# newModelValue				= bimapPutback nvalue (fst (fromOk oldModel))
				# tst						= appIWorldTSt (snd o writeShared (shared taskNr) newModelValue) tst
				= (conflict,tst)
			Nothing
				= (False,tst)
		= appIWorldTSt (setCommitStoreInfo taskNr (outdatedClient,conflict,map fst edits,localTimestamp,isJust mbClientTimestamp)) tst

	interactiveTaskC tst=:{taskNr,newTask,triggerPresent}
		// init task if it's new
		# (res,tst)							= if newTask (accIWorldTSt (initTask taskNr) tst) (Ok Void,tst)
		| isError res						= (sharedException res,tst)
		// read local value/masks & model value/timestamp
		# (nvmask,tst)						= accIWorldTSt (readVMask taskNr) tst
		# (model,tst)						= accIWorldTSt (readModelValue taskNr) tst
		| isError model						= (sharedException model,tst)
		# (modelV,modelT)					= fromOk model
		// check auto event
		# mbAutoEvent						= maybe Nothing (\autoEventF -> autoEventF (if (isValidValue nvmask) (Valid modelV) Invalid)) mbAutoEventF
		| isJust mbAutoEvent				= (TaskFinished (fromJust mbAutoEvent,(if (isValidValue nvmask) (Just modelV) Nothing)),tst)
		// check for action event
		# (mbActionEvent,tst)				= actionEvent (if (isValidValue nvmask) (Valid modelV) Invalid) actions tst
		| isJust mbActionEvent				= (TaskFinished (fromJust mbActionEvent,if (isValidValue nvmask) (Just modelV) Nothing),tst)
		// task is still busy, set trigger flag
		# tst								= {tst & triggerPresent = triggerPresent || isJust mbAutoEventF}
		# tst								= setInteractiveFuncs (appSnd (deleteCommitStores taskNr) o tuiFunc,appSnd (deleteCommitStores taskNr) o jsonFunc) tst
		= (TaskBusy,tst)
	where	
		jsonFunc iworld
			# (new,iworld) = newViewValue taskNr iworld
			= (toJSON (fst3 new),iworld)
			
		tuiFunc iworld
			# ((modelV,_),iworld)				= appFst fromOk (readModelValue taskNr iworld)
			# ((outdatedClient,_,updatedPaths,localTimestamp,clientTimestampSent),iworld) = getCommitStoreInfo taskNr iworld
			# (mbOldViz,iworld)					= readTUIDef taskNr iworld
			# ((nvalue,numask,nvmask),iworld)	= newViewValue taskNr iworld
			# form 								= visualizeAsEditor (editorId taskNr) nvalue nvmask
			# (mbContext,iworld) = case mbAbout of
				Nothing							= (Nothing,iworld)
				Just (AboutValue a)				= (Just (visualizeAsHtmlDisplay (aboutView a)),iworld)
				Just (SharedAbout ref)			= appFst (Just o visualizeAsHtmlDisplay o aboutView o fromOk) (readShared ref iworld)
			# tui								= taskPanel taskNr mbContext form
			# iworld							= setTUIDef taskNr tui iworld
			# evalActions						= evaluateConditions actions (isValidValue nvmask) modelV
			| not clientTimestampSent || outdatedClient || isNothing mbOldViz // refresh UI if client is outdated, no timestamp is send (refresh) or task is new for this request (no old value stored)
				= (Definition tui evalActions,iworld)
			| otherwise	// update UI
				# updates						= diffEditorDefinitions (fromJust mbOldViz) tui updatedPaths
				= (Updates updates evalActions,iworld)

	// for local mode use auto generated store name, for shared mode use given store
	shared taskNr = case interactiveTaskMode of
		SharedUpdate shared	= shared
		_					= mapSharedRead o2i (sharedStore (iTaskId taskNr "model"))
	
	o2i = case interactiveTaskMode of
		LocalUpdateMode _ f	= f
		EnterMode f			= f
		_					= abort "no o2i function"
		
	enterMode = case interactiveTaskMode of
		EnterMode _	= True
		_			= False
			
	// initialises the task the first time it is ran
	initTask taskNr iworld
		// auto generate model store if in local mode
		# iworld = case interactiveTaskMode of
			LocalUpdateMode initial	_	= snd (writeShared (shared taskNr) initial iworld)
			_							= iworld
		// determine initial view value based on model if not in enter mode
		| not enterMode
			# (model,iworld)						= readModelValue taskNr iworld
			| isError model							= (liftError model,iworld)
			# (modelValue,modelTimestamp)			= fromOk model
			# value									= bimapGet modelValue
			# umask									= defaultMask value
			# (vmask,iworld)						= verifyValue value umask iworld
			# iworld								= setStores taskNr (value,umask,vmask) iworld
			= (Ok Void,iworld)
		| otherwise
			# (value,iworld)						= defaultValue iworld
			# umask									= Untouched
			# (vmask,iworld)						= verifyValue value umask iworld
			# iworld								= setStores taskNr (value,umask,vmask) iworld
			// make use of 'bimapPutback' to help compiler determining the type of the default value
			# (defModel,iworld)						= defaultValue iworld
			# (_,iworld) 							= (bimapPutback value defModel,iworld)
			= (Ok Void,iworld)
			
	readModelValue taskNr iworld
		| enterMode // don't read model in enter mode, but compute from view
			# (view,iworld)				= readValue taskNr iworld
			# (localTimestamp,iworld)	= getLocalTimestamp taskNr iworld
			= (Ok (o2i (bimapPutback view undef),localTimestamp),iworld)
		| otherwise
			# (value,iworld) 	= readShared (shared taskNr) iworld
			| isError value		= (liftError value,iworld)
			# (timest,iworld)	= getSharedTimestamp (shared taskNr) iworld
			| isError timest	= (liftError timest,iworld)
			= (Ok (fromOk value,fromOk timest),iworld)
			
	newViewValue taskNr iworld
		// read local value/masks & model value/timestamp
		# (new=:(nvalue,numask,nvmask),iworld)	= readStores taskNr iworld
		# ((modelV,modelT),iworld)				= appFst fromOk (readModelValue taskNr iworld)
		# ((outdatedClient,conflict,updatedPaths,localTimestamp,_),iworld) = getCommitStoreInfo taskNr iworld
		// rebuild view value from model if not in enter mode, model is newer than local value or an error occurred and valid was not updated to an invalid one this request
		= case not enterMode && (localTimestamp < modelT && (isEmpty updatedPaths || isValidValue nvmask) || conflict || outdatedClient) of
			True
				# errors = case (outdatedClient,conflict) of
					(True,_)					= map (\p -> (p,ErrorMessage "The client is outdated. The form was refreshed with the most recent value.")) updatedPaths
					(_,True)					= map (\p -> (p,ErrorMessage "An edit conflict occurred. The field was reset to the most recent value.")) updatedPaths
					_							= []
				= updateViewValue taskNr bimapGet new modelV modelT errors iworld
			False
				= (new,iworld)

// store for information provided to the commit pass
// outdated client & conflict flag & update paths
setCommitStoreInfo :: !TaskNr (!Bool,!Bool,![DataPath],!Timestamp,!Bool) !*IWorld -> *IWorld
setCommitStoreInfo taskNr info iworld = setTaskStoreFor taskNr "commit-info" ((\(o,c,dps,t,ct) -> (o,c,map dataPathList dps,t,ct)) info) iworld
getCommitStoreInfo :: !TaskNr !*IWorld -> (!(!Bool,!Bool,![DataPath],!Timestamp,!Bool),!*IWorld)
getCommitStoreInfo taskNr iworld=:{IWorld|timestamp}
	# (mbInfo,iworld) = getTaskStoreFor taskNr "commit-info" iworld
	= (fromMaybe (False,False,[],timestamp,True) (fmap (\(o,c,dps,t,ct) -> (o,c,map dataPathFromList dps,t,ct)) mbInfo),iworld)
// delete stores providing information for commit pass
deleteCommitStores :: !TaskNr !*IWorld -> *IWorld
deleteCommitStores taskNr iworld
	# iworld	= deleteTaskStoreFor taskNr "commit-old" iworld
	# iworld	= deleteTaskStoreFor taskNr "commit-info" iworld
	= iworld
					
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

readTUIDef :: !TaskNr !*IWorld -> *(!Maybe TUIDef,!*IWorld)
readTUIDef taskNr iworld = getTaskStoreFor taskNr "tui-def" iworld
		
setStores :: !TaskNr !(!a,!UpdateMask,!VerifyMask) !*IWorld -> *IWorld | JSONEncode{|*|} a & JSONDecode{|*|} a & TC a			
setStores taskNr (value,umask,vmask) iworld
	# iworld			= setTaskStoreFor taskNr "value" value iworld
	# iworld			= setTaskStoreFor taskNr "umask" umask iworld
	# iworld			= setTaskStoreFor taskNr "vmask" vmask iworld
	= iworld
	
setTUIDef :: !TaskNr !TUIDef !*IWorld -> *IWorld	
setTUIDef taskNr tuiDef iworld = setTaskStoreFor taskNr "tui-def" tuiDef iworld

getLocalTimestamp :: !TaskNr !*IWorld -> *(!Timestamp,!*IWorld)			
getLocalTimestamp taskNr iworld
	= appFst fromJust (getTaskStoreTimestampFor taskNr "value" iworld)
	
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
taskPanel :: !TaskNr !(Maybe HtmlTag) ![TUIDef] -> TUIDef
taskPanel taskNr mbContext form = case maybeToList (fmap (taskContextPanel taskNr) mbContext) ++ form of
	[item]	= item
	items	= TUIStaticContainer {TUIStaticContainer|id = "", items = items, fieldLabel = Nothing, optional = False}
	
taskContextPanel :: !TaskNr !a -> TUIDef | toString a		
taskContextPanel taskNr context = TUIHtmlContainer	{ TUIHtmlContainer
													| id = (contextId taskNr)
													, html = toString context
													, fieldLabel = Nothing
													}

contextId :: !TaskNr -> String											
contextId taskNr = "context-" +++ taskNrToString taskNr

editorId :: !TaskNr -> String
editorId taskNr = "tf-" +++ taskNrToString taskNr
	
//Evaluate action's conditions
evaluateConditions :: ![(!Action, (Verified a) -> Bool)] !Bool a -> [(Action, Bool)]
evaluateConditions actions valid value = [(action,pred (if valid (Valid value) Invalid)) \\ (action,pred) <- actions]

//Get action event for current task if present & pred is true
actionEvent :: !(Verified a) ![TaskAction a] !*TSt-> (!Maybe Action,!*TSt)
actionEvent v actions tst
	# (mbActionEvent,tst) = getActionEvent tst
	# mbEvent = case mbActionEvent of
		Just (JSONString aname)								= mbAction aname
		_													= Nothing
	= (mbEvent,tst)
where
	mbAction aname = listToMaybe [action \\ (action,pred) <- actions | actionName action == aname && pred v]
	addData data mbAction = fmap (\a -> (a,data)) mbAction

sharedException :: !(MaybeErrorString a) -> (TaskResult b)
sharedException err = taskException (SharedException (fromError err))
