implementation module InteractionTasks

import StdList, StdBool, StdInt, StdTuple, Util, HtmlUtil, Error, StdMisc
import iTaskClass, Task, TSt, CoreCombinators, Shared, ExceptionCombinators, TuningCombinators
from StdFunc		import o
from SharedTasks	import createSharedStore
from iTasks			import dynamicJSONEncode, dynamicJSONDecode

derive JSONEncode UpdateMask, VerifyMask, ErrorMessage
derive JSONDecode UpdateMask, VerifyMask, ErrorMessage
JSONEncode{|InteractionPart|} _ ip		= dynamicJSONEncode ip
JSONDecode{|InteractionPart|} _ [json:r]= (dynamicJSONDecode json,r)
JSONDecode{|InteractionPart|} _ _ 		= (Nothing,[])

derive bimap Maybe,(,)

interact :: !d !(l r Bool -> [InteractionPart (!l,!Maybe w)]) !(l r Bool -> InteractionTerminators a) !l !(Shared r w) -> Task a | descr d & iTask l & iTask a & iTask w
interact description partFunc termFunc initLocal shared = mkInteractionTask description (editorE,editorC)
where
	editorE tst=:{taskNr,iworld=w=:{IWorld|timestamp}}
		# (mbEdit,tst)					= getEdit tst
		= case mbEdit of
			Nothing						= tst
			Just (idx,dp,editV)
				// check if part with target idx is present & update(view) part
				# (local,tst)			= readLocalState tst
				# (views,tst)			= accIWorldTSt (readViews taskNr local) tst
				| length views <= idx	= tst
				// check for edit conflict
				# (mbT,tst)				= getTaskStoreTimestamp VIEWS_STORE tst
				# (changed,tst)			= accIWorldTSt (isSharedChanged shared (fromMaybe (Timestamp 0) mbT)) tst
				= case changed of
					Ok True
						= setTaskStore EDIT_CONFLICT_STORE True tst
					_
						// update last edit timestamp
						# tst					= appIWorldTSt (setTaskStoreFor taskNr LAST_EDIT_STORE timestamp) tst
						# (mbV,part)			= views !! idx
						= case part of
							UpdateView (_,putback)
								| isNothing mbV			= tst
								# (jsonV,umask,_)		= fromJust mbV
								# value					= fromJSON jsonV
								| isNothing value		= tst
								// update value & masks
								# (value,umask,iworld)	= updateValueAndMask dp editV (fromJust value) umask tst.TSt.iworld
								# vmask					= verifyForm value umask
								# tst					= {TSt|tst & iworld = iworld}
								# views					= updateAt idx (Just (toJSON value,umask,vmask),part) views
								# tst					= appIWorldTSt (writeViews taskNr views) tst
								// calculate new local & model value
								# newStates				= putback (if (isValidValue vmask) (Just value) Nothing)
								= updateStates newStates tst
							Update _ newStates
								= updateStates newStates tst
							_
								= tst

	editorC tst=:{taskNr,newTask}
		# (model,tst) 					= accIWorldTSt (readShared shared) tst
		| isError model					= (sharedException model,tst)
		# (localTimestamp,tst)			= accIWorldTSt (getLocalTimestamp taskNr) tst
		# (changed,tst)					= accIWorldTSt (isSharedChanged shared localTimestamp) tst
		| isError changed				= (sharedException changed,tst)
		# (local,tst)					= readLocalState tst
		= case termFunc local (fromOk model) (fromOk changed) of
			StopInteraction result
				= (TaskFinished result,tst)
			UserActions actions
				# (mbResult,tst)		= getActionResult actions tst
				= case mbResult of
					Just result
						= (TaskFinished result,tst)
					Nothing
						# (mbEdit,tst)	= getEditEvent tst
						# tst			= setInteractionFuncs (tuiFunc local mbEdit actions,jsonFunc) tst
						= (TaskBusy,tst)
	where
		tuiFunc local mbEdit actions iworld
			# (Ok model,iworld)			= readShared shared iworld
			# (localTimestamp,iworld)	= getLocalTimestamp taskNr iworld
			# (Ok changed,iworld)		= isSharedChanged shared localTimestamp iworld
			# parts						= partFunc local model changed
			# (oldVs,iworld)			= readViews taskNr local iworld
			# (tui,newVs)				= visualizeParts taskNr parts oldVs mbEdit
			# iworld					= writeViews taskNr (zip2 newVs parts) iworld
			# buttons					= map (appSnd isJust) actions
			# (mbConflict,iworld)		= getTaskStoreFor taskNr EDIT_CONFLICT_STORE iworld
			# iworld					= setTaskStoreFor taskNr EDIT_CONFLICT_STORE False iworld
			# warning = case mbConflict of
				Just True				= Just EDIT_CONFLICT_WARNING
				_						= Nothing
			= (tui,buttons,warning,iworld)
	
		jsonFunc iworld = (JSONNull,iworld)
	
	readViews taskNr local iworld
		# (mbVs,iworld) = getTaskStoreFor taskNr VIEWS_STORE iworld
		= case mbVs of
			Just views
				= (views,iworld)
			Nothing
				# (model,iworld)= readShared shared iworld
				| isError model	= ([],iworld)
				= ([(Nothing,part) \\ part <- partFunc local (fromOk model) True],iworld)
			
	readLocalState tst
		# (mbL,tst) = getTaskStore LOCAL_STORE tst
		= (fromMaybe initLocal mbL,tst)
		
	getLocalTimestamp taskNr iworld=:{IWorld|timestamp}
		# (mbTs,iworld)	= getTaskStoreFor taskNr LAST_EDIT_STORE iworld
		= (fromMaybe (Timestamp 0) mbTs,iworld)
	
	updateStates (local,mbModel) tst
		# tst = setTaskStore LOCAL_STORE local tst
		= case mbModel of
			Just model	= snd (accIWorldTSt (writeShared shared model) tst)
			Nothing		= tst

interactLocal :: !d !(l -> [InteractionPart l]) !(l -> InteractionTerminators a) !l -> Task a | descr d & iTask l & iTask a
interactLocal d partFunc termFunc l = LocalInteractionTask @>> interact d (\l _ _ -> map toSharedRes (partFunc l)) (\l _ _ -> termFunc l) l nullShared`
where
	toSharedRes (UpdateView (formView,putback))	= UpdateView (formView,\mbV -> (putback mbV,Nothing))
	toSharedRes (Update label l)				= Update label (l,Nothing)
	toSharedRes (DisplayView v)					= DisplayView v
	
	nullShared` :: SymmetricShared Void
	nullShared` = nullShared
	
:: Views a :== [(!Maybe (!JSONNode,!UpdateMask,!VerifyMask),!InteractionPart a)]
	
writeViews :: !TaskNr !(Views a) !*IWorld -> *IWorld | JSONEncode{|*|}, JSONDecode{|*|}, TC a
writeViews taskNr views iworld = setTaskStoreFor taskNr VIEWS_STORE views iworld

getEdit tst
	# (mbEdit,tst)		= getEditEvent tst
	= case mbEdit of
		Nothing			= (Nothing,tst)
		Just (editDp,val)
			# dpList	= reverse (dataPathList editDp)
			# idx		= hd dpList
			# dp		= dataPathFromList (reverse (tl dpList))
			= (Just (idx,dp,val),tst)

getActionResult :: ![(!Action,!Maybe a)] !*TSt -> (!Maybe a,!*TSt)
getActionResult actions tst
	# (mbActionEvent,tst) = getActionEvent tst
	# mbRes = case mbActionEvent of
		Nothing		= Nothing
		Just name 	= listToMaybe (catMaybes [result \\ (action,result) <- actions | actionName action == name])
	= (mbRes,tst)
		
visualizeParts :: !TaskNr ![InteractionPart w] !(Views w) !(Maybe (!DataPath,!JSONNode)) -> (![TUIDef],![Maybe (!JSONNode,!UpdateMask,!VerifyMask)])
visualizeParts taskNr parts oldVs mbEdit
	# res			= [visualizePart (part,mbV,idx) \\ part <- parts & (mbV,_) <- (oldVs ++ repeat (Nothing,undef)) & idx <- [0..]]
	= unzip res
where
	visualizePart (part,mbV,idx)
		= case part of
			UpdateView (formView,putback) = case formView of
				FormValue value
					# umask				= defaultMask value
					# vmask				= verifyForm value umask
					# tui				= visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit
					= (tui,Just (toJSON value,umask,vmask))
				Unchanged init = case mbV of
					Nothing				= visualizePart (UpdateView (init,putback),mbV,idx)
					Just (jsonV,_,vmask) = case fromJSON` formView jsonV of
						Nothing			= visualizePart (UpdateView (init,putback),mbV,idx)
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

VIEWS_STORE				:== "views"
LOCAL_STORE				:== "local"
LAST_EDIT_STORE			:== "lastEdit"
EDIT_CONFLICT_STORE		:== "editConflict"
EDIT_CONFLICT_WARNING	:== "An edit conflict occurred. The form was refreshed with the most recent value."

// auxiliary types/function for derived interaction tasks
always :: (Verified a) -> Bool
always _ = True

ifvalid :: !(Verified a) -> Bool
ifvalid (Valid _) 	= True
ifvalid _			= False 

ifinvalid :: !(Verified a) -> Bool
ifinvalid Invalid	= True
ifinvalid _			= False

alwaysShared :: (Valid,a) -> Bool
alwaysShared _ = True

ifvalidShared :: !(!Valid,a) -> Bool
ifvalidShared (valid,_) = valid

ifinvalidShared :: !(!Valid,a) -> Bool
ifinvalidShared (valid,a) = not valid

mb2Ver :: !(Maybe a) -> Verified a
mb2Ver mb = maybe Invalid (\v -> Valid v) mb

ver2Mb :: !(Verified a) -> Maybe a
ver2Mb ver = case ver of
	Invalid	= Nothing
	Valid v	= Just v

okAction :: !(Maybe a) -> InteractionTerminators a
okAction r = UserActions [(ActionOk,r)]

addAbout :: !(Maybe about) ![InteractionPart o] -> [InteractionPart o] | iTask about
addAbout mbAbout parts = maybe parts (\about -> [DisplayView about:parts]) mbAbout

fromPredActions :: !(l r Bool -> p) !(Action l r Bool -> a) ![PredAction p] -> (l r Bool -> InteractionTerminators a)
fromPredActions toP toR actions = \l r c -> UserActions (map (\(a,pred) -> (a,if (pred (toP l r c)) (Just (toR a l r c)) Nothing)) actions)

fromPredActionsLocal :: !(l -> p) !(Action l -> a) ![PredAction p] -> (l -> InteractionTerminators a)
fromPredActionsLocal toP toR actions = \l -> UserActions (map (\(a,pred) -> (a,if (pred (toP l)) (Just (toR a l)) Nothing)) actions)