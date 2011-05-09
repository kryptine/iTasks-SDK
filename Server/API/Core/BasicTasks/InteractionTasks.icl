implementation module InteractionTasks

import StdList, StdBool, StdInt, StdTuple, Util, HtmlUtil, Error, StdMisc
import iTaskClass, Task, TSt, CoreCombinators, Shared, ExceptionCombinators, TuningCombinators
from StdFunc		import o
from SharedTasks	import createSharedStore
from iTasks			import dynamicJSONEncode, dynamicJSONDecode

derive JSONEncode UpdateMask, VerifyMask, ErrorMessage
derive JSONDecode UpdateMask, VerifyMask, ErrorMessage
JSONEncode{|InteractivePart|} _ ip		= dynamicJSONEncode ip
JSONDecode{|InteractivePart|} _ [json:r]= (dynamicJSONDecode json,r)
JSONDecode{|InteractivePart|} _ _ 		= (Nothing,[])

derive bimap Maybe,(,)

interact :: !d !(l r Bool -> [InteractivePart (!l,!Maybe w)]) !(l r -> InteractiveTerminators a) !l !(Shared r w) -> Task a | descr d & iTask l & iTask a & iTask w
interact description partFunc termFunc initLocal shared = mkInteractiveTask description (editorE,editorC)
where
	editorE tst=:{taskNr}
		# (mbEdit,tst)					= getEdit tst
		= case mbEdit of
			Nothing						= tst
			Just (idx,dp,editV)
				// check if part with target idx is present & update(view) part
				# (local,tst)			= readLocalState tst
				# (views,tst)			= accIWorldTSt (readViews taskNr local) tst
				| length views <= idx	= tst
				# (mbV,part)			= views !! idx
				= case part of
					UpdateView (_,putback)
						| isNothing mbV			= tst
						# (jsonV,umask,_)		= fromJust mbV
						# value					= fromJSON jsonV
						| isNothing value		= tst
						// update value & masks
						# (value,umask,iworld)	= updateValueAndMask dp editV (fromJust value) umask tst.TSt.iworld
						# (vmask,iworld)		= verifyValue value umask iworld
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
		# (local,tst)					= readLocalState tst
		= case termFunc local (fromOk model) of
			StopInteraction result
				= (TaskFinished result,tst)
			UserActions actions
				# (mbResult,tst)		= getActionResult actions tst
				= case mbResult of
					Just result
						= (TaskFinished result,tst)
					Nothing
						# (mbEdit,tst)	= getEditEvent tst
						# tst			= setInteractiveFuncs (tuiFunc local mbEdit actions,jsonFunc) tst
						= (TaskBusy,tst)
	where
		tuiFunc local mbEdit actions iworld
			# (Ok model,iworld)			= readShared shared iworld
			# (localTimestamp,iworld)	= getLocalTimestamp taskNr iworld
			# (Ok changed,iworld)		= isSharedChanged shared localTimestamp iworld
			# parts						= partFunc local model changed
			# (oldVs,iworld)			= readViews taskNr local iworld
			# (tui,newVs,iworld)		= visualizeParts taskNr parts oldVs mbEdit iworld
			# iworld					= writeViews taskNr (zip2 newVs parts) iworld
			# buttons					= map (appSnd isJust) actions
			= (tui,buttons,iworld)
	
		jsonFunc iworld = (JSONNull,iworld)
	
	readViews taskNr local iworld
		# (mbVs,iworld) = getTaskStoreFor taskNr "views" iworld
		= case mbVs of
			Just views
				= (views,iworld)
			Nothing
				# (model,iworld)= readShared shared iworld
				| isError model	= ([],iworld)
				= ([(Nothing,part) \\ part <- partFunc local (fromOk model) True],iworld)
			
	readLocalState tst
		# (mbL,tst) = getTaskStore "local" tst
		= (fromMaybe initLocal mbL,tst)
		
	getLocalTimestamp taskNr iworld=:{IWorld|timestamp}
		# (mbTs,iworld)	= getTaskStoreFor taskNr "timestamp" iworld
		# iworld		= setTaskStoreFor taskNr "timestamp" timestamp iworld
		= (fromMaybe (Timestamp 0) mbTs,iworld)
	
	updateStates (local,mbModel) tst
		# tst = setTaskStore "local" local tst
		= case mbModel of
			Just model	= snd (accIWorldTSt (writeShared shared model) tst)
			Nothing		= tst

interactLocal :: !d !(l -> [InteractivePart l]) !(l -> InteractiveTerminators a) !l -> Task a | descr d & iTask l & iTask a
interactLocal d partFunc termFunc l = LocalInteractionTask @>> interact d (\l _ _ -> map toSharedRes (partFunc l)) (\l _ -> termFunc l) l nullShared`
where
	toSharedRes (UpdateView (formView,putback))	= UpdateView (formView,\mbV -> (putback mbV,Nothing))
	toSharedRes (Update label l)				= Update label (l,Nothing)
	toSharedRes (DisplayView v)					= DisplayView v
	
	nullShared` :: SymmetricShared Void
	nullShared` = nullShared
	
:: Views a :== [(!Maybe (!JSONNode,!UpdateMask,!VerifyMask),!InteractivePart a)]
	
writeViews :: !TaskNr !(Views a) !*IWorld -> *IWorld | JSONEncode{|*|}, JSONDecode{|*|}, TC a
writeViews taskNr views iworld = setTaskStoreFor taskNr "views" views iworld

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
		
visualizeParts :: !TaskNr ![InteractivePart w] !(Views w) !(Maybe (!DataPath,!JSONNode)) !*IWorld -> (![TUIDef],![Maybe (!JSONNode,!UpdateMask,!VerifyMask)],!*IWorld)
visualizeParts taskNr parts oldVs mbEdit iworld
	# (res,iworld)	= mapSt visualizePart [(part,mbV,idx) \\ part <- parts & (mbV,_) <- (oldVs ++ repeat (Nothing,undef)) & idx <- [0..]] iworld
	# (tuis,views)	= unzip res
	= (tuis,views,iworld)
where
	visualizePart (part,mbV,idx) iworld
		= case part of
			UpdateView (formView,_) = case formView of
				FormValue value
					# umask				= defaultMask value
					# (vmask,iworld)	= verifyValue value umask iworld
					# tui				= visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit
					= ((tui,Just (toJSON value,umask,vmask)),iworld)
				Unchanged = case mbV of
					Nothing				= blankForm formView mbEdit iworld
					Just (jsonV,_,vmask) = case fromJSON` formView jsonV of
						Nothing			= blankForm formView mbEdit iworld
						Just value		= ((visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit,mbV),iworld)
				Blank					= blankForm formView mbEdit iworld
			DisplayView v				= ((htmlDisplay Nothing (toString (visualizeAsHtmlDisplay v)),Nothing),iworld)
			Update label _				=	(({ content = TUIButton	{ TUIButton
																	| name			= toString idx
																	, taskId		= taskNrToString taskNr
																	, text			= label
																	, disabled		= False
																	, iconCls		= ""
																	, actionButton	= False
																	}
											, width = Auto, height = Auto, margins = Nothing},Nothing),iworld)
	where
		fromJSON` :: !(FormView v) !JSONNode -> (Maybe v) | JSONDecode{|*|} v
		fromJSON` _ json = fromJSON json
		
		blankForm :: !(FormView v) !(Maybe (!DataPath,!JSONNode)) !*IWorld -> (!(!TUIDef,!Maybe (!JSONNode,!UpdateMask,!VerifyMask)),!*IWorld) | iTask v
		blankForm formView mbEdit iworld
			# value				= defaultValue` formView
			# umask				= Untouched
			# (vmask,iworld)	= verifyValue value umask iworld
			= ((visualizeAsEditor value (taskNrToString taskNr) idx vmask mbEdit,Just (toJSON value,umask,vmask)),iworld)
		
		defaultValue` :: !(FormView v) -> v | gUpdate{|*|} v
		defaultValue` _ = defaultValue
	
sharedException :: !(MaybeErrorString a) -> (TaskResult b)
sharedException err = taskException (SharedException (fromError err))

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

okAction :: !(Maybe a) -> InteractiveTerminators a
okAction r = UserActions [(ActionOk,r)]

addAbout :: !(Maybe about) ![InteractivePart o] -> [InteractivePart o] | iTask about
addAbout mbAbout parts = maybe parts (\about -> [DisplayView about:parts]) mbAbout

fromPredActions :: !(l r -> p) !(Action l r -> a) ![PredAction p] -> (l r -> InteractiveTerminators a)
fromPredActions toP toR actions = \l r -> UserActions (map (\(a,pred) -> (a,if (pred (toP l r)) (Just (toR a l r)) Nothing)) actions)

fromPredActionsLocal :: !(l -> p) !(Action l -> a) ![PredAction p] -> (l -> InteractiveTerminators a)
fromPredActionsLocal toP toR actions = \l -> UserActions (map (\(a,pred) -> (a,if (pred (toP l)) (Just (toR a l)) Nothing)) actions)

:: Valid :== Bool