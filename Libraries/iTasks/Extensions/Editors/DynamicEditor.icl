implementation module iTasks.Extensions.Editors.DynamicEditor

import StdEnv
import Data.Func, Data.Functor, Data.Tuple, Data.List
import qualified Data.Map as Map
import Text, Text.GenPrint
import iTasks, iTasks.UI.Editor.Common

dynamicEditor :: !(DynamicEditor a) -> Editor (DynamicEditorValue a) | TC a
dynamicEditor dynEditor = compoundEditorToEditor $ dynamicCompoundEditor dynEditor

parametrisedDynamicEditor ::
	!(p -> DynamicEditor a) -> Editor (!p, !DynamicEditorValue a) | TC a & gEq{|*|}, JSONEncode{|*|}, JSONDecode{|*|} p
parametrisedDynamicEditor editor =
	compoundEditorToEditor
		{CompoundEditor| genUI = genUI, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
	genUI attr dp mode vst
		= case editModeValue mode of
			Nothing
				= abort "Enter mode not supported by parametrisedDynamicEditor.\n"
			Just (p, _)
				= appFst
					(fmap $ appSnd3 \st -> (p, st))
					((dynamicCompoundEditor $ editor p).CompoundEditor.genUI attr dp (mapEditMode snd mode) vst)

	onEdit dp event (p, mbSt) childSts vst
		= appFst
			(fmap $ appSnd3 \st -> (p, st))
			((dynamicCompoundEditor $ editor p).CompoundEditor.onEdit dp event mbSt childSts vst)

	onRefresh dp (p, new) st=:(p`, mbSt) childSts vst
		| p === p` =
			appFst
				(fmap $ appSnd3 \st -> (p, st))
				((dynamicCompoundEditor $ editor p).CompoundEditor.onRefresh dp new mbSt childSts vst)
		| otherwise =
			appFst
				(fmap $ \(ui, st, childSts) -> (ReplaceUI ui, (p, st), childSts))
				((dynamicCompoundEditor $ editor p).CompoundEditor.genUI 'Map'.newMap dp (Update new) vst)

	valueFromState (p, st) childSts
		= (\val -> (p, val)) <$> (dynamicCompoundEditor $ editor p).CompoundEditor.valueFromState st childSts

// Bool part of result indicates whether the type is correct, i.e. the child types are matching
dynamicCompoundEditor
	:: !(DynamicEditor a) -> CompoundEditor (Maybe (!DynamicConsId, !ConsType, !Bool)) (DynamicEditorValue a) | TC a
dynamicCompoundEditor dynEditor=:(DynamicEditor elements)
	| not $ isEmpty duplicateIds
		= abort $ concat ["duplicate cons IDs in dynamic editor: ", printToString duplicateIds, "\n"]
	= {CompoundEditor| genUI = genUI, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
	// conses with optional group labels
	conses = consesOf elements

	duplicateIds = duplicateIds` $ (\(b, _) -> b.consId) <$> conses
	where
		duplicateIds` :: ![DynamicConsId] -> [DynamicConsId]
		duplicateIds` [] = []
		duplicateIds` [x: xs]
			| isMember x xs = [x: duplicateIds` xs]
			| otherwise     = duplicateIds` xs

	genUI ::
		!UIAttributes !DataPath !(EditMode (DynamicEditorValue a)) !*VSt
		-> *(!MaybeErrorString (!UI, !Maybe (!DynamicConsId, !ConsType, !Bool), ![EditState]), !*VSt)
	genUI attr dp mode vst=:{VSt|taskId} = case mode of
		Enter = case matchingConses of
			[(onlyChoice, _)] | hideCons
				# (mbUis, _, type, _, vst) = genChildEditors dp onlyChoice.consId Enter vst
				# mbUis =
					( \(uis, childSts) ->
						(uiContainer attr uis, Just (onlyChoice.consId, type, True), [nullState: childSts])
					) <$>
						mbUis
				= (mbUis, vst)
			_ = case filter (\(cons, _) -> cons.useAsDefault) matchingConses of
				[(defaultChoice, _): _]
					# (mbUis, idx, type, label, vst) = genChildEditors dp defaultChoice.consId Enter vst
					# attrs = 'Map'.union (withContainerClassAttr defaultChoice.uiAttributes) attr
					= case mbUis of
						Ok (uis, childSts)
							| hideCons =
								( Ok (uiContainer attrs uis, Just (defaultChoice.consId, type, True), [nullState: childSts])
								, vst
								)
							| otherwise
								# (consChooseUI, chooseSt) = genConsChooseUI taskId dp (Just idx)
								= ( Ok ( uiContainer attrs [consChooseUI: uis]
									   , Just (defaultChoice.consId, type, True)
									   , [chooseSt: childSts]
									   )
								  , vst
								  )
						Error e = (Error e, vst)
				_
					# (consChooseUI, chooseSt) = genConsChooseUI taskId dp Nothing
					= (Ok (uiContainer attr [consChooseUI], Nothing, [chooseSt]), vst)
		Update Undefined = genUI attr dp Enter vst
		Update (DynamicEditorValue cid val)
			# (mbUis, idx, type, label, vst) = genChildEditors dp cid (Update val) vst
			# (cons, _)                      = consWithId cid matchingConses
			= case mbUis of
				Ok (uis, childSts)
					# attrs = 'Map'.union (withContainerClassAttr cons.uiAttributes) attr
					| hideCons
						= (Ok (uiContainer attrs uis, Just (cid, type, True), [nullState: childSts]), vst)
					| otherwise
						# (consChooseUI, chooseSt) = genConsChooseUI taskId dp (Just idx)
						=
							( Ok (uiContainer attrs [consChooseUI: uis], Just (cid, type, True), [chooseSt: childSts])
							, vst
							)
				Error e = (Error e, vst)

		View (DynamicEditorValue cid val)
			# (mbUis, _, type, label, vst) = genChildEditors dp cid (View val) vst
			# (cons, _)                    = consWithId cid matchingConses
			= case mbUis of
				Ok (uis, childSts)
					# attrs = 'Map'.union (withContainerClassAttr cons.uiAttributes) attr
					| hideCons
						= (Ok (uiContainer attrs uis, Just (cid, type, True), [nullState: childSts]), vst)
					| otherwise
						# consChooseUI = uia UITextView $ valueAttr $ JSONString label
						=
							( Ok (uiContainer attrs [consChooseUI: uis], Just (cid, type, True), [nullState: childSts])
							, vst
							)
				Error e = (Error e, vst)

	genConsChooseUI :: !String !DataPath !(Maybe Int) -> (!UI, !EditState)
	genConsChooseUI taskId dp mbSelectedCons = (consChooseUI, consChooseSt)
	where
		consOptions =
			[ JSONObject $
					[("id", JSONInt i), ("text", JSONString cons.DynamicCons.label)]
				++
					maybe [] (\label -> [("grouplabel", JSONString label)]) mbGroupLabel
			\\ (cons, mbGroupLabel) <- matchingConses & i <- [0..]
			]
		consChooseUI =
			uia
				UIDropdown
				( 'Map'.put
					"width"
					JSONNull
					(choiceAttrs taskId (editorId dp) (maybe [] (\x -> [x]) mbSelectedCons) consOptions)
				)
		consChooseSt = LeafState {touched=False,state=maybe JSONNull (\x -> JSONInt x) mbSelectedCons}

	onEdit ::
		!DataPath
		!(!DataPath, !JSONNode)
		!(Maybe (!DynamicConsId, !ConsType, !Bool))
		![EditState]
		!*VSt
		-> *(!MaybeErrorString (!UIChange, !Maybe (!DynamicConsId, !ConsType, !Bool), ![EditState]) , !*VSt)
	// new builder is selected: create a UI for the new builder
	onEdit dp ([], JSONArray [JSONInt builderIdx]) st [_: childrenSts] vst
		| builderIdx < 0 || builderIdx >= length matchingConses
			= (Error "Dynamic editor selection out of bounds", vst)
		# (cons, _) = matchingConses !! builderIdx
		# (mbRes, _, type, _, vst) = genChildEditors dp cons.consId Enter vst
		= case mbRes of
			Ok (uis, childSts)
				// insert new UIs for arguments
				# inserts = [(i, InsertChild ui) \\ ui <- uis & i <- [1..]]
				# removals = removeNChildren $ length childrenSts
				// add "itasks-container" classes as this class always has to be present for containers
				# uiAttrs = withContainerClassAttr cons.uiAttributes
				# attrChange  = if (typeWasInvalid st) removeErrorIconAttrChange []
				# childChange =
						if (typeWasInvalid st) removeErrorIconChange []
					++
						[	( 0
							, ChangeChild $
								ChangeUI (uncurry SetAttribute <$> 'Map'.toList uiAttrs) (removals ++ inserts)
							)
						]
				# builderChooseState = LeafState {touched = True, state = JSONInt $ length uis}
				# change             = ChangeUI attrChange childChange
				# state              = Just (cons.consId, type, True)
				# childStates        = [builderChooseState: childSts]
				= (Ok (change, state, childStates), vst)
			Error e = (Error e, vst)

	// other events targeted directly at this cons
	onEdit dp ([],e) st [_: childSts] vst
		| e =: JSONNull || e =: (JSONArray []) // A null or an empty array are accepted as a reset events
			//If necessary remove the fields of the previously selected cons
			# attrChange  = if (typeWasInvalid st) removeErrorIconAttrChange []
			# childChange =
					if (typeWasInvalid st) removeErrorIconChange []
				++
					[(0, ChangeChild $ ChangeUI [] $ removeNChildren $ length childSts)]
			= (Ok (ChangeUI attrChange childChange, Nothing, [nullState]), vst)
		| otherwise
			= (Error $ concat ["Unknown dynamic editor select event: '", toString e, "'"], vst)

	// update is targeted somewhere inside this value
	onEdit dp ([argIdx: tp], e) (Just (cid, type, typeWasCorrect)) childSts vst
		# (cons, _) = consWithId cid matchingConses
		# (res, vst) = case cons.builder of
			FunctionCons fbuilder
				# children = childrenEditors fbuilder
				| argIdx < 0 || argIdx >= length children
					= (Error "Edit event for dynamic editor has invalid path", vst)
				# (E editor) = children !! argIdx
				= editor.Editor.onEdit (dp ++ [argIdx]) (tp, e) (childSts !! (argIdx + 1)) vst
			ListCons lbuilder
				= (listBuilderEditor lbuilder).Editor.onEdit (dp ++ [0]) (tp, e) (childSts !! 1) vst
			CustomEditorCons editor
				= editor.Editor.onEdit (dp ++ [0]) (tp, e) (childSts !! 1) vst
		= case res of
			Ok (change, childSt)
				# childChange = [(0, ChangeChild $ ChangeUI [] [(argIdx + if hideCons 0 1, ChangeChild change)])]
				# change      = ChangeUI mbErrorIconAttrChange $ childChange ++ mbErrorIconChange
				// replace state for this child
				= (Ok (change, Just (cid, type, isOk typeIsCorrect), childSts`), vst)
			where
				(mbErrorIconChange, mbErrorIconAttrChange) = mbErrorIconUpd
				mbErrorIconUpd
					| typeWasCorrect && isError typeIsCorrect
						# classes =
							JSONString <$> ["itasks-container", "itasks-horizontal", "itasks-dynamic-editor-error"]
						=	( [(1, InsertChild errorIcon)]
							, [SetAttribute "class" $ JSONArray classes]
							)
					with
						errorIcon =
							UI
								UIContainer
								('Map'.singleton "class" $ JSONString "itasks-dynamic-editor-icon-error-container")
								[ UI
									UIIcon
									('Map'.union (iconClsAttr "icon-invalid") (tooltipAttr $ fromError typeIsCorrect))
									[]
								]
					| not typeWasCorrect && isOk typeIsCorrect =
						(removeErrorIconChange, removeErrorIconAttrChange)
					| otherwise = ([], [])
				typeIsCorrect = childTypesAreMatching cons.builder (drop 1 childSts`)
				childSts` = updateAt (argIdx + 1) childSt childSts
			Error e = (Error e, vst)

	onEdit _ _ _ _ vst = (Error "Invalid edit event for dynamic editor.", vst)

	typeWasInvalid :: !(Maybe (!DynamicConsId, !ConsType, !Bool)) -> Bool
	typeWasInvalid (Just (_, _, False)) = True
	typeWasInvalid _                    = False

	removeErrorIconChange     = [(1, RemoveChild)]
	removeErrorIconAttrChange =
		[SetAttribute "class" $ JSONArray [JSONString "itasks-container", JSONString "itasks-horizontal"]]

	// add "itasks-container" classes as this class always has to be present for containers
	withContainerClassAttr :: !(Map String JSONNode) -> Map String JSONNode
	withContainerClassAttr attrs = 'Map'.alter (Just o addContainerClass) "class" attrs
	where
		addContainerClass :: !(Maybe JSONNode) -> JSONNode
		addContainerClass mbJSONClasses = JSONArray [JSONString "itasks-container": otherClasses]
		where
			otherClasses = maybe [] (\(JSONArray classes) -> classes) mbJSONClasses

	removeNChildren :: !Int -> [(Int, UIChildChange)]
	removeNChildren nrArgs = repeatn nrArgs (1, RemoveChild)

	onRefresh ::
		!DataPath
		!(DynamicEditorValue a)
		!(Maybe (!DynamicConsId, !ConsType, !Bool))
		![EditState]
		!*VSt
		-> *(!MaybeErrorString (!UIChange, !Maybe (!DynamicConsId, !ConsType, !Bool), ![EditState]), !*VSt)
	// TODO: how to get UI attributes?
	// TODO: fine-grained replacement
    onRefresh dp new st childSts vst
		| isNotChanged (valueFromState st childSts) new = (Ok (NoChange, st, childSts), vst)
		= appFst (fmap $ appFst3 ReplaceUI) $ genUI 'Map'.newMap dp (Update new) vst
	where
		isNotChanged (Just (DynamicEditorValue consId val)) (DynamicEditorValue consId` val`) =
			consId == consId` && val === val`
		isNotChanged _ _ = False

	genChildEditors ::
		!DataPath !DynamicConsId !(EditMode DEVal) !*VSt
		-> *(!MaybeErrorString (![UI], ![EditState]), Int, ConsType, String, !*VSt)
	genChildEditors dp cid mode vst = case cons.builder of
		FunctionCons fbuilder
			# children     = reverse $ zip4 vals (childrenEditors fbuilder) (cons.labels ++ repeat Nothing) [0..]
			# (mbUis, vst) = genChildEditors` children [] [] vst
			= (mbUis, idx, type, cons.DynamicCons.label, vst)
		where
			genChildEditors` [] accUi accSt vst = (Ok (accUi, accSt), vst)
			genChildEditors` [(mbVal, E editor, mbLabel, i): children] accUi accSt vst =
				case editor.Editor.genUI 'Map'.newMap (dp ++ [i]) (maybe Enter (if viewMode View Update) mbVal) vst of
					(Ok (ui, st), vst) = genChildEditors` children [withLabel mbLabel ui: accUi] [st: accSt] vst
					(Error e,     vst) = (Error e, vst)
			where
				withLabel :: !(Maybe String) !UI -> UI
				withLabel (Just label) (UI type attrs item) = UI type ('Map'.union attrs $ labelAttr label) item
				withLabel Nothing      ui                   = ui

			vals = case editModeValue mode of
				// update or view mode
				Just (DEApplication children) = [Just $ DynamicEditorValue cid val \\ (cid, val) <- children]
				// enter mode
				_                             = repeat Nothing
		ListCons lbuilder
			# listEditorMode = mapEditMode (\(DEApplication listElems) -> listElems) mode
			# (mbUi, vst) = (listBuilderEditor lbuilder).Editor.genUI 'Map'.newMap (dp ++ [0]) listEditorMode vst
			= ((\(ui, st) -> ([ui], [st])) <$> mbUi, idx, type, cons.DynamicCons.label, vst)
		CustomEditorCons editor
			# editorMode = mapEditMode
				(\(DEJSONValue json) -> fromMaybe (abort "Invalid dynamic editor state") $ fromJSON json)
				mode
			# (mbUi, vst) = editor.Editor.genUI 'Map'.newMap (dp ++ [0]) editorMode vst
			= ((\(ui, st) -> ([ui], [st])) <$> mbUi, idx, type, cons.DynamicCons.label, vst)
	where
		(cons, idx) = consWithId cid matchingConses
		type = case cons.builder of
			FunctionCons     _ = Function
			ListCons         _ = List
			CustomEditorCons _ = CustomEditor
		viewMode = mode =: View _
 
	hideCons = case matchingConses of
		[(onlyChoice, _)] | not onlyChoice.showIfOnlyChoice = True
		_                                                   = False

	matchingConses = catMaybes $
		(\(cons, mbGroupLabel) -> (\cons` -> (cons`, mbGroupLabel)) <$> matchingCons dynEditor cons) <$> conses

	// first arg only used for type
	// packs matching conses, with possibly updated (= more specific) type
	matchingCons :: !(DynamicEditor a) !DynamicCons -> Maybe DynamicCons | TC a
	matchingCons dynEd cons=:{builder} = (\b -> {cons & builder = b}) <$> mbBuilder`
	where
		mbBuilder` = case builder of
			FunctionCons     fbuilder = matchf fbuilder
			CustomEditorCons editor   = matchc editor
			ListCons         lbuilder = matchl lbuilder

		// works for functions with upto 10 args
		// the type of the dynamic is updated by unifying the function result with the type produced by the editor
		matchf :: !Dynamic -> Maybe DynamicConsBuilder
		matchf b = case (b, dynamic dynEd) of
			(b :: a b c d e f g h i j -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
			(b :: a b c d e f g h i   -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
			(b :: a b c d e f g h     -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
			(b :: a b c d e f g       -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
			(b :: a b c d e f         -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
			(b :: a b c d e           -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
			(b :: a b c d             -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
			(b :: a b c               -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
			(b :: a b                 -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
			(b :: a                   -> z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
			(b ::                        z, _ :: DynamicEditor z) = Just $ FunctionCons (dynamic b)
			_                                                     = Nothing

		// custom editors do not allow for quantified variables, so no type update is required
		matchc e = case (dynamic e, dynamic dynEd) of
			(_ :: Editor a, _ :: DynamicEditor a) = Just $ CustomEditorCons e
			_                                     = Nothing

		matchl f = case (f, dynamic dynEd) of
			(f :: [a] -> b, _ :: DynamicEditor b) = Just $ ListCons (dynamic f)
			_                                     = Nothing

	listBuilderEditor :: !Dynamic -> Editor [(DynamicConsId, DEVal)]
	listBuilderEditor (lbuilder :: [a] -> b) = listEditor (Just $ const Nothing) True True Nothing childrenEd`
	where
		childrenEd  = childrenEditorList lbuilder
		childrenEd` =
			bijectEditorValue
				(\(cid, val)                   -> DynamicEditorValue cid val)
				(\(DynamicEditorValue cid val) -> (cid, val))
				childrenEd

		// first argument only used for type
		childrenEditorList :: ([a] -> b) -> Editor (DynamicEditorValue a) | TC a
		childrenEditorList _ = dynamicEditor (DynamicEditor elements)
	listBuilderEditor _ = abort "dynamic editors: invalid list builder value"

	uiContainer :: !UIAttributes ![UI] -> UI
	uiContainer attr uis =
		UI
			UIContainer
			('Map'.singleton "class" $ JSONArray [JSONString "itasks-container", JSONString "itasks-horizontal"])
			[UI UIContainer attr uis]

	valueFromState :: !(Maybe (!DynamicConsId, !ConsType, !Bool)) ![EditState] -> *Maybe (DynamicEditorValue a)
	valueFromState (Just (cid, CustomEditor, True)) [_: [editorSt]] =
		mapMaybe (DynamicEditorValue cid o DEJSONValue o toJSON`) $ editor.Editor.valueFromState editorSt
	where
		({builder}, _) = consWithId cid conses

		// toJSON` is used to solve overloading, JSONEncode{|*|} is attached to CustomEditorCons
		(editor, toJSON`) = case builder of
			CustomEditorCons editor = (editor, toJSON)
			_                       = abort "corrupt dynamic editor state"

	valueFromState (Just (cid, type, True)) [_: childSts] =
		mapMaybe (\childVals -> DynamicEditorValue cid $ DEApplication childVals) $ childValuesFor childSts` []
	where
		childSts` = case (type, childSts) of
			(List, [CompoundState _ childSts]) = childSts
			(_,    childSts)                   = childSts

		childValuesFor :: ![EditState] ![(DynamicConsId, DEVal)] -> Maybe [(DynamicConsId, DEVal)]
		childValuesFor [] acc = Just $ reverse acc
		childValuesFor [childSt: childSts] acc = case (dynamicEditor dynEditor).Editor.valueFromState childSt of
			Just (DynamicEditorValue childCid childVal) = childValuesFor childSts [(childCid, childVal): acc]
			_                                           = Nothing
	valueFromState _ _ = Nothing

	childrenEditors :: !Dynamic -> [E]
	childrenEditors (f :: a -> b) = [E $ dynamicEditorFstArg f : childrenEditors (dynamic (f undef))]
	where
		// first argument only used for type
		dynamicEditorFstArg :: (a -> b) -> Editor (DynamicEditorValue a) | TC a
		dynamicEditorFstArg _ = dynamicEditor $ DynamicEditor elements
	childrenEditors _         = []

	childTypesAreMatching :: !DynamicConsBuilder [EditState] -> MaybeErrorString ()
	childTypesAreMatching (FunctionCons cons) childStates =
		childTypesAreMatching` cons (childValueOf <$> zip2 childStates (childrenEditors cons))
	where
		childTypesAreMatching` :: !Dynamic ![Maybe Dynamic] -> MaybeErrorString ()
		childTypesAreMatching` _ [] = Ok ()
		childTypesAreMatching` cons [Nothing: otherArgs] =
			case cons of
				(cons` :: a -> z) = childTypesAreMatching` (dynamic cons` undef) otherArgs
		childTypesAreMatching` cons [Just nextArg: otherArgs] =
			case (cons, nextArg) of
				// `cons` undef` has type z`, which is z updated by unifying the type of the next arg
				(cons` :: a -> z, _ :: a) = childTypesAreMatching` (dynamic cons` undef) otherArgs
				_                         =
					Error $
						concat
							[ "Could not unify\n    ", toString (argOf $ typeCodeOfDynamic cons), "\nwith\n    "
							, toString (typeCodeOfDynamic nextArg)
							]

		childValueOf :: !(!EditState, !E) -> Maybe Dynamic
		childValueOf (state, E editor) =
			valueCorrespondingToDyn (DynamicEditor elements) <$> editor.Editor.valueFromState state

		argOf :: !TypeCode -> TypeCode
		argOf (TypeApp (TypeApp _ arg) _) = arg
		argOf (TypeScheme _ type)         = argOf type
	// only function conses can have not matching child types
	childTypesAreMatching _ _ = Ok ()

valueCorrespondingTo :: !(DynamicEditor a) !(DynamicEditorValue a) -> a | TC a
valueCorrespondingTo  dynEditor dynEditorValue = case valueCorrespondingToDyn dynEditor dynEditorValue of
	(v :: a^) = v
	_         = abort "corrupt dynamic editor value"

stringCorrespondingTo :: !(DynamicEditor a) !(DynamicEditorValue a) -> String
stringCorrespondingTo (DynamicEditor elements) (DynamicEditorValue cid val) =
	concat $ withCapitalisedFirstLetter $
		dropWhile (\s -> textSize (trim s) == 0) $ reverse [".": stringCorrespondingTo` (cid, val) []]
where
	withCapitalisedFirstLetter :: ![String] -> [String]
	withCapitalisedFirstLetter [firstString: rest] = [upperCaseFirst firstString: rest]

	stringCorrespondingTo` :: !(!DynamicConsId, !DEVal) ![String] -> [String]
	stringCorrespondingTo` (cid, val) accum = case val of
		DEApplication args = case cons.builder of
			FunctionCons fbuilder =
				foldl (flip stringCorrespondingTo`) [" ", cons.DynamicCons.label : accum] args
			ListCons lbuilder
				# listElStrs =
					flatten $
						intersperse
							[" ", cons.DynamicCons.label]
							((\arg -> stringCorrespondingTo` arg []) <$> reverse args)
				= listElStrs ++ [" "] ++ accum
			_ = abort "corrupt dynamic editor value"
		DEJSONValue json = case cons.builder of
			CustomEditorCons editor = [ " ", stringCorrespondingToGen editor json
									  , " ", cons.DynamicCons.label
									  : accum
									  ]
			_ = abort "corrupt dynamic editor value"
	where
		(cons, _) = consWithId cid $ consesOf elements

	stringCorrespondingToGen :: (Editor a) !JSONNode -> String | gText{|*|}, JSONDecode{|*|}  a
	stringCorrespondingToGen editor json = toSingleLineText $ fromJSON` editor json
	where
		fromJSON` :: (Editor a) !JSONNode -> a | JSONDecode{|*|} a
		fromJSON` _ json = fromMaybe (abort "corrupt dynamic editor value") $ fromJSON json

:: DynamicCons =
	{ consId           :: !DynamicConsId
	, label            :: !String
	, builder          :: !DynamicConsBuilder
	, showIfOnlyChoice :: !Bool
	, useAsDefault     :: !Bool
	, uiAttributes     :: !UIAttributes
	, labels           :: ![Maybe String]
	}

:: DynamicConsBuilder
	=      FunctionCons     !Dynamic
	| E.a: CustomEditorCons !(Editor a) & JSONEncode{|*|}, JSONDecode{|*|}, gText{|*|}, TC a
	|      ListCons         !Dynamic    //* must contain a value of type [a] -> b

functionCons :: !DynamicConsId !String !a -> DynamicCons | TC a
functionCons consId label func = functionConsDyn consId label (dynamic func)

functionConsDyn :: !DynamicConsId !String !Dynamic -> DynamicCons
functionConsDyn consId label func =
	{ consId           = consId
	, label            = label
	, builder          = FunctionCons func
	, showIfOnlyChoice = True
	, useAsDefault     = False
	, uiAttributes     = 'Map'.newMap
	, labels           = []
	}

listCons :: !DynamicConsId !String !([a] -> b) -> DynamicCons | TC a & TC b
listCons consId label func = listConsDyn consId label (dynamic func)

listConsDyn :: !DynamicConsId !String !Dynamic -> DynamicCons
listConsDyn consId label func =
	{ consId           = consId
	, label            = label
	, builder          = ListCons func
	, showIfOnlyChoice = True
	, useAsDefault     = False
	, uiAttributes     = 'Map'.newMap
	, labels           = []
	}

customEditorCons ::
	!DynamicConsId !String !(Editor a) -> DynamicCons | TC, JSONEncode{|*|}, JSONDecode{|*|}, gText{|*|} a
customEditorCons consId label editor =
	{ consId           = consId
	, label            = label
	, builder          = CustomEditorCons editor
	, showIfOnlyChoice = True
	, useAsDefault     = False
	, uiAttributes     = 'Map'.newMap
	, labels           = []
	}

instance tune DynamicConsOption DynamicCons where
	tune :: !DynamicConsOption !DynamicCons -> DynamicCons
	tune HideIfOnlyChoice          cons = {cons & showIfOnlyChoice = False}
	tune UseAsDefault              cons = {cons & useAsDefault = True}
	tune (ApplyCssClasses classes) cons = {cons & uiAttributes = 'Map'.union (classAttr classes) cons.uiAttributes}
	tune (AddLabels labels)        cons = {cons & labels = labels}

valueCorrespondingToDyn :: !(DynamicEditor a) !(DynamicEditorValue a) -> Dynamic | TC a
valueCorrespondingToDyn (DynamicEditor elements) (DynamicEditorValue cid val) = valueCorrespondingTo` (cid, val)
where
	valueCorrespondingTo` :: !(!DynamicConsId, !DEVal) -> Dynamic
	valueCorrespondingTo` (cid, val) = case val of
		DEApplication args = case cons.builder of
			FunctionCons fbuilder = valueCorrespondingToFunc fbuilder args
			ListCons     lbuilder = valueCorrespondingToList lbuilder args
			_                     = abort "corrupt dynamic editor value"
		DEJSONValue json = case cons.builder of
			CustomEditorCons editor = valueCorrespondingToGen editor json
			_                       = abort "corrupt dynamic editor value"
	where
		(cons, _) = consWithId cid $ consesOf elements

	valueCorrespondingToFunc :: !Dynamic ![(DynamicConsId, DEVal)] -> Dynamic
	valueCorrespondingToFunc v [] = v
	valueCorrespondingToFunc f [x : xs] = case (f, valueCorrespondingTo` x) of
		(f :: a -> b, x :: a) = valueCorrespondingToFunc (dynamic (f x)) xs
		_                     = abort "corrupt dynamic editor value"

	valueCorrespondingToGen :: (Editor a) !JSONNode -> Dynamic | JSONDecode{|*|}, TC a
	valueCorrespondingToGen editor json = dynamic (fromJSON` editor json)
	where
		fromJSON` :: (Editor a) !JSONNode -> a | JSONDecode{|*|} a
		fromJSON` _ json = fromMaybe (abort "corrupt dynamic editor value") $ fromJSON json

	valueCorrespondingToList :: !Dynamic ![(DynamicConsId, DEVal)] -> Dynamic
	valueCorrespondingToList (f :: [a] -> b) [] = dynamic (f [])
	valueCorrespondingToList f args=:[fst : _] = case (f, valueCorrespondingTo` fst) of
		(g :: [a] -> b, _ :: a) -> dynamic (g $ fromDynList [valueCorrespondingTo` val \\ val <- args])
		_                       -> abort "corrupt dynamic editor value"
	valueCorrespondingToList _ _ = abort "corrupt dynamic editor value"

	fromDynList :: ![Dynamic] -> [a] | TC a
	fromDynList dyns = fromDynList` dyns []
	where
		fromDynList` [] acc = reverse acc
		fromDynList` [(a :: a^) : dyns] acc = fromDynList` dyns [a:acc]
		fromDynList` _ _ = abort "corrupt dynamic editor value"

:: E = E.a: E (Editor (DynamicEditorValue a)) & TC a
:: ConsType = Function | List | CustomEditor

consWithId :: !DynamicConsId ![(DynamicCons, Maybe String)] -> (!DynamicCons, !Int)
consWithId cid conses = case filter (\(({consId}, _), _) -> consId == cid) $ zip2 conses [0..] of
	[((cons, _), idx)] = (cons, idx)
	[]                 = abort $ concat ["dynamic editor: cons not found: '",   cid, "'\n"]
	_                  = abort $ concat ["dynamic editor: duplicate conses: '", cid, "'\n"]

nullState :: EditState
nullState = LeafState {touched = True, state = JSONNull}

consesOf :: ![DynamicEditorElement] -> [(DynamicCons, Maybe String)]
consesOf elements = flatten $ consesOf <$> elements
where
	consesOf :: !DynamicEditorElement -> [(DynamicCons, Maybe String)]
	consesOf (DynamicCons cons)              = [(cons, Nothing)]
	consesOf (DynamicConsGroup label conses) = (\cons -> (cons, Just label)) <$> conses

derive class iTask DynamicEditorValue, DEVal
derive JSONEncode ConsType
derive JSONDecode ConsType
