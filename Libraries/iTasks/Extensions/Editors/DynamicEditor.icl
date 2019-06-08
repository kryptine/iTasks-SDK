implementation module iTasks.Extensions.Editors.DynamicEditor

import StdEnv => qualified foldl
import StdMisc, Data.Tuple, Text, Data.Maybe, Text.GenPrint
from StdFunc import seq, flip
from Data.Tuple import appFst
import iTasks, iTasks.UI.Definition, iTasks.UI.Editor.Common, iTasks.UI.Editor.Modifiers
import qualified Data.Map as Map
from Data.Func import $
from Data.List import zip3, intersperse
import Data.Functor

:: DynamicCons =
    { consId           :: !DynamicConsId
    , label            :: !String
    , builder          :: !DynamicConsBuilder
    , showIfOnlyChoice :: !Bool
    , useAsDefault     :: !Bool
    , uiAttributes     :: !UIAttributes
    , labels           :: ![Maybe String]
    }

(<<@@@) infixl 2 :: !DynamicCons !DynamicConsOption -> DynamicCons
(<<@@@) cons opt = tunedDynamicConsEditor opt cons

(@@@>>) infixr 2 :: !DynamicConsOption !DynamicCons -> DynamicCons
(@@@>>) opt cons = cons <<@@@ opt

tunedDynamicConsEditor :: !DynamicConsOption !DynamicCons -> DynamicCons
tunedDynamicConsEditor HideIfOnlyChoice cons = {cons & showIfOnlyChoice = False}
tunedDynamicConsEditor UseAsDefault     cons = {cons & useAsDefault = True}
tunedDynamicConsEditor (ApplyCssClasses classes) cons =
	{cons & uiAttributes = 'Map'.union (classAttr classes) cons.uiAttributes}
tunedDynamicConsEditor (AddLabels labels) cons = {cons & labels = labels}

functionCons :: !String !String !a -> DynamicCons | TC a
functionCons consId label func = functionConsDyn consId label (dynamic func)

functionConsDyn :: !String !String !Dynamic -> DynamicCons
functionConsDyn consId label func = { consId           = consId
                                    , label            = label
                                    , builder          = FunctionCons func
                                    , showIfOnlyChoice = True
                                    , useAsDefault     = False
                                    , uiAttributes     = 'Map'.newMap
                                    , labels           = []
                                    }

listCons :: !String !String !([a] -> b) -> DynamicCons | TC a & TC b
listCons consId label func = listConsDyn consId label (dynamic func)

listConsDyn :: !String !String !Dynamic -> DynamicCons
listConsDyn consId label func = { consId           = consId
                                , label            = label
                                , builder          = ListCons func
                                , showIfOnlyChoice = True
                                , useAsDefault     = False
                                , uiAttributes     = 'Map'.newMap
                                , labels           = []
                                }

customEditorCons :: !String !String !(Editor a) -> DynamicCons
                  | TC, JSONEncode{|*|}, JSONDecode{|*|}, gText{|*|} a
customEditorCons consId label editor = { consId           = consId
                                       , label            = label
                                       , builder          = CustomEditorCons editor
                                       , showIfOnlyChoice = True
                                       , useAsDefault     = False
                                       , uiAttributes     = 'Map'.newMap
                                       , labels           = []
                                       }

// TODO: don't use aborts here
toValue :: !(DynamicEditor a) !(DynamicEditorValue a) -> a | TC a
toValue  dynEditor dynEditorValue = case toValueDyn dynEditor dynEditorValue of
    (v :: a^) = v
    _         = abort "corrupt dynamic editor value"

toValueDyn :: !(DynamicEditor a) !(DynamicEditorValue a) -> Dynamic | TC a
toValueDyn (DynamicEditor elements) (DynamicEditorValue cid val) = toValue` (cid, val)
where
    toValue` :: !(!DynamicConsId, !DEVal) -> Dynamic
    toValue` (cid, val) = case val of
        DEApplication args = case cons.builder of
            FunctionCons fbuilder = toValueFunc fbuilder args
            ListCons     lbuilder = toValueList lbuilder args
            _                     = abort "corrupt dynamic editor value"
        DEJSONValue json = case cons.builder of
            CustomEditorCons editor = toValueGen editor json
            _                       = abort "corrupt dynamic editor value"
    where
        (cons, _) = consWithId cid $ consesOf elements

    toValueFunc :: !Dynamic ![(!DynamicConsId, !DEVal)] -> Dynamic
    toValueFunc v [] = v
    toValueFunc f [x : xs] = case (f, toValue` x) of
        (f :: a -> b, x :: a) = toValueFunc (dynamic (f x)) xs
        _                     = abort "corrupt dynamic editor value"

    toValueGen :: (Editor a) !JSONNode -> Dynamic | JSONDecode{|*|}, TC a
    toValueGen editor json = dynamic (fromJSON` editor json)
    where
        fromJSON` :: (Editor a) !JSONNode -> a | JSONDecode{|*|} a
        fromJSON` _ json = fromMaybe (abort "corrupt dynamic editor value") $ fromJSON json

    toValueList :: !Dynamic ![(!DynamicConsId, !DEVal)] -> Dynamic
    toValueList (f :: [a] -> b) [] = dynamic (f [])
    toValueList f args=:[fst : _] = case (f, toValue` fst) of
        (g :: [a] -> b, _ :: a) -> dynamic (g $ fromDynList [toValue` val \\ val <- args])
        _                       -> abort "corrupt dynamic editor value"
    toValueList _ _ = abort "corrupt dynamic editor value"

    fromDynList :: ![Dynamic] -> [a] | TC a
    fromDynList dyns = fromDynList` dyns []
    where
        fromDynList` [] acc = reverse acc
        fromDynList` [(a :: a^) : dyns] acc = fromDynList` dyns [a:acc]
        fromDynList` _ _ = abort "corrupt dynamic editor value"

dynEditorValToString :: !(DynamicEditor a) !(DynamicEditorValue a) -> String
dynEditorValToString (DynamicEditor elements) (DynamicEditorValue cid val) =
	concat $ withCapitalisedFirstLetter $
		dropWhile (\s -> textSize (trim s) == 0) $ reverse [".": dynEditorValToString` (cid, val) []]
where
	withCapitalisedFirstLetter [firstString: rest] = [upperCaseFirst firstString: rest]

    dynEditorValToString` :: !(!DynamicConsId, !DEVal) ![String] -> [String]
    dynEditorValToString` (cid, val) accum = case val of
        DEApplication args = case cons.builder of
            FunctionCons fbuilder = 'StdEnv'.foldl (flip dynEditorValToString`)
                                                   [" ", cons.DynamicCons.label : accum]
                                                   args
            ListCons lbuilder
                # listElStrs = flatten $ intersperse [" ", cons.DynamicCons.label] $
                                                     (\arg -> dynEditorValToString` arg []) <$> reverse args
                = listElStrs ++ [" "] ++ accum
            _ = abort "corrupt dynamic editor value"
        DEJSONValue json = case cons.builder of
            CustomEditorCons editor = [ " ", toStringGen editor json
                                      , " ", cons.DynamicCons.label
                                      : accum
                                      ]
            _ = abort "corrupt dynamic editor value"
    where
        (cons, _) = consWithId cid $ consesOf elements

    toStringGen :: (Editor a) !JSONNode -> String | gText{|*|}, JSONDecode{|*|}  a
    toStringGen editor json = toSingleLineText $ fromJSON` editor json
    where
        fromJSON` :: (Editor a) !JSONNode -> a | JSONDecode{|*|} a
        fromJSON` _ json = fromMaybe (abort "corrupt dynamic editor value") $ fromJSON json

derive class iTask DynamicEditorValue, DEVal

:: E = E.a: E (Editor (DynamicEditorValue a)) & TC a
:: ConsType = Function | List | CustomEditor

derive JSONEncode ConsType
derive JSONDecode ConsType

parametrisedDynamicEditor
	:: !(p -> DynamicEditor a) -> Editor (!p, !DynamicEditorValue a)
	| TC a & gEq{|*|}, JSONEncode{|*|}, JSONDecode{|*|} p
parametrisedDynamicEditor editor
	= compoundEditorToEditor
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
		= appFst
			(fmap $ appSnd3 \st -> (p, st))
			((dynamicCompoundEditor $ editor p).CompoundEditor.onRefresh dp new mbSt childSts vst)

	valueFromState (p, st) childSts
		= (\val -> (p, val)) <$> (dynamicCompoundEditor $ editor p).CompoundEditor.valueFromState st childSts


dynamicEditor :: !(DynamicEditor a) -> Editor (DynamicEditorValue a) | TC a
dynamicEditor dynEditor = compoundEditorToEditor $ dynamicCompoundEditor dynEditor

// Bool element if state indicates whether the type is correct, i.e. the child types are matching
dynamicCompoundEditor
	:: !(DynamicEditor a) -> CompoundEditor (Maybe (!DynamicConsId, !ConsType, !Bool)) (DynamicEditorValue a) | TC a
dynamicCompoundEditor dynEditor=:(DynamicEditor elements)
	| not $ isEmpty duplicateIds
		= abort $ concat ["duplicate cons IDs in dynamic editor: ", printToString duplicateIds, "\n"]
	= {CompoundEditor| genUI = genUI, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
    // conses with optional group labels
    conses :: [(!DynamicCons, !Maybe String)]
    conses = consesOf elements

    duplicateIds = duplicateIds` $ (\(b, _) -> b.consId) <$> conses
    where
		duplicateIds` [] = []
		duplicateIds` [x: xs]
			| isMember x xs = [x: duplicateIds` xs]
			| otherwise     = duplicateIds` xs

    genUI :: !UIAttributes !DataPath !(EditMode (DynamicEditorValue a)) !*VSt
          -> *(!MaybeErrorString (!UI, !Maybe (!DynamicConsId, !ConsType, !Bool), ![EditState]), !*VSt)
    genUI attr dp mode vst=:{VSt|taskId} = case mode of
        Enter = case matchingConses of
            [(onlyChoice, _)] | hideCons
                # (mbUis, _, type, _, vst) = genChildEditors dp onlyChoice.consId Enter vst
                # mbUis = ( \(uis, childSts) -> (uiContainer attr onlyChoice.labels uis, Just (onlyChoice.consId, type, True), [nullState: childSts])
                          ) <$>
                          mbUis
                = (mbUis, vst)
            _ = case filter (\(cons, _) -> cons.useAsDefault) matchingConses of
                [(defaultChoice, _): _]
                    # (mbUis, idx, type, label, vst) = genChildEditors dp defaultChoice.consId Enter vst
                    = case mbUis of
                        Ok (uis, childSts)
                            | hideCons
                                = (Ok (uiContainer attr defaultChoice.labels uis, Just (defaultChoice.consId, type, True), [nullState: childSts]), vst)
                            | otherwise
                                # (consChooseUI, chooseSt) = genConsChooseUI taskId dp (Just idx)
                                = ( Ok ( uiContainer attr [Nothing: defaultChoice.labels] [consChooseUI: uis]
                                       , Just (defaultChoice.consId, type, True)
                                       , [chooseSt: childSts]
                                       )
                                  , vst
                                  )
                        Error e = (Error e, vst)
                _
                    # (consChooseUI, chooseSt) = genConsChooseUI taskId dp Nothing
                    = (Ok (uiContainer attr [] [consChooseUI], Nothing, [chooseSt]), vst)
		Update Undefined = genUI attr dp Enter vst
        Update (DynamicEditorValue cid val)
            # (mbUis, idx, type, label, vst) = genChildEditors dp cid (Update val) vst
			# (cons, _) = consWithId cid matchingConses
            = case mbUis of
                Ok (uis, childSts)
                    | hideCons
                        = (Ok (uiContainer attr cons.labels uis, Just (cid, type, True), [nullState: childSts]), vst)
                    | otherwise
                        # (consChooseUI, chooseSt) = genConsChooseUI taskId dp (Just idx)
                        = (Ok (uiContainer attr [Nothing: cons.labels] [consChooseUI: uis], Just (cid, type, True), [chooseSt: childSts]), vst)
                Error e = (Error e, vst)

        View (DynamicEditorValue cid val)
            # (mbUis, _, type, label, vst) = genChildEditors dp cid (View val) vst
			# (cons, _) = consWithId cid matchingConses
            = case mbUis of
                Ok (uis, childSts)
                    | hideCons
                        = (Ok (uiContainer attr cons.labels uis, Just (cid, type, True), [nullState: childSts]), vst)
                    | otherwise
                        # consChooseUI = uia UITextView $ valueAttr $ JSONString label
                        = (Ok (uiContainer attr [Nothing: cons.labels] [consChooseUI: uis], Just (cid, type, True), [nullState: childSts]), vst)
                Error e = (Error e, vst)

    genConsChooseUI taskId dp mbSelectedCons = (consChooseUI, consChooseSt)
    where
        consOptions = [ JSONObject $ [("id",JSONInt i),("text",JSONString cons.DynamicCons.label)] ++
                                     maybe [] (\label -> [("grouplabel", JSONString label)]) mbGroupLabel
                      \\ (cons, mbGroupLabel) <- matchingConses & i <- [0..]
                      ]
        consChooseUI = uia UIDropdown
                           ( 'Map'.put "width" JSONNull $
                             choiceAttrs taskId (editorId dp) (maybe [] (\x -> [x]) mbSelectedCons) consOptions
                           )
        consChooseSt = LeafState {touched=False,state=maybe JSONNull (\x -> JSONInt x) mbSelectedCons}

    onEdit :: !DataPath
              !(!DataPath, !JSONNode)
              !(Maybe (!DynamicConsId, !ConsType, !Bool))
              ![EditState]
              !*VSt
           -> *( !MaybeErrorString (!UIChange, !Maybe (!DynamicConsId, !ConsType, !Bool), ![EditState])
               , !*VSt
               )
    // new builder is selected: create a UI for the new builder
    onEdit dp ([], JSONArray [JSONInt builderIdx]) _ [_: childrenSts] vst
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
                # uiAttrs = 'Map'.alter (Just o addContainerClass) "class" cons.uiAttributes
                # change = ChangeUI (uncurry SetAttribute <$> 'Map'.toList uiAttrs) (removals ++ inserts)
                # builderChooseState = LeafState {touched = True, state = JSONInt $ length uis}
                = (Ok (change, Just (cons.consId, type, True), [builderChooseState: childSts]), vst)
            Error e = (Error e, vst)
	where
		addContainerClass :: !(Maybe JSONNode) -> JSONNode
		addContainerClass mbJSONClasses = JSONArray [JSONString "itasks-container": otherClasses]
		where
			otherClasses = maybe [] (\(JSONArray classes) -> classes) mbJSONClasses

    // other events targeted directly at this cons
    onEdit dp ([],e) _ [_: childSts] vst
        | e =: JSONNull || e =: (JSONArray []) // A null or an empty array are accepted as a reset events
            //If necessary remove the fields of the previously selected cons
            # change = ChangeUI [] $ removeNChildren $ length childSts
            = (Ok (change, Nothing, [nullState]), vst)
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
				# change = ChangeUI [] ([(argIdx + if hideCons 0 1, ChangeChild change)] ++ mbErrorIconChange)
				// replace state for this child
				= (Ok (change, Just (cid, type, isOk typeIsCorrect), childSts`), vst)
			where
				mbErrorIconChange
					| typeWasCorrect && isError typeIsCorrect =
						[(length childSts, InsertChild errorIcon)]
					with
						errorIcon =
							UI
								UIIcon
								('Map'.union (iconClsAttr "icon-invalid") (tooltipAttr $ fromError typeIsCorrect))
								[]
					| not typeWasCorrect && isOk typeIsCorrect =
						[(length childSts, RemoveChild)]
					| otherwise = []
				typeIsCorrect = childTypesAreMatching cons.builder (drop 1 childSts`)
				childSts` = updateAt (argIdx + 1) childSt childSts
            Error e = (Error e, vst)

    onEdit _ _ _ _ vst = (Error "Invalid edit event for dynamic editor.", vst)

    removeNChildren :: !Int -> [(!Int, !UIChildChange)]
    removeNChildren nrArgs = repeatn nrArgs (1, RemoveChild)

    onRefresh :: !DataPath
                 !(DynamicEditorValue a)
                 !(Maybe (!DynamicConsId, !ConsType, !Bool))
                 ![EditState]
                 !*VSt
              -> *( !MaybeErrorString ( !UIChange
                                      , !Maybe (!DynamicConsId, !ConsType, !Bool)
                                      , ![EditState]
                                      )
                  , !*VSt
                  )
	// TODO: how to get UI attributes?
	// TODO: fine-grained replacement
    onRefresh dp new st childSts vst
		| isNotChanged (valueFromState st childSts) new = (Ok (NoChange, st, childSts), vst)
		= appFst (fmap $ appFst3 ReplaceUI) $ genUI 'Map'.newMap dp (Update new) vst
	where
		isNotChanged (Just (DynamicEditorValue consId val)) (DynamicEditorValue consId` val`) =
			consId == consId` && val === val`
		isNotChanged _ _ = False

    // TODO: accept ID or index
    genChildEditors :: !DataPath !DynamicConsId !(EditMode DEVal) !*VSt
                    -> *(!MaybeErrorString (![UI], ![EditState]), Int, ConsType, String, !*VSt)
    genChildEditors dp cid mode vst= case cons.builder of
        FunctionCons fbuilder
            # (mbUis, vst) = genChildEditors` (reverse $ zip3 vals (childrenEditors fbuilder) [0..]) [] [] vst
            = (mbUis, idx, type, cons.DynamicCons.label, vst)
        where
            genChildEditors` [] accUi accSt vst = (Ok (accUi, accSt), vst)
            genChildEditors` [(mbVal, E editor, i): children] accUi accSt vst =
                case editor.Editor.genUI 'Map'.newMap (dp ++ [i]) (maybe Enter (if viewMode View Update) mbVal) vst of
                    (Ok (ui, st), vst) = genChildEditors` children [ui: accUi] [st: accSt] vst
                    (Error e,     vst) = (Error e, vst)

            vals :: [Maybe (DynamicEditorValue a)]
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

    matchingConses :: [(!DynamicCons, !Maybe String)]
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

    listBuilderEditor :: !Dynamic -> Editor [(!DynamicConsId, !DEVal)]
    listBuilderEditor (lbuilder :: [a] -> b) = listEditor (Just $ const Nothing) True True Nothing childrenEd`
    where
        childrenEd  = childrenEditorList lbuilder
        childrenEd` = bijectEditorValue (\(cid, val)                   -> DynamicEditorValue cid val)
                                        (\(DynamicEditorValue cid val) -> (cid, val))
                                        childrenEd

        // first argument only used for type
        childrenEditorList :: ([a] -> b) -> Editor (DynamicEditorValue a) | TC a
        childrenEditorList _ = dynamicEditor (DynamicEditor elements)
    listBuilderEditor _ = abort "dynamic editors: invalid list builder value"

    uiContainer :: !UIAttributes ![Maybe String] ![UI] -> UI
    uiContainer attr labels uis =
		UI UIRecord attr (withLabels <$> zip2 uis (labels ++ repeat Nothing))
	where
		withLabels :: !(!UI, !Maybe String) -> UI
		withLabels (UI type attrs item, Just label) = UI type ('Map'.union attrs $ labelAttr label) item
		withLabels (ui,                 Nothing)    = ui

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

        childValuesFor :: ![EditState] ![(!DynamicConsId, !DEVal)]
                       -> Maybe [(!DynamicConsId, !DEVal)]
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
							[ "\"", toString (argOf $ typeCodeOfDynamic cons), "\" and \""
							, toString (typeCodeOfDynamic nextArg), "\" cannot be unified."
							]

		childValueOf :: !(!EditState, !E) -> Maybe Dynamic
		childValueOf (state, E editor) = toValueDyn (DynamicEditor elements) <$> editor.Editor.valueFromState state

		argOf :: !TypeCode -> TypeCode
		argOf (TypeApp (TypeApp _ arg) _) = arg
		argOf (TypeScheme _ type)         = argOf type
	// only function conses can have not matching child types
	childTypesAreMatching _ _ = Ok ()

consWithId :: !DynamicConsId ![(!DynamicCons, !Maybe String)] -> (!DynamicCons, !Int)
consWithId cid conses = case filter (\(({consId}, _), _) -> consId == cid) $ zip2 conses [0..] of
    [((cons, _), idx)] = (cons, idx)
    []                 = abort $ concat ["dynamic editor: cons not found: '",   cid, "'\n"]
    _                  = abort $ concat ["dynamic editor: duplicate conses: '", cid, "'\n"]

nullState :: EditState
nullState = LeafState {touched = True, state = JSONNull}

consesOf :: ![DynamicEditorElement] -> [(!DynamicCons, !Maybe String)]
consesOf elements = flatten $ consesOf <$> elements
where
    consesOf :: !DynamicEditorElement -> [(!DynamicCons, !Maybe String)]
    consesOf (DynamicCons cons)              = [(cons, Nothing)]
    consesOf (DynamicConsGroup label conses) = (\cons -> (cons, Just label)) <$> conses
