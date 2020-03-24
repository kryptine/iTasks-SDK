implementation module iTasks.UI.Editor

import StdEnv
import Data.Maybe, Data.Functor, Data.Tuple, Data.Func, Data.Error
import iTasks.Internal.IWorld
import iTasks.Internal.Serialization
import iTasks.UI.Definition, iTasks.WF.Definition
import qualified Data.Map as DM
import Text, Text.GenJSON
import Data.GenEq
import ABC.Interpreter.JavaScript

derive JSONEncode EditState, LeafState, EditMode
derive JSONDecode EditState, LeafState, EditMode
derive gEq        EditState, LeafState

leafEditorToEditor :: !(LeafEditor edit st r w) -> Editor r w | JSONEncode{|*|}, JSONDecode{|*|} st & JSONDecode{|*|} edit
leafEditorToEditor leafEditor = leafEditorToEditor_ JSONEncode{|*|} JSONDecode{|*|} leafEditor

leafEditorToEditor_ :: !(Bool st -> [JSONNode]) !(Bool [JSONNode] -> (Maybe st, [JSONNode])) !(LeafEditor edit st r w)
                    -> Editor r w | JSONDecode{|*|} edit
leafEditorToEditor_ jsonEncode jsonDecode leafEditor =
	{Editor| onReset = onReset, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
	onReset attr dp val vst = mapRes False $ leafEditor.LeafEditor.onReset attr dp val vst

	onEdit dp (tp, jsone) (LeafState {state}) vst = case fromJSON` state of
		Just st = case fromJSON jsone of
			Just e = case leafEditor.LeafEditor.onEdit dp (tp, e) st vst of
				(Ok (ui,st,mbw),vst) = (Ok (ui, LeafState {touched = True, state = toJSON` st}, mbw),vst)
				(Error e,vst) = (Error e,vst)
			_      = (Error ("Invalid edit event for leaf editor: " +++ toString jsone), vst)
		_       = (Error "Corrupt internal state in leaf editor", vst)
	onEdit _ _ _ vst = (Error "Corrupt editor state in leaf editor", vst)

	onRefresh dp val (LeafState leafSt) vst = case fromJSON` leafSt.state of
		Just st = case leafEditor.LeafEditor.onRefresh dp val st vst of
			(Ok (ui,st,mbw),vst) = (Ok (ui, LeafState {touched = leafSt.touched, state = toJSON` st}, mbw),vst)
			(Error e,vst) = (Error e,vst)
		_       = (Error "Corrupt internal state in leaf editor", vst)
	onRefresh _ _ _ vst = (Error "Corrupt editor state in leaf editor", vst)

	valueFromState (LeafState {state}) = case fromJSON` state of
		Just st = case leafEditor.LeafEditor.valueFromState st of
			Just val = Just val
			_        = Nothing
		_       = Nothing
	valueFromState _ = Nothing

	mapRes touched (Ok (ui,st,mbw), vst) = (Ok (ui, LeafState {touched = touched, state = toJSON` st}, mbw), vst)
	mapRes touched (Error e, vst) = (Error e, vst)

	toJSON` x = case (jsonEncode False x) of
		[node] = node
		_      = JSONError

	fromJSON` node = fst (jsonDecode False [node])

compoundEditorToEditor :: !(CompoundEditor st r w) -> Editor r w | JSONDecode{|*|}, JSONEncode{|*|} st
compoundEditorToEditor compoundEditor =
	{Editor| onReset = onReset, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
	onReset attr dp val vst = mapRes $ compoundEditor.CompoundEditor.onReset attr dp val vst

	onEdit dp e (CompoundState jsonSt childSts) vst = case fromJSON jsonSt of
		Just st = case compoundEditor.CompoundEditor.onEdit dp e st childSts vst of
			(Ok (ui, st, childSts, Just w),vst) = (Ok (ui, CompoundState (toJSON st) childSts, Just w), vst)
			(Ok (ui, st, childSts, Nothing),vst) = (Ok (ui, CompoundState (toJSON st) childSts, Nothing), vst)
			(Error e,vst) = (Error e, vst)
		_       = (Error "Corrupt internal state in compound editor", vst)
	onEdit _ _ _ vst = (Error "Corrupt editor state in compound editor", vst)

	onRefresh dp val (CompoundState jsonSt childSts) vst = case fromJSON jsonSt of
		Just st = case compoundEditor.CompoundEditor.onRefresh dp val st childSts vst of
			(Ok (ui, st, childSts,Just w),vst) = (Ok (ui, CompoundState (toJSON st) childSts, Just w), vst)
			(Ok (ui, st, childSts,Nothing),vst) = (Ok (ui, CompoundState (toJSON st) childSts, Nothing), vst)
			(Error e,vst) = (Error e, vst)
		_       = (Error "Corrupt internal state in compound editor", vst)
	onRefresh _ _ _ vst = (Error "Corrupt editor state in compound", vst)

	valueFromState (CompoundState jsonSt childSts) = case fromJSON jsonSt of
		Just st = case compoundEditor.CompoundEditor.valueFromState st childSts of
			Just val = Just val
			_        = Nothing
		_ = Nothing
	valueFromState _ = Nothing

	mapRes (Ok (ui, st, childSts, mbw), vst) = (Ok (ui, CompoundState (toJSON st) childSts, mapMaybe id mbw), vst)
	mapRes (Error e, vst) = (Error e, vst)

editorModifierWithStateToEditor :: !(EditorModifierWithState st r w) -> Editor r w | JSONDecode{|*|}, JSONEncode{|*|} st
editorModifierWithStateToEditor modifier =
	{Editor| onReset = onReset, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
	onReset attr dp val vst = mapRes $ modifier.EditorModifierWithState.onReset attr dp val vst

	onEdit dp e (AnnotatedState jsonSt childSt) vst = case fromJSON jsonSt of
		Just st = case modifier.EditorModifierWithState.onEdit dp e st childSt vst of
			(Ok (ui,st,childSt,Just w),vst) = (Ok (ui, AnnotatedState (toJSON st) childSt, Just w),vst)
			(Ok (ui,st,childSt,Nothing),vst) = (Ok (ui, AnnotatedState (toJSON st) childSt, Nothing),vst)
			(Error e,vst) = (Error e,vst)
		_       = (Error "Corrupt internal state in editor modifier", vst)
	onEdit _ _ _ vst = (Error "Corrupt editor state in editor modifier", vst)

	onRefresh dp val (AnnotatedState jsonSt childSt) vst = case fromJSON jsonSt of
		Just st = case modifier.EditorModifierWithState.onRefresh dp val st childSt vst of
			(Ok (ui,st,childSt,Just w),vst) = (Ok (ui, AnnotatedState (toJSON st) childSt, Just w),vst)
			(Ok (ui,st,childSt,Nothing),vst) = (Ok (ui, AnnotatedState (toJSON st) childSt, Nothing),vst)
			(Error e,vst) = (Error e,vst)
		_       = (Error "Corrupt internal state in editor modifier", vst)
	onRefresh _ _ _ vst = (Error "Corrupt editor state in editor modifier", vst)

	valueFromState (AnnotatedState jsonSt childSt) = case fromJSON jsonSt of
		Just st = case modifier.EditorModifierWithState.valueFromState st childSt of
			Just val = Just val
			_        = Nothing
		_ = Nothing
	valueFromState _ = Nothing

	mapRes (Ok (ui, st, childSt, mbw), vst) = (Ok (ui, AnnotatedState (toJSON st) childSt, mapMaybe id mbw), vst)
	mapRes (Error e, vst) = (Error e, vst)

	mapResWrite :: !(!MaybeErrorString (!ui, !st, !EditState,!Maybe w), !*VSt) -> (!MaybeErrorString (!ui, !EditState, !Maybe w), !*VSt)
	        | JSONEncode{|*|} st
	mapResWrite (mbRes, vst) = ((\(ui, st, childSt,mbw) -> (ui, AnnotatedState (toJSON st) childSt, mbw)) <$> mbRes, vst)

editModeValue :: !(EditMode a) -> Maybe a
editModeValue Enter        = Nothing
editModeValue (Update val) = Just val
editModeValue (View   val) = Just val

mapEditMode :: .(.x -> .y) !(EditMode .x) -> EditMode .y
mapEditMode _ Enter      = Enter
mapEditMode f (Update x) = Update $ f x
mapEditMode f (View x)   = View   $ f x

derive bimap EditMode

withVSt :: !TaskId !.(*VSt -> (a, *VSt)) !*IWorld -> (!a, !*IWorld)
withVSt taskId f iworld=:{IWorld| abcInterpreterEnv}
	# (x, vst) = f { VSt
	               | taskId            = toString taskId
	               , optional          = False
	               , selectedConsIndex = -1
	               , pathInEditMode    = abort "VSt.dataPathInEditMode should be set by OBJECT instance of gEditor"
	               , abcInterpreterEnv = abcInterpreterEnv
	               }
	= (x, iworld)

newLeafState :: EditState
newLeafState = LeafState {LeafState|touched=False,state=JSONNull}

editorId :: !DataPath -> String
editorId dp = "v" + join "-" (map toString dp)

s2dp :: !String -> DataPath
s2dp str 
	| textSize str < 2	= []
						= map toInt (split "-" (subString 1 (textSize str) str))

isTouched :: !EditState -> Bool
isTouched (LeafState      {LeafState|touched}) = touched
isTouched (CompoundState  _ childSts)          = or (map isTouched childSts)
isTouched (AnnotatedState _ childSt)           = isTouched childSt

isCompound :: !EditState -> Bool
isCompound (LeafState _)              = False
isCompound (AnnotatedState _ childSt) = isCompound childSt
isCompound (CompoundState _ _)        = True

withClientSideInit ::
	!(JSVal *JSWorld -> *JSWorld)
	!(UIAttributes DataPath a *VSt -> *(*MaybeErrorString (!UI, !st, !*Maybe w), *VSt))
	!UIAttributes !DataPath !a !*VSt -> *(!*MaybeErrorString (!UI, !st, !*Maybe w), !*VSt)
withClientSideInit initUI onReset attr dp val vst=:{VSt| taskId} = case onReset attr dp val vst of
	(Ok (UI type attr items,mask, mbw),vst)
		# (initUI, vst) = serializeForClient (wrapInitFunction initUI) vst
		# extraAttr = 'DM'.fromList
			[("taskId",  JSONString taskId)
			,("editorId",JSONString (editorId dp))
			,("initUI",  JSONString initUI)
			]
		= (Ok (UI type ('DM'.union extraAttr attr) items,mask,mbw), vst)
	e = e
