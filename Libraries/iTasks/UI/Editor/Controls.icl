implementation module iTasks.UI.Editor.Controls

import StdEnv
import iTasks.UI.Definition, iTasks.UI.Editor
import Data.GenEq, Data.Error, Text.GenJSON, Text.HTML, Data.Func, Data.Functor, Data.Tuple, Data.List, Data.Maybe, Data.Map.GenJSON
import qualified Data.Map as DM

import iTasks.WF.Derives
import iTasks.UI.Definition
import iTasks.UI.Editor.Modifiers

disableOnView e = selectByMode (e <<@ enabledAttr False) e e

textField :: Editor String String
textField
	= mapEditorWrite (fromMaybe "")
	$ fieldComponent UITextField (Just "") isValidString

textArea :: Editor String String
textArea
	= mapEditorWrite (fromMaybe "")
	$ fieldComponent UITextArea (Just "") isValidString

passwordField :: Editor String String
passwordField
	= mapEditorWrite (fromMaybe "")
	$ fieldComponent UIPasswordField (Just "") isValidString

isValidString :: !UIAttributes !String -> Bool
isValidString attrs str
	= lStr >= getLengthAttr 0 "minlength" && lStr <= getLengthAttr lStr "maxlength"
where
	getLengthAttr :: !Int !String -> Int
	getLengthAttr default attr = case 'DM'.get attr attrs of
		Just (JSONInt l) = l
		_                = default

	lStr = size str

integerField :: Editor Int (Maybe Int)
integerField = fieldComponent UIIntegerField Nothing valid
where
	valid :: UIAttributes Int -> Bool
	valid _ _ = True

decimalField :: Editor Real (Maybe Real)
decimalField = fieldComponent UIDecimalField Nothing (\_ _ -> True)

documentField :: Editor (String,String,String,String,Int) (Maybe (String,String,String,String,Int))
documentField = fieldComponent UIDocumentField Nothing (\_ _ -> True)

checkBox :: Editor Bool Bool
checkBox
	= mapEditorWrite (fromMaybe False)
	$ fieldComponent UICheckbox (Just False) (\_ _ -> True)

slider :: Editor Int Int
slider
	= mapEditorWrite (fromMaybe 0)
	$ fieldComponent UISlider (Just 50) (\_ _ -> True)

button :: Editor Bool Bool
button
	= mapEditorWrite (fromMaybe False)
	$ fieldComponent UIButton Nothing (\_ _ -> True)

label :: Editor String a
label = viewComponent textAttr UILabel

icon :: Editor (String,Maybe String) a
icon = viewComponent (\(iconCls,tooltip) -> 'DM'.unions [iconClsAttr iconCls,maybe 'DM'.newMap tooltipAttr tooltip])
                     UIIcon

textView :: Editor String a
textView = viewComponent (valueAttr o JSONString o escapeForAttribute) UITextView

htmlView :: Editor HtmlTag a
htmlView = viewComponent (valueAttr o JSONString o toString) UIHtmlView

progressBar :: Editor (Maybe Int, Maybe String) a
progressBar = viewComponent combine UIProgressBar
where
	combine (amount,text) =
		'DM'.unions ((maybe [] (\t -> [textAttr t]) text) ++ (maybe [] (\v -> [valueAttr (JSONInt v)]) amount))
						
dropdown :: Editor ([ChoiceText], [Int]) [Int]
dropdown = choiceComponent (const 'DM'.newMap) id toOptionText checkBoundsText UIDropdown

dropdownWithGroups :: Editor ([(ChoiceText, Maybe String)], [Int]) [Int]
dropdownWithGroups = choiceComponent (const 'DM'.newMap) id toOptionText (checkBoundsText o fmap fst) UIDropdown
where
	toOptionText :: !(!ChoiceText, !Maybe String) -> JSONNode
	toOptionText ({ChoiceText|id,text}, groupLabel) =
		JSONObject
			[ ("id",JSONInt id),("text",JSONString text)
			: maybe [] (\label -> [("grouplabel", JSONString label)]) groupLabel
			]

checkGroup :: Editor ([ChoiceText], [Int]) [Int]
checkGroup = choiceComponent (const 'DM'.newMap) id toOptionText checkBoundsText UICheckGroup

choiceList :: Editor ([ChoiceText], [Int]) [Int]
choiceList = choiceComponent (const 'DM'.newMap) id toOptionText checkBoundsText UIChoiceList

tabBar :: Editor ([ChoiceText], [Int]) [Int]
tabBar = choiceComponent (const 'DM'.newMap) id toOptionText checkBoundsText UITabBar

toOptionText {ChoiceText|id,text}= JSONObject [("id",JSONInt id),("text",JSONString text)]
checkBoundsText options idx = or [id == idx \\ {ChoiceText|id} <- options]

derive JSONEncode ChoiceText
derive JSONDecode ChoiceText

grid :: Editor (ChoiceGrid, [Int]) [Int]
grid = choiceComponent (\{ChoiceGrid|header} -> columnsAttr header) (\{ChoiceGrid|rows} -> rows) toOption checkBounds UIGrid
where
	toOption {ChoiceRow|id,cells}= JSONObject [("id",JSONInt id),("cells",JSONArray (map (JSONString o toString) cells))]
	checkBounds options idx = or [id == idx \\ {ChoiceRow|id} <- options]

derive JSONEncode ChoiceGrid, ChoiceRow
derive JSONDecode ChoiceGrid, ChoiceRow

tree :: Editor ([ChoiceNode], [Int]) [Int]
tree = choiceComponent (const 'DM'.newMap) id toOption checkBounds UITree
where
	toOption {ChoiceNode|id,label,icon,expanded,children}
		= JSONObject [("text",JSONString label)
					 ,("iconCls",maybe JSONNull (\i -> JSONString ("icon-"+++i)) icon)
					 ,("id",JSONInt id)
					 ,("expanded",JSONBool expanded)
					 ,("children",JSONArray (map toOption children))
					]

	checkBounds options idx 
		= or (map (checkNode idx) options)
	checkNode idx {ChoiceNode|id,children}
		| idx == id = True
		| otherwise = or (map (checkNode idx) children)

derive JSONEncode ChoiceNode
derive JSONDecode ChoiceNode

withConstantChoices :: !choices !(Editor (!choices, ![Int]) [Int]) -> Editor [Int] [Int]
withConstantChoices choices editor = bijectEditorValue (\sel -> (choices, sel)) snd
                                     (withChangedEditMode editModeFor editor)
where
	// enter mode has to be changed to update mode to pass the choices to the editor
	editModeFor Enter = Update (choices, [])
	editModeFor other = other

//Field like components for which simply knowing the UI type is sufficient
fieldComponent
	:: !UIType !(Maybe a) !(UIAttributes a -> Bool) -> Editor a (Maybe a)
	| JSONDecode{|*|}, JSONEncode{|*|}, gEq{|*|} a
fieldComponent type mbEditModeInitValue isValid = disableOnView $ editorWithJSONEncode (leafEditorToEditor o leafEditor)
where 
	leafEditor toJSON
		= {LeafEditor|genUI=genUI toJSON,onEdit=onEdit,onRefresh=onRefresh toJSON,valueFromState=valueFromState}

	genUI toJSON attr dp mode vst=:{VSt|taskId,optional}
		# mbVal   = maybe mbEditModeInitValue Just $ editModeValue mode
		# mbVal   = maybe Nothing (\val -> if (isValid attr val) (Just val) Nothing) mbVal
		# attr    = 'DM'.unions [ optionalAttr optional
		                        , taskIdAttr taskId
		                        , editorIdAttr $ editorId dp
		                        , valueAttr $ maybe JSONNull toJSON mbVal
		                        , attr
		                        ]
		= (Ok (uia type attr, (mbVal, attr)), vst)

	onEdit _ (_, mbVal) (_, attrs) vst
		= (Ok (ChangeUI [SetAttribute "value" valJSON] [], (mbVal`, attrs), Just mbVal`), vst)
	where
		(mbVal`, valJSON) = case mbVal of
			Just val | isValid attrs val = (Just val, toJSON val)
			_                            = (Nothing,  JSONNull)

	onRefresh toJSON dp new (mbOld, attrs) vst
		| mbOld === Just new = (Ok (NoChange, (mbOld, attrs), Nothing), vst)
		| otherwise          = (Ok (ChangeUI [SetAttribute "value" (toJSON new)] [], (if (isValid attrs new) (Just new) Nothing, attrs), Nothing), vst)

	valueFromState (mbVal, _) = mbVal

	editorWithJSONEncode :: !((a -> JSONNode) -> Editor a (Maybe a)) -> Editor a (Maybe a) | JSONEncode{|*|} a
	editorWithJSONEncode genFunc = genFunc toJSON

//Components which cannot be edited 
viewComponent :: !(a -> UIAttributes) !UIType -> Editor a b | JSONEncode{|*|}, JSONDecode{|*|} a
viewComponent toAttributes type = leafEditorToEditor leafEditor
where
	leafEditor = {LeafEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}

	genUI attr dp mode vst = case editModeValue mode of
		Just val = (Ok (uia type ('DM'.union attr $ toAttributes val), val), vst)
		_        = (Error "View components cannot be used in enter mode", vst)

	onEdit _ (_, ()) _ vst = (Error "Edit event for view component",vst)

	onRefresh dp new val vst = (Ok (changes, new, Nothing), vst)
	where
        changes = case setChanges ++ delChanges of
			[]      = NoChange
			changes = ChangeUI changes []

		setChanges = [ SetAttribute key val
		             \\ (key, val) <- 'DM'.toList $ toAttributes new
		             | 'DM'.get key oldAttrs <> Just val
		             ]
		delChanges = [DelAttribute key \\ (key, _) <- 'DM'.toList $ 'DM'.difference oldAttrs newAttrs]

		oldAttrs = toAttributes val
		newAttrs = toAttributes new

	valueFromState val = Just val

//Choice components that have a set of options
choiceComponent :: !(a -> UIAttributes) !(a -> [o]) !(o -> JSONNode) !([o] Int -> Bool) !UIType -> Editor (!a, ![Int]) [Int]
                 | JSONEncode{|*|}, JSONDecode{|*|} a
choiceComponent attr getOptions toOption checkBounds type = disableOnView $
	leafEditorToEditor {LeafEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI attrs dp mode vst=:{VSt|taskId}
		# (mbVal, sel) = maybe (Nothing, []) (appFst Just) $ editModeValue mode
		# attr = 'DM'.unions [attrs, maybe 'DM'.newMap attr mbVal, choiceAttrs taskId (editorId dp) sel $ mbValToOptions mbVal]

		# multiple = maybe False (\(JSONBool b) -> b) ('DM'.get "multiple" attr)
		= (Ok (uia type attr, (mbVal, sel, multiple)), vst)

	onEdit dp (tp, selection) (mbVal, sel, multiple) vst=:{VSt|optional}
		# options = maybe [] getOptions mbVal
		| all (checkBounds options) selection
			= (Ok (NoChange, (mbVal, selection, multiple), Just selection),vst)
		| otherwise
			= (Error ("Choice event out of bounds: " +++ toString (toJSON selection)), vst)

	onRefresh dp (newVal, newSel) (mbOldVal, oldSel, multiple) vst
		//Check options
		# oldOptsJson        = mbValToOptions mbOldVal
		# newOpts            = getOptions newVal
		# newOptsJson        = toOption <$> newOpts
		# cOptions           = if (newOptsJson =!= oldOptsJson)
		                          (ChangeUI [SetAttribute "options" (JSONArray newOptsJson)] [])
		                          NoChange
		//Check selection, if the selection is out of bounds assume the empty selection
		# newSel             = if (all (checkBounds newOpts) newSel) newSel []
		# cSel               = if (newSel =!= oldSel) (ChangeUI [SetAttribute "value" (toJSON newSel)] []) NoChange
		= (Ok (mergeUIChanges cOptions cSel, (Just newVal, newSel, multiple), Nothing),vst)

	valueFromState (Just val, sel, multiple)
		// Non-multi select choice are only valid with a single selected item
		| not multiple && lengthSel > 1 = Nothing
		| otherwise                     = Just (val, sel)
	where
		lengthSel = length sel
	valueFromState _               = Nothing

	mbValToOptions mbVal = toOption <$> maybe [] getOptions mbVal
