definition module iTasks.UI.Editor.Builtin
/**
* This module provides a set of editors that are standard available
* in the client-side UI framework.
*/
import iTasks.UI.Editor
from Data.Maybe import :: Maybe
from Text.HTML import :: HtmlTag
from iTasks.API.Core.Types import :: Document

// ## Form components ##
// UITextField, UITextArea, UIPasswordField, UIIntegerField, UIDecimalField, UIDocumentField
// UICheckbox, UISlider, UIButton, UILabel, UIIcon

textField     :: Editor String
textArea      :: Editor String
passwordField :: Editor String
integerField  :: Editor Int
decimalField  :: Editor Real
documentField :: Editor Document
checkBox      :: Editor Bool
slider        :: Int Int -> Editor Int //Min, max
button        :: String -> Editor Bool
label         :: Editor String
icon          :: Editor (String,Maybe String)

// ## Display components ##
// UITextView, UIHtmlView, UIProgressBar
textView      :: Editor String
htmlView      :: Editor HtmlTag
progressBar   :: Editor (Maybe Int,Maybe String) //Percentage, description

// ## Selection components ## 
// UIDropdown, UIRadioGroup, UICheckboxGroup, UIChoiceList, UIGrid, UITree
dropdown      :: Bool -> Editor ([String], [Int])
checkGroup    :: Bool -> Editor ([String], [Int])
choiceList    :: Bool -> Editor ([String], [Int])
grid          :: Bool -> Editor (ChoiceGrid, [Int])
tree          :: Bool -> Editor ([ChoiceNode], [Int])

:: ChoiceGrid =
	{ header  :: [String]
	, rows    :: [[HtmlTag]]
	}

:: ChoiceNode =
	{ id       :: Int
	, label    :: String
	, icon     :: Maybe String
	, expanded :: Bool
	, children :: [ChoiceNode]
	}
