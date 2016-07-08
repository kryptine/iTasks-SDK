definition module iTasks.UI.Editor.Builtin
/**
* This module provides a set of editors that are standard available
* in the client-side UI framework.
*/
import iTasks.UI.Editor
from Data.Maybe import :: Maybe
from Text.HTML import :: HtmlTag
from iTasks.API.Core.Types import :: Document
from iTasks.UI.Definition import :: UIAttributes

// ## Form components ##
// UITextField, UITextArea, UIPasswordField, UIIntegerField, UIDecimalField, UIDocumentField
// UICheckbox, UISlider, UIButton, UILabel, UIIcon

textField     :: UIAttributes -> Editor String
textArea      :: UIAttributes -> Editor String
passwordField :: UIAttributes -> Editor String
integerField  :: UIAttributes -> Editor Int
decimalField  :: UIAttributes -> Editor Real
documentField :: UIAttributes -> Editor Document
checkBox      :: UIAttributes -> Editor Bool
slider        :: UIAttributes -> Editor Int
button        :: UIAttributes -> Editor Bool
label         :: UIAttributes -> Editor String
icon          :: UIAttributes -> Editor (String,Maybe String)

// ## Display components ##
// UITextView, UIHtmlView, UIProgressBar
textView      :: UIAttributes -> Editor String
htmlView      :: UIAttributes -> Editor HtmlTag
progressBar   :: UIAttributes -> Editor (Maybe Int,Maybe String) //Percentage, description

// ## Selection components ## 
// UIDropdown, UIRadioGroup, UICheckboxGroup, UIChoiceList, UIGrid, UITree
dropdown      :: UIAttributes -> Editor ([String], [Int])
checkGroup    :: UIAttributes -> Editor ([String], [Int])
choiceList    :: UIAttributes -> Editor ([String], [Int])
grid          :: UIAttributes -> Editor (ChoiceGrid, [Int])
tree          :: UIAttributes -> Editor ([ChoiceNode], [Int])

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
