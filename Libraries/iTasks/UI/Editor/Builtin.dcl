definition module iTasks.UI.Editor.Builtin
/**
* This module provides a set of editors that are standard available
* in the client-side UI framework.
*/
from iTasks.UI.Editor import :: Editor
from iTasks.UI.Definition import :: UIAttributes
from Data.Maybe import :: Maybe
from Data.Map import :: Map
from Text.HTML import :: HtmlTag
from Text.JSON import :: JSONNode

// ## Form components ##
// UITextField, UITextArea, UIPasswordField, UIIntegerField, UIDecimalField, UIDocumentField
// UICheckbox, UISlider, UIButton, UILabel, UIIcon

textField     :: UIAttributes -> Editor String
textArea      :: UIAttributes -> Editor String
passwordField :: UIAttributes -> Editor String
integerField  :: UIAttributes -> Editor Int
decimalField  :: UIAttributes -> Editor Real
documentField :: UIAttributes -> Editor (!String,!String,!String,!String,!Int)
checkBox      :: UIAttributes -> Editor Bool
slider        :: UIAttributes -> Editor Int
button        :: UIAttributes -> Editor Bool
label         :: UIAttributes -> Editor String
icon          :: UIAttributes -> Editor (!String,!Maybe String)

// ## Display components ##
// UITextView, UIHtmlView, UIProgressBar
textView      :: UIAttributes -> Editor String
htmlView      :: UIAttributes -> Editor HtmlTag
progressBar   :: UIAttributes -> Editor (Maybe Int,Maybe String) //Percentage, description

// ## Selection components ## 
// UIDropdown, UIRadioGroup, UICheckboxGroup, UIChoiceList, UIGrid, UITree
dropdown      :: UIAttributes -> Editor ([ChoiceText], [Int])
checkGroup    :: UIAttributes -> Editor ([ChoiceText], [Int])
choiceList    :: UIAttributes -> Editor ([ChoiceText], [Int])
grid          :: UIAttributes -> Editor (ChoiceGrid, [Int])
tree          :: UIAttributes -> Editor ([ChoiceNode], [Int])

//Convenient types for describing the values of grids and trees
:: ChoiceText =
	{ id     :: Int
	, text   :: String
    }
:: ChoiceGrid =
	{ header  :: [String]
	, rows    :: [ChoiceRow]
	}
:: ChoiceRow =
	{ id      :: Int
	, cells   :: [HtmlTag]
	}

:: ChoiceNode =
	{ id       :: Int
	, label    :: String
	, icon     :: Maybe String
	, expanded :: Bool
	, children :: [ChoiceNode]
	}
