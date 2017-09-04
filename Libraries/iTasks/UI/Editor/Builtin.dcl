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

/**
* Basic textfield
* Supported attributes:
*/
textField     :: UIAttributes -> Editor String
/**
* Multiple line text area 
* Supported attributes:
*/
textArea      :: UIAttributes -> Editor String
/**
* Password field that hides what you type
* Supported attributes:
*/
passwordField :: UIAttributes -> Editor String
/**
* Textfield that only allows you to enter integer numbers
* Supported attributes:
*/
integerField  :: UIAttributes -> Editor Int
/**
* Textfield that only allows you to enter decimal (or integer) numbers
* Supported attributes:
*/
decimalField  :: UIAttributes -> Editor Real
/**
* Form field that allows you to upload files
* Supported attributes:
*/
documentField :: UIAttributes -> Editor (!String,!String,!String,!String,!Int)
/**
* Simple checkbox
* Supported attributes:
*/
checkBox      :: UIAttributes -> Editor Bool
/**
* Slider for integer values in a limited range
* Supported attributes:
*/
slider        :: UIAttributes -> Editor Int
/**
* A basic clickable button
* Supported attributes:
*/
button        :: UIAttributes -> Editor Bool
/**
* A plain text label
* Supported attributes:
*/
label         :: UIAttributes -> Editor String
/**
* A small icon with a tooltip
* Supported attributes:
*/
icon          :: UIAttributes -> Editor (!String,!Maybe String)

// ## Display components ##
// UITextView, UIHtmlView, UIProgressBar

/**
* A component that displays arbitrary text (html is automatically escaped)
* Supported attributes:
*/
textView      :: UIAttributes -> Editor String
/**
* A component that displays arbitrary HTML (nothing is escaped)
* Supported attributes:
*/
htmlView      :: UIAttributes -> Editor HtmlTag
/**
* A progress bar with a percentage and a description of the progress
* Supported attributes:
*/
progressBar   :: UIAttributes -> Editor (Maybe Int,Maybe String) //Percentage, description

// ## Selection components ## 
// UIDropdown, UIRadioGroup, UICheckboxGroup, UIChoiceList, UIGrid, UITree
/**
* A dropdown box
* Supported attributes:
*/
dropdown      :: UIAttributes -> Editor ([ChoiceText], [Int])
/**
* A group of checkboxes or radiobuttons depending on whether the multiple 
* attribute is set or not
* Supported attributes:
*/
checkGroup    :: UIAttributes -> Editor ([ChoiceText], [Int])
/**
* A list of text items to choose from
* Supported attributes:
*/
choiceList    :: UIAttributes -> Editor ([ChoiceText], [Int])
/**
* A typical grid component with a header
* Supported attributes:
*/
grid          :: UIAttributes -> Editor (ChoiceGrid,   [Int])
/**
* A typical tree selection component with expanding "folders"
* Supported attributes:
*/
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
