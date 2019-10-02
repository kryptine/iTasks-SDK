definition module iTasks.UI.Editor.Controls
/**
* This module provides a set of editors for builtin controls
* of the client-side UI framework.
*/
from iTasks.UI.Editor import :: Editor
from iTasks.UI.Definition import :: UIAttributes, :: UIType
from Data.Maybe import :: Maybe
from Data.Map import :: Map
from Text.HTML import :: HtmlTag
from Text.GenJSON import :: JSONNode, generic JSONDecode, generic JSONEncode
from Data.GenEq import generic gEq

// ## Form components ##
// UITextField, UITextArea, UIPasswordField, UIIntegerField, UIDecimalField, UIDocumentField
// UICheckbox, UISlider, UIButton, UILabel, UIIcon

/**
* Basic textfield
* Supported attributes:
*/
textField     :: Editor String String
/**
* Multiple line text area 
* Supported attributes:
*/
textArea      :: Editor String String
/**
* Password field that hides what you type
* Supported attributes:
*/
passwordField :: Editor String String
/**
* Textfield that only allows you to enter integer numbers
* Supported attributes:
*/
integerField  :: Editor Int Int
/**
* Textfield that only allows you to enter decimal (or integer) numbers
* Supported attributes:
*/
decimalField  :: Editor Real Real
/**
* Form field that allows you to upload files
* Supported attributes:
*/
documentField :: Editor (!String,!String,!String,!String,!Int) (!String,!String,!String,!String,!Int)
/**
* Simple checkbox
* Supported attributes:
*/
checkBox      :: Editor Bool Bool
/**
* Slider for integer values in a limited range
* Supported attributes:
*/
slider        :: Editor Int Int
/**
* A basic clickable button
* Supported attributes:
*/
button        :: Editor Bool Bool
/**
* A plain text label
* Supported attributes:
*/
label         :: Editor String String
/**
* A small icon with a tooltip
* Supported attributes:
*/
icon          :: Editor (!String,!Maybe String) (!String,!Maybe String)

// ## Display components ##
// UITextView, UIHtmlView, UIProgressBar

/**
* A component that displays arbitrary text (html is automatically escaped)
* Supported attributes:
*/
textView      :: Editor String String
/**
* A component that displays arbitrary HTML (nothing is escaped)
* Supported attributes:
*/
htmlView      :: Editor HtmlTag HtmlTag
/**
* A progress bar with a percentage and a description of the progress
* Supported attributes:
*/
progressBar   :: Editor (Maybe Int,Maybe String) (Maybe Int,Maybe String) //Percentage, description

// ## Selection components ## 
// UIDropdown, UIRadioGroup, UICheckboxGroup, UIChoiceList, UIGrid, UITree
/**
* A dropdown box
* Supported attributes:
*/
dropdown      :: Editor ([ChoiceText], [Int]) [Int]
/**
* A group of checkboxes or radiobuttons depending on whether the multiple 
* attribute is set or not
* Supported attributes:
*/
checkGroup    :: Editor ([ChoiceText], [Int]) [Int]
/**
* A list of text items to choose from
* Supported attributes:
*/
choiceList    :: Editor ([ChoiceText], [Int]) [Int]
/**
* A typical grid component with a header
* Supported attributes:
*/
grid          :: Editor (ChoiceGrid,   [Int]) [Int]
/**
* A typical tree selection component with expanding "folders"
* Supported attributes:
*/
tree          :: Editor ([ChoiceNode], [Int]) [Int]

/**
* A horizontal bar with tabs to make a selection with
*/
tabBar        :: Editor ([ChoiceText], [Int]) [Int]

/**
 * Modifies the above editors for making choices such that they use a constant set of choices.
 */
withConstantChoices :: !choices !(Editor (!choices, ![Int]) [Int]) -> Editor [Int] [Int]

fieldComponent
	:: !UIType !(Maybe a) !(UIAttributes a -> Bool) -> Editor a a
	| JSONDecode{|*|}, JSONEncode{|*|}, gEq{|*|} a

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
