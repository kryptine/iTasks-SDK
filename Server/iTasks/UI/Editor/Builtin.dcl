definition module iTasks.UI.Editor.Builtin
/**
* This module provides a set of editors that are standard available
* in the client-side UI framework.
*/
import iTasks.UI.Editor
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
slider        :: Editor Int
label         :: Editor String
icon          :: Editor String

// ## Display components ##
// UITextView, UIHtmlView, UIProgressBar
textView      :: Editor String
htmlView      :: Editor HtmlTag
progressBar   :: Editor Int

// ## Selection components ## 

dropdownBox   :: Editor ([String],Maybe Int)
radioGroup    :: Editor ([String],Maybe Int)

choiceList    :: Editor ([String], Maybe Int)

:: ChoiceGrid =
	{ header  :: [String]
	, rows    :: [[HtmlTag]]
	}

choiceGrid    :: Editor (ChoiceGrid, Maybe Int)

:: ChoiceNode =
	{ id       :: Int
	, label    :: String
	, icon     :: Maybe String
	, expanded :: Bool
	, children :: [ChoiceNode]
	}

choiceTree    :: Editor ([ChoiceNode], Maybe Int)

