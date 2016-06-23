definition module iTasks.UI.Editor.Builtin
/**
* This module provides a set of editors that are standard available
* in the client-side UI framework.
*/
import iTasks.UI.Editor
from Text.HTML import :: HtmlTag

textField     :: Editor String
integerField  :: Editor Int
decimalField  :: Editor Real
passwordField :: Editor String

checkBox      :: Editor Bool
textArea      :: Editor String
slider        :: Editor Int
progressBar   :: Editor Int

dropdownBox   :: Editor ([String],Maybe Int)
radioGroup    :: Editor ([String],Maybe Int)

choiceList    ::             Editor ([String], Maybe Int)

:: ChoiceGrid =
	{ header  :: [String]
	, rows    :: [[String]]
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

textView :: Editor String
htmlView :: Editor HtmlTag
icon     :: Editor String
