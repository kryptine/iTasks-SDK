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

checkBox     :: Editor Bool
textArea     :: Editor String
slider       :: Editor Int
dropdownBox  :: Editor String
progressBar  :: Editor Int

textView :: Editor String
htmlView :: Editor HtmlTag
icon     :: Editor String
