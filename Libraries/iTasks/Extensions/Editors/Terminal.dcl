definition module iTasks.Extensions.Editors.Terminal

from iTasks.UI.Editor import :: Editor
from Text.HTML import :: HtmlTag

/*
 * View the string as a VT100 terminal
 * @param width
 * @param size
 */
terminalRender :: Int Int String -> HtmlTag
