definition module ExperimentalDomain

import GenPrint, GenParse, GenUpdate, GenVisualize, GenMerge, GenCopy

derive gPrint			FormattedText, SourceCode, Color
derive gParse			FormattedText, SourceCode, Color
derive gVisualize		FormattedText, SourceCode, Color
derive gUpdate			FormattedText, SourceCode, Color
derive gMerge			FormattedText, SourceCode, Color
derive gMakeSharedCopy	FormattedText, SourceCode, Color
derive gMakeLocalCopy	FormattedText, SourceCode, Color
derive gError			FormattedText, SourceCode, Color
derive gHint			FormattedText, SourceCode, Color

// Html-formatted text
:: FormattedText = FormattedText !String !FormattedTextControls
:: FormattedTextControls =
	{ alignmentControls	:: !Bool // Enable the left, center, right alignment buttons
	, colorControls		:: !Bool // Enable the fore/highlight color buttons
	, fontControl		:: !Bool // Enable font selection
	, fontSizeControls	:: !Bool // Enable the increase/decrease font size buttons
	, formatControls	:: !Bool // Enable the bold, italic and underline buttons
	, linkControl		:: !Bool // Enable the create link button
	, listControls		:: !Bool // Enable the bullet and numbered list buttons
	, sourceEditControl	:: !Bool // Enable the switch to source edit button
	}

/**
* Creates empty formatted text.
*
* @param The controls shown by the user interface when editing the text.
* @return Created formatted text
*/
mkEmptyFormattedText :: !FormattedTextControls -> FormattedText

/**
* Creates formatted text with given html source.
*
* @param Html source
* @param The controls shown by the user interface when text is edited
* @return Created formatted text
*/
mkFormattedText :: !String !FormattedTextControls -> FormattedText

/**
* Set html source of formatted text.
*
* @param Html source
* @param Formatted text to change
* @return Changed formatted text
*/
setFormattedTextSrc :: !String !FormattedText -> FormattedText

/**
* Get html source of formatted text.
*
* @param Formatted text
* @return Html source
*/
getFormattedTextSrc :: !FormattedText -> String

// all possible controls
allControls	:: FormattedTextControls
// no controls
noControls	:: FormattedTextControls

// markers to indicate start/end of selection
// ('\0' normally never occurs in string)
SelectionStartMarker	:== "\0s"
SelectionEndMarker		:== "\0e"

/**
* Converts formatted text to unformatted string.
*
* @param The formatted text
* @param Include selection markers in string
* @return The Unformatted string
*/
toUnformattedString :: !FormattedText !Bool -> String

/**
* Remove selection markers from string.
*
* @param String possibly including selection markers
* @return String without selection markers
*/
removeMarkers :: !String -> String

instance toString FormattedText

// source code
:: SourceCode =	SourceCode !String !SourceCodeLanguage
:: SourceCodeLanguage = JS | CSS | PHP | HTML | XML | Clean

mkSourceCode :: !String !SourceCodeLanguage -> SourceCode
setSource :: !String !SourceCode -> SourceCode
getSource :: !SourceCode -> String

instance toString SourceCode

// colors
:: Color = Color !String

colorBlack		:== Color "000000"
colorRed		:== Color "FF0000"
colorGreen		:== Color "00FF00"
colorBlue		:== Color "0000FF"
colorYellow		:== Color "FFFF00"
colorFuchsia	:== Color "FF00FF"
colorAqua		:== Color "00FFFF"
colorPurple		:== Color "800080"
colorOrange		:== Color "FF9900"
colorWhite		:== Color "FFFFFF"

instance toString Color