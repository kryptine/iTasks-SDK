definition module iDataWidgets
/**
* This modules contains a collection of types and their gForm/gUpd instances that can be used
* to construct convenient view types with.
*/

import iDataForms
import Html
import GenLexOrd

derive gForm 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, RefreshTimer
derive gUpd  	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, RefreshTimer
derive gPrint 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, RefreshTimer
derive gParse 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, RefreshTimer
derive gerda 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, RefreshTimer
//derive read 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, RefreshTimer
//derive write 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, RefreshTimer


// Layout types
:: <-> a b		= (<->) infixl 5 a b					// place b to the left of a
:: <|> a b		= (<|>) infixl 4 a b					// place b below a

:: DisplayMode a 
	= DisplayMode a										// non-editable display of a
	| EditMode    a										// editable
	| HideMode    a										// hiding a
	| EmptyMode											// nothing to display or hide

// Buttons representing basic html buttons
:: HtmlButton	= HtmlButton String Bool				// basic button. The String is the label of the button, the Bool parameter indicates if it was clicked.

// Other elementary widgets
:: HtmlCheckbox	= HtmlCheckbox [HtmlTag] Bool			// Checkbox, the html is a label which and the boolean determines if the checkbox is checked or not
:: HtmlSelect	= HtmlSelect [(String,String)] String	// Selectbox, the list contains label, value tuples, the string is the current value

:: HtmlTextarea	= HtmlTextarea Int String				// Text area
:: HtmlPassword	= HtmlPassword String					// Password input

:: HtmlDate 	= HtmlDate Int Int Int					// Day Month Year
:: HtmlTime 	= HtmlTime Int Int Int					// Hours Minutes Seconds


//TODO: Change this hacked input, to a nicer way of auto refreshing forms
:: RefreshTimer	= RefreshTimer Int						// The editor for this type refreshes it's form after n milliseconds

//TODO: See what is still needed of these definitions.
//      The types in this module are supposed to be just views,
//      so there should not be a need for those instances

derive   gEq		HtmlDate, HtmlTime
instance ==			HtmlDate, HtmlTime
instance == 		(DisplayMode a) | == a
derive   gLexOrd	HtmlDate, HtmlTime
instance <			HtmlDate, HtmlTime
instance toString	HtmlDate, HtmlTime
instance +			HtmlTime
instance -			HtmlTime
instance toInt		HtmlSelect