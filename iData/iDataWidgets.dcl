definition module iDataWidgets
/**
* This modules contains a collection of types and their gForm/gUpd instances that can be used
* to construct convenient view types with.
*/

import iDataForms
import Html
import GenLexOrd

derive gForm 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlRadiogroup, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, HtmlCurrency, HtmlLabel
derive gUpd  	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlRadiogroup, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, HtmlCurrency, HtmlLabel
derive gPrint 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlRadiogroup, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, HtmlCurrency, HtmlCurrencyCode, HtmlLabel
derive gParse 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlRadiogroup, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, HtmlCurrency, HtmlCurrencyCode, HtmlLabel
//derive read 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlRadiogroup, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, HtmlCurrency, HtmlCurrencyCode, HtmlLabel
//derive write 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlRadiogroup, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, HtmlCurrency, HtmlCurrencyCode, HtmlLabel


// Layout types
:: <-> a b		= (<->) infixl 5 a b					// place b to the left of a
:: <|> a b		= (<|>) infixl 4 a b					// place b below a

:: DisplayMode a 
	= DisplayMode a										// non-editable display of a
	| EditMode    a										// editable
	| HideMode    a										// hiding a
	| EmptyMode											// nothing to display or hide

// Buttons representing basic html buttons
:: HtmlButton		= HtmlButton String Bool				// basic button. The String is the label of the button, the Bool parameter indicates if it was clicked.

// Other elementary widgets
:: HtmlCheckbox		= HtmlCheckbox [HtmlTag] Bool			// Checkbox, the html is a label which and the boolean determines if the checkbox is checked or not
:: HtmlSelect		= HtmlSelect [(String,String)] String	// Selectbox, the list contains label, value tuples, the string is the current value
:: HtmlRadiogroup	= HtmlRadiogroup [[HtmlTag]] Int		// Radiogroup, the list contains the labels, the int the current value

:: HtmlTextarea		= HtmlTextarea Int String				// Text area
:: HtmlPassword		= HtmlPassword String					// Password input

:: HtmlDate 		= HtmlDate Int Int Int					// Day Month Year
:: HtmlTime 		= HtmlTime Int Int Int					// Hours Minutes Seconds

:: HtmlCurrency		= HtmlCurrency HtmlCurrencyCode Int		// Currency: Currency code and amount in cents
:: HtmlCurrencyCode	= EUR									// A selection of ISO4217 currency codes 
					| GBP
					| USD
					| JPY

:: HtmlLabel		= HtmlLabel [HtmlTag]					// Label used for showing html


//TODO: See what is still needed of these definitions.
//      The types in this module are supposed to be just views,
//      so there should not be a need for those instances

derive   gEq		HtmlDate, HtmlTime
instance ==			HtmlDate, HtmlTime
instance == 		(DisplayMode a) | == a
derive   gLexOrd	HtmlDate, HtmlTime
instance <			HtmlDate, HtmlTime
instance toString	HtmlDate, HtmlTime, HtmlCurrency
instance +			HtmlTime
instance -			HtmlTime

instance toInt		HtmlSelect, HtmlRadiogroup, HtmlCurrency
instance toBool		HtmlButton