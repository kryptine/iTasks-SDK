definition module iDataWidgets
/**
* This modules contains a collection of types and their gForm/gUpd instances that can be used
* to construct convenient view types with.
*/

import iDataForms
import Html
import GenLexOrd

derive gForm 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlDate, HtmlTime, RadioButton, RadioGroup,/* PullDownMenu,*/ TextInput, TextArea, HTML, PasswordBox, RefreshTimer
derive gUpd  	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlDate, HtmlTime, RadioButton, RadioGroup,/* PullDownMenu,*/ TextInput, TextArea, HTML, PasswordBox, RefreshTimer
derive gPrint 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlDate, HtmlTime, RadioButton, RadioGroup,/* PullDownMenu,*/ TextInput, TextArea, HTML, PasswordBox, RefreshTimer
derive gParse 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlDate, HtmlTime, RadioButton, RadioGroup,/* PullDownMenu,*/ TextInput, TextArea, HTML, PasswordBox, RefreshTimer
derive gerda 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlDate, HtmlTime, RadioButton, RadioGroup,/* PullDownMenu,*/ TextInput, TextArea, HTML, PasswordBox, RefreshTimer
//derive read 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlDate, HtmlTime, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer
//derive write 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlDate, HtmlTime, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer


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

:: HtmlDate 	= 	HtmlDate Int Int Int				// Day Month Year
:: HtmlTime 	= 	HtmlTime Int Int Int				// Hours Minutes Seconds

	
:: TextInput	= TI Int							// Input box of size Size for Integers
				| TR Real							// Input box of size Size for Reals
				| TS String							// Input box of size Size for Strings
:: TextArea		= TextArea Int Int String			// Input Area Box, row col initial string
													// Only works in Submit mode due to Html restrictions!
:: PasswordBox	= PasswordBox String
	
// special's



:: RadioGroup	= RadioGroup (Int,[String])			// radiobutton group (item chosen, label list)
:: RefreshTimer	=	RefreshTimer Int				// The editor for this type refreshes it's form after n milliseconds

instance toBool		RadioButton						// True if checkbox checked, button pressed
//instance toInt		PullDownMenu					// Current index in pull down list
//instance toString	PullDownMenu					// Corresponding element in pull down list
derive   gEq		HtmlDate, HtmlTime, PasswordBox
instance ==			HtmlDate, HtmlTime, PasswordBox
instance == 		(DisplayMode a) | == a
derive   gLexOrd	HtmlDate, HtmlTime
instance <			HtmlDate, HtmlTime
instance toString	HtmlDate, HtmlTime
instance +			HtmlTime
instance -			HtmlTime
instance toInt		HtmlSelect

// OBSOLETE SECTION:

:: RadioButton	= RBChecked String					// radiobutton 	checked
				| RBNotChecked String				// radiobutton	not checked

getTimeAndDate :: !*HSt -> *(!(!HtmlTime,!HtmlDate),!*HSt)
:: HTML = HTML [HtmlTag]							// to print html: NOT Parsed, CANNOT be stored NOR retrieved
