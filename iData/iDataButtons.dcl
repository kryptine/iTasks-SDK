definition module iDataButtons

// Prdefined i-Data making html Buttons, forms, and lay-out 
// (c) 2005 MJP

import iDataHandler
import GenLexOrd

derive gForm 	(,), (,,), (,,,), (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, HTML, PasswordBox, RefreshTimer
derive gUpd  	(,), (,,), (,,,), (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, HTML, PasswordBox, RefreshTimer
derive gPrint 	(,), (,,), (,,,), (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, HTML, PasswordBox, RefreshTimer
derive gParse 	(,), (,,), (,,,), (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, HTML, PasswordBox, RefreshTimer
derive gerda 	(,), (,,), (,,,), (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, HTML, PasswordBox, RefreshTimer
derive read 					  (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer
derive write 	  				  (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer

instance toBool		CheckBox, Button, RadioButton	// True if checkbox checked, button pressed
instance toInt		PullDownMenu					// Current index in pull down list
instance toString	PullDownMenu					// Corresponding element in pull down list
derive   gEq		HtmlDate, HtmlTime, PasswordBox
instance ==			HtmlDate, HtmlTime, PasswordBox
instance == 		(DisplayMode a) | == a
derive   gLexOrd	HtmlDate, HtmlTime
instance <			HtmlDate, HtmlTime
instance toString	HtmlDate, HtmlTime
instance +			HtmlTime
instance -			HtmlTime

// lay out

:: <-> a b		= (<->) infixl 5 a b				// place b to the left of a
:: <|> a b		= (<|>) infixl 4 a b				// place b below a

:: DisplayMode a 
				= DisplayMode a						// non-editable display of a
				| EditMode    a						// editable
				| HideMode    a						// hiding a
				| EmptyMode							// nothing to display or hide

:: HTML = HTML [BodyTag]							// to print html: NOT Parsed, CANNOT be stored NOR retrieved

// buttons representing classical html buttons

:: Button 		= Pressed 							// button pressed
				| LButton Int String				// label   button, size in pixels, label of button
				| PButton (Int,Int) String			// picture button, (height,width), reference to picture
:: CheckBox		= CBChecked String 					// checkbox 	checked
				| CBNotChecked String				// checkbox 	not checked	
				
//OBSOLETE: use RadioGroup instead whenever possible			
:: RadioButton	= RBChecked String					// radiobutton 	checked
				| RBNotChecked String				// radiobutton	not checked


:: RadioGroup	= RadioGroup (Int,[String])			// radiobutton group (item chosen, label list)

:: PullDownMenu	= PullDown (Int,Int) (Int,[String]) // pulldownmenu (number visible,width) (item chosen,menulist)		
:: TextInput	= TI Int Int						// Input box of size Size for Integers
				| TR Int Real						// Input box of size Size for Reals
				| TS Int String						// Input box of size Size for Strings
:: TextArea		= TextArea Int Int String			// Input Area Box, row col initial string
													// Only works in Submit mode due to Html restrictions!
:: PasswordBox	= PasswordBox String
	
// special's

:: HtmlDate 	= 	Date Int Int Int				// Day Month Year
:: HtmlTime 	= 	Time Int Int Int				// Hours Minutes Seconds


:: RefreshTimer	=	RefreshTimer Int				// The editor for this type refreshes it's form after n milliseconds

getTimeAndDate :: !*HSt -> *(!(!HtmlTime,!HtmlDate),!*HSt)
