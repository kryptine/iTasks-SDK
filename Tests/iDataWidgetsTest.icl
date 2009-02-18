module iDataWidgetsTest
/**
* This module tests the gForm and gUpd instances for the
* view types in the iDataWidgets module
*/
import StdEnv, iTasks, iData
import BasicCombinators, CommonCombinators, PromptingCombinators

Start :: *World -> *World
Start world = startEngine tests world
where
	tests = [ 	{ name		= "widgets_test"
		  		, label		= "Test all widgets"
		  		, roles		= []
		  		, mainTask	= widgets_test #>> return_V Void
		  		}
		  	]

:: AllWidgets = 	{ int			:: Int
					, real			:: Real
					, bool			:: Bool
					, string		:: String
					, maybe			:: Maybe String
					, htmlButton	:: HtmlButton
					, htmlCheckbox	:: HtmlCheckbox
					, htmlSelect	:: HtmlSelect
					, htmlRadiogroup:: HtmlRadiogroup
					, htmlTextarea	:: HtmlTextarea
					, htmlPassword	:: HtmlPassword
					, htmlDate		:: HtmlDate
					, htmlTime		:: HtmlTime
					, htmlLabel		:: HtmlLabel
					}

derive gForm	AllWidgets
derive gUpd		AllWidgets
derive gPrint	AllWidgets
derive gParse	AllWidgets

widgets_test :: Task AllWidgets
widgets_test
	=	editTask "Show" createDefault =>> \val -> editTask "Done" val