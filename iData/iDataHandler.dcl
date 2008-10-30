definition module iDataHandler

// Converting Clean types to iData for automatic generation and dealing with Html form's ..
// (c) MJP 2005, 2006, 2007

import iDataSettings, iDataFormData, iDataForms
import iDataState
import GenPrint, GenParse
import HSt
import NWorld
import Html

derive gForm	Inline
derive gUpd 	Inline
derive gParse 	Inline
derive gPrint 	Inline
derive gerda 	Inline
derive read 	Inline
derive write 	Inline

:: Inline 		= Inline String

:: UserPage 	:== .(*HSt -> .((!Bool,!String),HtmlTag,!*HSt))

// doHtmlServer & doHtmlClient main wrappers for generating & handling of Html forms

doHtmlWrapper :: !UserPage !*World -> *World

// utility functions
toBody 				:: (Form a) -> HtmlTag											// just (BodyTag form.body)
showHtml 			:: [HtmlTag] -> Inline											// enabling to show Html code in Clean data

// Specialists section...

// Added for testing of iData applications with GAST
runUserApplication	:: .(*HSt -> *(.a,*HSt)) HTTPRequest *FormStates *NWorld -> *(.a,*FormStates,*NWorld)



