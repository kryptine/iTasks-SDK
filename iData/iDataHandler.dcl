definition module iDataHandler

// Converting Clean types to iData for automatic generation and dealing with Html form's ..
// (c) MJP 2005, 2006, 2007

import iDataFormData, iDataSettings
import iDataState
import GenPrint, GenParse
import HSt
import NWorld
import Html

generic gForm a	:: !(InIDataId a) !*HSt -> *(Form a, !*HSt)							// user defined gForms: use "specialize"	
generic gUpd  a	:: UpdMode a -> (UpdMode,a)											// gUpd can simply be derived

derive gForm Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD
derive gUpd  Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD

derive gForm	Inline
derive gUpd 	Inline
derive gParse 	Inline
derive gPrint 	Inline
derive gerda 	Inline
derive read 	Inline
derive write 	Inline

derive bimap Form, FormId

:: Inline 		= Inline String

:: UserPage 	:== .(*HSt -> .((!Bool,!String),HtmlTag,!*HSt))

// doHtmlServer & doHtmlClient main wrappers for generating & handling of Html forms

doHtmlWrapper :: !UserPage !*World -> *World

// mkViewForm is the *swiss army knife* function creating stateful interactive forms with a view v of data d.
// Make sure that all editors have a unique identifier!

mkViewForm 			:: !(InIDataId d) !(HBimap d v) !*HSt -> (Form d,!*HSt) | iData v

// Explicit removal of all (Persistent) IData for with the same prefix IData form id
// Change lifespan of all IData with the same prefix IData form id

deleteIData			:: !String !*HSt -> *HSt
changeLifespanIData :: !String !Lifespan !Lifespan !*HSt -> *HSt

// specialize has to be used if one wants to specialize gForm for a user-defined type

specialize			:: !((InIDataId a) *HSt -> (Form a,*HSt)) !(InIDataId a) !*HSt -> (!Form a,!*HSt) | gUpd {|*|} a
		
// utility functions

toHtml 				:: a -> HtmlTag 			| gForm {|*|} a						// toHtml displays any type into a non-editable form
toHtmlForm 			:: !(*HSt -> *(Form a,*HSt)) -> [HtmlTag] 						// toHtmlForm displays any form one can make with a form function
												| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
toBody 				:: (Form a) -> HtmlTag											// just (BodyTag form.body)
createDefault 		:: a						| gUpd{|*|} a 						// creates a default value of requested type

showHtml 			:: [HtmlTag] -> Inline											// enabling to show Html code in Clean data

// Specialists section...

// Added for testing of iData applications with GAST


runUserApplication	:: .(*HSt -> *(.a,*HSt)) HTTPRequest *FormStates *NWorld -> *(.a,*FormStates,*NWorld)

// Some low level utility functions handy when specialize cannot be used, only to be used by experts !!

mkInput				:: !Int !(InIDataId d) String UpdValue !*HSt -> (HtmlTag,*HSt)	// Html Form Creation utility 
getChangedId		:: !*HSt -> ([String],!*HSt)									// id's of form(s) that have been changed by user

:: UpdMode			= UpdSearch UpdValue Int										// search for indicated postion and update it
					| UpdCreate [ConsPos]											// create new values if necessary
					| UpdDone														// and just copy the remaining stuff

