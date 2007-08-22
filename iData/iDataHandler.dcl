definition module iDataHandler

// Converting Clean types to iData for automatic generation and dealing with Html form's ..
// (c) MJP 2005, 2006, 2007

import iDataHtmlDef, iDataFormData, iDataSettings
import GenPrint, GenParse

generic gForm a	:: !(InIDataId a) !*HSt -> *(Form a, !*HSt)							// user defined gForms: use "specialize"	
generic gUpd  a	:: UpdMode a -> (UpdMode,a)											// gUpd can simply be derived

derive gForm Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD
derive gUpd  Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD
derive bimap Form, FormId

derive gForm	Inline
derive gUpd 	Inline
derive gParse 	Inline
derive gPrint 	Inline
derive gerda 	Inline
derive read 	Inline
derive write 	Inline

:: *HSt 		= { cntr 	:: Int 			// counts position in expression
				  , submits	:: Bool			// True if we are in submitting mode
				  , states	:: *FormStates  // all form states are collected here ... 	
				  , world	:: *NWorld		// to enable all other kinds of I/O
				  }	

:: Inline = Inline String

// doHtmlServer main wrapper for generating & handling of an Html form
// depending on the option set (see iDataSettings) it will either 
//			- link in an http 1.0 server
//			- administrate itself as subserver to an http 1.1 server

doHtmlServer 		:: !(*HSt -> (Html,!*HSt)) !*World -> *World 					
doHtmlServer2 		:: ![(String,*HSt -> (Html,!*HSt))] !*World -> *World

// don't use the following experimental functions
doHtmlClient 		:: !(*HSt -> (Html,!*HSt)) !*World -> *World 					

// mkViewForm is the *swiss army knife* function creating stateful interactive forms with a view v of data d.
// Make sure that all editors have a unique identifier!

mkViewForm 			:: !(InIDataId d) !(HBimap d v) !*HSt -> (Form d,!*HSt) | iData v

// Explicit removal of all (Persistent) IData for which the predicate holds applied on the IData form id

deleteIData			:: !String !*HSt -> *HSt

// specialize has to be used if one wants to specialize gForm for a user-defined type

specialize			:: !((InIDataId a) *HSt -> (Form a,*HSt)) !(InIDataId a) !*HSt -> (!Form a,!*HSt) | gUpd {|*|} a
		
// utility functions

toHtml 				:: a -> BodyTag 			| gForm {|*|} a						// toHtml displays any type into a non-editable form
toHtmlForm 			:: !(*HSt -> *(Form a,*HSt)) -> [BodyTag] 						// toHtmlForm displays any form one can make with a form function
												| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
toBody 				:: (Form a) -> BodyTag											// just (BodyTag form.body)
createDefault 		:: a						| gUpd{|*|} a 						// creates a default value of requested type

showHtml 			:: [BodyTag] -> Inline											// enabling to show Html code in Clean data

// definitions on HSt

instance FileSystem HSt																// enabling file IO on HSt

appWorldHSt			:: !.(*World -> *World)       !*HSt -> *HSt						// enabling World operations on HSt
accWorldHSt			:: !.(*World -> *(.a,*World)) !*HSt -> (.a,!*HSt)				// enabling World operations on HSt

// Specialists section...

// Added for testing of iData applications with GAST

import iDataState

runUserApplication	:: .(*HSt -> *(.a,*HSt)) *FormStates *NWorld -> *(.a,*FormStates,*NWorld)

// Some low level utility functions handy when specialize cannot be used, only to be used by experts !!

incrHSt				:: Int !*HSt -> *HSt											// Cntr := Cntr + 1
CntrHSt				:: !*HSt -> (Int,*HSt)											// Hst.Cntr
mkInput				:: !Int !(InIDataId d) Value UpdValue !*HSt -> (BodyTag,*HSt)	// Html Form Creation utility 
getChangedId		:: !*HSt -> ([String],!*HSt)									// id's of form(s) that has been changed by user

:: UpdMode			= UpdSearch UpdValue Int										// search for indicated postion and update it
					| UpdCreate [ConsPos]											// create new values if necessary
					| UpdDone														// and just copy the remaining stuff

