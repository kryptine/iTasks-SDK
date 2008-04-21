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

:: *HSt 		= { cntr 	:: !Int 			// counts position in expression
				  , submits	:: !Bool			// True if we are in submitting mode
				  , states	:: !*FormStates  	// all form states are collected here ... 
				  , request :: !HTTPRequest		// to enable access to the current HTTP request	
				  , world	:: *NWorld			// to enable all other kinds of I/O
				  }	

:: Inline 		= Inline String

:: UserPage 	:== .(*HSt -> .(!Bool,Html,!*HSt))

// doHtmlServer & doHtmlClient main wrappers for generating & handling of Html forms

doHtmlWrapper		:: UserPage !*World -> *World	//Combined wrapper which starts the server or client wrapper

doHtmlServer 		:: UserPage !*World -> *World	//Server-side engine
doHtmlClient 		:: UserPage !*World -> *World	//Client-side engine (SAPL)		

//doHtmlServer2 		:: ![(String,UserPage)] !*World -> *World


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

