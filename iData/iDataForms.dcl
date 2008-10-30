definition module iDataForms

import iDataFormData, iDataSettings
import iDataState
import GenPrint, GenParse

import HSt

/**
* mkViewForm is the *swiss army knife* function creating stateful interactive forms with a view v of data d.
* Make sure that all editors have a unique identifier!
*/
mkViewForm 			:: !(InIDataId d) !(HBimap d v) !*HSt -> (Form d,!*HSt) | iData v

/**
* specialize has to be used if one wants to specialize gForm for a user-defined type
*/
specialize			:: !((InIDataId a) *HSt -> (Form a,*HSt)) !(InIDataId a) !*HSt -> (!Form a,!*HSt) | gUpd {|*|} a

/**
* Generic machinery for generating forms and updating after events.
*/
generic gForm a	:: !(InIDataId a) !*HSt -> *(Form a, !*HSt)							// user defined gForms: use "specialize"	
generic gUpd  a	:: UpdMode a -> (UpdMode,a)											// gUpd can simply be derived

:: UpdMode			= UpdSearch UpdValue Int										// search for indicated postion and update it
					| UpdCreate [ConsPos]											// create new values if necessary
					| UpdDone														// and just copy the remaining stuff

derive gForm Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD
derive gUpd  Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD

derive bimap Form, FormId

/**
* Use the defaults of gUpd to create a default value of any type.
*/
createDefault 		:: a						| gUpd{|*|} a

//Utility
toHtml 				:: a -> HtmlTag 			| gForm {|*|} a						// toHtml displays any type into a non-editable form
toHtmlForm 			:: !(*HSt -> *(Form a,*HSt)) -> [HtmlTag] 						// toHtmlForm displays any form one can make with a form function
												| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
mkInput				:: !(InIDataId d) String UpdValue !*HSt -> (HtmlTag,*HSt)	// Html Form Creation utility 
