definition module FormId


import iDataForms
import StdMaybe, StdBool
import GenEq
import Html

:: FormId d										// properties one has to assign to any form 
	=	{ id 		:: !String					// id *uniquely* identifying the form
		, issub		:: !Bool					// subform of another form or not
		, lifespan	:: !Lifespan				// lifespan of form
		, mode		:: !Mode					// editable or not
		, storage	:: !StorageFormat			// serialization method
		, ival		:: !d						// initial value
		}

:: Lifespan										// 	defines how long a form will be maintained		
	=	LSTxtFile								// 	persistent form stored in a file in StorageFormat
	|	LSTxtFileRO								//	persistent form stored in a file in StorageFormat, which is used Read-Only
	|	LSDataFile								//	persistent form stored in a file using the Poor-Mans-Database-Format
	| 	LSSession								// 	form in StorageFormat will live as long as one browses between the pages offered by the application
	| 	LSPage									// 	form in StorageFormat will be automatically garbage collected when no reference is made to it in a page			
	|	LSClient								//	form will be handled on Client, if option enabled, otherwise as Session
	|	LSTemp									//	form setting is not stored at all, only lives temporaly in the Clean application	

:: Mode											// one can choose:
	=	Edit									// 	an editable form where every change is commited to the server
	|	Submit									//	an editable form where the whole content is commited on submit 
	| 	Display									// 	a non-editable form
	|	NoForm									//	do not generate a form, only a value

:: StorageFormat								// Serialization method:
	=	StaticDynamic							// + higher order types, fast, NO dynamic linker needed; - works only for a specific application !
	| 	PlainString								// - first order types only, slow (requires generic parser); + can be used by anyone who knows the type

:: Init											//  Usage of the value stored in FormId
	=	Const									//	The value is a constant
	|	Init 									// 	The value is taken as initial value
	|	Set  									// 	The value will be used as new iData value

:: InIDataId d	:==	(!Init,!FormId d)			// Often used parameter of iData editors


/**
* Create a default FormId
*/
mkFormId :: !String !d -> FormId d				// mkFormId str val = {id = str, ival = val} <@ Page <@ Edit <@ PlainString

/**
* Set the attributes of a FormId
*/
class   (<@) infixl 4 att :: !(FormId d) !att -> FormId d

instance <@ String								// formId <@ x = {formId & id       = x}
instance <@ Lifespan							// formId <@ x = {formId & lifespan = x}
instance <@ Mode								// formId <@ x = {formId & mode     = x}
instance <@ StorageFormat						// formId <@ x = {formId & storage  = x}

setFormId 	:: !(FormId d) !d	-> FormId d		// set new initial value in formid

/**
* Create common InIDataId values
*/
initID		:: !(FormId d)				-> InIDataId d	// (Init,FormId a)
setID		:: !(FormId d) !d			-> InIDataId d	// (Set,FormId a)

/**
* Alternative functions for creating FormIds
*/

// editable, string format
tFormId		:: !String !d -> FormId d			// temp
nFormId		:: !String !d -> FormId d			// page
sFormId		:: !String !d -> FormId d			// session
pFormId		:: !String !d -> FormId d			// persistent
rFormId		:: !String !d -> FormId d			// persistent read only

// non-editable, string format
tdFormId	:: !String !d -> FormId d			// temp                 + display
ndFormId	:: !String !d -> FormId d			// page                 + display
sdFormId	:: !String !d -> FormId d			// session              + display
pdFormId	:: !String !d -> FormId d			// persistent           + display
rdFormId	:: !String !d -> FormId d			// persistent read only + display

// noform, string format
xtFormId	:: !String !d -> FormId d			// temp                 + no form
xnFormId	:: !String !d -> FormId d			// page                 + no form
xsFormId	:: !String !d -> FormId d			// session              + no form
xpFormId	:: !String !d -> FormId d			// persistent           + no form
xrFormId	:: !String !d -> FormId d			// persistent read only + no form

// editable, dynamic format also allows to store functions
nDFormId	:: !String !d -> FormId d			// page                 + static dynamic format
sDFormId	:: !String !d -> FormId d			// session              + static dynamic format
pDFormId	:: !String !d -> FormId d			// persistent           + static dynamic format
rDFormId	:: !String !d -> FormId d			// persistent read only + static dynamic format

// non-editable, dynamic format also allows to store functions
ndDFormId	:: !String !d -> FormId d			// page                 + static dynamic format + display
sdDFormId	:: !String !d -> FormId d			// session              + static dynamic format + display
pdDFormId	:: !String !d -> FormId d			// persistent           + static dynamic format + display
rdDFormId	:: !String !d -> FormId d			// persistent read only + static dynamic format + display

// to create new FormId's ou of an existing one, handy for making unique identifiers
extidFormId :: !(FormId d) !String 		-> FormId d		// make new id by adding sufix 
subFormId 	:: !(FormId a) !String !d 	-> FormId d		// make new id for a new type by adding suffix
subnFormId 	:: !(FormId a) !String !d 	-> FormId d		// idem with lifespan Page
subsFormId 	:: !(FormId a) !String !d 	-> FormId d		// idem with lifespan Session
subpFormId 	:: !(FormId a) !String !d 	-> FormId d		// idem with lifespan Persistent
subtFormId 	:: !(FormId a) !String !d 	-> FormId d		// idem with lifespan Temp
reuseFormId :: !(FormId a) !d			-> FormId d		// reuse id for new type (only to be used in gform)

/**
* Utility functions
*/
onMode 		:: !Mode a a a a -> a						// chose arg depending on Edit, Submit, Display, NoForm

// manipulating initial values

toViewId	:: !Init !d! (Maybe d) -> d					// copy second on Set or if third is Nothing
toViewMap	:: !(d -> v) !Init !d !(Maybe v) -> v		// same, but convert to view domain

// neccesary instances of standard classes
instance toBool Init
instance <  Lifespan
instance toString Lifespan
instance toString StorageFormat
instance == Init, Mode, Lifespan
derive  gEq Init, Mode, Lifespan
