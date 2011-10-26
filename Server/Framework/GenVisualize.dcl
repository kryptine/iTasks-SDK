definition module GenVisualize

import HTML, JSON, TUIDefinition
import StdGeneric, Maybe, Void, Either
import GenUpdate, GenVerify
from Map import :: Map

:: StaticVisualizationMode = AsDisplay | AsLabel

//Generic text visualization function
generic gVisualizeText a :: !StaticVisualizationMode !a -> [String]

//Default available instances
derive gVisualizeText UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gVisualizeText Int, Real, Char, Bool, String
derive gVisualizeText Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), Void, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gVisualizeText Note, Username, Password, Date, Time, DateTime, Document, FormButton, Currency, User, UserDetails, RadioChoice, ComboChoice, CheckMultiChoice, Map, TreeChoice, Tree, TreeNode, Table
derive gVisualizeText EmailAddress, Action, HtmlInclude, ManagementMeta, TaskPriority, ControlSize, FillControlSize, FillWControlSize, FillHControlSize

//Generic editor function
generic gVisualizeEditor a | gVisualizeText a :: !(Maybe a) !*VSt -> (![TUIDef], !*VSt)

//Default available instances
derive gVisualizeEditor UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gVisualizeEditor Int, Real, Char, Bool, String
derive gVisualizeEditor Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), Void, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gVisualizeEditor Note, Username, Password, Date, Time, DateTime, Document, FormButton, Currency, User, UserDetails, RadioChoice, ComboChoice, CheckMultiChoice, Map, TreeChoice, Tree, TreeNode, Table
derive gVisualizeEditor EmailAddress, Action, HtmlInclude, ManagementMeta, TaskPriority, ControlSize, FillControlSize, FillWControlSize, FillHControlSize

//Wrapper functions for visualization
visualizeAsEditor		:: !a !TaskId !Int !VerifyMask !(Maybe (!DataPath,!JSONNode))	-> Maybe TUIDef	| gVisualizeEditor{|*|} a
visualizeAsDisplay		:: !a															-> Maybe TUIDef	| gVisualizeEditor{|*|} a
visualizeAsText			:: !StaticVisualizationMode !a									-> String		| gVisualizeText{|*|} a

//Type definitions for visualization
:: *VSt =
	{ currentPath		:: !DataPath											// Accumulated path through the data structure, used to identify sub-structures
	, verifyMask		:: ![VerifyMask]	
	, selectedConsIndex	:: !Int													// Index of the selected constructor in an Object
	, optional			:: !Bool												// Create optional form fields
	, renderAsStatic	:: !Bool												// If true, flag the form items as being static
	, editEvent			:: !(Maybe (!DataPath,!JSONNode))						// The edit event (if present) for keeping track of values sent by the client (used for diff)
	, taskId			:: !Maybe TaskId										// The id of the task the visualisation belongs to
	, controlSize		:: !(!Maybe TUISize,!Maybe TUISize,!Maybe TUIMargins)	// The width, height & margins of generated controls
	}
	
:: VerifyResult = HintMsg !String | ValidMsg !String | ErrorMsg !String | NoMsg

//Utility functions making specializations of gVisualizeEditor

/**
* Generates an empty visualization.
*
* @param VSt
*
* @return An empty visualization
*/
noVisualization :: !*VSt -> *(![TUIDef],!*VSt)

/**
* Generates a control visualization.
*
* @param The type of the control
* @param The current value to visualize (if present)
* @param VSt
*
* @return The generated visualization
*/
visualizeControl :: !TUIControlType !(Maybe a) !*VSt -> *(![TUIDef], !*VSt) | JSONEncode{|*|} a

/**
* Generates a basic control visualization.
* Visualizations are generated by custom functions using VSt.
*
* @param Function for generating a TUI definition (see comment of TUIVizFunctionCustom for details)
* @param VSt
*
* @return The generated visualization
*/
visualizeCustom :: !TUIVizFunction !*VSt -> *(![TUIDef],!*VSt)

/**
* A function using VSt for generating TUI definitions.
*
* @param The name of the TUI element
* @param A flag indicating if the value is touched
* @param Hint or Error message
* @param A flag indicating if a static editor should be generated
* @param VSt (currentPath points to first child of element, updateMasks & verifyMasks are masks of children)
*
* @return The generated TUI definition
*/
:: TUIVizFunction :== TUIName Bool VerifyResult -> .(*VSt -> *(![TUIDef],!*VSt))

(+++>) infixr 5		:: !a	!String	-> String | gVisualizeText{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gVisualizeText{|*|} a