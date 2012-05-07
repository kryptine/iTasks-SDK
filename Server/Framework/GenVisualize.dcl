definition module GenVisualize

import HTML, JSON_NG, TUIDefinition
import StdGeneric, Maybe, Void, Either
import GenUpdate, GenVerify
from Map import :: Map

:: StaticVisualizationMode = AsDisplay | AsLabel

//Generic text visualization function
generic gVisualizeText a :: !StaticVisualizationMode !a -> [String]

//Default available instances
derive gVisualizeText UNIT, PAIR, EITHER, CONS of {gcd_name,gcd_type_def}, OBJECT, RECORD, FIELD of {gfd_name}
derive gVisualizeText Int, Real, Char, Bool, String
derive gVisualizeText Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), JSONNode, Void, HtmlTag, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gVisualizeText URL, Note, Username, Password, Date, Time, DateTime, Document, FormButton, EUR, USD, BoundedInt, User, UserConstraint, RadioChoice, ComboChoice, GridChoice, CheckMultiChoice, Map, TreeChoice, Tree, TreeNode, Table
derive gVisualizeText EmailAddress, Action, HtmlInclude, ManagementMeta, TaskPriority, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive gVisualizeText DynamicChoice, DynamicChoiceNoView

/**
* Generic editor function
*
* @param Value to visualize (or Nothing to visualize a default)
* @param Visualization state, contains visualization options
*
* @return The visualization result
* @return The visualization state
*/
generic gVisualizeEditor a | gVisualizeText a, gHeaders a, gGridRows a :: !(Maybe a) !*VSt -> (!VisualizationResult,!*VSt)

//Default available instances
derive gVisualizeEditor UNIT,
	OBJECT of {gtd_num_conses,gtd_conses} with ve1 _ _ _,
	CONS of {gcd_index} with ve1 _ _ _,
	RECORD with ve1 _ _ _,
	FIELD of {gfd_name} with ve1 _ _ _,
	PAIR with ve1 _ _ _ ve2 _ _ _,
	EITHER with ve1 _ _ _ ve2 _ _ _
derive gVisualizeEditor Int, Real, Char, Bool, String
derive gVisualizeEditor Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), JSONNode, Void, HtmlTag, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gVisualizeEditor URL, Note, Username, Password, Date, Time, DateTime, Document, FormButton, EUR, USD, BoundedInt, User, UserConstraint, RadioChoice, ComboChoice, GridChoice, CheckMultiChoice, Map, TreeChoice, Tree, TreeNode, Table
derive gVisualizeEditor EmailAddress, Action, HtmlInclude, ManagementMeta, TaskPriority, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive gVisualizeEditor DynamicChoice, DynamicChoiceNoView

//Generic headers function for getting grid headers for a type (typically names of record fields)
//The argument is ignored. It's needed to indicate the type the function works on!
generic gHeaders a :: a -> [String]

//Default available instances
derive gHeaders UNIT,
	OBJECT with _,
	CONS with _,
	RECORD of {grd_fields} with _,
	FIELD with _,
	PAIR with _ _,
	EITHER with _ _
derive gHeaders Int, Real, Char, Bool, String
derive gHeaders Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), JSONNode, Void, HtmlTag, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gHeaders URL, Note, Username, Password, Date, Time, DateTime, Document, FormButton, EUR, USD, BoundedInt, User, UserConstraint, RadioChoice, ComboChoice, GridChoice, CheckMultiChoice, Map, TreeChoice, Tree, TreeNode, Table
derive gHeaders EmailAddress, Action, HtmlInclude, ManagementMeta, TaskPriority, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive gHeaders DynamicChoice, DynamicChoiceNoView

//Generic function for getting grid rows (typically values of record fields)
generic gGridRows a | gVisualizeText a :: !a ![String] -> Maybe [String]

//Default available instances
derive gGridRows UNIT,
	OBJECT with _ _,
	CONS with _ _,
	RECORD with gr1 _,
	FIELD with _ vt1,
	PAIR with gr1 _ gr2 _,
	EITHER with _ _ _ _
derive gGridRows Int, Real, Char, Bool, String
derive gGridRows Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), JSONNode, Void, HtmlTag, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gGridRows URL, Note, Username, Password, Date, Time, DateTime, Document, FormButton, EUR, USD, BoundedInt, User, UserConstraint, RadioChoice, ComboChoice, GridChoice, CheckMultiChoice, Map, TreeChoice, Tree, TreeNode, Table
derive gGridRows EmailAddress, Action, HtmlInclude, ManagementMeta, TaskPriority, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive gGridRows DynamicChoice, DynamicChoiceNoView

//Wrapper functions for visualization
visualizeAsEditor		:: !a !VerifyMask !TaskId !*IWorld 		-> (!Maybe TUIDef,!*IWorld)	| gVisualizeEditor{|*|} a
visualizeAsText			:: !StaticVisualizationMode !a			-> String					| gVisualizeText{|*|} a

//Type definitions for visualization
:: *VSt =
	{ currentPath		:: !DataPath											// Accumulated path through the data structure, used to identify sub-structures
	, verifyMask		:: ![VerifyMask]	
	, selectedConsIndex	:: !Int													// Index of the selected constructor in an Object
	, optional			:: !Bool												// Create optional form fields
	, renderAsStatic	:: !Bool												// If true, flag the form items as being static
	, taskId			:: !Maybe TaskId										// The id of the task the visualisation belongs to
	, controlSize		:: !(!Maybe TUISize,!Maybe TUISize,!Maybe TUIMargins)	// The width, height & margins of generated controls
	, iworld			:: !*IWorld												// The iworld, used for example if external tools are needed to create editors
	}

:: VisualizationResult = NormalEditor [TUIDef] | OptionalEditor [TUIDef] | HiddenEditor
	
:: VerifyResult = HintMsg !String | ValidMsg !String | ErrorMsg !String | NoMsg

//Utility functions making specializations of gVisualizeEditor

/**
* Generates an empty visualization.
*
* @param VSt
*
* @return An empty visualization
*/
noVisualization :: !*VSt -> *(!VisualizationResult,!*VSt)

/**
* Generates a control visualization.
*
* @param The type of the control
* @param The current value to visualize (if present)
* @param VSt
*
* @return The generated visualization
*/
visualizeControl :: !TUIControlType !(Maybe a) !*VSt -> *(!VisualizationResult, !*VSt) | JSONEncode{|*|} a

/**
* Generates a basic control visualization.
* Visualizations are generated by custom functions using VSt.
*
* @param Function for generating a TUI definition (see comment of TUIVizFunctionCustom for details)
* @param VSt
*
* @return The generated visualization
*/
visualizeCustom :: !TUIVizFunction !*VSt -> *(!VisualizationResult,!*VSt)

/**
* A function using VSt for generating TUI definitions.
*
* @param The name of the TUI element
* @param A flag indicating if the value is touched
* @param Hint or Error message
* @param A flag indicating if a static editor should be generated
* @param The value sent as event for this control (used to avoid sending updates for value the user just entered)
* @param VSt (currentPath points to first child of element, updateMasks & verifyMasks are masks of children)
*
* @return The generated TUI definition
*/
:: TUIVizFunction :== String Bool VerifyResult -> .(*VSt -> *(![TUIDef],!*VSt))

(+++>) infixr 5		:: !a	!String	-> String | gVisualizeText{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gVisualizeText{|*|} a
