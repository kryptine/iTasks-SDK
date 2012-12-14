definition module GenVisualize

import HTML, JSON_NG, UIDefinition
import StdGeneric, Maybe, Void, Either
import GenUpdate, GenVerify
from Map import :: Map
from LayoutCombinators import :: Layout

:: StaticVisualizationMode = AsDisplay | AsLabel

//* Generic text visualization function
generic gVisualizeText a :: !StaticVisualizationMode !a -> [String]

//Default available instances
derive gVisualizeText UNIT, PAIR, EITHER, CONS of {gcd_name,gcd_type_def}, OBJECT, RECORD, FIELD of {gfd_name}
derive gVisualizeText Int, Real, Char, Bool, String, [], (,), (,,), (,,,), (->), Dynamic 
derive gVisualizeText Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

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
derive gVisualizeEditor
	UNIT,
	EITHER with ve1 _ _ _ ve2 _ _ _,
	PAIR with ve1 _ _ _ ve2 _ _ _,
	OBJECT of {gtd_num_conses,gtd_conses} with ve1 _ _ _,
	CONS of {gcd_index} with ve1 _ _ _,
	RECORD with ve1 _ _ _,
	FIELD of {gfd_name} with ve1 _ _ _
	
derive gVisualizeEditor Int, Real, Char, Bool, String, [], (,), (,,), (,,,), (->), Dynamic
derive gVisualizeEditor Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

//Generic headers function for getting grid headers for a type (typically names of record fields)
//The argument is ignored. It's needed to indicate the type the function works on!
generic gHeaders a :: a -> [String]

//Default available instances
derive gHeaders
	UNIT,
	EITHER with _ _,
	PAIR with _ _,
	OBJECT with _,
	CONS with _,
	RECORD of {grd_fields} with _,
	FIELD with _
	
derive gHeaders Int, Real, Char, Bool, String, [], (,), (,,), (,,,), (->), Dynamic
derive gHeaders Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

//Generic function for getting grid rows (typically values of record fields)
generic gGridRows a | gVisualizeText a :: !a ![String] -> Maybe [String]

//Default available instances
derive gGridRows
	UNIT,
	PAIR with gr1 _ gr2 _,
	EITHER with _ _ _ _,
	OBJECT with _ _,
	CONS with _ _,
	RECORD with gr1 _,
	FIELD with _ vt1

derive gGridRows Int, Real, Char, Bool, String, [], (,), (,,), (,,,), (->), Dynamic
derive gGridRows Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp	

//Wrapper functions for visualization
visualizeAsEditor		:: !a !VerifyMask !TaskId !Layout !*IWorld	-> (![(!UIControl,!UIAttributes)],!*IWorld)	| gVisualizeEditor{|*|} a
visualizeAsText			:: !StaticVisualizationMode !a				-> String									| gVisualizeText{|*|} a

//Type definitions for visualization
:: *VSt =
	{ currentPath		:: !DataPath											// Accumulated path through the data structure, used to identify sub-structures
	, verifyMask		:: ![VerifyMask]	
	, selectedConsIndex	:: !Int													// Index of the selected constructor in an Object
	, optional			:: !Bool												// Create optional form fields
	, disabled			:: !Bool												// If true the editor is not editable
	, taskId			:: !TaskId												// The id of the task the visualisation belongs to
	, layout			:: !Layout												// Layout for composite structures
	, iworld			:: !*IWorld												// The iworld, used for example if external tools are needed to create editors
	}

:: VisualizationResult
		= NormalEditor [(!UIControl,!UIAttributes)]
		| OptionalEditor [(!UIControl,!UIAttributes)]
		| HiddenEditor
	
:: VerifyResult = HintMsg !String | ValidMsg !String | ErrorMsg !String | NoMsg

//Utility functions making specializations of gVisualizeEditor

checkMask			:: !Bool !(Maybe a) -> (Maybe a)
verifyElementStr	:: !VerifyMask -> VerifyResult
addVerAttributes	:: !VerifyResult !UIAttributes -> UIAttributes

/**
* Generates an empty visualization.
*
* @param VSt
*
* @return An empty visualization
*/
noVisualization :: !*VSt -> *(!VisualizationResult,!*VSt)

/**
* Generates a basic control visualization.
* Visualizations are generated by custom functions using VSt.
*
* @param Function for generating a TUI definition (see comment of TUIVizFunctionCustom for details)
* @param VSt
*
* @return The generated visualization
*/
visualizeCustom :: !UIVizFunction !*VSt -> *(!VisualizationResult,!*VSt)

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
:: UIVizFunction :== String Bool VerifyResult -> .(*VSt -> *(![(!UIControl,!UIAttributes)],!*VSt))

(+++>) infixr 5		:: !a	!String	-> String | gVisualizeText{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gVisualizeText{|*|} a
