definition module GenVisualize

import HTML, JSON, TUIDefinition
import StdGeneric, Maybe, Void, Either
import GenUpdate, GenVerify
from Map import :: Map
from Shared import :: Shared

//Generic visualization function
generic gVisualize a	:: (Maybe a) *VSt -> ([Visualization], *VSt)

//Default available instances
derive gVisualize UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gVisualize Int, Real, Char, Bool, String
derive gVisualize Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), Void, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gVisualize Note, Password, Date, Time, DateTime, Document, FormButton, Currency, User, UserDetails, Choice, MultipleChoice, Shared, Map, Tree, TreeNode
derive gVisualize EmailAddress, Action, Table, HtmlDisplay

//Wrapper functions for visualization
visualizeAsEditor		:: !a !TaskId !VerifyMask ![(!DataPath,!JSONNode)]	-> [TUIDef]	| gVisualize{|*|} a
visualizeAsHtmlDisplay	:: !a												-> HtmlTag	| gVisualize{|*|} a
visualizeAsTextDisplay	:: !a												-> String	| gVisualize{|*|} a
visualizeAsHtmlLabel	:: !a												-> HtmlTag	| gVisualize{|*|} a
visualizeAsTextLabel	:: !a												-> String	| gVisualize{|*|} a

//Type definitions for visualization
:: *VSt =
	{ vizType			:: !VisualizationType			// Type of preferred visualization
	, origVizType		:: !VisualizationType			// Type of the preferred visualization at initialization, this should not be edited.
	, label				:: !Maybe String				// Optional label to attach to editor fields
	
	// Additional information for form generation
	, currentPath		:: !DataPath					// Accumulated path through the data structure, used to identify sub-structures
	, verifyMask		:: ![VerifyMask]
	, selectedConsIndex	:: !Int							// Index of the selected constructor in an Object
	, useLabels			:: !Bool						// Indent for labels, whether there is a label or not
	, optional			:: !Bool						// Create optional form fields
	, renderAsStatic	:: !Bool						// If true, flag the form items as being static
	, editEvents		:: ![(!DataPath,!JSONNode)]		// The edit events for keeping track of values sent by the client (used for diff)
	, taskId			:: !TaskId						// The id of the task the visualisation belongs to
	}

:: VisualizationType
	= VEditorDefinition
	| VHtmlDisplay
	| VTextDisplay
	| VHtmlLabel
	| VTextLabel
	
:: Visualization
	= TextFragment !String
	| HtmlFragment !HtmlTag
	| TUIFragment !TUIDef

//Utility functions making specializations of gVisualize

/**
* Generates an empty visualization.
*
* @param VSt
*
* @return An empty visualization
*/
noVisualization :: !*VSt -> *(![Visualization],!*VSt)

/**
* Generates a control visualization.
* Static visualizations are just a 'toString' of the value.
*
* @param The type of the control
* @param The current value to visualize (if present)
* @param VSt
*
* @return The generated visualization
*/
visualizeControlSimple :: !TUIControlType !(Maybe a) !*VSt -> *(![Visualization],!*VSt) | JSONEncode{|*|}, toString a

/**
* Generates a control visualization.
* Static visualizations are generated by a custom function.
*
* @param The type of the control
* @param Functions defining the static visualizations.
* @param The current value to visualize (if present)
* @param VSt
*
* @return The generated visualization
*/
visualizeControl :: !TUIControlType !(StaticVizFunctions a) !(Maybe a) !*VSt -> *(![Visualization],!*VSt) | JSONEncode{|*|} a
visualizeControl2 :: !TUIControlType !(StaticVizFunctions b) !(Maybe (!a,!b)) !*VSt -> *(![Visualization],!*VSt) | JSONEncode{|*|} a

/**
* Generates a basic control visualization.
* Visualizations are generated by custom functions using VSt.
*
* @param Function for generating a TUI definition (see comment of TUIVizFunctionCustom for details)
* @param Function for generating static visualizations (see comment of StaticVizFunctionCustom for details)
* @param The current value to visualize (if present, possibly different types are used for TUI and static visualizations)
* @param Indicates if for static editor definitions automatically a HTML container with a static representation should be generated
         (the TUIVizFunctionCustom is not called in this case)
* @param VSt
*
* @return The generated visualization
*/
visualizeCustomSimple	:: !(TUIVizFunction a) !(StaticVizFunctionCustom a) !(Maybe a)			!Bool !*VSt -> *(![Visualization],!*VSt)
visualizeCustom			:: !(TUIVizFunction a) !(StaticVizFunctionCustom b) !(Maybe (!a,!b))	!Bool !*VSt -> *(![Visualization],!*VSt)


/**
* A function using VSt for generating TUI definitions.
*
* @param The name of the TUI element
* @param The value of the TUI element (if present)
* @param A flag indicating if the value is touched
* @param The field label (if present)
* @param A flag indicating if the value is optional
* @param Error message
* @param Hint message
* @param A flag indicating if a static editor should be generated
* @param VSt (currentPath points to first child of element, updateMasks & verifyMasks are masks of children)
*
* @return The generated TUI definition
*/
:: TUIVizFunction				a :==	TUIName (Maybe a) Bool (Maybe String) Bool String String Bool -> .(*VSt -> *(![TUIDef],!*VSt))
/**
* Functions for string and html visualizations.
*
* string function:
* @param The value to visualize (if present)
* @return The generated text visualization
*
* html function:
* @param The value to visualize (if present)
* @return The generated html visualization
*/
:: StaticVizFunctions			a :==	(!(Maybe a) -> String,!(Maybe a) -> HtmlTag)
/**
* A custom function for generating static visualizations.
*
* @param The value to visualize (if present)
* @param A flag indicating if the value is touched
* @param VSt
*
* @return VSt
*/
:: StaticVizFunctionCustom		a :==	(Maybe a) Bool -> .(*VSt -> *(![Visualization],!*VSt))
										
/**
* Generates functions for static visualizations a single function for text visualizations.
*/
textOnly :: !((Maybe a) -> String) -> StaticVizFunctions a

(+++>) infixr 5		:: !a	!String	-> String | gVisualize{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gVisualize{|*|} a
