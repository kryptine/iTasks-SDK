definition module GenVisualize

import Html, JSON, TUIDefinition
import StdGeneric, StdMaybe, Void, Either
import GenUpdate, GenVerify
from Map import :: Map
from Shared import :: Shared, :: SharedReadOnly

//Generic visualization function
generic gVisualize a	:: (Maybe a) *VSt -> ([Visualization], *VSt)

//Default available instances
derive gVisualize UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gVisualize Int, Real, Char, Bool, String
derive gVisualize Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), Void, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gVisualize Note, Password, Date, Time, DateTime, Document, FormButton, Currency, User, UserDetails, Choice, MultipleChoice, Shared, SharedReadOnly, Map, Tree
derive gVisualize ProcessRef, EmailAddress, Action

//Wrapper functions for visualization
visualizeAsEditor		:: String a UpdateMask VerifyMask -> [TUIDef]			| gVisualize{|*|} a
visualizeAsHtmlDisplay	:: a -> [HtmlTag]										| gVisualize{|*|} a
visualizeAsTextDisplay	:: a -> String											| gVisualize{|*|} a
visualizeAsHtmlLabel	:: a -> [HtmlTag]										| gVisualize{|*|} a
visualizeAsTextLabel	:: a -> String											| gVisualize{|*|} a

fromVisualizationHint :: !(VisualizationHint .a) -> .a
toVisualizationHint :: !.a -> (VisualizationHint .a)

fromEditable :: !(Editable .a) -> .a
toEditable :: !.a -> (Editable .a)

fromDisplay :: !(Display .a) -> .a
toDisplay :: !.a -> (Display .a)

fromHidden :: !(Hidden .a) -> .a
toHidden :: !.a -> (Hidden .a)

//Wrapper function for calculating form delta's
determineEditorUpdates	:: String (a, UpdateMask, VerifyMask) (a, UpdateMask, VerifyMask) ![DataPath] -> [TUIUpdate] | gVisualize{|*|} a

//Type definitions for visualization
:: VisualizationValue a
	= VValue a
	| VBlank

:: *VSt =
	{ vizType			:: !VisualizationType			// Type of preferred visualization
	, origVizType		:: !VisualizationType			// Type of the preferred visualization at initialization, this should not be edited.
	, idPrefix			:: !String						// Prefix for all identity strings of editor fields 
	, label				:: !Maybe String				// Optional label to attach to editor fields
	// Additional information for form generation
	, currentPath		:: !DataPath					// Accumulated path through the data structure, used to identify sub-structures
	, selectedConsIndex	:: !Int							// Index of the selected constructor in an Object
	, useLabels			:: !Bool						// Indent for labels, whether there is a label or not
	, optional			:: !Bool						// Create optional form fields
	, updateMask		:: ![UpdateMask]
	, verifyMask		:: ![VerifyMask]
	, updates			:: ![DataPath]
	, renderAsStatic	:: !Bool						// If true, flag the form items as being static
	}

:: VisualizationType
	= VEditorDefinition
	| VHtmlDisplay
	| VTextDisplay
	| VHtmlLabel
	| VTextLabel
	
:: Visualization
	= TextFragment String
	| HtmlFragment [HtmlTag]
	| TUIFragment TUIDef
	| TUIUpdate TUIUpdate

//Utility functions making specializations of gVisualize

visualizeBasicControl	:: !(Maybe a) !*VSt -> (!TUIBasicControl, !*VSt) | toString a

verifyElementStr		:: !UpdateMask !VerifyMask -> (!String, !String)

value2s					:: !UpdateMask !(Maybe a)	-> String | toString a
labelAttr				:: !Bool !(Maybe String)	-> Maybe String
formatLabel				:: !String					-> String

(+++>) infixr 5		:: !a	!String	-> String | gVisualize{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gVisualize{|*|} a