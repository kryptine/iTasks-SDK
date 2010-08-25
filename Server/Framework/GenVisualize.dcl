definition module GenVisualize

import Html, JSON, TUIDefinition
import StdGeneric, StdMaybe, Void, Either
import GenUpdate, GenVerify

//Generic visualization function
generic gVisualize a	:: (VisualizationValue a) (VisualizationValue a) *VSt -> ([Visualization], *VSt)

//Default available instances
derive gVisualize UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gVisualize Int, Real, Char, Bool, String, Document
derive gVisualize Dynamic, [], Maybe, Either, (,), (,,), (,,,), Void, Display, Editable, Hidden, VisualizationHint

//Wrapper functions for visualization
visualizeAsEditor		:: String (Maybe SubEditorIndex) UpdateMask VerifyMask a -> ([TUIDef],Bool)		| gVisualize{|*|} a
visualizeAsHtmlDisplay	:: a -> [HtmlTag]																| gVisualize{|*|} a
visualizeAsTextDisplay	:: a -> String																	| gVisualize{|*|} a
visualizeAsHtmlLabel	:: a -> [HtmlTag]																| gVisualize{|*|} a
visualizeAsTextLabel	:: a -> String																	| gVisualize{|*|} a

//Wrapper function for calculating form delta's
determineEditorUpdates	:: String (Maybe SubEditorIndex) [DataPath] UpdateMask VerifyMask a a -> ([TUIUpdate],Bool)	| gVisualize{|*|} a

//Type definitions for visualization
:: VisualizationValue a
	= VValue a
	| VBlank

//Bimap for visualization values
derive bimap VisualizationValue

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
	, valid				:: !Bool						// Is the form valid
	, updateMask		:: !UpdateMask
	, verifyMask		:: !VerifyMask
	, updates			:: ![DataPath]
	, renderAsStatic	:: !Bool						// If true, flag the form items as being static
	}

:: VisualizationType
	= VEditorDefinition
	| VEditorUpdate
	| VConsSelectorUpdate
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
instance toString (VisualizationValue a) | toString a

restoreField :: DataPath [DataPath] String String -> [Visualization]
//updateVizValue:: !String !String !*VSt -> (![Visualization],!*VSt)
//getMessageUpdates:: *VSt -> (!Bool,![Visualization], !*VSt)

visualizeBasicControl :: !(VisualizationValue a) !*VSt -> (!TUIBasicControl, !*VSt) | toString a
updateBasicControl :: !(VisualizationValue a) !(VisualizationValue a) !*VSt -> (![Visualization],!*VSt) | toString a

verifyElementStr :: !Bool !UpdateMask !VerifyMask -> (!Bool, !String, !String)
verifyElementUpd :: !Bool !String !UpdateMask !VerifyMask -> (!Bool, ![Visualization])

value2s 		:: !UpdateMask !(VisualizationValue a) 								-> String | toString a
labelAttr 		:: !Bool !(Maybe String)									-> Maybe String