definition module GenVisualize

import Html, JSON, TUIDefinition
import StdGeneric, StdMaybe, Void, Either
import GenUpdate

//Generic visualization function
generic gVisualize a	:: (VisualizationValue a) (VisualizationValue a) *VSt -> ([Visualization], *VSt)

//Default available instances
derive gVisualize UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gVisualize Int, Real, Char, Bool, String
derive gVisualize Dynamic, [], Maybe, Either, (,), (,,), (,,,), Void

//Wrapper functions for visualization
visualizeAsEditor		:: String DataMask a -> ([TUIDef],Bool)	| gVisualize{|*|} a
visualizeAsHtmlDisplay	:: a -> [HtmlTag]							| gVisualize{|*|} a
visualizeAsTextDisplay	:: a -> String								| gVisualize{|*|} a
visualizeAsHtmlLabel	:: a -> [HtmlTag]							| gVisualize{|*|} a
visualizeAsTextLabel	:: a -> String								| gVisualize{|*|} a

//Wrapper function for calculating form delta's
determineEditorUpdates	:: String DataMask DataMask a a -> ([TUIUpdate],Bool)	| gVisualize{|*|} a

//Type definitions for visualization
:: VisualizationValue a
	= VValue a DataMask
	| VBlank

//Bimap for visualization values
derive bimap VisualizationValue

:: *VSt =
	{ vizType			:: !VisualizationType		// Type of preferred visualization
	, idPrefix			:: !String					// Prefix for all identity strings of editor fields 
	, label				:: !Maybe String			// Optional label to attach to editor fields
	// Additional information for form generation
	, currentPath		:: !DataPath				// Accumulated path through the data structure, used to identify sub-structures
	, useLabels			:: !Bool					// Indent for labels, whether there is a label or not
	, onlyBody			:: !Bool					// Only generate a constructor body for editors
	, optional			:: !Bool					// Create optional form fields
	, valid				:: !Bool					// Is the form valid
	}

:: VisualizationType
	= VEditorDefinition
	| VEditorUpdate
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

value2s 	:: DataPath (VisualizationValue a)				-> String | toString a
label2s 	:: Bool (Maybe String)							-> Maybe String
stillValid	:: DataPath (VisualizationValue a) Bool Bool	-> Bool