definition module GenVisualize

import Html, ExtJS, JSON
import StdGeneric, StdMaybe, Void, Either
import GenUpdate

//Generic visualization function
generic gVisualize a	:: a a		*VSt -> ([Visualization], *VSt)

//Default available instances
derive gVisualize UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gVisualize Int, Real, Char, Bool, String
derive gVisualize Dynamic, [], Maybe, Either, (,), (,,), (,,,), Void

//Wrapper functions for visualization
visualizeAsEditor		:: String DataMask a -> ([ExtJSDef],Bool)	| gVisualize{|*|} a
visualizeAsHtmlDisplay	:: a -> [HtmlTag]							| gVisualize{|*|} a
visualizeAsTextDisplay	:: a -> String								| gVisualize{|*|} a
visualizeAsHtmlLabel	:: a -> [HtmlTag]							| gVisualize{|*|} a
visualizeAsTextLabel	:: a -> String								| gVisualize{|*|} a

//Wrapper function for calculating form delta's
determineEditorUpdates	:: String DataMask a a -> ([ExtJSUpdate],Bool)			| gVisualize{|*|} a

//Type definitions for visualization
:: *VSt =
	{ vizType			:: VisualizationType	// Type of preferred visualization
	, idPrefix			:: String				// Prefix for all identity strings of editor fields 
	, label				:: Maybe String			// Optional label to attach to editor fields
	// Additional information for form generation
	, currentPath		:: DataPath				// Accumulated path through the data structure, used to identify sub-structures
	, consBody			:: Bool					// Only generate a constructor body for editors
	, optional			:: Bool					// Create optional form fields
	, blank				:: Bool					// Build a structure  with undefs
	, mask				:: DataMask				// The section of the datastructure that is "complete"
	, valid				:: Bool					// Is the form valid
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
	| ExtJSFragment ExtJSDef
	| ExtJSUpdate ExtJSUpdate

//Utility functions making specializations of gVisualize
value2s 	:: DataPath DataMask a			-> String | toString a
label2s 	:: Bool (Maybe String)			-> Maybe String
stillValid	:: DataPath DataMask Bool Bool	-> Bool