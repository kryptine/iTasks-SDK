definition module GUICore

import Html, ExtJS, JSON
import StdMaybe, Void, Either
import StdGeneric

//Datapath is used to point to substructures of data structures
:: DataPath :== [Int]

//Generic visualization function
generic gVisualize a	:: a a		*VSt -> ([Visualization], *VSt)
generic gUpdate a		:: a 		*USt -> (a, *USt)

//At least create instances for:
// -All generic types
// -All basic types
// [], (,), (,,), (,,,), Maybe, Task, Void, HtmlTag

//Default available instances
derive gVisualize UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gVisualize Int, Real, Char, Bool, String
derive gVisualize Dynamic, [], Maybe, Either, (,), (,,), (,,,), Void

derive gUpdate UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gUpdate Int, Real, Char, Bool, String
derive gUpdate Dynamic, [], Maybe, Either, (,), (,,), (,,,), Void

//Additional available instances for "special" data
//derive gVisualize HtmlDate, HtmlTime, HtmlButton 

//Wrapper functions for visualization
visualizeAsEditor		:: String a -> [ExtJSDef]		| gVisualize{|*|} a
visualizeAsHtmlDisplay	:: a -> [HtmlTag]				| gVisualize{|*|} a
visualizeAsTextDisplay	:: a -> String					| gVisualize{|*|} a

//Wrapper function for calculating form delta's
determineEditorUpdates	:: String a a -> [ExtJSUpdate]	| gVisualize{|*|} a

//Wrapper functions for updating
createDefault			:== defaultValue

defaultValue			:: a					| gUpdate{|*|} a
updateValue				:: String String a -> a	| gUpdate{|*|} a 

//Type definitions for visualization & updating
:: *VSt =
	{ vizType			:: VisualizationType	//Type of preferred visualization
	, idPrefix			:: String				//Prefix for all identity strings of editor fields 
	, dataPath			:: DataPath				//Accumulated path through the data structure, used to identify sub-structures
	, label				:: Maybe String			//Optional label to attach to editor fields
	, consBody			:: Bool					//Only generate a constructor body for editors
	}

:: *USt =
	{ mode				:: UpdateMode
	, searchPath		:: String
	, currentPath		:: DataPath
	, update			:: String
	, consPath			:: [ConsPos]
	}

:: UpdateMode
	= UDSearch
	| UDCreate
	| UDDone

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
	