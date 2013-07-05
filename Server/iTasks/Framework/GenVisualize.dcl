definition module iTasks.Framework.GenVisualize

import StdGeneric
from Text.JSON import :: JSONNode
from Data.Either import :: Either
from Data.Void import :: Void
from Data.Maybe import :: Maybe
from Text.HTML import :: HtmlTag
from Data.Map import :: Map

import iTasks.Framework.GenUpdate, iTasks.Framework.GenVerify, iTasks.Framework.UIDefinition, iTasks.Framework.UIDiff

from iTasks.API.Core.LayoutCombinators import :: Layout

:: VisualizationFormat
	= AsLabel			//A single line of text	
	| AsText			//Multiple lines of text
	| AsRow				//A list of cells to display in a grid or table

//* Generic text visualization function
generic gVisualizeText a :: !VisualizationFormat !a -> [String]

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
generic gEditor a | gVisualizeText a, gDefault a, gEditMeta a, JSONEncode a, JSONDecode a
				  :: !DataPath !(VerifiedValue a) !*VSt -> (!VisualizationResult,!*VSt)

//Default available instances
derive gEditor
	UNIT,
	EITHER with ve1 _ _ _ _ _ ve2 _ _ _ _ _,
	PAIR with ve1 _ _ _ _ _ ve2 _ _ _ _ _,
	OBJECT of {gtd_num_conses,gtd_conses} with ve1 _ _ ve4 _ _,
	CONS of {gcd_index,gcd_arity} with ve1 _ _ _ _ _,
	RECORD of {grd_arity} with ve1 _ _ _ _ _,
	FIELD of {gfd_name} with ve1 _ _ _ _ _
	
derive gEditor Int, Real, Char, Bool, String, [], (,), (,,), (,,,), (->), Dynamic
derive gEditor Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

//Generic headers function for getting grid headers for a type (typically names of record fields)
//The argument is ignored. It's needed to indicate the type the function works on!

:: EditMeta
	= { label	:: Maybe String
	  , hint	:: Maybe String
	  }
	  
generic gEditMeta a :: a -> [EditMeta]

//Default available instances
derive gEditMeta
	UNIT,
	EITHER with fx fy,
	PAIR with fx fy,
	OBJECT with fx,
	CONS with fx,
	RECORD with fx,
	FIELD of {gfd_name} with fx
	
derive gEditMeta Int, Real, Char, Bool, String, [], (,), (,,), (,,,), (->), Dynamic
derive gEditMeta Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

//Wrapper functions for visualization
visualizeAsEditor		:: !(VerifiedValue a) !TaskId !Layout !*IWorld	-> (![(!UIControl,!UIAttributes)],!*IWorld)	| gEditor{|*|} a

visualizeAsLabel		:: !a -> String		| gVisualizeText{|*|} a
visualizeAsText			:: !a -> String		| gVisualizeText{|*|} a
visualizeAsRow			:: !a -> [String]	| gVisualizeText{|*|} a

//Type definitions for visualization
:: *VSt =
	{ selectedConsIndex	:: !Int													// Index of the selected constructor in an Object
	, optional			:: !Bool												// Create optional form fields
	, disabled			:: !Bool												// If true the editor is not editable
	, taskId			:: !String												// The id of the task the visualisation belongs to
	, layout			:: !Layout												// Layout for composite structures
	, iworld			:: !*IWorld												// The iworld, used for example if external tools are needed to create editors
	}

:: VisualizationResult
		= NormalEditor [(!UIControl,!UIAttributes)]
		| OptionalEditor [(!UIControl,!UIAttributes)]
		| HiddenEditor
	
//Utility functions making specializations of gVisualizeEditor
checkMask			:: !InteractionMask a -> Maybe a
checkMaskValue      :: !InteractionMask a -> Maybe JSONNode | JSONEncode{|*|} a

verifyAttributes	:: !(VerifiedValue a) [EditMeta] -> UIAttributes

(+++>) infixr 5		:: !a	!String	-> String | gVisualizeText{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gVisualizeText{|*|} a
