definition module iTasks.Framework.Generic.Visualization

from StdGeneric import ::UNIT,::PAIR,::EITHER,::CONS,::OBJECT,::RECORD,::FIELD
from Text.JSON import :: JSONNode
from Text.HTML import :: HtmlTag
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Void import :: Void
from Data.Map import :: Map
from System.Time import :: Timestamp

:: VisualizationFormat
	= AsLabel			//A single line of text	
	| AsText			//Multiple lines of text
	| AsRow				//A list of cells to display in a grid or table
	

//* Generic text visualization function
generic gVisualizeText a :: !VisualizationFormat !a -> [String]

//Default available instances
derive gVisualizeText UNIT, PAIR, EITHER, CONS of {gcd_name,gcd_type_def}, OBJECT, RECORD, FIELD of {gfd_name}
derive gVisualizeText Int, Real, Char, Bool, String, [], (), (,), (,,), (,,,), (->), Dynamic 
derive gVisualizeText Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

//Wrapper functions for visualization
visualizeAsLabel		:: !a -> String		| gVisualizeText{|*|} a
visualizeAsText			:: !a -> String		| gVisualizeText{|*|} a
visualizeAsRow			:: !a -> [String]	| gVisualizeText{|*|} a

(+++>) infixr 5		:: !a	!String	-> String | gVisualizeText{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gVisualizeText{|*|} a
