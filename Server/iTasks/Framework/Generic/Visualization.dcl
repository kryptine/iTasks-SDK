definition module iTasks.Framework.Generic.Visualization

from StdGeneric import ::UNIT,::PAIR,::EITHER,::CONS,::OBJECT,::RECORD,::FIELD
from Text.JSON import :: JSONNode
from Text.HTML import :: HtmlTag
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Error import :: MaybeError
from Data.Void import :: Void
from Data.Map import :: Map
from System.Time import :: Timestamp

:: TextFormat
	= AsSingleLine		//A single line of text	
	| AsMultiLine		//Multiple lines of text
	| AsRow				//A list of cells to display in a grid or table
    | AsHeader          //A list of headers to display above a grid or table
	

//* Generic text visualization function
generic gText a :: !TextFormat (Maybe a) -> [String]

//Default available instances
derive gText UNIT, PAIR, EITHER, CONS of {gcd_name,gcd_type_def}, OBJECT, RECORD, FIELD of {gfd_name}
derive gText Int, Real, Char, Bool, String, [], (), (,), (,,), (,,,), (->), Dynamic
derive gText Maybe, Either, MaybeError, Void, Map, JSONNode, HtmlTag, Timestamp

//Wrapper functions for visualization
toSingleLineText        :: !a -> String		| gText{|*|} a
toMultiLineText			:: !a -> String		| gText{|*|} a

(+++>) infixr 5		:: !a	!String	-> String | gText{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gText{|*|} a
