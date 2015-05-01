implementation module iTasks._Framework.Generic.Visualization

import StdGeneric, StdList, StdMisc
import Data.Maybe, Data.Either, Data.Void, Data.Functor
from Data.Map import :: Map, :: Size
import qualified Data.Map as DM
import qualified Data.List as DL
import Text, Text.JSON, Text.HTML
import System.Time
import iTasks._Framework.Util
import iTasks._Framework.SDS

toSingleLineText :: !a -> String | gText{|*|} a
toSingleLineText v = concat (gText{|*|} AsSingleLine (Just v))

toMultiLineText :: !a -> String | gText{|*|} a
toMultiLineText v = join "\n" (gText{|*|} AsMultiLine (Just v))

//Generic text visualizer
generic gText a :: !TextFormat (Maybe a) -> [String]

gText{|UNIT|} _ _ = []

gText{|RECORD|} fx AsSingleLine (Just (RECORD x))   = [join ", " (fx AsSingleLine (Just x))]
gText{|RECORD|} fx mode         (Just (RECORD x))   = fx mode (Just x)
gText{|RECORD|} fx mode         Nothing             = fx mode Nothing
		
gText{|FIELD of {gfd_name}|} fx AsHeader _                  = [camelCaseToWords gfd_name]
gText{|FIELD of {gfd_name}|} fx AsRow (Just (FIELD x))      = [concat (fx AsSingleLine (Just x))]
gText{|FIELD of {gfd_name}|} fx AsMultiLine (Just (FIELD x))= [camelCaseToWords gfd_name +++ ":": fx AsMultiLine (Just x)]
gText{|FIELD of {gfd_name}|} fx mode (Just (FIELD x))       = fx mode (Just x)
	
gText{|OBJECT|} fx mode (Just (OBJECT x))   = fx mode (Just x)
gText{|OBJECT|} fx mode Nothing             = fx mode Nothing

gText{|CONS of {gcd_name,gcd_type_def}|} fx mode (Just (CONS x))
    # parts = (if (gcd_type_def.gtd_num_conses > 1) [gcd_name] []) ++ fx mode (Just x)
    = case mode of
        AsSingleLine    = 'DL'.intersperse " " parts
        _               = parts
gText{|CONS of {gcd_name,gcd_type_def}|} fx mode Nothing = fx mode Nothing

gText{|PAIR|} fx fy mode (Just (PAIR x y))  = fx mode (Just x) ++ fy mode (Just y)
gText{|PAIR|} fx fy mode Nothing            = fx mode Nothing ++ fy mode Nothing

gText{|EITHER|} fx fy mode (Just (LEFT x))  = fx mode (Just x)
gText{|EITHER|} fx fy mode (Just (RIGHT y)) = fy mode (Just y)
gText{|EITHER|} fx fy mode Nothing          = [""]

gText{|Int|}			_ val				= [maybe "" toString val]
gText{|Real|}			_ val				= [maybe "" toString val]
gText{|Char|}			_ val				= [maybe "" toString val]
gText{|String|}		    _ val				= [maybe "" toString val]
gText{|Bool|}			_ val				= [maybe "" toString val]

gText{|[]|} fx mode (Just val)				= [concat (["[":  flatten ('DL'.intersperse [", "] [fx mode (Just x) \\ x <- val])] ++ ["]"])]
gText{|[]|} fx mode Nothing                 = [""]
gText{|Maybe|} fx mode (Just val)			= fromMaybe ["-"] (fmap (\v -> fx mode (Just v)) val)
gText{|Maybe|} fx mode Nothing              = fx AsHeader Nothing

gText{|Void|} _ _					= []
gText{|Dynamic|} _ _				= []
gText{|(->)|} _ _ _ _				= []
gText{|JSONNode|} _ val			    = [maybe "" toString val]
gText{|HtmlTag|} _ val              = [maybe "" toString val]
gText{|RWShared|} _ _ _ _ _			= []

gText{|()|} _ _                    = []

gText{|(,)|} fa fb AsHeader     _               = ["",""]
gText{|(,)|} fa fb AsRow        (Just (a,b))    = [concat (fa AsSingleLine (Just a)),concat (fb AsSingleLine (Just b))]
gText{|(,)|} fa fb AsSingleLine (Just (a,b))    = [concat (fa AsSingleLine (Just a)),", ",concat (fb AsSingleLine (Just b))]
gText{|(,)|} fa fb mode         (Just (a,b))    = fa mode (Just a) ++ fb mode (Just b)
gText{|(,)|} fa fb mode         Nothing         = fa mode Nothing ++ fb mode Nothing

gText{|(,,)|} fa fb fc AsHeader     _                = ["","",""]
gText{|(,,)|} fa fb fc AsRow        (Just (a,b,c))   = [concat (fa AsSingleLine (Just a)),concat (fb AsSingleLine (Just b)),concat (fc AsSingleLine (Just c))]
gText{|(,,)|} fa fb fc AsSingleLine (Just (a,b,c))   = [concat (fa AsSingleLine (Just a)),", ",concat (fb AsSingleLine (Just b)),", ",concat (fc AsSingleLine (Just c))]
gText{|(,,)|} fa fb fc mode         (Just (a,b,c))   = fa mode (Just a) ++ fb mode (Just b) ++ fc mode (Just c)
gText{|(,,)|} fa fb fc mode         Nothing          = fa mode Nothing ++ fb mode Nothing ++ fc mode Nothing

gText{|(,,,)|} fa fb fc fd AsHeader     _                   = ["","","",""]
gText{|(,,,)|} fa fb fc fd AsRow        (Just (a,b,c,d))    = [concat (fa AsSingleLine (Just a)),concat (fb AsSingleLine (Just b)),concat (fc AsSingleLine (Just c)),concat (fd AsSingleLine (Just d))]
gText{|(,,,)|} fa fb fc fd AsSingleLine (Just (a,b,c,d))    = [concat (fa AsSingleLine (Just a)),", ",concat (fb AsSingleLine (Just b)),", ",concat (fc AsSingleLine (Just c)),", ",concat (fd AsSingleLine (Just d))]
gText{|(,,,)|} fa fb fc fd mode         (Just (a,b,c,d))    = fa mode (Just a) ++ fb mode (Just b) ++ fc mode (Just c) ++ fd mode (Just d)

derive gText Either, MaybeError, Timestamp, Map

(+++>) infixr 5	:: !a !String -> String | gText{|*|} a
(+++>) a s = toSingleLineText a +++ s

(<+++) infixl 5	:: !String !a -> String | gText{|*|} a
(<+++) s a = s +++ toSingleLineText a