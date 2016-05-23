implementation module iTasks._Framework.Generic.Interaction

from Data.Map import :: Map
import qualified Data.Map as DM
from StdFunc import const
import StdList, StdBool, StdTuple, StdMisc, StdArray
import Data.Maybe, Data.Either, Data.Error, Data.Generic, Data.Functor, Data.Tuple
import Text, Text.JSON
import iTasks._Framework.IWorld
import iTasks.UI.Definition
import iTasks._Framework.Util
import iTasks.API.Core.Types
import iTasks.UI.Layout
import iTasks.UI.Editor, iTasks.UI.Definition
import iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Common, iTasks.UI.Editor.Combinators

//Generic Verify
generic gVerify a :: !VerifyOptions (Masked a) -> Verification

gVerify{|UNIT|} _ (value,mask) = CorrectValue Nothing

gVerify{|PAIR|} fx fy options (PAIR x y, CompoundMask [xmask,ymask]) 
	= CompoundVerification [fx options (x,xmask), fy options (y,ymask)]
	
gVerify{|EITHER|} fx _ options (LEFT x,mask) = fx options (x,mask)
	
gVerify{|EITHER|} _ fy options (RIGHT y,mask) = fy options (y,mask)
	
gVerify{|RECORD of {grd_arity}|} fx options (RECORD x, mask)
	= fromPairVerification grd_arity (fx options (x, toPairMask grd_arity mask))
	
gVerify{|FIELD|} fx options (FIELD x,mask) = fx options (x,mask)
	
gVerify{|OBJECT|} fx options=:{VerifyOptions|optional} (OBJECT x,mask) = case mask of
 //   Blanked     = if optional MissingValue (CorrectValue Nothing)
    _           = fx options (x,mask)
	
gVerify{|CONS of {gcd_arity}|} fx options (CONS x,mask)
	= fromPairVerification gcd_arity (fx options (x, toPairMask gcd_arity mask))
	
gVerify{|Int|} options mv = simpleVerify options mv
gVerify{|Real|} options mv = simpleVerify options mv
gVerify{|Char|} options mv = simpleVerify options mv
gVerify{|String|} options mv = simpleVerify options mv
gVerify{|Bool|} options mv = alwaysValid mv

gVerify{|Maybe|} fx options (Nothing, mask) = CorrectValue Nothing
gVerify{|Maybe|} fx options (Just x, mask) = fx {VerifyOptions|options & optional = True} (x,mask)
	
gVerify{|[]|} fx  options=:{VerifyOptions|optional,disabled} (list,mask)
	= CompoundVerification (verifyListItems list (subMasks (length list) mask))
where
	verifyListItems [] [] = []
	verifyListItems [x:xs] [m:ms] = [fx options (x,m):verifyListItems xs ms]

gVerify{|EditableList|} fx options=:{VerifyOptions|optional,disabled} ({EditableList|items},mask)
	= CompoundVerification (verifyListItems items (subMasks (length items) mask))
where
	verifyListItems [] [] = []
	verifyListItems [x:xs] [m:ms] = [fx options (x,m):verifyListItems xs ms]

gVerify{|(->)|} _ _ _ mv	= alwaysValid mv
gVerify{|Dynamic|}	_ mv	= alwaysValid mv

gVerify{|HtmlTag|} _ mv = alwaysValid mv
gVerify{|JSONNode|} _ mv = alwaysValid mv
gVerify{|()|} _ mv      = alwaysValid mv
gVerify{|RWShared|} _ _ _ _ mv = alwaysValid mv

derive gVerify (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), Void, Either, MaybeError, Timestamp, Map

updConsPath i n
 	| i >= n	
		= []
	| n == 1
		= []
	| i < (n/2)
		= [ -1: updConsPath i (n/2) ]
	| otherwise
		= [ -2: updConsPath (i - (n/2)) (n - (n/2)) ]

updPairPath i n
	| i >= n
		= []
	| n == 1
		= []
	| i < (n /2)
		= [0: updPairPath i (n /2)]
	| otherwise
		= [1: updPairPath (i - (n/2)) (n - (n/2))]


addLabel :: !String !UIAttributes -> UIAttributes
addLabel label attr = putCond LABEL_ATTRIBUTE (JSONString label) attr
where
    putCond k v m = maybe ('DM'.put k v m) (const m) ('DM'.get k m)

verifyValue :: !a -> Verification | gVerify{|*|} a
verifyValue val = verifyMaskedValue (val,CompoundMask [])
	
verifyMaskedValue :: !(Masked a) -> Verification | gVerify{|*|} a
verifyMaskedValue mv = gVerify{|*|} {VerifyOptions|optional = False, disabled = False} mv 

isValid :: !Verification -> Bool
isValid (CorrectValue _) = True
isValid (WarningValue _) = True
isValid (CompoundVerification vs) = foldr (\v t -> t && isValid v) True vs 
isValid _ = False

alwaysValid :: !(Masked a) -> Verification
alwaysValid _ = CorrectValue Nothing

simpleVerify :: !VerifyOptions !(Masked a) -> Verification
simpleVerify options mv = customVerify (const True) (const undef) options mv

customVerify :: !(a -> Bool) !(a -> String) !VerifyOptions (Masked a) -> Verification
customVerify pred mkErrMsg options=:{VerifyOptions|optional,disabled} (val,mask)
	= case mask of
		//Untouched				= if optional (CorrectValue Nothing) MissingValue
		//Touched					= validateValue val
		_		    = validateValue val
where
	validateValue val
		| pred val	= CorrectValue Nothing
		| otherwise	= IncorrectValue(mkErrMsg val)

basicUpdate :: !(upd a -> Maybe a) !DataPath !JSONNode !a !EditMask !*USt -> *(!a, !EditMask, !*USt) | JSONDecode{|*|} upd
basicUpdate toV [] upd v vmask ust=:{USt|optional}
	= case upd of
		JSONNull = (v,FieldMask {touched=True,valid=optional,state=JSONNull},ust)
		json = case fromJSON upd of
			Nothing  = (v,FieldMask {touched=True,valid=False,state=upd},ust)
			(Just e) = case toV e v of
				Nothing = (v,FieldMask {touched=True,valid=False,state=upd},ust)
				Just val = (val,FieldMask {touched=True,valid=True,state=upd},ust)
basicUpdate toV _ upd v vmask ust
		= (v,vmask,ust)

basicUpdateSimple :: !DataPath !JSONNode !a !EditMask !*USt -> *(!a,!EditMask,!*USt) | JSONDecode{|*|} a
basicUpdateSimple target upd val mask iworld = basicUpdate (\json old -> fromJSON json) target upd val mask iworld
