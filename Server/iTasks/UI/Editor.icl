implementation module iTasks.UI.Editor

import StdBool, StdMisc
import iTasks._Framework.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks._Framework.IWorld
import iTasks.UI.Definition
import qualified Data.Map as DM
import Text, Text.JSON
import GenEq

derive JSONEncode EditMask, FieldMask
derive JSONDecode EditMask, FieldMask
derive gEq        EditMask, FieldMask

emptyEditor :: Editor a
emptyEditor = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI _ _ vst			    = (Ok (ui UIEmpty,newFieldMask),vst)
	updUI _ _ _ _ _ vst 		= (Ok NoChange,vst)
	onEdit _ _ val mask ust 	= (val,mask,ust)

newFieldMask :: EditMask
newFieldMask = FieldMask {FieldMask|touched=False,valid=True,state=JSONNull}

newCompoundMask :: EditMask
newCompoundMask = CompoundMask []

editorId :: !DataPath -> String
editorId dp = "v" + join "-" (map toString dp)

s2dp :: !String -> DataPath
s2dp str 
	| textSize str < 2	= []
						= map toInt (split "-" (subString 1 (textSize str) str))

subMasks :: !Int EditMask -> [EditMask]
subMasks n (CompoundMask ms) = ms
subMasks n m = repeatn n m

isTouched :: !EditMask -> Bool
isTouched (FieldMask {FieldMask|touched}) = touched
isTouched (CompoundMask ms) = or (map isTouched ms) 

containsInvalidFields :: !EditMask -> Bool
containsInvalidFields (FieldMask {FieldMask|valid}) = not valid
containsInvalidFields (CompoundMask ms) = or (map containsInvalidFields ms)

toPairMask :: !Int !EditMask -> EditMask
toPairMask len mask = split len (subMasks len mask)
where
	split 1 [mask] = mask
	split 2 masks 	= CompoundMask masks
	split n masks	= CompoundMask [split middle left,split (n - middle) right]
	where
		middle = n / 2
		(left,right) = splitAt middle masks

checkMask :: !EditMask a -> Maybe a
checkMask mask val
    | isTouched mask    = Just val
                        = Nothing

checkMaskValue :: !EditMask a -> Maybe JSONNode | JSONEncode{|*|} a
checkMaskValue (FieldMask {FieldMask|touched,state}) _ = if touched (Just state) Nothing
checkMaskValue _ _                       = Nothing

/**
* Set basic hint and error information based on the verification
*/
stdAttributes :: String Bool EditMask -> UIAttributes
stdAttributes typename optional (CompoundMask _) = 'DM'.newMap
stdAttributes typename optional mask
	# (touched,valid,state) = case mask of
		(FieldMask {FieldMask|touched,valid,state}) = (touched,valid,state)
		mask = (isTouched mask,True,JSONNull)
	| not touched || (state =:JSONNull && optional)
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INFO)
                        ,(HINT_ATTRIBUTE,JSONString ("Please enter a " +++ typename +++ if optional "" " (this value is required)"))]
	| valid
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_VALID)
						,(HINT_ATTRIBUTE,JSONString ("You have correctly entered a " +++ typename))]
	| state =: JSONNull
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INVALID)
						,(HINT_ATTRIBUTE,JSONString ("You need to enter a "+++ typename +++ " (this value is required)"))]
	| otherwise
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INVALID)
						,(HINT_ATTRIBUTE,JSONString ("This value not in the required format of a " +++ typename))]

stdAttributeChanges :: String Bool EditMask EditMask -> [UIAttributeChange]
stdAttributeChanges typename optional om nm 
	| om === nm = [] //Nothing to change
	| otherwise = [SetAttribute k v \\ (k,v) <- 'DM'.toList (stdAttributes typename optional nm)]

basicEdit :: !(upd a -> Maybe a) !DataPath !JSONNode !a !EditMask !*VSt -> *(!a, !EditMask, !*VSt) | JSONDecode{|*|} upd
basicEdit toV [] upd v vmask ust=:{VSt|optional}
	= case upd of
		JSONNull = (v,FieldMask {touched=True,valid=optional,state=JSONNull},ust)
		json = case fromJSON upd of
			Nothing  = (v,FieldMask {touched=True,valid=False,state=upd},ust)
			(Just e) = case toV e v of
				Nothing = (v,FieldMask {touched=True,valid=False,state=upd},ust)
				Just val = (val,FieldMask {touched=True,valid=True,state=upd},ust)
basicEdit toV _ upd v vmask ust = (v,vmask,ust)

basicEditSimple :: !DataPath !JSONNode !a !EditMask !*VSt -> *(!a,!EditMask,!*VSt) | JSONDecode{|*|} a
basicEditSimple target upd val mask iworld = basicEdit (\json _ -> fromJSON json) target upd val mask iworld

fromEditlet :: (Editlet a) -> (Editor a) | JSONEncode{|*|} a & JSONDecode{|*|} a & gDefault{|*|} a
fromEditlet editlet=:{Editlet|genUI,initUI,updUI,onEdit} = {Editor|genUI=genUI`,updUI=updUI,onEdit=onEdit}
where
	genUI` dp val vst=:{VSt|taskId}
		= case genUI dp val vst of
			(Ok (UI type attr items,mask),vst=:{VSt|iworld}) = case editletLinker initUI iworld of
				(Ok (saplDeps, saplInit),iworld)
					# editletAttr = 'DM'.fromList [("taskId",JSONString taskId)
                   		                    ,("editorId",JSONString (editorId dp))
                   		                    ,("saplDeps",JSONString saplDeps)
                       		                ,("saplInit",JSONString saplInit)
                           		            ]
					= (Ok (UI type ('DM'.union editletAttr attr) items,mask), {VSt|vst & iworld = iworld})
				(Error e,iworld)
					= (Error e, {VSt|vst & iworld = iworld})
			(Error e,vst) = (Error e,vst)

