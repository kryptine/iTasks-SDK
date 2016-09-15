implementation module iTasks.UI.Editor

import StdBool, StdMisc
import iTasks._Framework.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks._Framework.IWorld
import iTasks.UI.Definition
import qualified Data.Map as DM
import Text, Text.JSON
import GenEq

derive JSONEncode EditMask, FieldMask, CompoundMask
derive JSONDecode EditMask, FieldMask, CompoundMask
derive gEq        EditMask, FieldMask, CompoundMask

newFieldMask :: EditMask
newFieldMask = FieldMask {FieldMask|touched=False,valid=True,state=JSONNull}

newCompoundMask :: EditMask
newCompoundMask = CompoundMask {CompoundMask|fields=[],state=JSONNull}

editorId :: !DataPath -> String
editorId dp = "v" + join "-" (map toString dp)

s2dp :: !String -> DataPath
s2dp str 
	| textSize str < 2	= []
						= map toInt (split "-" (subString 1 (textSize str) str))

subMasks :: !Int EditMask -> [EditMask]
subMasks n (CompoundMask {CompoundMask|fields}) = fields
subMasks n m = repeatn n m

isTouched :: !EditMask -> Bool
isTouched (FieldMask {FieldMask|touched}) = touched
isTouched (CompoundMask {CompoundMask|fields}) = or (map isTouched fields) 

containsInvalidFields :: !EditMask -> Bool
containsInvalidFields (FieldMask {FieldMask|valid}) = not valid
containsInvalidFields (CompoundMask {CompoundMask|fields}) = or (map containsInvalidFields fields)

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
	| state =:JSONNull && not touched
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INFO)
                        ,(HINT_ATTRIBUTE,JSONString ("Please enter a " +++ typename +++ if optional "" " (this value is required)"))]
	| state =: JSONNull 
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INVALID)
						,(HINT_ATTRIBUTE,JSONString ("You need to enter a "+++ typename +++ " (this value is required)"))]
	| valid
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_VALID)
						,(HINT_ATTRIBUTE,JSONString ("You have correctly entered a " +++ typename))]
	| otherwise
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INVALID)
						,(HINT_ATTRIBUTE,JSONString ("This value not in the required format of a " +++ typename))]

stdAttributeChanges :: String Bool EditMask EditMask -> [UIAttributeChange]
stdAttributeChanges typename optional om nm 
	| om === nm = [] //Nothing to change
	| otherwise = [SetAttribute k v \\ (k,v) <- 'DM'.toList (stdAttributes typename optional nm)]

basicEdit :: !(upd a -> Maybe a) !DataPath !(!DataPath,!JSONNode) !a !EditMask !*VSt -> *(!MaybeErrorString (!UIChange,!EditMask), !a, !*VSt) | JSONDecode{|*|} upd
basicEdit toV dp ([],e) v vmask vst=:{VSt|optional}
	= case e of
		JSONNull = (Ok (NoChange,FieldMask {touched=True,valid=optional,state=JSONNull}),v,vst)
		json = case fromJSON json of
			Nothing  = (Ok (NoChange,FieldMask {touched=True,valid=False,state=e}),v,vst)
			(Just event) = case toV event v of
				Nothing = (Ok (NoChange,FieldMask {touched=True,valid=False,state=e}),v,vst)
				Just val = (Ok (NoChange,FieldMask {touched=True,valid=True,state=e}),val,vst)
basicEdit toV _ upd v vmask vst = (Ok (NoChange,vmask),v,vst)

basicEditSimple :: !DataPath !(!DataPath,!JSONNode) !a !EditMask !*VSt -> *(!MaybeErrorString (!UIChange,!EditMask),!a,!*VSt) | JSONDecode{|*|} a
basicEditSimple dp (tp,e) val mask iworld = basicEdit (\json _ -> fromJSON json) dp (tp,e) val mask iworld

fromEditlet :: (Editlet a) -> (Editor a) | JSONEncode{|*|} a & JSONDecode{|*|} a & gDefault{|*|} a
fromEditlet editlet=:{Editlet|genUI,initUI,onEdit,onRefresh} = {Editor|genUI=genUI`,onEdit=onEdit,onRefresh=onRefresh}
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

