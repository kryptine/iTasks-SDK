implementation module iTasks.UI.Editor

import StdBool, StdMisc, StdList
import iTasks.Internal.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks.Internal.IWorld
import iTasks.UI.Definition
import qualified Data.Map as DM
import Text, Text.JSON
import GenEq

derive JSONEncode EditMask, FieldMask, CompoundMask
derive JSONDecode EditMask, FieldMask, CompoundMask
derive gEq        EditMask, FieldMask, CompoundMask

instance toString EditMode
where
	toString Enter = "enter"
	toString Update = "update"
	toString View = "view"

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

withClientSideInit ::
	((JSObj ()) *JSWorld -> *JSWorld)
	(DataPath a *VSt -> *(!MaybeErrorString (!UI, !EditMask), !*VSt))
	DataPath a *VSt -> *(!MaybeErrorString (!UI, !EditMask), !*VSt)
withClientSideInit initUI genUI dp val vst=:{VSt|taskId} = case genUI dp val vst of
    (Ok (UI type attr items,mask),vst=:{VSt|iworld}) = case editorLinker initUI iworld of
        (Ok (saplDeps, saplInit),iworld)
			# extraAttr = 'DM'.fromList [("taskId",JSONString taskId)
                                         ,("editorId",JSONString (editorId dp))
                                         ,("saplDeps",JSONString saplDeps)
                                         ,("saplInit",JSONString saplInit)
                                        ]
            = (Ok (UI type ('DM'.union extraAttr attr) items,mask), {VSt|vst & iworld = iworld})
        (Error e,iworld)
            = (Error e, {VSt|vst & iworld = iworld})
    (Error e,vst) = (Error e,vst)

