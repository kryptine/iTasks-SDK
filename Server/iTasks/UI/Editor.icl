implementation module iTasks.UI.Editor

import StdMisc
import iTasks._Framework.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks._Framework.IWorld
import iTasks.UI.Definition
import qualified Data.Map as DM

emptyEditor :: Editor a
emptyEditor = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI _ _ _ vst			    = (Ok (ui UIEmpty),vst)
	updUI _ _ _ _ _ vst 		= (Ok NoChange,vst)
	onEdit _ _ val mask ust 	= (val,mask,ust)

subMasks :: !Int EditMask -> [EditMask]
subMasks n (CompoundMask ms) = ms
subMasks n m = repeatn n m

isTouched :: !EditMask -> Bool
isTouched Touched = True
isTouched (TouchedUnparsed _)	= True
isTouched (TouchedWithState _)	= True
isTouched Blanked	 			= True
isTouched (CompoundMask ms) 	= isTouched` ms
where
	isTouched` [] = False
	isTouched` [m:ms]
		| isTouched m 	= True
		| otherwise 	= isTouched` ms
isTouched _						= False

toPairMask :: !Int !EditMask -> EditMask
toPairMask len mask = split len (subMasks len mask)
where
	split 1 [mask] = mask
	split 2 masks 	= CompoundMask masks
	split n masks	= CompoundMask [split middle left,split (n - middle) right]
	where
		middle = n / 2
		(left,right) = splitAt middle masks

fromEditlet :: (Editlet a) -> (Editor a) | JSONEncode{|*|} a & JSONDecode{|*|} a & gDefault{|*|} a
fromEditlet editlet=:{Editlet| genUI, initUI, updUI, onEdit} = {Editor|genUI=genUI`,updUI=updUI,onEdit=onEdit}
where
	genUI` dp currVal mask vst=:{VSt|taskId}
		= case genUI dp currVal mask vst of
			(Ok uiDef,vst=:{VSt|iworld}) = case editletLinker initUI iworld of
				(Ok (saplDeps, saplInit),iworld)
					# attr = 'DM'.fromList [("taskId",JSONString taskId)
                   		                    ,("editorId",JSONString (editorId dp))
                   		                    ,("saplDeps",JSONString saplDeps)
                       		                ,("saplInit",JSONString saplInit)
                           		            ]
					= (Ok (eui uiDef attr), {VSt|vst & iworld = iworld})
				(Error e,iworld)
					= (Error e, {VSt|vst & iworld = iworld})
			(Error e,vst) = (Error e,vst)
	where
		eui (UI type attr items) editletAttr = UI type (addAll editletAttr attr) items
		addAll a1 a2 = foldl (\a (k,v) -> 'DM'.put k v a) a2 ('DM'.toList a1)

