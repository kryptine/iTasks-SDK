implementation module iTasks.UI.Editor

import StdMisc
import iTasks._Framework.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks._Framework.IWorld
import iTasks.UI.Definition
import qualified Data.Map as DM

emptyEditor :: Editor a
emptyEditor = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI _ _ _ vst			    = (ui UIEmpty,vst)
	updUI _ _ _ _ _ vst 		= (NoChange,vst)
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

fromEditlet :: (Editlet a d) -> (Editor a) | JSONEncode{|*|} a & JSONDecode{|*|} a & gDefault{|*|} a & JSONEncode{|*|} d & JSONDecode{|*|} d
fromEditlet editlet=:{Editlet| genUI, initUI, genDiffSrv, appDiffSrv} = {Editor|genUI=genUI`,updUI=updUI, onEdit=onEdit`}
where
	genUI` dp currVal mask vst=:{VSt|taskId}
		# (uiDef,vst=:{VSt|iworld}) = genUI dp currVal mask vst
		= case editletLinker initUI iworld of
			(Ok (saplDeps, saplInit),iworld)
				# attr = 'DM'.fromList [("taskId",JSONString taskId)
                                       ,("editorId",JSONString (editorId dp))
                                       ,("saplDeps",JSONString saplDeps)
                                       ,("saplInit",JSONString saplInit)
                                       ]
				= (eui uiDef attr, {VSt|vst & iworld = iworld})
			(Error e,iworld) //TODO: Propagate the error to the interact task that creates the editor
				= (eui uiDef 'DM'.newMap, {VSt|vst & iworld = iworld})
	where
		eui (UI type attr items) editletAttr = UI type (addAll editletAttr attr) items
		addAll a1 a2 = foldl (\a (k,v) -> 'DM'.put k v a) a2 ('DM'.toList a1)

	updUI dp ov om nv nm vst=:{VSt|iworld} //TODO: -> Properly track version numbers
		= case (genDiffSrv ov nv) of
			Nothing 			= (NoChange,{VSt|vst & iworld=iworld})
			currentDiff 		= (ChangeUI [("setAttribute",[JSONString "diff", toJSON (fromJust currentDiff)])] [],{VSt|vst & iworld=iworld})

	onEdit` [] jsonDiff ov om ust
	//appDiff` [] (JSONArray [JSONInt ver, JSONInt diffId, jsonDiff]) ov om ust
		= case fromJSON jsonDiff of
			Just diff
				# (nv,nm) = (appDiffSrv diff ov,Touched)
				= (nv,nm,ust)
			Nothing
				= (ov,om,ust)

	onEdit` dp _ val mask ust =(val,mask,ust)
