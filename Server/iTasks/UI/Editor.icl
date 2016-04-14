implementation module iTasks.UI.Editor

import StdMisc
import iTasks._Framework.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks._Framework.IWorld
import iTasks.UI.Definition
import qualified Data.Map as DM

emptyEditor :: Editor a
emptyEditor = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI _ _ _ vst			    = (ui UIEmpty,vst)
	genDiff _ _ _ _ _ vst 		= (NoChange,vst)
	appDiff _ _ val mask ust 	= (val,mask,ust)

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

createEditletEventHandler :: (EditletEventHandlerFunc d a) !ComponentId -> JSFun b
createEditletEventHandler handler id = undef

fromEditlet :: (Editlet a d cl) -> (Editor a) | JSONEncode{|*|} a & JSONDecode{|*|} a & gDefault{|*|} a & JSONEncode{|*|} d & JSONDecode{|*|} d
fromEditlet editlet=:{Editlet| genUI, saplInit, initClient, appDiffClt, genDiffSrv, appDiffSrv} = {Editor|genUI=genUI`,genDiff=genDiff`,appDiff=appDiff`}
where
	genUI` dp currVal mask vst=:{VSt|taskId,iworld=iworld=:{IWorld|world}}
		# (uiDef, world)        = genUI currVal world
  		# iworld                = {iworld & world = world} 
		= case editletLinker initDiff saplInit (appDiffClt createEditletEventHandler) iworld of
		//= case editletLinker initDiff (initClient currVal createEditletEventHandler) (appDiffClt createEditletEventHandler) iworld of
			(Ok (jsScript, jsID, jsIC, jsAD),iworld)
				# attr = editletAttr jsScript jsID jsIC jsAD
				= (eui uiDef attr, {VSt|vst & iworld = iworld})
			(Error e,iworld) //TODO: Propagate the error to the interact task that creates the editor
				= (eui uiDef 'DM'.newMap, {VSt|vst & iworld = iworld})
	where
		initDiff = genDiffSrv gDefault{|*|} currVal
		editletAttr jsScript jsID jsIC jsAD
			= 'DM'.fromList [("taskId",JSONString taskId)
							,("editorId",JSONString (editorId dp))
							,("saplDeps",JSONString jsScript)
							,("saplInit",JSONString jsIC)
							,("initDiff",JSONString jsID)
							,("appDiff",JSONString jsAD)
							]

		eui (UI type attr items) editletAttr = UI type (addAll editletAttr attr) items
		addAll a1 a2 = foldl (\a (k,v) -> 'DM'.put k v a) a2 ('DM'.toList a1)

	genDiff` dp ov om nv nm vst=:{VSt|iworld} //TODO: -> Properly track version numbers
		= case (genDiffSrv ov nv) of
			Nothing 			= (NoChange,{VSt|vst & iworld=iworld})
			currentDiff
				# (res,iworld)  = diffLinker currentDiff Nothing iworld
				= case res of
					Ok (jsScript,jsCDiff,_)
						= (ChangeUI [("setAttribute",[JSONString "diff", toJSON (fromJust currentDiff)])] [],{VSt|vst & iworld=iworld})
						//= (ChangeUI [("setAttribute",[JSONString "diff", JSONArray [JSONInt 0,JSONString jsCDiff,JSONString jsScript]])] [],{VSt|vst & iworld=iworld})
						//= (ChangeUI [("applyDiff",[JSONInt 0,JSONString jsCDiff,JSONString jsScript])] [],{VSt|vst & iworld=iworld})
					Error e
						//TODO Propagate error up
						= (NoChange,{VSt|vst & iworld=iworld})

	appDiff` [] (JSONArray [JSONInt ver, JSONInt diffId, jsonDiff]) ov om ust
		= case fromJSON jsonDiff of
			Just diff
				# (nv,nm) = (appDiffSrv diff ov,Touched)
				= (nv,nm,ust)
			Nothing
				= (ov,om,ust)

	appDiff` dp _ val mask ust = (val,mask,ust)
