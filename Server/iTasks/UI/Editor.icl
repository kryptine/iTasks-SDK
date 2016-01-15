implementation module iTasks.UI.Editor

import StdMisc
import iTasks._Framework.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks._Framework.IWorld
from iTasks.UI.Diff import :: UIChangeDef(..), :: UILocalChange(..), :: UIChildChange(..)
from iTasks.UI.Editor import :: Editor(..), :: USt(..) 
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

fromEditlet :: (Editlet a d cl) -> (Editor a) | JSONEncode{|*|} a & JSONDecode{|*|} a & gDefault{|*|} a & JSONDecode{|*|} d
fromEditlet editlet=:{Editlet| genUI, initClient, appDiffClt, genDiffSrv, appDiffSrv} = {Editor|genUI=genUI`,genDiff=genDiff`,appDiff=appDiff`}
where
	genUI` dp currVal mask vst=:{VSt|taskId,iworld=iworld=:{IWorld|world}}
		# (uiDef, world)        = genUI htmlId currVal world
  		# iworld                = {iworld & world = world} 
		= case editletLinker initDiff (initClient currVal createEditletEventHandler) (appDiffClt createEditletEventHandler) iworld of
			(Ok (jsScript, jsID, jsIC, jsAD),iworld)
				# opts = editletOpts jsScript jsID jsIC jsAD uiDef
				= (uic (UIEditor {UIEditor|optional=False}) [eui uiDef opts], {VSt|vst & iworld = iworld})
			(Error e,iworld) //TODO: Propagate the error to the interact task that creates the editor
				# opts = editletOpts "" "" "" "" uiDef
				= (uic (UIEditor {UIEditor|optional=False}) [eui uiDef opts], {VSt|vst & iworld = iworld})
	where
		initDiff = genDiffSrv gDefault{|*|} currVal
		htmlId = "editlet-" +++ taskId +++ "-" +++ editorId dp
		editletOpts jsScript jsID jsIC jsAD uiDef
			= { UIEditletOpts
			  | taskId 	    = taskId
			  , editorId	= editorId dp
			  , value		= toJSONA currVal
			  , html 		= toString uiDef.ComponentHTML.html
			  , script	    = jsScript
			  , initClient  = jsIC
			  , initDiff	= jsID
			  , appDiff 	= jsAD
			  }

		eui uiDef opts = setSize uiDef.ComponentHTML.width uiDef.ComponentHTML.height (ui (UIControl (UIEditlet defaultSizeOpts opts)))
		toJSONA a = case JSONEncode{|*|} False a of
			[json:_]	= json
			_			= JSONNull

	genDiff` dp ov om nv nm vst=:{VSt|iworld} //TODO: -> Properly track version numbers
		= case (genDiffSrv ov nv) of
			Nothing 			= (NoChange,{VSt|vst & iworld=iworld})
			currentDiff
				# (res,iworld)  = diffLinker currentDiff Nothing iworld
				= case res of
					Ok (jsScript,jsCDiff,_)
						= (ChangeUI [("applyDiff",[JSONInt 0,JSONString jsCDiff,JSONString jsScript])] [],{VSt|vst & iworld=iworld})
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
