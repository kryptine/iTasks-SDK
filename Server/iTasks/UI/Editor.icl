implementation module iTasks.UI.Editor

import StdMisc
import iTasks._Framework.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks._Framework.IWorld
from iTasks.UI.Diff import :: MessageType (MDiff,MRollback,MCommit), :: UIDiffResult(..)
from iTasks.UI.Editor import ::VisualizationResult(..), :: Editor(..), :: USt(..) 
from Data.Map import :: Map, newMap, put
from Data.Map import qualified get

editorControls :: !VisualizationResult -> [(UIControl,UIAttributes)]
editorControls (NormalEditor controls)		= controls
editorControls (OptionalEditor controls)	= controls
editorControls HiddenEditor					= []

createEditletEventHandler :: (EditletEventHandlerFunc d a) !ComponentId -> JSFun b
createEditletEventHandler handler id = undef

fromEditlet :: (Editlet a d cl) -> (Editor a) | JSONEncode{|*|} a & JSONDecode{|*|} a & gDefault{|*|} a & JSONDecode{|*|} d
fromEditlet editlet=:{Editlet| genUI, initClient, appDiffClt, genDiffSrv, appDiffSrv} = {Editor|genUI=genUI`,genDiff=genDiff`,appDiff=appDiff`}
where
	genUI` dp currVal mask ver meta vst=:{VSt|taskId,iworld=iworld=:{IWorld|current={editletDiffs},world}}
		# (uiDef, world)        = genUI htmlId currVal world
  		# iworld                = {iworld & world = world}
		= case 'Data.Map'.get (taskId,editorId dp) editletDiffs of
			  //Only diff with previous value
			  Just (ver,prevValue,opts,diffs)
				# currentDiff                        = diffWithPrevValue prevValue currVal
						
				# (jsScript,jsCDiff,jsIDiff,iworld)  = diffLinker currentDiff Nothing iworld
				// Store diffs
				# diffs                              = if (isJust currentDiff) [MDiff (jsCDiff,jsScript):diffs] diffs
						
				// Increase version number if there is a difference between the reference and the new value
				# ver                                = if (isJust currentDiff) (ver + 1) ver
				# iworld                             = setEditletDiffs ver currVal opts diffs iworld
				= (NormalEditor [(ui uiDef {UIEditletOpts|opts & value = toJSONA currVal, initDiff = jsIDiff}, newMap)],{VSt|vst & iworld = iworld})
			  //Create editlet definition and store reference value for future diffs
			  Nothing
				# diffs = initDiff
				# (jsScript, jsID, jsIC, jsAD, iworld)
					= editletLinker initDiff (initClient currVal createEditletEventHandler) (appDiffClt createEditletEventHandler) iworld
				# opts = editletOpts jsScript jsID jsIC jsAD uiDef
				# iworld = setEditletDiffs 1 currVal {UIEditletOpts|opts & value = JSONNull} [] iworld
				= (NormalEditor [(ui uiDef opts, newMap)],{VSt|vst & iworld = iworld})
		where
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

			ui uiDef opts = setSize uiDef.ComponentHTML.width uiDef.ComponentHTML.height (UIEditlet defaultSizeOpts opts)
			
			toJSONA a = case JSONEncode{|*|} False a of
				[json:_]	= json
				_			= JSONNull

			fromJSONA json = fst (JSONDecode{|*|} False [json])
			
			initDiff = genDiffSrv gDefault{|*|} currVal

			diffWithPrevValue jsonPrev currVal
				= case fromJSONA jsonPrev of
					Just prev = genDiffSrv prev currVal
					_         = Nothing

			setEditletDiffs ver value opts diffs iworld=:{IWorld|current=current=:{editletDiffs}}
				= {IWorld|iworld & current = {current & editletDiffs = put (taskId,editorId dp) (ver,toJSONA value,opts,diffs) editletDiffs}}

	genDiff` up old new vst = (DiffImpossible,vst)

	appDiff` [] jsonDiff ov omask ust=:{USt|taskId,editorId,iworld=iworld=:{IWorld|current=current=:{editletDiffs}}}
		// Bit dirty, but we need to unwrap the "unexpected" version number and the expected diff
		# (ver, diffId, jsonDiff) = case jsonDiff of
			JSONArray [ver, diffId, diff] 	= (maybe -1 id (fromJSON ver), maybe -1 id (fromJSON diffId), diff)
											= (-1, -1, JSONNull)
		= case JSONDecode{|*|} False [jsonDiff] of
			(Just diff,_)	
				# iworld = case 'Data.Map'.get (taskId,editorId) editletDiffs of
                	Just (refver,jsonRef,opts,diffs) = case JSONDecode{|*|} False [jsonRef] of
                    	(Just ref,_)
                    		| ver <> refver
                    			= {IWorld|iworld & current = {current & 
                    					editletDiffs = put (taskId,editorId) (ver,jsonRef,opts,[MRollback diffId:diffs]) editletDiffs}}

  	                      	# ref = appDiffSrv diff ref
   	                     	# [jsonRef:_] = JSONEncode{|*|} False ref
                        	// If the reference value is changed by its client, keep the version number
                        	= {IWorld|iworld & current = {current &
                        			editletDiffs = put (taskId,editorId) (ver,jsonRef,opts,[MCommit diffId:diffs]) editletDiffs}}
                    	_ = iworld
					Nothing = iworld
				= (appDiffSrv diff ov, Touched, {USt|ust & iworld = iworld})
			_	= (ov,omask,ust)
	appDiff` dp _ val mask ust = (val,mask,ust)

