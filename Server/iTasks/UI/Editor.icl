implementation module iTasks.UI.Editor

import StdMisc
import iTasks._Framework.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks._Framework.IWorld
from iTasks.UI.Diff import :: MessageType (MDiff,MRollback,MCommit), :: UIChangeDef(..), :: UIChildChange(..), :: UIChange(..)
from iTasks.UI.Editor import :: Editor(..), :: USt(..) 
import qualified Data.Map as DM

createEditletEventHandler :: (EditletEventHandlerFunc d a) !ComponentId -> JSFun b
createEditletEventHandler handler id = undef

fromEditlet :: (Editlet a d cl) -> (Editor a) | JSONEncode{|*|} a & JSONDecode{|*|} a & gDefault{|*|} a & JSONDecode{|*|} d
fromEditlet editlet=:{Editlet| genUI, initClient, appDiffClt, genDiffSrv, appDiffSrv} = {Editor|genUI=genUI`,genDiff=genDiff`,appDiff=appDiff`}
where
	genUI` dp currVal mask ver vst=:{VSt|taskId,iworld=iworld=:{IWorld|world}}
		# (uiDef, world)        = genUI htmlId currVal world
  		# iworld                = {iworld & world = world} 
		= case editletLinker initDiff (initClient currVal createEditletEventHandler) (appDiffClt createEditletEventHandler) iworld of
			(Ok (jsScript, jsID, jsIC, jsAD),iworld)
				# opts = editletOpts jsScript jsID jsIC jsAD uiDef
				= (UIEditor {UIEditor|optional=False,attributes='DM'.newMap} (ui uiDef opts), {VSt|vst & iworld = iworld})
			(Error e,iworld) //TODO: Propagate the error to the interact task that creates the editor
				# opts = editletOpts "" "" "" "" uiDef
				= (UIEditor {UIEditor|optional=False,attributes='DM'.newMap} (ui uiDef opts), {VSt|vst & iworld = iworld})
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

		ui uiDef opts = setSize uiDef.ComponentHTML.width uiDef.ComponentHTML.height (UIEditlet defaultSizeOpts opts)
		toJSONA a = case JSONEncode{|*|} False a of
			[json:_]	= json
			_			= JSONNull
/*
		= case 'Data.Map'.get (taskId,editorId dp) editletDiffs of //TODO: -> Diffing should not happen during UI Generation
			  //Only diff with previous value
			  Just (ver,prevValue,opts,diffs)
				# currentDiff   = diffWithPrevValue prevValue currVal
				# (res,iworld)  = diffLinker currentDiff Nothing iworld
				= case res of
					Ok (jsScript,jsCDiff,jsIDiff)
						// Store diffs
						# diffs                              = if (isJust currentDiff) [MDiff (jsCDiff,jsScript):diffs] diffs
						// Increase version number if there is a difference between the reference and the new value
						# ver                                = if (isJust currentDiff) (ver + 1) ver
						# iworld                             = setEditletDiffs ver currVal opts diffs iworld
						= (UIEditor {UIEditor|optional=False,attributes=newMap}
							(ui uiDef {UIEditletOpts|opts & value = toJSONA currVal, initDiff = jsIDiff}), {VSt|vst & iworld = iworld})

					Error e //TODO: Propagate error up
						# jsIDiff = ""
						= (UIEditor {UIEditor|optional=False,attributes=newMap}
							(ui uiDef {UIEditletOpts|opts & value = toJSONA currVal, initDiff = jsIDiff}), {VSt|vst & iworld = iworld})
		where
			toJSONA a = case JSONEncode{|*|} False a of
				[json:_]	= json
				_			= JSONNull

			fromJSONA json = fst (JSONDecode{|*|} False [json])
			
			diffWithPrevValue jsonPrev currVal
				= case fromJSONA jsonPrev of
					Just prev = genDiffSrv prev currVal
					_         = Nothing

			setEditletDiffs ver value opts diffs iworld=:{IWorld|current=current=:{editletDiffs}}
				= {IWorld|iworld & current = {current & editletDiffs = put (taskId,editorId dp) (ver,toJSONA value,opts,diffs) editletDiffs}}
*/
	genDiff` dp old new vst=:{VSt|iworld} //TODO: -> Properly track version numbers
		# currentDiff 	= genDiffSrv old new
		# (res,iworld)  = diffLinker currentDiff Nothing iworld
		= case res of
			Ok (jsScript,jsCDiff,_)
				= (ChangeUI [("applyDiff",[JSONInt 42,JSONString jsCDiff,JSONString jsScript])] [],{VSt|vst & iworld=iworld})
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

		//TODO: Reinstate version checking
/*
		= case JSONDecode{|*|} False [jsonDiff] of
			(Just diff,_)	
				# iworld = case 'DM'.get (taskId,editorId) editletDiffs of
                	Just (refver,jsonRef,opts,diffs) = case JSONDecode{|*|} False [jsonRef] of
                    	(Just ref,_)
                    		| ver <> refver
                    			= {IWorld|iworld & current = {current & 
                    					editletDiffs = 'DM'.put (taskId,editorId) (ver,jsonRef,opts,[MRollback diffId:diffs]) editletDiffs}}
  	                      	# ref = appDiffSrv diff ref
   	                     	# [jsonRef:_] = JSONEncode{|*|} False ref
                        	// If the reference value is changed by its client, keep the version number
                        	= {IWorld|iworld & current = {current &
                        			editletDiffs = 'DM'.put (taskId,editorId) (ver,jsonRef,opts,[MCommit diffId:diffs]) editletDiffs}}
                    	_ = iworld
					Nothing = iworld
				= (appDiffSrv diff ov, Touched, {USt|ust & iworld = iworld})
			_	= (ov,omask,ust)
*/
	appDiff` dp _ val mask ust = (val,mask,ust)

