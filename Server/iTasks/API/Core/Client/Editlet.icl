implementation module iTasks.API.Core.Client.Editlet

import iTasks._Framework.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks._Framework.IWorld
from iTasks._Framework.UIDiff import :: MessageType (MDiff,MRollback,MCommit)
from Data.Map import :: Map, newMap, put
from Data.Map import qualified get
import StdMisc
 
// Client-side types
JSONEncode{|Editlet|} _ _ _ _ tt = [dynamicJSONEncode tt]		
JSONDecode{|Editlet|} _ _ _ _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Editlet|} _ _ _ _ c = (Nothing,c)
 
gDefault{|Editlet|} fa _ fc
  = { Editlet
    | currVal   = fa
    , defValSrv = fa
        
    , genUI     = \_ world -> ({ComponentHTML | html = RawText "", height = FlexSize, width = FlexSize}, world)
    , initClient = \_ _ world -> (fc, world)
    , appDiffClt = \_ _ _ a world -> (a, world)
    , genDiffSrv = \_ _ -> Nothing
    , appDiffSrv = \_ x -> x                  
  }

gEq{|Editlet|} fa _ _ editlet1 editlet2 = fa editlet1.Editlet.currVal editlet2.Editlet.currVal //Only compare values

gText{|Editlet|} fa _ _ mode (Just editlet) = fa mode (Just editlet.Editlet.currVal)
gText{|Editlet|} fa _ _ mode Nothing = fa mode Nothing

import graph_to_sapl_string

gEditor{|Editlet|} fa textA defaultA headersA jsonEncA jsonDecA _ _ _ _ _ _ _ _ _ _ jsonEncD jsonDecD dp
    ({ Editlet | currVal, defValSrv, genUI, initClient, appDiffClt, genDiffSrv, appDiffSrv}, mask, ver) 
    meta vst=:{VSt|taskId,iworld=iworld=:{IWorld|current={editletDiffs},world}}
    
  # (uiDef, world)        = genUI htmlId world
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
            = editletLinker initDiff (initClient createEditletEventHandler) (appDiffClt createEditletEventHandler) iworld
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
	
	toJSONA a = case jsonEncA False a of
		[json:_]	= json
		_			= JSONNull

    fromJSONA json = fst (jsonDecA False [json])
	
    initDiff = genDiffSrv defValSrv currVal

    diffWithPrevValue jsonPrev currVal
        = case fromJSONA jsonPrev of
            Just prev = genDiffSrv prev currVal
            _         = Nothing

    setEditletDiffs ver value opts diffs iworld=:{IWorld|current=current=:{editletDiffs}}
        = {IWorld|iworld & current = {current & editletDiffs = put (taskId,editorId dp) (ver,toJSONA value,opts,diffs) editletDiffs}}

gEditMeta{|Editlet|} fa _ _ editlet = fa editlet.Editlet.currVal

gUpdate{|Editlet|} fa _ jEnca jDeca _ _ jEncd jDecd _ _ _ _ [] jsonDiff (ov, omask) ust=:{USt|taskId,editorId,iworld=iworld=:{IWorld|current=current=:{editletDiffs}}}

	// Bit dirty, but we need to unwrap the "unexpected" version number and the expected diff
	# (ver, diffId, jsonDiff) = case jsonDiff of
			JSONArray [ver, diffId, diff] = (maybe -1 id (fromJSON ver), maybe -1 id (fromJSON diffId), diff)
								  = (-1, -1, JSONNull)
				
	= case jDecd False [jsonDiff] of
		(Just diff,_)	
            # iworld = case 'Data.Map'.get (taskId,editorId) editletDiffs of
                Just (refver,jsonRef,opts,diffs) = case jDeca False [jsonRef] of
                    (Just ref,_)
                    	| ver <> refver
                    		= {IWorld|iworld & current = {current & 
                    				editletDiffs = put (taskId,editorId) (ver,jsonRef,opts,[MRollback diffId:diffs]) editletDiffs}}

                        # ref = ov.Editlet.appDiffSrv diff ref
                        # [jsonRef:_] = jEnca False ref
                        // If the reference value is changed by its client, keep the version number
                        = {IWorld|iworld & current = {current &
                        			editletDiffs = put (taskId,editorId) (ver,jsonRef,opts,[MCommit diffId:diffs]) editletDiffs}}
                        			
                    _ = iworld
                Nothing = iworld
            = (({ ov & currVal = ov.Editlet.appDiffSrv diff ov.Editlet.currVal }
                , Touched),{USt|ust & iworld = iworld})
		_	= ((ov,omask), trace_n ("Failed to decode JSON: " +++ toString jsonDiff) ust)
gUpdate{|Editlet|} fa _ _ _ _ _ _ _ _ _ _ _ _ _ mv iworld = (mv,iworld)
gVerify{|Editlet|} fa _ _ _ mv = alwaysValid mv
import StdDebug

createEditletEventHandler :: (EditletEventHandlerFunc d a) !ComponentId -> JSFun b
createEditletEventHandler handler id = undef

