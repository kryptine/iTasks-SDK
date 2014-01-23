implementation module iTasks.API.Core.Client.Editlet

import iTasks.Framework.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks.Framework.IWorld
from Data.Map import :: Map, newMap, put
from Data.Map import qualified get
import StdMisc

toEditlet :: (EditletSimpl a d) -> (Editlet a d) | iTask a
toEditlet (EditletSimpl a {EditletSimplDef|genUI,updateUI,genDiff,appDiff})
	= Editlet a {EditletServerDef|genUI = genUI, defVal = gDefault{|*|}, genDiff = genDiff, appDiff = appDiff}
				{EditletClientDef|updateUI = updateUI, defVal = gDefault{|*|}, genDiff = genDiff, appDiff = appDiff}

//* Client-side types
JSONEncode{|Editlet|} _ _ tt = [dynamicJSONEncode tt]		
JSONDecode{|Editlet|} _ _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Editlet|} _ _ c = (Nothing,c)

gDefault{|Editlet|} fa _
    = Editlet fa { EditletServerDef
    			 | genUI	= \_ world -> ({html = RawText "", eventHandlers = [], height = FlexSize, width = FlexSize}, world)
    			 , defVal	= fa
    			 , genDiff	= \_ _ -> Nothing
    			 , appDiff	= \_ x -> x
    			 }
				 { EditletClientDef
    			 | updateUI = \_ _ a world -> (a, world)
    			 , defVal	= fa
    			 , genDiff	= \_ _ -> Nothing
    			 , appDiff	= \_ x -> x
    			 }

gEq{|Editlet|} fa _ (Editlet x _ _) (Editlet y _ _) = fa x y //Only compare values

gVisualizeText{|Editlet|} fa _ mode (Editlet value _ _) = fa mode value

gEditor{|Editlet|} fa textA defaultA headersA jsonEncA jsonDecA _ _ _ _ jsonEncD jsonDecD dp
						(Editlet value serverDef clientDef, mask, ver) meta vst=:{VSt|taskId,iworld=iworld=:{IWorld|current={editletDiffs},world}}
	# (uiDef, world)        = serverDef.EditletServerDef.genUI htmlId world
	# iworld                = {iworld & world = world}
    = case 'Data.Map'.get (taskId,editorId dp) editletDiffs of
        //Only diff with previous value
        Just (prevValue,opts,diffs)
            # currentDiff                        = diffWithPrevValue prevValue value
            # (jsScript,jsCDiff,jsIDiff,iworld)  = diffLinker currentDiff initDiff iworld
            // Store diffs
            # diffs                              = if (isJust currentDiff) [(jsCDiff,jsScript):diffs] diffs
            # iworld                             = setEditletDiffs value opts diffs iworld
            = (NormalEditor [(ui uiDef {UIEditletOpts|opts & value = toJSONA value, initDiff = jsIDiff}, newMap)],{VSt|vst & iworld = iworld})
        //Create editlet definition and store reference value for future diffs
        Nothing
	        # (jsScript, jsEvents, jsID, jsDV, jsUU, jsGD, jsAD, iworld)
			    = editletLinker [(cid, event, f) \\ ComponentEvent cid event f <- uiDef.eventHandlers]
							initDiff defValueFun clientUpdateUI clientGenDiff clientAppDiff iworld
            # opts = editletOpts jsScript jsEvents jsID jsDV jsUU jsGD jsAD uiDef
            # iworld = setEditletDiffs value {UIEditletOpts|opts & value = JSONNull} [] iworld
	        = (NormalEditor [(ui uiDef opts, newMap)],{VSt|vst & iworld = iworld})
where
    htmlId = "editlet-" +++ taskId +++ "-" +++ editorId dp

    editletOpts jsScript jsEvents jsID jsDV jsUU jsGD jsAD uiDef
        = { UIEditletOpts
		  | taskId 	    = taskId
		  , editorId	= editorId dp
		  , value		= toJSONA value
		  , html 		= toString uiDef.ComponentHTML.html
		  , script	    = jsScript
		  , events 	    = jsEvents
		  , defVal	    = jsDV
		  , initDiff	= jsID
		  , updateUI 	= jsUU
		  , genDiff 	= jsGD
		  , appDiff 	= jsAD
          }

	ui uiDef opts = setSize uiDef.ComponentHTML.width uiDef.ComponentHTML.height (UIEditlet defaultSizeOpts opts)
	
	toJSONA a = case jsonEncA a of
		[json:_]	= json
		_			= JSONNull

    fromJSONA json = fst (jsonDecA [json])

	// Argument is necessary to stop evaluation on the server
	defValueFun _ = clientDef.EditletClientDef.defVal
	
    initDiff = serverDef.EditletServerDef.genDiff serverDef.EditletServerDef.defVal value

    diffWithPrevValue jsonPrev value
        = case fromJSONA jsonPrev of
            Just prev = serverDef.EditletServerDef.genDiff prev value
            _         = Nothing

    clientAppDiff = clientDef.EditletClientDef.appDiff

	clientGenDiff old new = case (clientDef.EditletClientDef.genDiff old new) of
		Just diff		= toJSOND diff
		_				= JSONNull
    where
	    toJSOND d = case jsonEncD d of
		    [json:_]	= json
		    _			= JSONNull

	clientUpdateUI = clientDef.EditletClientDef.updateUI

    setEditletDiffs value opts diffs iworld=:{IWorld|current=current=:{editletDiffs}}
        = {IWorld|iworld & current = {current & editletDiffs = put (taskId,editorId dp) (toJSONA value,opts,diffs) editletDiffs}}

gEditMeta{|Editlet|} fa _ (Editlet value _ _) = fa value

gUpdate{|Editlet|} fa _ jEnca jDeca _ _ jEncd jDecd [] jsonDiff (ov=:(Editlet value defsv=:{EditletServerDef|appDiff} defcl),omask) ust=:{USt|taskId,editorId,iworld=iworld=:{IWorld|current=current=:{editletDiffs}}}
	= case jDecd [jsonDiff] of
		(Just diff,_)
            # iworld = case 'Data.Map'.get (taskId,editorId) editletDiffs of
                Just (jsonRef,opts,diffs) = case jDeca [jsonRef] of
                    (Just ref,_)
                        # ref = appDiff diff ref
                        # [jsonRef:_] = jEnca ref
                        = {IWorld|iworld & current = {current & editletDiffs = put (taskId,editorId) (jsonRef,opts,diffs) editletDiffs}}
                    _ = iworld
                Nothing = iworld
            = ((Editlet (appDiff diff value) defsv defcl,Touched),{USt|ust & iworld = iworld})
		_				= ((ov,omask),ust)

gUpdate{|Editlet|} fa _ _ _ _ _ _ _ _ _ mv iworld = (mv,iworld)
gVerify{|Editlet|} fa _ _ mv = alwaysValid mv

createEditletEventHandler :: (EditletEventHandlerFunc a) !ComponentId -> (JSVal (JSFunction b)) 
createEditletEventHandler handler id = undef

