implementation module iTasks.API.Core.Client.Editlet

import iTasks.Framework.Client.LinkerSupport, Data.Maybe, Data.Functor
import iTasks.Framework.IWorld
from Data.Map import :: Map, newMap, put
from Data.Map import qualified get
import StdMisc

toEditlet :: (EditletSimpl a d) -> (Editlet a d) | iTask a
toEditlet (EditletSimpl a {EditletSimplDef|genUI,updateUI,genDiff,appDiff})
  = { Editlet
    | currVal   = a
    , genUI     = genUI
    , serverDef = {EditletDef | performIO = \_ _ s w -> (s, w), defVal = gDefault{|*|}, genDiff = genDiff, appDiff = appDiff}
    , clientDef = {EditletDef | performIO = updateUI, defVal = gDefault{|*|}, genDiff = genDiff, appDiff = appDiff}
    }

//* Client-side types
JSONEncode{|Editlet|} _ _ _ tt = [dynamicJSONEncode tt]		
JSONDecode{|Editlet|} _ _ _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Editlet|} _ _ _ c = (Nothing,c)

gDefault{|Editlet|} fa _
  = { Editlet
    | currVal   = fa
    , genUI     = \_ world -> ({html = RawText "", eventHandlers = [], height = FlexSize, width = FlexSize}, world)
    , serverDef = { EditletDef
                  | performIO = \_ _ s w -> (s, w)
                  , defVal    = fa
                  , genDiff   = \_ _ -> Nothing
                  , appDiff   = \_ x -> x
                  }
    , clientDef = { EditletDef
                  | performIO = \_ _ a world -> (a, world)
                  , defVal  = fa
                  , genDiff  = \_ _ -> Nothing
                  , appDiff  = \_ x -> x
                  }
  }

gEq{|Editlet|} fa _ editlet1 editlet2 = fa editlet1.Editlet.currVal editlet2.Editlet.currVal //Only compare values

gText{|Editlet|} fa _ mode (Just editlet) = fa mode (Just editlet.Editlet.currVal)
gText{|Editlet|} fa _ mode Nothing = fa mode Nothing

gEditor{|Editlet|} fa textA defaultA headersA jsonEncA jsonDecA _ _ _ _ jsonEncD jsonDecD dp
    ({ Editlet | currVal, genUI, serverDef, clientDef}, mask, ver) meta vst=:{VSt|taskId,iworld=iworld=:{IWorld|current={editletDiffs},world}}
  # (uiDef, world)        = genUI htmlId world
  # iworld                = {iworld & world = world}
  = case 'Data.Map'.get (taskId,editorId dp) editletDiffs of
      //Only diff with previous value
      Just (prevValue,opts,diffs)
        # currentDiff                        = diffWithPrevValue prevValue currVal
        # (jsScript,jsCDiff,jsIDiff,iworld)  = diffLinker currentDiff initDiff iworld
        // Store diffs
        # diffs                              = if (isJust currentDiff) [(jsCDiff,jsScript):diffs] diffs
        # iworld                             = setEditletDiffs currVal opts diffs iworld
        = (NormalEditor [(ui uiDef {UIEditletOpts|opts & value = toJSONA currVal, initDiff = jsIDiff}, newMap)],{VSt|vst & iworld = iworld})
      //Create editlet definition and store reference value for future diffs
      Nothing
        # (jsScript, jsEvents, jsID, jsDV, jsUU, jsGD, jsAD, iworld)
            = editletLinker [(cid, event, f) \\ ComponentEvent cid event f <- uiDef.eventHandlers]
                initDiff defValueFun clientUpdateUI clientGenDiff clientAppDiff iworld
        # opts = editletOpts jsScript jsEvents jsID jsDV jsUU jsGD jsAD uiDef
        # iworld = setEditletDiffs currVal {UIEditletOpts|opts & value = JSONNull} [] iworld
        = (NormalEditor [(ui uiDef opts, newMap)],{VSt|vst & iworld = iworld})
where
    htmlId = "editlet-" +++ taskId +++ "-" +++ editorId dp

    editletOpts jsScript jsEvents jsID jsDV jsUU jsGD jsAD uiDef
        = { UIEditletOpts
		  | taskId 	    = taskId
		  , editorId	= editorId dp
		  , value		= toJSONA currVal
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
	
	toJSONA a = case jsonEncA False a of
		[json:_]	= json
		_			= JSONNull

    fromJSONA json = fst (jsonDecA False [json])

	// Argument is necessary to stop evaluation on the server
	defValueFun _ = clientDef.EditletDef.defVal
	
    initDiff = serverDef.EditletDef.genDiff serverDef.EditletDef.defVal currVal

    diffWithPrevValue jsonPrev currVal
        = case fromJSONA jsonPrev of
            Just prev = serverDef.EditletDef.genDiff prev currVal
            _         = Nothing

    clientAppDiff = clientDef.EditletDef.appDiff

	clientGenDiff old new = case (clientDef.EditletDef.genDiff old new) of
		Just diff		= toJSOND diff
		_				= JSONNull
    where
	    toJSOND d = case jsonEncD False d of
		    [json:_]	= json
		    _			= JSONNull

	clientUpdateUI = clientDef.EditletDef.performIO

    setEditletDiffs value opts diffs iworld=:{IWorld|current=current=:{editletDiffs}}
        = {IWorld|iworld & current = {current & editletDiffs = put (taskId,editorId dp) (toJSONA value,opts,diffs) editletDiffs}}

gEditMeta{|Editlet|} fa _ editlet = fa editlet.Editlet.currVal

gUpdate{|Editlet|} fa _ jEnca jDeca _ _ jEncd jDecd [] jsonDiff (ov, omask) ust=:{USt|taskId,editorId,iworld=iworld=:{IWorld|current=current=:{editletDiffs}}}
  # serverDef = ov.Editlet.serverDef
  = case jDecd False [jsonDiff] of
      (Just diff, _)
        # iworld = case 'Data.Map'.get (taskId,editorId) editletDiffs of
                     Just (jsonRef,opts,diffs)
                       = case jDeca False [jsonRef] of
                           (Just ref, _)
                             # ref           = serverDef.EditletDef.appDiff diff ref
                             # (ref`, world) = serverDef.EditletDef.performIO
                                                 ("editlet-" +++ taskId +++ "-" +++ editorId)
                                                 (Just diff) ref iworld.world
                             # iworld        = { iworld & world = world }
                             # [jsonRef:_]   = jEnca False ref`
                             # ioDiff        = serverDef.EditletDef.genDiff ref ref`
                             # initDiff      = serverDef.EditletDef.genDiff serverDef.EditletDef.defVal ref`
                             # (jsScript, jsCDiff, _, iworld) = diffLinker ioDiff initDiff iworld
                             # diffs                          = if (isJust ioDiff) [(jsCDiff,jsScript):diffs] diffs
                             = { IWorld
                               | iworld
                               & current = {current & editletDiffs = put (taskId,editorId) (jsonRef,opts,diffs) editletDiffs}
                               }
                           _ = iworld
                     Nothing = iworld
        = (({ ov & currVal = serverDef.EditletDef.appDiff diff ov.Editlet.currVal }
            , Touched),{USt|ust & iworld = iworld})
      _ = ((ov,omask),ust)

gUpdate{|Editlet|} fa _ _ _ _ _ _ _ _ _ mv iworld = (mv,iworld)
gVerify{|Editlet|} fa _ _ mv = alwaysValid mv

createEditletEventHandler :: (EditletEventHandlerFunc a) !ComponentId -> JSFun b
createEditletEventHandler handler id = undef

