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
						(Editlet value serverDef clientDef, mask, ver) meta vst=:{VSt|taskId,iworld}
	# (uiDef, world)        = serverDef.EditletServerDef.genUI htmlId iworld.world
	# iworld                = {iworld & world = world}
    # (mbPrevValue,iworld)  = getPreviousEditletValue iworld
    # nextDiff              = diffWithPrevValue mbPrevValue value
	# (jsScript, jsEvents, jsID, jsPD, jsDV, jsUU, jsGD, jsAD, iworld)
			= editletLinker [(cid, event, f) \\ ComponentEvent cid event f <- uiDef.eventHandlers]
							initDiff nextDiff defValueFun clientUpdateUI clientGenDiff clientAppDiff iworld
    # iworld = updEditletDiffs value initDiff nextDiff jsPD iworld
	= (NormalEditor [(ui jsScript jsEvents jsID jsDV jsUU jsGD jsAD uiDef, newMap)],{VSt|vst & iworld = iworld})
where
    htmlId = "editlet-" +++ taskId +++ "-" +++ editorId dp

	ui jsScript jsEvents jsID jsDV jsUU jsGD jsAD uiDef
		= setSize uiDef.ComponentHTML.width uiDef.ComponentHTML.height
			(UIEditlet defaultSizeOpts { UIEditletOpts
									| taskId 	= taskId
									, editorId	= editorId dp
									, value		= toJSONA value
									, html 		= toString uiDef.ComponentHTML.html
								    , script	= jsScript
								    , events 	= jsEvents
								    , defVal	= jsDV
								    , initDiff	= jsID
								    , updateUI 	= jsUU
								    , genDiff 	= jsGD
								    , appDiff 	= jsAD})
	
	toJSONA a = case jsonEncA a of
		[json:_]	= json
		_			= JSONNull

    fromJSONA json = fst (jsonDecA [json])

	// Argument is necessary to stop evaluation on the server
	defValueFun _ = clientDef.EditletClientDef.defVal
	
    initDiff = serverDef.EditletServerDef.genDiff serverDef.EditletServerDef.defVal value

    diffWithPrevValue (Just jsonPrev) value
        = case fromJSONA jsonPrev of
            Just prev = serverDef.EditletServerDef.genDiff prev value
            _         = Nothing
    diffWithPrevValue _ _ = Nothing

    clientAppDiff = clientDef.EditletClientDef.appDiff

	clientGenDiff old new = case (clientDef.EditletClientDef.genDiff old new) of
		Just diff		= toJSOND diff
		_				= JSONNull
    where
	    toJSOND d = case jsonEncD d of
		    [json:_]	= json
		    _			= JSONNull

	clientUpdateUI = clientDef.EditletClientDef.updateUI

    getPreviousEditletValue iworld=:{IWorld|editletDiffs}
        = (fmap fst ('Data.Map'.get (taskId,editorId dp) editletDiffs),{IWorld|iworld & editletDiffs = editletDiffs})

    updEditletDiffs value iDiff tDiff jsDiff iworld=:{IWorld|editletDiffs}
        # diffs = maybe [] snd ('Data.Map'.get (taskId,editorId dp) editletDiffs)
        # diffs = if (tDiff=:Nothing) diffs (diffs ++ [jsDiff])
        = {IWorld|iworld & editletDiffs = put (taskId,editorId dp) (toJSONA value,diffs) editletDiffs}

gEditMeta{|Editlet|} fa _ (Editlet value _ _) = fa value

gUpdate{|Editlet|} fa _ jEnca jDeca _ _ jEncd jDecd [] jsonDiff (ov=:(Editlet value defsv=:{EditletServerDef|appDiff} defcl),omask) ust=:{USt|taskId,editorId,iworld=iworld=:{IWorld|editletDiffs}}
	= case jDecd [jsonDiff] of
		(Just diff,_)
            # iworld = case 'Data.Map'.get (taskId,editorId) editletDiffs of
                Just (jsonRef,diffs) = case jDeca [jsonRef] of
                    (Just ref,_)
                        # ref = appDiff diff ref
                        # [jsonRef:_] = jEnca ref
                        = {IWorld|iworld & editletDiffs = put (taskId,editorId) (jsonRef,diffs) editletDiffs}
                    _ = iworld
                Nothing = iworld
            = ((Editlet (appDiff diff value) defsv defcl,Touched),{USt|ust & iworld = iworld})
		_				= ((ov,omask),ust)

gUpdate{|Editlet|} fa _ _ _ _ _ _ _ _ _ mv iworld = (mv,iworld)
gVerify{|Editlet|} fa _ _ mv = alwaysValid mv

createEditletEventHandler :: (EditletEventHandlerFunc a) !ComponentId -> (JSVal (JSFunction b)) 
createEditletEventHandler handler id = undef

