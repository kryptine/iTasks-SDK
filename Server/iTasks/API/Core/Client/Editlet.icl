implementation module iTasks.API.Core.Client.Editlet

import iTasks.Framework.Client.LinkerSupport, Data.Maybe
from Data.Map import :: Map, newMap, put
import StdMisc

//* Client-side types
JSONEncode{|Editlet|} _ _ tt = [dynamicJSONEncode tt]		
JSONDecode{|Editlet|} _ _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Editlet|} _ _ c = (Nothing,c)

gDefault{|Editlet|} fa _
	= {Editlet|value=fa,html = \_ -> RawText "", updateUI = \_ _ a st world -> (a, st, world), handlers= \_ -> [], genDiff = \_ _ -> Nothing, appDiff = \_ x -> x}

gEq{|Editlet|} fa _ x y = fa x.Editlet.value y.Editlet.value //Only compare values

gVisualizeText{|Editlet|} fa _ mode {Editlet|value} = fa mode value

gEditor{|Editlet|} fa textA defaultA headersA jsonEncA jsonDecA _ _ _ _ jsonEncD jsonDecD dp ({Editlet|value,html,updateUI,handlers,genDiff,appDiff},mask,ver) meta vst=:{VSt|taskId,iworld}
	# (jsScript, jsEvents, jsIV, jsUU, jsGD, jsAD, iworld)
			= editletLinker [(id, event, f) \\(ComponentEvent id event f) <- handlers htmlId] clientInit clientUpdateUI clientGenDiff clientAppDiff iworld
	# iworld									= addDiffer iworld
	= (NormalEditor [(ui jsScript jsEvents jsIV jsUU jsGD jsAD, newMap)],{VSt|vst & iworld = iworld})
where
    htmlId = "editlet-" +++ taskId +++ "-" +++ editorId dp

	ui jsScript jsEvents jsIV jsUU jsGD jsAD
		= UIEditlet defaultSizeOpts {UIEditletOpts|taskId=taskId,editorId=editorId dp,value=toJSONA value, html = toString (html htmlId)
								    ,script = Just jsScript, events = Just jsEvents, initValue = Just jsIV, updateUI = Just jsUU, genDiff = Just jsGD, appDiff = Just jsAD}
	
	toJSONA a = case jsonEncA a of
		[json:_]	= json
		_			= JSONNull
	toJSOND d = case jsonEncD d of
		[json:_]	= json
		_			= JSONNull
	
	clientInit json = case jsonDecA [json] of
		(Just a,_)	= a
		_			= abort "Editlet cannot initialize its value"
	
	serverGenDiff jsonOld jsonNew
		= case (jsonDecA [jsonOld],jsonDecA [jsonNew]) of
			((Just old,_),(Just new,_))	= case genDiff old new of
				Just diff				= Just (toJSOND diff)
				Nothing					= Nothing
			_							= Nothing
	
	clientAppDiff json old = case jsonDecD [json] of
		(Just diff,_)	= appDiff diff old
		_				= old
	
	clientGenDiff old new = case (genDiff old new) of
		Just diff		= toJSOND diff
		_				= JSONNull
	
	clientUpdateUI id Nothing val mbSt world = updateUI id Nothing val mbSt world
	clientUpdateUI id (Just json) val mbSt world 
			= case jsonDecD [json] of
				(Just diff,_) = updateUI id (Just diff) val mbSt world
				_		      = abort "Error in JSON argument"

	addDiffer iworld=:{IWorld|uiDiffers}
		= {IWorld|iworld & uiDiffers = put (taskId,editorId dp) serverGenDiff uiDiffers}

gEditMeta{|Editlet|} fa _ {Editlet|value} = fa value

gUpdate{|Editlet|} fa _ jDeca _ _ jDecd [] json (ov=:{Editlet|value,appDiff},omask)
	= case jDecd [json] of
		(Just diff,_)	= ({Editlet|ov & value = appDiff diff value},Touched)
		_				= (ov,omask)

gUpdate{|Editlet|} fa _ _ _ _ _ _ _ mv = mv

gVerify{|Editlet|} fa _ _ mv = alwaysValid mv

createEditletEventHandler :: (ComponentEventHandlerFunc a st) !ComponentId -> (JSVal (JSFunction b)) 
createEditletEventHandler handler id = undef

