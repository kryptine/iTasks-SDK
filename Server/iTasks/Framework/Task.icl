implementation module iTasks.Framework.Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc
import Text.HTML, Internet.HTTP, Data.Map, Data.Error, Text.JSON
import iTasks.Framework.IWorld, iTasks.Framework.UIDefinition, iTasks.Framework.Util
import iTasks.API.Core.SystemTypes
import iTasks.Framework.Generic, iTasks.Framework.Generic.Interaction

from iTasks.Framework.TaskState			import :: TaskTree(..), :: DeferredJSON(..), :: TIMeta(..), :: SessionInfo(..)
from iTasks.API.Core.LayoutCombinators	import :: LayoutRules(..), autoLayoutRules
from iTasks								import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode

mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a
mkInstantTask iworldfun = Task (evalOnce iworldfun)
where
	evalOnce f _ repOpts (TCInit taskId ts) iworld = case f taskId iworld of	
		(Ok a,iworld)							= (ValueResult (Value a True) {lastEvent=ts,refreshSensitive=False} (finalizeRep repOpts NoRep) (TCStable taskId ts (DeferredJSON a)), iworld)
		(Error (e,s), iworld)					= (ExceptionResult e s, iworld)

	evalOnce f _ repOpts state=:(TCStable taskId ts enc) iworld = case fromJSONOfDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,refreshSensitive=False} (finalizeRep repOpts NoRep) state, iworld)
		Nothing	= (exception "Corrupt task result", iworld)

	evalOnce f _ _ (TCDestroy _) iworld	= (DestroyedResult,iworld)

fromJSONOfDeferredJSON :: !DeferredJSON -> Maybe a | TC a & JSONDecode{|*|} a
fromJSONOfDeferredJSON (DeferredJSON v)
	= case make_dynamic v of
		(v :: a^)
			-> Just v
fromJSONOfDeferredJSON (DeferredJSONNode json)
	= fromJSON json

make_dynamic v = dynamic v

JSONEncode{|Task|} _ tt = [dynamicJSONEncode tt]		
JSONDecode{|Task|} _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Task|} _ c = (Nothing,c)

gUpdate{|Task|} _ _ _ target upd val = basicUpdate (\Void t -> Just t) target upd val

gVerify{|Task|} _ _ mv = alwaysValid mv

gVisualizeText{|Task|} _ _ _ = ["<Task>"]
gEditor{|Task|} _ _ _ _ _ _ _ _ _ vst = (NormalEditor [(stringDisplay "<Task>",newMap)],vst)

gEditMeta{|Task|} _ _ 		= [{label=Just "Task",hint=Nothing,unit=Nothing}]
gEq{|Task|} _ _ _			= True // tasks are always equal??

gDefault{|Task|} gDefx = Task (\_ -> abort error)
where
	error = "Creating default task functions is impossible"
	
toRefresh :: Event -> Event
toRefresh (EditEvent no _ _ _)	= RefreshEvent (Just no)
toRefresh (ActionEvent no _ _)	= RefreshEvent (Just no)
toRefresh (FocusEvent no _)		= RefreshEvent (Just no)
toRefresh (RefreshEvent mbNo)	= RefreshEvent mbNo

exception :: !e -> TaskResult a | TC, toString e
exception e = ExceptionResult (dynamic e) (toString e)

repLayoutRules :: !TaskRepOpts -> LayoutRules
repLayoutRules {TaskRepOpts|useLayout,modLayout}	= (fromMaybe id modLayout) (fromMaybe autoLayoutRules useLayout)

finalizeRep :: !TaskRepOpts !TaskRep -> TaskRep
finalizeRep repOpts=:{TaskRepOpts|noUI=True} _ = NoRep
finalizeRep repOpts=:{TaskRepOpts|appFinalLayout=True} rep=:(TaskRep def parts) = TaskRep (UIFinal ((repLayoutRules repOpts).LayoutRules.layoutFinal def)) parts
finalizeRep repOpts rep = rep

