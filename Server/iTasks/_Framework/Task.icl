implementation module iTasks._Framework.Task

from StdFunc import const, id
import StdClass, StdArray, StdTuple, StdInt, StdList, StdBool, StdMisc
from Data.Map import :: Map
import qualified Data.Map as DM
import Text.HTML, Internet.HTTP, Data.Error, Text.JSON
import iTasks._Framework.IWorld, iTasks.UI.Definition, iTasks._Framework.Util
import iTasks.API.Core.Types
import iTasks._Framework.Generic, iTasks._Framework.Generic.Interaction

from iTasks._Framework.TaskState		import :: TaskTree(..), :: DeferredJSON(..), :: TIMeta(..)
from iTasks.UI.Layout 					import :: LayoutRules(..), autoLayoutRules
from iTasks.API.Common.SDSCombinators	import toDynamic 
from iTasks								import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
import qualified Data.CircularStack as DCS

mkEvalOpts :: TaskEvalOpts
mkEvalOpts =
  { TaskEvalOpts
  | useLayout = Nothing
  , modLayout = Nothing
  , noUI      = False
  , tonicOpts = defaultTonicOpts
  }

defaultTonicOpts :: TonicOpts
defaultTonicOpts = { TonicOpts
                   | inAssignNode            = Nothing
                   , inParallel              = Nothing
                   , captureParallel         = False
                   , currBlueprintModuleName = ""
                   , currBlueprintFuncName   = ""
                   , currBlueprintTaskId     = TaskId 0 0
                   , currBlueprintExprId     = []
                   , callTrace               = 'DCS'.newStack 1024
                   }

fromJSONOfDeferredJSON :: !DeferredJSON -> Maybe a | TC a & JSONDecode{|*|} a
fromJSONOfDeferredJSON (DeferredJSON v)
	= case make_dynamic v of
		(v :: a^)
			-> Just v
fromJSONOfDeferredJSON (DeferredJSONNode json)
	= fromJSON json

make_dynamic v = dynamic v

JSONEncode{|Task|} _ _ tt = [dynamicJSONEncode tt]		
JSONDecode{|Task|} _ _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Task|} _ _ c = (Nothing,c)

gUpdate{|Task|} _ _ _ _ target upd val iworld = basicUpdate (\Void t -> Just t) target upd val iworld

gVerify{|Task|} _ _ mv = alwaysValid mv

gText{|Task|} _ _ _ = ["<Task>"]
gEditor{|Task|} _ _ _ _ _ _ _ _ _ vst = (NormalEditor [(stringDisplay "<Task>", 'DM'.newMap)],vst)

gEditMeta{|Task|} _ _ 		= [{label=Just "Task",hint=Nothing,unit=Nothing}]
gEq{|Task|} _ _ _			= True // tasks are always equal??

gDefault{|Task|} gDefx = Task (\_ -> abort error)
where
	error = "Creating default task functions is impossible"
	
toRefresh :: Event -> Event
toRefresh (EditEvent _ _ _)		= RefreshEvent "Converted from Edit"
toRefresh (ActionEvent _ _)		= RefreshEvent "Converted from Action"
toRefresh (FocusEvent _)		= RefreshEvent "Converted from Focus"
toRefresh (RefreshEvent reason)	= RefreshEvent reason
toRefresh (ResetEvent)          = RefreshEvent "Converted from Reset"

exception :: !e -> TaskException | TC, toString e
exception e = (dynamic e, toString e)

repLayoutRules :: !TaskEvalOpts -> LayoutRules
repLayoutRules {TaskEvalOpts|useLayout,modLayout}	= (fromMaybe id modLayout) (fromMaybe autoLayoutRules useLayout)

finalizeRep :: !TaskEvalOpts !TaskRep -> TaskRep
finalizeRep repOpts=:{TaskEvalOpts|noUI=True} _ = NoRep
finalizeRep repOpts rep = rep

extendCallTrace :: !TaskId !TaskEvalOpts -> TaskEvalOpts
extendCallTrace taskId repOpts=:{TaskEvalOpts|tonicOpts = {callTrace = xs}}
  = case 'DCS'.peek xs of
      Just topTaskId
        | taskId == topTaskId = repOpts
      _ = {repOpts & tonicOpts = {repOpts.tonicOpts & callTrace = 'DCS'.push taskId repOpts.tonicOpts.callTrace}}

wrapConnectionTask :: (ConnectionHandlers l r w) (RWShared () r w) -> ConnectionTask | TC l & TC r & TC w
wrapConnectionTask {ConnectionHandlers|onConnect,whileConnected,onDisconnect} sds
    = ConnectionTask {ConnectionHandlersIWorld|onConnect=onConnect`,whileConnected=whileConnected`,onDisconnect=onDisconnect`} (toDynamic sds)
where
    onConnect` host (r :: r^) env = case onConnect host r of
        (Ok l, mbw, out, close) = case mbw of
            Just w  = (Ok (dynamic l :: l^), Just (dynamic w :: w^), out, close, env)
            Nothing = (Ok (dynamic l :: l^), Nothing, out, close, env)
        (Error e, mbw, out, close) = case mbw of
            Just w  = (Error e, Just (dynamic w :: w^), out, close, env)
            Nothing = (Error e, Nothing, out, close, env)

    whileConnected` mbIn (l :: l^) (r :: r^) env = case whileConnected mbIn l r of
        (Ok l, mbw, out, close) = case mbw of
            Just w  = (Ok (dynamic l :: l^), Just (dynamic w :: w^), out, close, env)
            Nothing = (Ok (dynamic l :: l^), Nothing, out, close, env)
        (Error e, mbw, out, close) = case mbw of
            Just w = (Error e, Just (dynamic w :: w^), out, close, env)
            Nothing = (Error e, Nothing, out, close, env)

    onDisconnect` (l :: l^) (r :: r^) env = case onDisconnect l r of
        (Ok l, mbw) = case mbw of
            Just w  = (Ok (dynamic l :: l^), Just (dynamic w :: w^), env)
            Nothing = (Ok (dynamic l :: l^), Nothing, env)
        (Error e, mbw) = case mbw of
            Just w  = (Error e, Just (dynamic w :: w^), env)
            Nothing = (Error e, Nothing, env)

wrapIWorldConnectionTask :: (ConnectionHandlersIWorld l r w) (RWShared () r w) -> ConnectionTask | TC l & TC r & TC w
wrapIWorldConnectionTask {ConnectionHandlersIWorld|onConnect,whileConnected,onDisconnect} sds
    = ConnectionTask {ConnectionHandlersIWorld|onConnect=onConnect`,whileConnected=whileConnected`,onDisconnect=onDisconnect`} (toDynamic sds)
where
    onConnect` host (r :: r^) env = case onConnect host r env of
        (Ok l, mbw, out, close, env) = case mbw of
            Just w  = (Ok (dynamic l :: l^), Just (dynamic w :: w^), out, close, env)
            Nothing = (Ok (dynamic l :: l^), Nothing, out, close, env)
        (Error e, mbw, out, close, env) = case mbw of
            Just w  = (Error e, Just (dynamic w :: w^), out, close, env)
            Nothing = (Error e, Nothing, out, close, env)

    whileConnected` mbIn (l :: l^) (r :: r^) env = case whileConnected mbIn l r env of
        (Ok l, mbw, out, close, env) = case mbw of
            Just w  = (Ok (dynamic l :: l^), Just (dynamic w :: w^), out, close, env)
            Nothing = (Ok (dynamic l :: l^), Nothing, out, close, env)
        (Error e, mbw, out, close, env) = case mbw of
            Just w = (Error e, Just (dynamic w :: w^), out, close, env)
            Nothing = (Error e, Nothing, out, close, env)

    onDisconnect` (l :: l^) (r :: r^) env = case onDisconnect l r env of
        (Ok l, mbw, env) = case mbw of
            Just w  = (Ok (dynamic l :: l^), Just (dynamic w :: w^), env)
            Nothing = (Ok (dynamic l :: l^), Nothing, env)
        (Error e, mbw, env) = case mbw of
            Just w  = (Error e, Just (dynamic w :: w^), env)
            Nothing = (Error e, Nothing, env)

mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a
mkInstantTask iworldfun = Task (evalOnce iworldfun)
where
	evalOnce f _ repOpts (TCInit taskId ts) iworld = case f taskId iworld of	
		(Ok a,iworld)							= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep repOpts NoRep) (TCStable taskId ts (DeferredJSON a)), iworld)
		(Error e, iworld)					    = (ExceptionResult e, iworld)

	evalOnce f _ repOpts state=:(TCStable taskId ts enc) iworld = case fromJSONOfDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep repOpts NoRep) state, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

	evalOnce f _ _ (TCDestroy _) iworld	= (DestroyedResult,iworld)

