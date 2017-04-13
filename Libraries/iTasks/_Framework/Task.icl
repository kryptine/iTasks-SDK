implementation module iTasks._Framework.Task

from StdFunc import const, id
import StdClass, StdArray, StdTuple, StdInt, StdList, StdBool, StdMisc
from Data.Map import :: Map
import qualified Data.Map as DM
import Text.HTML, Internet.HTTP, Data.Error, Text.JSON
import iTasks._Framework.IWorld, iTasks.UI.Definition, iTasks._Framework.Util, iTasks._Framework.DynamicUtil
import iTasks.API.Core.Types
import iTasks._Framework.Generic
from System.OSError import :: MaybeOSError

import iTasks.UI.Editor, iTasks.UI.Editor.Common

from iTasks._Framework.TaskState		import :: TaskTree(..), :: DeferredJSON(..), :: TIMeta(..)
from iTasks.API.Common.SDSCombinators	import toDynamic 
from iTasks._Framework.Serialization    import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
import qualified Data.CircularStack as DCS
from Data.CircularStack import :: CircularStack
from iTasks._Framework.Tonic.AbsSyn import :: ExprId (..)

mkEvalOpts :: TaskEvalOpts
mkEvalOpts =
  { TaskEvalOpts
  | noUI        = False
  , tonicOpts   = defaultTonicOpts
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

gText{|Task|} _ _ _ = ["<Task>"]
gEditor{|Task|} _ _ _ _ _ = emptyEditor
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

wrapExternalProcTask :: !(ExternalProcessHandlers l r w) !(RWShared () r w) -> ExternalProcessTask | TC l & TC r & TC w & iTask l
wrapExternalProcTask {onStartup, whileRunning, onExit} sds
    = ExternalProcessTask {onStartup = onStartup`, whileRunning = whileRunning`, onExit = onExit`} (toDynamic sds)
where
    onStartup` (r :: r^) = (toDyn <$> mbl, toDyn <$> mbw, out, close)
    where
        (mbl, mbw, out, close) = onStartup r
    whileRunning` mbData (l :: l^) (r :: r^) = (toDyn <$> mbl, toDyn <$> mbw, out, close)
    where
        (mbl, mbw, out, close) = whileRunning mbData l r
    onExit` eCode (l :: l^) (r :: r^) = (toDyn <$> mbl, toDyn <$> mbw)
    where
        (mbl, mbw) = onExit eCode l r

mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a
mkInstantTask iworldfun = Task (evalOnce iworldfun)
where
	evalOnce f event repOpts (TCInit taskId ts) iworld = case f taskId iworld of	
		(Ok a,iworld)							= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) (TCStable taskId ts (DeferredJSON a)), iworld)
		(Error e, iworld)					    = (ExceptionResult e, iworld)

	evalOnce f event repOpts state=:(TCStable taskId ts enc) iworld = case fromJSONOfDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) state, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

	evalOnce f _ _ (TCDestroy _) iworld	= (DestroyedResult,iworld)

	rep ResetEvent  = ReplaceUI (ui UIEmpty)
	rep _ 			= NoChange	

