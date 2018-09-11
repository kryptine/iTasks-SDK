implementation module iTasks.Internal.SDSService

import iTasks

from Internet.HTTP					import :: HTTPRequest {req_method, req_path, req_data}, :: HTTPResponse(..), :: HTTPMethod(..)
from iTasks.Internal.WebService   import :: ConnectionState, :: WebSockState, :: WebService(..)
from iTasks.Internal.TaskState 	import :: TIUIState

import iTasks.Internal.HtmlUtil, iTasks.Internal.DynamicUtil
import iTasks.Internal.RemoteAccess
from iTasks.Extensions.Web import callHTTP
import iTasks.Internal.SDS
import iTasks.Internal.AsyncSDS
import iTasks.Internal.IWorld

from StdFunc import o
import StdString, StdList, StdArray
import qualified Data.Map as DM
import Data.Maybe, Data.Error
import Text.GenJSON, Text.URI
import StdMisc, StdDebug
import Data.Queue, Data.Functor
 
import iTasks.Extensions.Distributed._Formatter
import iTasks.SDS.Definition
import iTasks.Internal.Distributed.Symbols

from iTasks.Internal.TaskStore import queueRefresh
import iTasks.Internal.TaskState
import iTasks.Internal.Task
import iTasks.Internal.TaskServer
import iTasks.Internal.TaskEval
import qualified Data.Set as Set

sdsServiceTask :: Int -> Task ()
sdsServiceTask port = Task eval
where
	eval event evalOpts tree=:(TCInit taskId ts) iworld
	# (symbols, iworld) = case read symbolsShare EmptyContext iworld of
		(Ok (ReadResult symbols _), iworld) = (readSymbols symbols, iworld)
	# (mbError, iworld) = addListener taskId port True (wrapIWorldConnectionTask (handlers symbols) (sharedStore "sdsServiceTaskShare" "empty")) iworld
	| mbError=:(Error _) = (ExceptionResult (fromError mbError), iworld)
	= (ValueResult (Value () False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (ReplaceUI (ui UIEmpty)) (TCBasic taskId ts (DeferredJSONNode JSONNull) False), iworld)

	eval event evalOpts (TCBasic taskId ts data bla) iworld = (ValueResult (Value () False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (ReplaceUI (ui UIEmpty)) (TCBasic taskId ts data bla), iworld)
	
	handlers symbols = {ConnectionHandlersIWorld|onConnect = onConnect
    	, onData = onData symbols
    	, onShareChange = onShareChange
    	, onTick = onTick
    	, onDisconnect = onDisconnect
		}

	onConnect    :: ConnectionId String String   *IWorld -> *(!MaybeErrorString String, Maybe w, ![String], !Bool, !*IWorld)
	onConnect connId clientName sdsValue iworld = (Ok clientName, Nothing, [], False, iworld)

	onData       :: {#Symbol} !String String r *IWorld -> *(!MaybeErrorString String, Maybe w, ![String], !Bool, !*IWorld)
	onData symbols receivedData state sdsValue iworld
	= case deserializeFromBase64 receivedData symbols of 
 		(SDSReadRequest sds p)							= case readSDS sds p EmptyContext Nothing (sdsIdentity sds) iworld of
				(Error (_, e), iworld) 						= (Error e, Nothing, [], True, iworld)
				(Ok (ReadResult v _), iworld)				= trace_n "Got read" (Ok state, Nothing, [serializeToBase64 v +++ "\n"], True, iworld)
		(SDSRegisterRequest sds p reqSDSId taskId port)	= case readSDS sds p (RemoteTaskContext taskId "test" port) (Just taskId) reqSDSId iworld of
				(Error (_, e), iworld) 						= (Error e, Nothing, [], True, iworld)
				(Ok (ReadResult v _), iworld)				= trace_n "Got register" (Ok state, Nothing, [serializeToBase64 v +++ "\n"], True, iworld)
		(SDSWriteRequest sds p val)						= case writeSDS sds p EmptyContext val iworld of
				(Error (_, e), iworld) 						= (Error e, Nothing, [], True, iworld)
				(Ok (WriteResult notify _), iworld)			= trace_n "Got write" (Ok state, Nothing, [serializeToBase64 () +++ "\n"], True, queueNotifyEvents (sdsIdentity sds) notify iworld)
		(SDSModifyRequest sds p f)						= case modifySDS f sds p EmptyContext iworld of
				(Error (_, e), iworld) 						= (Error e, Nothing, [], True, iworld)
				(Ok (ModifyResult r w _), iworld)			= trace_n "Got modify" (Ok state, Nothing, [serializeToBase64 (r,w) +++ "\n"], True, iworld)
		(SDSRefreshRequest taskId sdsId)
			# iworld = (queueRefresh [(taskId, "Notification for remote write of " +++ sdsId)] iworld)
			= (Ok state, Nothing, ["Refresh queued"], True, iworld)

	onShareChange :: !       String r *IWorld -> *(!MaybeErrorString String, Maybe w, ![String], !Bool, !*IWorld)
	onShareChange state sdsValue iworld = (Ok state, Nothing, [], False, iworld)

	onTick        :: !       String r *IWorld -> *(!MaybeErrorString String, Maybe w, ![String], !Bool, !*IWorld)
	onTick state sdsValue iworld = (Ok state, Nothing, [], False,iworld)

	onDisconnect  :: !       String r *IWorld -> *(!MaybeErrorString String, Maybe w,                   !*IWorld)
	onDisconnect state sdsValue iworld = (Ok state, Nothing, iworld)