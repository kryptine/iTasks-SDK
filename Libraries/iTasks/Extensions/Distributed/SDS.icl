implementation module iTasks.Extensions.Distributed.SDS

import iTasks
import Data.Func, Data.Either
import qualified Data.Map

import iTasks.Internal.Distributed.Formatter
import iTasks.UI.Layout.Default
import iTasks.Internal.TaskState
import iTasks.Internal.TaskStore
import iTasks.WF.Definition

import StdMisc, StdDebug

domainName :: SDSLens () (Maybe Domain) (Maybe Domain)
domainName = sharedStore "domain" Nothing

domainTaskResult :: (Task a) Domain -> SDSSequence InstanceNo (TaskValue a) () | iTask a
domainTaskResult task domain = maybeDomainShare domain "domainTaskResult"
	(mapReadError (read task) (toReadOnly taskInstanceValue))
where
	read :: (Task a) TIValue -> MaybeError TaskException (TaskValue a) | iTask a
	read _ (TIValue tValue) = case tValue of
		NoValue = trace_n "No value yet" (Ok NoValue)
		(Value djson stable)
		| not (trace_tn "Got some value") = undef
		= case fromDeferredJSON djson of
			Nothing = trace_n "Unable to transform value" (Ok NoValue)
			(Just a) = trace_n "Transformed value" (Ok (Value a stable))

	read _ (TIException exc str) = Error (exc, str)


addDistributedTask :: Domain -> SDSSequence (Task a, TaskAttributes) TaskId () | iTask a
addDistributedTask domain = maybeDomainShare domain "addDistributedTask" (SDSSource {SDSSourceOptions|name = "addDistributedTask"
					 , read = read
					 , write = write })
where
	read (task, attrs) iworld
	= case evalAppendTask (Detached attrs False) (\_ -> task <<@ ApplyLayout defaultSessionLayout @! ()) topLevelTasks (TaskId 0 0) iworld of
		(Error e, iworld) = (Error e, iworld)
		(Ok taskId, iworld) = (Ok taskId, iworld)

	write _ _ iworld = (Ok (\_ _. False), iworld)

// Selects to use a remote share when the current instance is not the host in the given domain.
maybeDomainShare :: Domain String (sds p r w) -> (SDSSequence p r w) | iTask r & iTask w & RWShared sds & iTask p
maybeDomainShare d=:(Domain host port) name sds = sdsSequence ("seq" +++ name)
	(const ())
	(\p mbDomain. (p, mbDomain))
	(\p mbDomain. Right snd)
	(SDSWriteConst \p w. Ok Nothing)
	(SDSWrite \p rs w. Ok (Just w))
	domainName
	select
where
	select = sdsSelect ("select" +++ name) selectp
		(SDSNotify \pw rs w ts p. True)
		(SDSNotify \pw rs w ts p. True)
		sds
		(remoteShare sds {SDSShareOptions|domain=host, port=port})

	selectp (p, Nothing) = trace_n "Acces domain share - no domain configured" (Right p)
	selectp (p, Just domain)
	| domain == d = trace_n "Acces local share" (Left p)
	= trace_n "Acces domain share - server for other domain" (Right p)