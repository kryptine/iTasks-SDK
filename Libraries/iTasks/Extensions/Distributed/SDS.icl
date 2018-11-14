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

distributedTaskResults :: SDSLens () ('Data.Map'.Map InstanceNo Dynamic) ('Data.Map'.Map InstanceNo Dynamic)
distributedTaskResults = sharedStore "dTaskResults" 'Data.Map'.newMap

distributedTaskResult :: SDSLens InstanceNo (Maybe Dynamic) (Maybe Dynamic)
distributedTaskResult = sdsLens "dTaskResult" (const ()) read write notify reduce distributedTaskResults
	where
		read = SDSRead \taskId map. trace_n ("Read value for task " +++ toString taskId) (Ok ('Data.Map'.get taskId map))
		write = SDSWrite \taskId map value. trace_n ("Write value for task " +++ toString taskId) (Ok (Just ('Data.Map'.put taskId (fromJust value) map)))
		notify = SDSNotify \p rs w ts ps. p == ps
		reduce = Just (\taskId map. Ok ('Data.Map'.get taskId map))

distributedTaskResultForTask :: (Task a) -> SDSLens InstanceNo (TaskValue a) (TaskValue a) | iTask a
distributedTaskResultForTask task = mapReadWrite (read task, write) Nothing distributedTaskResult
where
	read :: (Task a) (Maybe Dynamic) -> TaskValue a | TC a
	read _ Nothing = trace_n "read distributedTaskResultForTask: NoValue" NoValue
	read _ (Just dyn) = case dyn of
		(val :: (TaskValue a^)) = trace_n "read distributedTaskResultForTask: Value" val
		dyn = trace_n ("Reading distributedTaskResultForTask: Wrong type " +++ toString (typeCodeOfDynamic dyn)) NoValue

	write val mbDynamic = trace_n "Writing to distributedTaskResultForTask" (Just (Just (dynamic val)))

domainTaskResult :: (Task a) Domain -> SDSSequence InstanceNo (TaskValue a) () | iTask a
domainTaskResult task domain = maybeDomainShare domain "domainTaskResult" (toReadOnly (distributedTaskResultForTask task))

addDistributedTask :: Domain -> SDSSequence (Task a, TaskAttributes) TaskId () | iTask a
addDistributedTask domain = maybeDomainShare domain "addDistributedTask" (SDSSource {SDSSourceOptions|name = "addDistributedTask"
					 , read = read
					 , write = write })
where
	read (task, attrs) iworld
	= case evalAppendTask (Detached attrs False) (wrappedTask task) topLevelTasks (TaskId 0 0) iworld of
		(Error e, iworld) = (Error e, iworld)
		(Ok taskId, iworld) = (Ok taskId, iworld)

	write _ _ iworld = (Ok (\_ _. False), iworld)

	wrappedTask task _ = withTaskId (return ())
		>>= \(_, (TaskId instanceNo _)). (task >&^
				\stv-> (whileUnchanged stv
				\mtv-> case mtv of
    				Nothing = trace_n "While unchanged no value" (treturn ())
   					Just tv = storeValue task instanceNo (Value tv True) @! ()) <<@ NoUserInterface)
			<<@ ApplyLayout defaultSessionLayout @! ()

	storeValue task taskId v = trace_n ("Setting task value: " +++ toSingleLineText v) (set v (sdsFocus taskId (distributedTaskResultForTask task)))

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

	selectp (p, Nothing) = trace_n (name +++ ": Acces domain share - no domain configured") (Right p)
	selectp (p, Just domain)
	| domain == d = trace_n (name +++ ": Acces local share") (Left p)
	= trace_n (name +++ ": Acces domain share - server for other domain") (Right p)