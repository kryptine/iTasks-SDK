definition module iTasks.Extensions.Distributed.SDS

import iTasks

import iTasks.Internal.Distributed.Symbols

domainName :: SDSLens () (Maybe Domain) (Maybe Domain)

addDistributedTask :: Domain -> SDSSequence (Task a, TaskAttributes) TaskId () | iTask a

domainTaskResult :: (Task a) Domain -> SDSSequence InstanceNo (TaskValue a) () | iTask a
