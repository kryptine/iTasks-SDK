implementation module iTasks.WF.Tasks.SDS

import StdEnv

import iTasks.SDS.Definition
import iTasks.WF.Tasks.Core
import iTasks.WF.Definition
import iTasks.UI.Definition

import iTasks.Internal.AsyncSDS
import iTasks.Internal.Task
import iTasks.Internal.TaskEval
import iTasks.Internal.TaskState

from Data.Map import newMap
//instance toString SharedException
//where
//	toString (SharedException err) = "Error performing operation on shared:" +++ err
//
//derive class iTask SharedException

get :: !(sds () a w) -> Task a | iTask a & Readable sds & TC w
get sds = Task (readCompletely sds NoValue (unTask o treturn))

set :: !a !(sds () r a)  -> Task a | iTask a & TC r & Writeable sds
set val sds = Task (writeCompletely val sds NoValue (unTask (treturn val)))

upd :: !(r -> w) !(sds () r w) -> Task w | iTask r & iTask w & RWShared sds
upd fun sds = Task (modifyCompletely fun sds NoValue (\_->asyncSDSLoadUI Modify) (unTask o treturn))

watch :: !(sds () r w) -> Task r | iTask r & TC w & Readable, Registrable sds
watch sds = Task (readRegisterCompletely sds NoValue rep cont)
where
	cont r event {TaskEvalOpts|ts} iworld
		= (ValueResult (Value r False)
			{TaskEvalInfo|lastEvent=ts,attributes=newMap,removedTasks=[]}
			(rep event)
			(Task (readRegisterCompletely sds NoValue rep cont))
		, iworld)

	rep ResetEvent = ReplaceUI (ui UIEmpty)
	rep _ = NoChange
