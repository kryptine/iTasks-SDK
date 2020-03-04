implementation module iTasks.WF.Tasks.SDS

import StdEnv

import iTasks.SDS.Definition
import iTasks.WF.Tasks.Core
import iTasks.UI.Definition

import iTasks.Internal.AsyncSDS
import iTasks.Internal.Task
import iTasks.Internal.TaskEval
import iTasks.Internal.Util

get :: !(sds () a w) -> Task a | TC a & Readable sds & TC w
get sds = Task (readCompletely sds NoValue (\e->mkUIIfReset e (asyncSDSLoaderUI Read)) (unTask o return))

set :: !a !(sds () r a)  -> Task a | TC a & TC r & Writeable sds
set val sds = Task (writeCompletely val sds NoValue (\e->mkUIIfReset e (asyncSDSLoaderUI Write)) (unTask (return val)))

upd :: !(r -> w) !(sds () r w) -> Task w | TC r & TC w & RWShared sds
upd fun sds = Task (modifyCompletely fun sds NoValue (\e->mkUIIfReset e (asyncSDSLoaderUI Modify)) (unTask o return))

watch :: !(sds () r w) -> Task r | TC r & TC w & Readable, Registrable sds
watch sds = Task (readRegisterCompletely sds NoValue mkUi cont)
where
	cont r event {lastEval} iworld
		= (ValueResult (Value r False)
			(mkTaskEvalInfo lastEval)
			(mkUi event)
			(Task (readRegisterCompletely sds (Value r False) mkUi cont))
		, iworld)

	mkUi event = mkUIIfReset event (ui UIEmpty)
