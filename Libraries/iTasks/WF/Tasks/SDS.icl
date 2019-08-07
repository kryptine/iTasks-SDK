implementation module iTasks.WF.Tasks.SDS

import iTasks.WF.Derives
import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import iTasks.Internal.AsyncSDS
import iTasks.WF.Tasks.Core
import qualified iTasks.Internal.SDS as SDS
import StdFunctions
import StdString, Data.Func, Data.Error, StdBool
import qualified Data.Set as DS
import qualified Data.Map as DM

instance toString SharedException
where
	toString (SharedException err) = "Error performing operation on shared:" +++ err

derive class iTask SharedException

get :: !(sds () a w) -> Task a | iTask a & Readable sds & TC w
get sds = Task (readCompletely sds NoValue (unTask o treturn))

set :: !a !(sds () r a)  -> Task a | iTask a & TC r & Writeable sds
set val sds = Task (writeCompletely val sds NoValue (unTask (treturn val)))

upd :: !(r -> w) !(sds () r w) -> Task w | iTask r & iTask w & RWShared sds
upd fun sds = Task (modifyCompletely fun sds NoValue (unTask o treturn))

watch :: !(sds () r w) -> Task r | iTask r & TC w & Readable, Registrable sds
watch sds = Task (readCompletely sds NoValue
	let cont = \r->readRegisterCompletely sds (Value r False) rep cont in cont)

rep ResetEvent  = ReplaceUI (ui UIEmpty)
rep _ 			= NoChange
