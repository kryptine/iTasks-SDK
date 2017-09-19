implementation module iTasks.Extensions.Distributed.Engine

from iTasks.Core.Tasks import accWorld
from iTasks._Framework.SDS import read, write
import symbols_in_program
from iTasks._Framework.IWorld import :: IWorld{world}
from Data.Error import :: MaybeError(..)
from iTasks._Framework.Task import :: TaskException, mkInstantTask, :: Task(..), :: TaskResult, :: TaskEvalOpts, :: Event(..)
from iTasks._Framework.TaskState			import :: TaskTree
from iTasks._Framework.Generic import class iTask
from iTasks._Framework.SDS import :: ReadWriteShared, :: RWShared, :: Shared
from iTasks.Core.Types      import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor, :: TaskId
from Data.Maybe import :: Maybe
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks._Framework.Generic.Visualization    import :: TextFormat(..)
import StdFile
import dynamic_string
import Text.Encodings.Base64
from iTasks.Common.TaskCombinators import @!, >>-
from iTasks.Core.Tasks import set 
from iTasks.Core.SDSs import sharedStore
import StdTuple

symbolsShare :: Shared String
symbolsShare = sharedStore "symbols" ""

startDistributedEngine :: String -> Task ()
startDistributedEngine executable = storeSymbols executable

storeSymbols :: String -> Task ()
storeSymbols file = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		# (symbols, world) = accFiles (read_symbols file) world
		# iworld = {IWorld| iworld & world = world}
		# val = base64Encode (copy_to_string symbols)
		# (res,iworld) = write val symbolsShare iworld
		= case res of
			Ok _	= (Ok (), iworld)
			Error e	= (Error e, iworld)

accSymbols :: ({#Symbol} -> a) -> Task a | iTask a
accSymbols fun = mkInstantTask eval
where
	eval taskId iworld
		# (val, iworld) = read symbolsShare iworld
		= case val of
			Ok val		= (Ok (fun (fst (copy_from_string (base64Decode val)))), iworld) 
			Error e		= (Error e, iworld)

withSymbols :: ({#Symbol} -> Task a) -> Task a | iTask a
withSymbols taskfun = Task eval
where
	eval event evalOpts state iworld
                # (val, iworld) = read symbolsShare iworld
                = case val of
                        Ok val          = let (Task eval`) = taskfun (fst (copy_from_string (base64Decode val))) in eval` event evalOpts state iworld
//                       Error e         = (Error e, iworld)

