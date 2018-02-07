implementation module iTasks.Internal.Distributed.Symbols

import iTasks

import StdFile
//from iTasks.WF.Tasks.Core import accWorld
//from iTasks.Internal.SDS import read, write
import symbols_in_program
//from iTasks.Internal.IWorld import :: IWorld{world}
//from Data.Error import :: MaybeError(..)
//from iTasks.WF.Definition import :: TaskException, :: Task(..), :: TaskResult, :: TaskEvalOpts, :: Event(..), :: Set
//from iTasks.Internal.Task import mkInstantTask
//from iTasks.Internal.TaskState			import :: TaskTree
//from iTasks.WF.Definition import class iTask
//from iTasks.Internal.SDS import :: ReadWriteShared, :: RWShared, :: Shared
//from iTasks.WF.Definition      import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor, :: TaskId
//from Data.Maybe import :: Maybe
//from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
//from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
//import StdFile
import dynamic_string
import Text.Encodings.Base64

import iTasks.Internal.SDS
import iTasks.Internal.Task
import iTasks.Internal.IWorld
//from iTasks.WF.Combinators.Common import @!, >>-
//from iTasks.WF.Tasks.SDS import set 
//from iTasks.SDS.Sources.Store import :: SDS, sharedStore
//import StdTuple

symbolsShare :: Shared String
symbolsShare = sharedStore "symbols" ""

storeSymbols :: String !*IWorld -> (MaybeError TaskException String, !*IWorld)
storeSymbols file iworld
# (symbols, iworld) = accFiles (read_symbols file) iworld
# val = base64Encode (copy_to_string symbols)
# (res, iworld) = write val symbolsShare iworld
| isError res = (liftError res, iworld)
= (Ok val, iworld)

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
