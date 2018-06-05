implementation module iTasks.Internal.Distributed.Symbols

import iTasks

import StdFile
import StdDebug
import symbols_in_program
import dynamic_string
import Text.Encodings.Base64

import iTasks.Internal.SDS
import iTasks.Internal.Task
import iTasks.Internal.IWorld

symbolsShare :: SDSLens () String String
symbolsShare = sharedStore "symbols" ""

storeSymbols :: String !*IWorld -> (MaybeError TaskException String, !*IWorld)
storeSymbols file iworld
# (symbols, iworld) = accFiles (read_symbols file) iworld
# val = base64Encode (copy_to_string symbols)
# (res, iworld) = write val symbolsShare EmptyContext iworld
| isError res = (liftError res, iworld)
= (Ok val, iworld)

accSymbols :: ({#Symbol} -> a) -> Task a | iTask a
accSymbols fun = mkInstantTask eval
where
	eval taskId iworld
		# (val, iworld) = read symbolsShare EmptyContext iworld
		= case val of
			Ok (ReadResult val _)		= (Ok (fun (readSymbols val)), iworld) 
			Error e		= (Error e, iworld)

readSymbols :: String -> {#Symbol}
readSymbols shareValue = fst (copy_from_string (base64Decode shareValue))

withSymbols :: ({#Symbol} -> Task a) -> Task a | iTask a
withSymbols taskfun = Task eval
where
	eval event evalOpts state iworld
                # (val, iworld) = read symbolsShare EmptyContext iworld
                = case val of
                        Ok (ReadResult val _)          = let (Task eval`) = taskfun (fst (copy_from_string (base64Decode val))) in eval` event evalOpts state iworld
