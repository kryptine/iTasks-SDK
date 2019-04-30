implementation module iTasks.Internal.Client.Serialization

import StdEnv
import StdMaybe

import Data.Error
import System.FilePath

import ABC.Interpreter

import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.UI.Editor

serializeForClient :: f !*IWorld -> *(!MaybeErrorString String, !*IWorld)
serializeForClient f iworld=:{abcInterpreterEnv=Just e}
	= (Ok (serialize_for_prelinked_interpretation f e), iworld)
serializeForClient f iworld=:{abcInterpreterEnv=Nothing,world,options}
# (env,world) = prepare_prelinked_interpretation options.byteCodePath world
# iworld & world = world
= case env of
	Nothing -> (Error "Failed to parse bytecode, is ByteCode set in the project file?", iworld)
	Just e  -> serializeForClient f {iworld & abcInterpreterEnv=Just e}

serializeInVSt :: f !*VSt -> *(!String, !*VSt)
serializeInVSt f vst=:{iworld}
# (s,iworld) = serializeForClient f iworld
= case s of
	Error e -> abort (e+++"\n")
	Ok s    -> (s, {vst & iworld=iworld})
