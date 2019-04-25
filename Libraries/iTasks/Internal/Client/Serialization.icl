implementation module iTasks.Internal.Client.Serialization

import StdEnv
import StdMaybe

import Data.Error
import System.FilePath

import ABC.Interpreter

import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.UI.Editor

serialize_for_client :: f !*IWorld -> *(!MaybeErrorString String, !*IWorld)
serialize_for_client f iworld=:{abcInterpreterEnv=Just e}
	= (Ok (serialize_for_prelinked_interpretation f e), iworld)
serialize_for_client f iworld=:{abcInterpreterEnv=Nothing,world,options}
# (env,world) = prepare_prelinked_interpretation options.byteCodePath world
# iworld & world = world
= case env of
	Nothing -> (Error "Failed to parse bytecode, is ByteCode set in the project file?", iworld)
	Just e  -> serialize_for_client f {iworld & abcInterpreterEnv=Just e}

serialize_in_vst :: f !*VSt -> *(!String, !*VSt)
serialize_in_vst f vst=:{iworld}
# (s,iworld) = serialize_for_client f iworld
= case s of
	Error e -> abort (e+++"\n")
	Ok s    -> (s, {vst & iworld=iworld})
