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
serialize_for_client f iworld=:{world,options}
# (graph,world) = serialize_for_prelinked_interpretation f options.byteCodePath options.appPath world
# iworld & world = world
# graph = case graph of
	Nothing -> Error "Failed to serialize graph"
	Just g  -> Ok g
= (graph, iworld)

serialize_in_vst :: f !*VSt -> *(!String, !*VSt)
serialize_in_vst f vst=:{iworld}
# (s,iworld) = serialize_for_client f iworld
= case s of
	Error e -> abort (e+++"\n")
	Ok s    -> (s, {vst & iworld=iworld})
