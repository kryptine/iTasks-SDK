implementation module iTasks.Internal.Client.Serialization

import StdEnv
import StdMaybe

import Data.Error
import System.FilePath

import ABC.Interpreter

import iTasks.Engine
import iTasks.Internal.IWorld

serialize_for_client :: f !*IWorld -> *(!MaybeErrorString String, !*IWorld)
serialize_for_client f iworld=:{world,options}
# (graph,world) = serialize_for_prelinked_interpretation f (options.appName+++".bc") options.appPath world
	// TODO: store bytecodePath in EngineOptions
# iworld & world = world
# graph = case graph of
	Nothing -> Error "Failed to serialize graph"
	Just g  -> Ok g
= (graph, iworld)
