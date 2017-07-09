implementation module iTasks.SDS.Sources.Core

import iTasks.SDS.Definition
import iTasks.Internal.SDS
import iTasks.Internal.IWorld
import System.FilePath, System.Directory, System.File
import StdFile

from StdFunc import const
from iTasks.Internal.Task import exception

constShare :: !a -> SDS p a ()
constShare v = createReadOnlySDS (\_ env -> (v, env))

nullShare :: SDS p () a
nullShare = createReadWriteSDS "_core_" "nullShare" (\_ env -> (Ok (), env)) (\_ _ env -> (Ok (const False), env))

worldShare :: (p *World -> *(MaybeErrorString r,*World)) (p w *World -> *(MaybeErrorString (),*World)) -> SDS p r w 
worldShare read write = createReadWriteSDS "_core_" "worldShare" read` write`
where
	read` p iworld=:{IWorld|world} = case read p world of
		(Ok r,world) = (Ok r, {IWorld|iworld & world = world})
		(Error e,world) = (Error (exception e), {IWorld|iworld & world = world})

	write` p w iworld=:{IWorld|world} = case write p w world of
		(Ok (),world) = (Ok (const False), {IWorld|iworld & world = world})
		(Error e,world) = (Error (exception e), {IWorld|iworld & world = world})

// Random source
randomInt :: SDS () Int ()
randomInt = createReadOnlySDS randomInt
where
	randomInt () iworld=:{IWorld|random=[i:is]}
		= (i, {IWorld|iworld & random = is})

externalFile :: SDS FilePath String String
externalFile = createReadWriteSDS "_core_" "externalFile" read write
where
	read path iworld=:{world}
		# (ok,file,world)			= fopen path FReadData iworld.world
		| not ok					= (Ok "", {IWorld|iworld & world = world}) // empty string if file doesn't exist
		# (res,file)				= readAll file
		# (ok,world)				= fclose file world
		| not ok					= (Error (exception CannotClose) ,{IWorld|iworld & world = world})
        = case res of
            Error e                 = (Error (exception e), {IWorld|iworld & world = world})
            Ok content              = (Ok content, {IWorld|iworld & world = world})

	write path content iworld=:{world}
		# (ok,file,world)			= fopen path FWriteText world
		| not ok					= (Error (exception CannotOpen), {IWorld|iworld & world = world})
		# file						= fwrites content file
		# (ok,world)				= fclose file world
		| not ok					= (Error (exception CannotClose) ,{IWorld|iworld & world = world})
		= (Ok ((==) path), {IWorld|iworld & world = world})

externalDirectory :: SDS FilePath [FilePath] ()
externalDirectory = createReadOnlySDSError read
where
	read path iworld = case readDirectory path iworld of
		(Ok files,iworld) = (Ok files,iworld)
		(Error (_,e),iworld) = (Error (exception e),iworld)

