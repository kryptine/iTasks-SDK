implementation module iTasks.Extensions.Files

import StdFile, StdArray, StdFunctions

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Data.Error, Text
import Data.Functor
import Data.List
import Data.Tree
import System.Directory
import System.FilePath
import System.File
import System.Time
import qualified Control.Monad as CM
import qualified System.File as SF
import qualified System.Directory as SD

import iTasks

deleteFile :: !FilePath -> Task ()
deleteFile path = accWorldError ('SF'.deleteFile path) snd 

moveFile :: !FilePath !FilePath -> Task ()
moveFile srcPath dstPath = accWorldError ('SF'.moveFile srcPath dstPath) snd

copyFile :: !FilePath !FilePath -> Task ()
copyFile srcPath dstPath = accWorldError (copyFile` srcPath dstPath) id

//TODO: This is a very stupid way of copying files, should be replaced by a better way
copyFile` srcPath dstPath world = case 'SF'.readFile srcPath world of
	(Error e,world) = (Error e,world)
	(Ok content,world) = writeFile dstPath content world
		
createDirectory :: !FilePath !Bool -> Task ()
createDirectory path False = accWorldError ('SD'.createDirectory path) snd
createDirectory path True = accWorldError (createWithParents path) id 
where
	createWithParents path world = create [] (split {pathSeparator} path) world

	create _ [] world = (Ok (),world)
	create [] ["":rest] world = create [""] rest world //Special case for absolute paths
	create base [dir:rest] world
		# next = base ++ [dir]
		# path = join {pathSeparator} next
		# (exists,world) = 'SF'.fileExists path world
		| exists = create next rest world //This part exists, continue
		| otherwise = case 'SD'.createDirectory path world of
			(Error e,world) = (Error (snd e),world) 
			(Ok (),world) = create next rest world

deleteDirectory :: !FilePath !Bool -> Task ()
deleteDirectory path False = accWorldError ('SD'.removeDirectory path) snd
deleteDirectory path True = accWorldError (deleteDirectoryRecursive path) id

deleteDirectoryRecursive path world = case 'SD'.readDirectory path world of
	(Error e,world) = (Error (snd e), world)
	(Ok content,world) = case deleteContent content world of
		(Error e,world) = (Error e,world)
		(Ok (),world) = case 'SD'.removeDirectory path world of 
			(Error e,world) = (Error (snd e),world)
			(Ok (),world) = (Ok (),world)
where
	deleteContent [] world = (Ok (),world)
	deleteContent [".":rest] world = deleteContent rest world
	deleteContent ["..":rest] world = deleteContent rest world
	deleteContent [entry:rest] world = case getFileInfo (path </> entry) world of
		(Error e,world) = (Error (snd e), world)
		(Ok {FileInfo|directory},world) 
		| directory = case deleteDirectoryRecursive (path </> entry) world of
			(Error e,world) = (Error e,world)
			(Ok (),world) = deleteContent rest world
		| otherwise = case 'SF'.deleteFile (path </> entry) world of
			(Error e,world) = (Error (snd e),world)
			(Ok (),world) = deleteContent rest world

copyDirectory :: !FilePath !FilePath -> Task ()
copyDirectory  srcPath dstPath = accWorldError (copyDirectory` srcPath dstPath) id

copyDirectory` srcPath dstPath world = case readDirectory srcPath world of
		(Error e,world) = (Error (snd e), world)
		(Ok content,world) = case 'SD'.createDirectory dstPath world of
			(Error e,world) = (Error (snd e),world)
			(Ok (),world) = copyContent content world
where
	copyContent [] world  = (Ok (),world)
	copyContent [".":rest] world = copyContent rest world
	copyContent ["..":rest] world = copyContent rest world
	copyContent [entry:rest] world = case getFileInfo (srcPath </> entry) world of
		(Error e,world) = (Error (snd e), world)
		(Ok {FileInfo|directory},world) 
			| directory = case copyDirectory` (srcPath </> entry) (dstPath </> entry) world of
				(Error e,world) = (Error e,world)
				(Ok (),world) = copyContent rest world
			| otherwise = case copyFile` (srcPath </> entry) (dstPath </> entry) world of
				(Error e,world) = (Error (toString e), world)
				(Ok (),world) = copyContent rest world


//Why is this necessary?!?!?!?
derive class iTask RTree, FileInfo, Tm

selectFile :: !FilePath !d !Bool [FilePath]-> Task [FilePath] | toPrompt d
selectFile root prompt multi initial
	= accWorld (createDirectoryTree root) @ numberTree
	>>= \tree->editSelection prompt multi selectOption tree
		[i\\(i, (f, _))<-leafs tree | elem f initial]
where
	selectOption = SelectInTree
		(\tree->[{foldTree fp2cn tree & label=root}])
		(\tree sel->[f\\(i, (f, _))<-leafs tree | isMember i sel])

	fp2cn (i, (fp, mfi)) cs =
		{ id = case mfi of
			Error e = ~i
			Ok {directory=True} = ~i
			_ = i
		, label=dropDirectory fp
		, icon=Nothing
		, expanded=False
		, children=cs
		}

	numberTree :: ((RTree a) -> RTree (Int, a))
	numberTree = flip evalState zero o foldTree \a cs->
		(\lvs i->RNode (i, a) lvs) <$> 'CM'.sequence cs <*> getState <* modify inc
