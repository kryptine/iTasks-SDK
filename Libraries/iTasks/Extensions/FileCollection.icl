implementation module iTasks.Extensions.FileCollection
/**
* This extension provides a set of SDS functions to map
* somewhat complex data structures to a directory tree structure with very simple
* plain text files on disk.
*/
import iTasks
import StdFile
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Error, Data.Functor, Text
import System.Directory, System.File, System.FilePath

derive class iTask FileCollectionItem

//Writes a map of key/value pairs to a directory with one file per key/value
//It will ignore all files in the directory that don't match the filter
fileCollection :: FileFilter -> SDS FilePath FileCollection FileCollection
fileCollection isFileInCollection = worldShare (read isFileInCollection) (write isFileInCollection)
where
	read isFileInCollection dir world = case readDirectory dir world of
		(Error (_,msg),world) = (Error msg,world) 
		(Ok files,world) = case readFiles files isFileInCollection dir world of
			(Error e, world) = (Error e,world)
			(Ok collection,world) = (Ok ('DM'.fromList collection), world)
	
	readFiles [] isFileInCollection dir world = (Ok [],world)
	readFiles [f:fs] isFileIncollection dir world
		| f == "." || f == ".." = readFiles fs isFileInCollection dir world 
		| otherwise = case getFileInfo (dir </> f) world of
			(Error (_,msg),world) = (Error msg,world)
			(Ok {FileInfo|directory},world) 
				//Skip files that don't match the filter
				| not (isFileInCollection f directory)
					= readFiles fs isFileInCollection dir world 
				//Read a subcollection
				| directory = case read (\x -> isFileInCollection (f </> x)) (dir </> f) world of 
					(Error e,world) = (Error e,world)
					(Ok fcollection,world) = case readFiles fs isFileInCollection dir world of
						(Error e,world) = (Error e,world)
						(Ok collection,world) = (Ok [(f,FileCollection fcollection):collection], world)
				//Read the file content
				| otherwise = case readFile (dir </> f) world of
                    (Error e,world) = (Error (toString e),world)
					(Ok fcontent,world) = case readFiles fs isFileInCollection dir world of
						(Error e,world) = (Error e,world)
						(Ok collection,world) = (Ok [(f,FileContent fcontent):collection], world)

	write isFileInCollection dir collection world = case readDirectory dir world of 
		//We need to know the current content of the directory to be able to delete removed entries
		(Error (_,msg),world) = (Error msg,world) 
		(Ok curfiles,world) = case writeFiles ('DM'.toList collection) isFileInCollection dir world of
			(Error e,world) = (Error e,world)
			(Ok newfiles,world) = cleanFiles curfiles newfiles isFileInCollection world
		
	writeFiles [] isFileInCollection dir world = (Ok [],world)
	writeFiles [(name,FileContent content):fs] isFileInCollection dir world
		| not (isFileInCollection name False) = writeFiles fs isFileInCollection dir world //Don't write files that don't match the filter
		| otherwise = case writeFile (dir </> name) content world of
			(Error e,world) = (Error (toString e),world)	
			(Ok (),world) = case writeFiles fs isFileInCollection dir world of
				(Error e,world) = (Error e,world)
				(Ok curfiles,world) = (Ok [name:curfiles],world)

	writeFiles [(name,FileCollection collection):fs] isFileInCollection dir world 
		| not (isFileInCollection name True) = writeFiles fs isFileInCollection dir world //Don't write files that don't match the filter
		| otherwise = case write (\x -> isFileInCollection (name </> x)) (dir </> name) collection world  of
			(Error e,world) = (Error e,world)
			(Ok (),world) = case writeFiles fs isFileInCollection dir world of
				(Error e,world) = (Error e,world)
				(Ok curfiles,world) = (Ok [name:curfiles],world)

	//Check if files that existed before, are not in the newly written set.
	//If they match the filter they 'belong' to the collection and should be removed.
	//Otherwise they will be included on the next read of the collection
	cleanFiles [] newfiles pred world = (Ok (),world)
	cleanFiles [f:fs] newfiles pred world
		| isMember f newfiles = cleanFiles fs newfiles pred world //The file is still in the collection
		| f == "." || f == ".." || not (pred f False) = cleanFiles fs newfiles pred world //The file is not part of the collection, we can ignore it
		| otherwise
			//TODO: remove the file or directory
			= cleanFiles fs newfiles pred world

getStringContent :: String FileCollection -> Maybe String
getStringContent key collection = case 'DM'.get key collection of
	(Just (FileContent content)) = Just content
	_							 = Nothing

setStringContent:: String String FileCollection -> FileCollection
setStringContent key value collection = 'DM'.put key (FileContent value) collection

getIntContent :: String FileCollection -> Maybe Int
getIntContent key collection = fmap (toInt o trim) (getStringContent key collection)

setIntContent :: String Int FileCollection -> FileCollection
setIntContent key value collection = 'DM'.put key (FileContent (toString value)) collection

toPaths :: FileCollection -> [FilePath]
toPaths collection = flatten (map toPath ('DM'.toList collection)) 
where
	toPath (name,FileContent _) = [name]
	toPath (name,FileCollection collection) = [name:[name </> path \\ path <- toPaths collection]]

