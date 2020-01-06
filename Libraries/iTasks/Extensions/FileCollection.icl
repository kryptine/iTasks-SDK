implementation module iTasks.Extensions.FileCollection
/**
* This extension provides a set of SDS functions to map
* somewhat complex data structures to a directory tree structure with very simple
* plain text files on disk.
*/
import iTasks
import iTasks.Internal.Util

import StdFile, StdArray
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Map.GenJSON
import Data.Error, Data.Functor, Data.Func, Data.Maybe, Text
import System.Directory, System.File, System.FilePath, System.OS

derive class iTask FileCollectionItem

EXCLUDE_FILE :== "exclude.txt"

//Writes a map of key/value pairs to a directory with one file per key/value
//It will ignore all files in the directory that don't match the filter
fileCollection :: FileFilter Bool Bool -> SDSSource FilePath FileCollection FileCollection
fileCollection rules readOnly deleteRemovedFiles = worldShare (read (matchRules rules)) (write readOnly (matchRules rules)) notify
where
	read isFileInCollection dir world = case readDirectory dir world of
		(Error (2,msg),world) = (Ok 'DM'.newMap,world) //Directory does not exist yet
		(Error (errNo,msg),world) = (Error (toString errNo +++ msg),world)
		(Ok files,world) = case (if deleteRemovedFiles (Ok [],world) (readExcludeList dir world)) of 
			(Error e, world) = (Error e,world)
			(Ok excludes,world) = case readFiles isFileInCollection excludes dir files world of
				(Error e, world) = (Error e,world)
				(Ok collection,world) = (Ok ('DM'.fromList collection), world)
	
	readFiles isFileInCollection excludes dir [] world = (Ok [],world)
	readFiles isFileInCollection excludes dir [f:fs] world
		| f == "." || f == ".." || (not deleteRemovedFiles && isMember f excludes) = readFiles isFileInCollection excludes dir fs world 
		| otherwise = case getFileInfo (dir </> f) world of
			(Error (_,msg),world) = (Error msg,world)
			(Ok {FileInfo|directory},world) 
				# decision = isFileInCollection f False
				# intermediate = isFileInCollection f True
				//Read a subcollection
				| directory && (decision =: IncludeFile || intermediate =: IncludeFile)
					= case read (\p i -> (isFileInCollection (f </> p) i)) (dir </> f) world of 
					(Error e,world) = (Error e,world)
					(Ok fcollection,world) = case readFiles isFileInCollection excludes dir fs world of
						(Error e,world) = (Error e,world)
						(Ok collection,world)
							//Only include the directory if it was explicitly included or it has matching files in it
							| (decision =: IncludeFile) || (not $ 'DM'.null fcollection)
								= (Ok [(f,FileCollection fcollection):collection], world)
							| otherwise
								= (Ok collection, world)
				//Skip files that don't match the filter
				| decision =: ExcludeFile
					= readFiles isFileInCollection excludes dir fs world 
				//Add referenced files
				| decision =: ReferenceFile = case readFiles isFileInCollection excludes dir fs world of
						(Error e,world) = (Error e,world)
						(Ok collection,world) = (Ok [(f,FileReference):collection], world)
				//Read the file content
				| otherwise = case readFile (dir </> f) world of
                    (Error e,world) = (Error (toString e),world)
					(Ok fcontent,world) = case readFiles isFileInCollection excludes dir fs world of
						(Error e,world) = (Error e,world)
						(Ok collection,world) = (Ok [(f,FileContent fcontent):collection], world)

	readExcludeList dir world = case readFileLines (dir </> EXCLUDE_FILE) world of
		(Ok lines,world)         = (Ok [EXCLUDE_FILE:lines],world) //the exclude file itself should also be excluded
		(Error CannotOpen,world) = (Ok [EXCLUDE_FILE],world)
		(Error e,world)          = (Error (toString e),world)

	write True isFileInCollection dir collection world 
		= (Ok (),world)
	write readOnly isFileInCollection dir collection world = case readDirectory dir world of 
		//We need to know the current content of the directory to be able to delete removed entries
		(Ok curfiles,world) = case writeFiles ('DM'.toList collection) isFileInCollection dir world of
			(Error e,world) = (Error e,world)
			(Ok newfiles,world) = cleanupRemovedFiles curfiles newfiles isFileInCollection dir world
		//The directory does not exist yet, create it first and then write the collection
		(Error (2,_),world) = case ensureDirectory dir world of
			(Error e,world) = (Error e,world)
			(Ok (),world) = case writeFiles ('DM'.toList collection) isFileInCollection dir world of
				(Error e,world) = (Error e,world)
				(Ok newfiles,world) = cleanupRemovedFiles [] newfiles isFileInCollection dir world
		(Error (ecode,msg),world) = (Error (toString ecode +++ msg),world)
		
	writeFiles [] isFileInCollection dir world = (Ok [],world)
	writeFiles [(name,FileContent content):fs] isFileInCollection dir world
		# decision = isFileInCollection name False
		| decision =: ExcludeFile = writeFiles fs isFileInCollection dir world //Don't write files that don't match the filter
		| otherwise = case writeFile (dir </> name) content world of
			(Error e,world) = (Error (toString e),world)	
			(Ok (),world) = case writeFiles fs isFileInCollection dir world of
				(Error e,world) = (Error e,world)
				(Ok curfiles,world) = (Ok [name:curfiles],world)

	writeFiles [(name,FileCollection collection):fs] isFileInCollection dir world 
		# decision = isFileInCollection name False
		# intermediate = isFileInCollection name True
		//Don't write directories that don't match the filter
		| decision =: ExcludeFile && intermediate =: ExcludeFile
			= writeFiles fs isFileInCollection dir world 
		| otherwise = case ensureDirectory (dir </> name) world of
			(Error e,world) = (Error e,world)
			(Ok (),world) = case write False (\p i -> isFileInCollection (name </> p) i) (dir </> name) collection world  of
				(Error e,world) = (Error e,world)
				(Ok (),world) = case writeFiles fs isFileInCollection dir world of
					(Error e,world) = (Error e,world)
					(Ok curfiles,world) = (Ok [name:curfiles],world)

	writeFiles [(name,FileReference):fs] isFileInCollection dir world 
		 = case writeFiles fs isFileInCollection dir world of //Don't write referenced files
			(Error e,world) = (Error e,world)
			(Ok curfiles,world) = (Ok [name:curfiles],world)

	ensureDirectory path world = case getFileInfo path world of
		(Ok {FileInfo|directory},world) 
			| directory = (Ok (),world)
			| otherwise = (Error ("Can't create directory " +++ path), world)
		(Error _, world)
			//First ensure the parent exists and is a directory
			= case ensureDirectory (takeDirectory path) world of
				(Ok (),world) = case createDirectory path world of	
					(Ok (),world) = (Ok (),world)
					(Error (_,msg),world) = (Error msg,world)
				(Error e,world) = (Error e,world)

	//Check if files that existed before, are not in the newly written set.
	//If they match the filter they 'belong' to the collection and should be removed.
	//Otherwise they will be included on the next read of the collection
	cleanupRemovedFiles filesInDirectory filesInCollection isFileInCollection dir world
		| deleteRemovedFiles = deleteFiles filesToRemove dir world
		| otherwise          = excludeFiles filesToRemove dir world
	where
		filesToRemove
			= [name \\ name <- filesInDirectory
			  | name <> "." && name <> ".." && name <> EXCLUDE_FILE
			   && not (isMember name filesInCollection) //The file is not (longer) in the collection
			   && not ((isFileInCollection name True) =: ExcludeFile) //The file or directory is explicilty part of the collection
			]
			
		excludeFiles files dir world = case writeFile (dir </> EXCLUDE_FILE) (join OS_NEWLINE files) world of
			(Error e, world) = (Error (toString e),world)
			(Ok (),world)    = (Ok (),world)

		deleteFiles [] dir world = (Ok (),world) 
		deleteFiles [f:fs] dir world = case recursiveDelete (dir </> f) world of
			(Ok (),world) = deleteFiles fs dir world
			(Error (_,msg),world) = (Error msg,world)

	notify writeParameter _ registeredParameter
		= startsWith writeParameter registeredParameter || startsWith registeredParameter writeParameter

ignoreHiddenFiles :: FileFilter
ignoreHiddenFiles = [("**/.*",ExcludeFile),("**",IncludeFile)]

matchRules :: [(String,FileFilterDecision)] String Bool -> FileFilterDecision
matchRules [] path intermediate = ExcludeFile
matchRules [(pattern,decision):rs] path intermediate
	| match pattern 0 path 0 intermediate = if intermediate IncludeFile decision
	| otherwise = matchRules rs path intermediate
where
	//Because there is no 'proper' glob-like file matching library in Clean platform,
	//this simple and somewhat limited matcher will have to do 
	match :: !String !Int !String !Int !Bool -> Bool
	match pattern ppos input ipos intermediate
		//All input has been read. We have a match when:
		| ipos >= size input 
			= ppos == size pattern // - the pattern has been fully processed
			|| intermediate // - we accept intermediate paths
			|| (ppos == size pattern - 1 && pattern.[ppos] == '*') //- we were processing the last '*' we have a match
		//The pattern has been fully matched but there is input left
		| ppos >= size pattern = False
		//Special case: pattern ends with '/**' accept anything after the '/'
		| ppos + 3 == size pattern
			&& pattern.[ppos] == '/' && pattern.[ppos + 1] == '*' && pattern.[ppos + 2] == '*'
			&& input.[ipos] == '/' = True
		//Special case '**/' match any number of directories	
		| ppos + 2 < size pattern && pattern.[ppos] == '*' && pattern.[ppos + 1] == '*' && pattern.[ppos + 2] == '/'
			//Don't match any more characters
			= match pattern (ppos + 3) input ipos intermediate
			//.. or we try to match starting after the next slash
			|| maybe False (\ipos -> match pattern ppos input ipos intermediate) (nextDir input ipos)
		//Special case: pattern ends with '**' accept everything
		| ppos + 2 == size pattern && pattern.[ppos] == '*' && pattern.[ppos + 1] == '*'
			= True
		//Special case: '*' match any number of characters (but not '/')
		| pattern.[ppos] == '*'
			//Don't match any more characters
			= match pattern (ppos + 1) input ipos intermediate
			//.. or we can read an extra character and try to match
			|| (input.[ipos] <> '/' && match pattern ppos input (ipos + 1) intermediate)
		//Match the expected character
		| input.[ipos] == pattern.[ppos] = match pattern (ppos + 1) input (ipos + 1) intermediate
		| otherwise = False //The pattern does not match
	where
		nextDir input ipos
			| ipos >= size input = Nothing
			| input.[ipos] == '/' = Just (ipos + 1)
			| otherwise = nextDir input (ipos + 1)


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
	toPath (name,FileReference) = [name]
	toPath (name,FileContent _) = [name]
	toPath (name,FileCollection collection) = [name:[name </> path \\ path <- toPaths collection]]


