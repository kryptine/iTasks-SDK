implementation module iTasks.API.Common.ImportTasks

import StdBool, _SystemArray, StdInt
import Text.Encodings.MIME, Text, Text.CSV, System.File, Text.JSON, Data.Error, System.FilePath
import iTasks._Framework.IWorld, iTasks._Framework.Task, iTasks._Framework.TaskState, iTasks._Framework.TaskStore
import iTasks._Framework.Util
from StdFunc import id

CHUNK_SIZE :== 1048576 // 1M

importDocument :: !FilePath -> Task Document
importDocument filename = mkInstantTask eval
where
	eval taskId iworld =  readDocument taskId filename iworld
	
importTextFile :: !FilePath -> Task String
importTextFile filename = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename readAll iworld
	
importCSVFile :: !FilePath -> Task [[String]]
importCSVFile filename = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename readCSVFile iworld

importCSVDocument :: !Document -> Task [[String]]
importCSVDocument {Document|documentId} = mkInstantTask eval
where
	eval taskId iworld
		# (filename,iworld) = documentLocation documentId iworld
		= fileTask taskId filename readCSVFile iworld

importCSVFileWith :: !Char !Char !Char !FilePath -> Task [[String]]
importCSVFileWith delimitChar quoteChar escapeChar filename = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename (readCSVFileWith delimitChar quoteChar escapeChar) iworld

importCSVDocumentWith :: !Char !Char !Char !Document -> Task [[String]]
importCSVDocumentWith delimitChar quoteChar escapeChar {Document|documentId} = mkInstantTask eval
where
	eval taskId iworld
		# (filename,iworld) = documentLocation documentId iworld
		= fileTask taskId filename (readCSVFileWith delimitChar quoteChar escapeChar) iworld

importJSONFile :: !FilePath -> Task a | iTask a
importJSONFile filename = mkInstantTask eval
where
	eval taskId iworld = readJSON taskId filename fromJSON iworld

importJSONDocument  :: !Document -> Task a | iTask a
importJSONDocument {Document|documentId} = mkInstantTask eval
  where
  eval taskId iworld
    # (filename, iworld) = documentLocation documentId iworld
    # (mbstr, iworld)    = loadDocumentContent documentId iworld
    = case mbstr of
        Just str -> case fromJSON (fromString str) of
                      Just a -> (Ok a, iworld)
                      _      -> (parseException filename, iworld)
        _ -> (openException filename, iworld)

importJSONFileWith :: !(JSONNode -> Maybe a) !FilePath -> Task a | iTask a
importJSONFileWith parsefun filename = mkInstantTask eval
where
	eval taskId iworld = readJSON taskId filename parsefun iworld
	
fileTask taskId filename f iworld=:{IWorld|current={taskTime},world}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# (res,file)		= f file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	= (Ok res, {IWorld|iworld & world = world})
		
readAll file
	# (chunk,file) = freads file CHUNK_SIZE
	| size chunk < CHUNK_SIZE
		= (chunk,file)
	| otherwise
		# (rest,file) = readAll file
		= (chunk +++ rest,file)

readJSON taskId filename parsefun iworld=:{IWorld|current={taskTime},world}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# (content,file)	= readAll file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	= case (parsefun (fromString content)) of
		Just a
          = (Ok a, {IWorld|iworld & world = world})
		Nothing
          = (parseException filename, {IWorld|iworld & world = world})
		
readDocument taskId filename iworld=:{IWorld|current={taskTime},world}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# (content,file)	= readAll file
	# (ok,world)		= fclose file world
	| not ok				= (closeException filename,{IWorld|iworld & world = world})
	# name					= dropDirectory filename
	# mime					= extensionToMimeType (takeExtension name)
	# (mbDocument,iworld)	= createDocument name mime content {IWorld|iworld & world = world}
	= case mbDocument of
		(Ok document) 	= (Ok document, iworld)
		(Error e)		= (Error (dynamic e,toString e),iworld)
		
openException s	
	# e = FileException s CannotOpen
	= Error (dynamic e, toString e)
closeException s
	# e = FileException s CannotClose
	= Error (dynamic e, toString e)
parseException s
	# e = CannotParse ("Cannot parse JSON file " +++ s)
	= Error (dynamic e, toString e)