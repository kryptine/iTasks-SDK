implementation module ImportTasks
 
import StdBool, _SystemArray, StdInt, IWorld, Task, TaskContext, DocumentStore, MIME, Text, Util, CSV, File, Map
from StdFunc import id

CHUNK_SIZE :== 1024

importDocument :: !FilePath -> Task Document
importDocument filename = mkInstantTask ("Document import", ("Import of document " +++ filename)) eval
where
	eval taskNr iworld =  readDocument filename iworld
	
importTextFile :: !FilePath -> Task String
importTextFile filename = mkInstantTask  ("Text file import", ("Import of text file " +++ filename)) eval
where
	eval taskNr iworld = fileTask filename readAll iworld
	
importCSVFile :: !FilePath -> Task [[String]]
importCSVFile filename = mkInstantTask ("CSV file import", ("Import of CSV file " +++ filename)) eval
where
	eval taskNr iworld = fileTask filename readCSVFile iworld
	
importCSVFileWith :: !Char !Char !Char !FilePath -> Task [[String]]
importCSVFileWith delimitChar quoteChar escapeChar filename = mkInstantTask ("CSV file import", ("Import of CSV file " +++ filename)) eval
where
	eval taskNr iworld = fileTask filename (readCSVFileWith delimitChar quoteChar escapeChar) iworld
	
importJSONFile :: !FilePath -> Task a | iTask a
importJSONFile filename = mkInstantTask ("JSON file import", ("Import of JSON file " +++ filename)) eval
where
	eval taskNr iworld = readJSON filename fromJSON iworld
	
importJSONFileWith :: !(JSONNode -> Maybe a) !FilePath -> Task a | iTask a
importJSONFileWith parsefun filename = mkInstantTask ("JSON file import", ("Import of JSON file " +++ filename)) eval
where
	eval taskNr iworld = readJSON filename parsefun iworld
	
fileTask filename f iworld=:{IWorld|world}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# (res,file)		= f file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	= (TaskStable res (NoRep,[]) TCEmpty, {IWorld|iworld & world = world})
		
readAll file
	# (chunk,file) = freads file CHUNK_SIZE
	| size chunk < CHUNK_SIZE
		= (chunk,file)
	| otherwise
		# (rest,file) = readAll file
		= (chunk +++ rest,file)

readJSON filename parsefun iworld=:{IWorld|world}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# (content,file)	= readAll file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	= case (parsefun (fromString content)) of
		Just a 	= (TaskStable a (NoRep,[]) TCEmpty, {IWorld|iworld & world = world})
		Nothing	= (parseException filename, {IWorld|iworld & world = world})
		
readDocument filename iworld=:{IWorld|world}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# (content,file)	= readAll file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	# name				= dropDirectory filename 
	# mime				= extensionToMimeType (takeExtension name)
	# (document,iworld)	= createDocument name mime content {IWorld|iworld & world = world}
	= (TaskStable document (NoRep,[]) TCEmpty, iworld)

openException s		= taskException (FileException s CannotOpen)
closeException s	= taskException (FileException s CannotClose)
parseException s	= taskException (CannotParse ("Cannot parse JSON file " +++ s))
