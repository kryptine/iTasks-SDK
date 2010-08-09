implementation module ImportTasks

import iTasks
import CSV
import TSt, DocumentDB, ExtToMime, Text, Util

CHUNK_SIZE :== 1024

importDocument :: !String -> Task Document
importDocument filename = mkInstantTask ("Import of document " +++ filename) (readDocument filename)

importTextFile :: !String -> Task String
importTextFile filename = mkInstantTask ("Import of text file " +++ filename) (fileTask filename readAll)

importCSVFile :: !String -> Task [[String]]
importCSVFile filename = mkInstantTask ("Import of CSV file " +++ filename) (fileTask filename readCSVFile)

importCSVFileWith :: !Char !Char !Char !String -> Task [[String]]
importCSVFileWith delimitChar quoteChar escapeChar filename = mkInstantTask ("Import of CSV file " +++ filename) (fileTask filename (readCSVFileWith delimitChar quoteChar escapeChar))

importJSONFile :: !String -> Task a | JSONDecode{|*|} a
importJSONFile filename = mkInstantTask ("Import of JSON file " +++ filename) (readJSON filename fromJSON)

importJSONFileWith :: !(JSONNode -> Maybe a) !String -> Task a
importJSONFileWith parsefun filename = mkInstantTask ("Import of JSON file " +++ filename) (readJSON filename parsefun)

fileTask filename f tst=:{TSt|iworld=iworld=:{IWorld|world}}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (TaskException (openException filename),{TSt|tst & iworld={IWorld|iworld & world = world}})
	# (res,file)		= f file
	# (ok,world)		= fclose file world
	| not ok			= (TaskException (closeException filename),{TSt|tst & iworld={IWorld|iworld & world = world}})
	= (TaskFinished res, {TSt|tst & iworld={IWorld|iworld & world = world}})
		
readAll file
	# (chunk,file) = freads file CHUNK_SIZE
	| size chunk < CHUNK_SIZE
		= (chunk,file)
	| otherwise
		# (rest,file) = readAll file
		= (chunk +++ rest,file)

readJSON filename parsefun tst=:{TSt|iworld=iworld=:{IWorld|world}}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (TaskException (openException filename),{TSt|tst & iworld={IWorld|iworld & world = world}})
	# (content,file)	= readAll file
	# (ok,world)		= fclose file world
	| not ok			= (TaskException (closeException filename),{TSt|tst & iworld={IWorld|iworld & world = world}})
	= case (parsefun (fromString content)) of
		Just a 	= (TaskFinished a, {TSt|tst & iworld={IWorld|iworld & world = world}})
		Nothing	= (TaskException (parseException filename), {TSt|tst & iworld={IWorld|iworld & world = world}})
		
readDocument filename tst=:{TSt|iworld=iworld=:{IWorld|world}}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (TaskException (openException filename),{TSt|tst & iworld={IWorld|iworld & world = world}})
	# (content,file)	= readAll file
	# (ok,world)		= fclose file world
	| not ok			= (TaskException (closeException filename),{TSt|tst & iworld={IWorld|iworld & world = world}})
	# name				= baseName filename 
	# mime				= extToMimeType (fileExtension name)
	# (document,tst)	= createDocument name mime content {TSt|tst & iworld={IWorld|iworld & world = world}}
	= (TaskFinished document, tst)

openException s = (dynamic ("Could not open file: " +++ s))
closeException s = (dynamic ("Could not close file: " +++ s))
parseException s = (dynamic ("Could not parse file: " +++ s))
