implementation module ImportTasks
 
import StdBool, _SystemArray, StdInt, TSt, DocumentDB, MIME, Text, Util, CSV, File, ExceptionCombinators

CHUNK_SIZE :== 1024

importDocument :: !FilePath -> Task Document
importDocument filename = mkInstantTask ("Document import", ("Import of document " +++ filename)) (readDocument filename)

importTextFile :: !FilePath -> Task String
importTextFile filename = mkInstantTask  ("Text file import", ("Import of text file " +++ filename)) (fileTask filename readAll)

importCSVFile :: !FilePath -> Task [[String]]
importCSVFile filename = mkInstantTask ("CSV file import", ("Import of CSV file " +++ filename)) (fileTask filename readCSVFile)

importCSVFileWith :: !Char !Char !Char !FilePath -> Task [[String]]
importCSVFileWith delimitChar quoteChar escapeChar filename = mkInstantTask ("CSV file import", ("Import of CSV file " +++ filename)) (fileTask filename (readCSVFileWith delimitChar quoteChar escapeChar))

importJSONFile :: !FilePath -> Task a | JSONDecode{|*|} a
importJSONFile filename = mkInstantTask ("JSON file import", ("Import of JSON file " +++ filename)) (readJSON filename fromJSON)

importJSONFileWith :: !(JSONNode -> Maybe a) !FilePath -> Task a
importJSONFileWith parsefun filename = mkInstantTask ("JSON file import", ("Import of JSON file " +++ filename)) (readJSON filename parsefun)

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
	# mime				= extensionToMimeType (fileExtension name)
	# (document,tst)	= createDocument name mime content {TSt|tst & iworld={IWorld|iworld & world = world}}
	= (TaskFinished document, tst)

openException s		= dynamic (FileException s CannotOpen)
closeException s	= dynamic (FileException s CannotClose)
parseException s	= dynamic (CannotParse ("Cannot parse JSON file " +++ s))
