implementation module ExportTasks

import StdBool, FilePath, TSt, CSV, File, DocumentDB, ExceptionCombinators

exportDocument :: !FilePath !Document -> Task Document
exportDocument filename document = mkInstantTask ("Document export", ("Export of document " +++ filename)) (writeDocument filename document)

exportTextFile :: !FilePath !String -> Task String
exportTextFile filename content = mkInstantTask ("Text file export", ("Export of text file " +++ filename))(fileTask filename content writeAll)

exportCSVFile :: !FilePath ![[String]] -> Task [[String]]
exportCSVFile filename content = mkInstantTask ("CSV file export", ("Export of CSV file " +++ filename)) (fileTask filename content writeCSVFile)

exportCSVFileWith :: !Char !Char !Char !FilePath ![[String]] -> Task [[String]]
exportCSVFileWith delimitChar quoteChar escapeChar filename content = mkInstantTask ("CSV file export", ("Export of CSV file " +++ filename)) (fileTask filename content (writeCSVFileWith delimitChar quoteChar escapeChar))

exportJSONFile :: !FilePath a -> Task a | JSONEncode{|*|} a
exportJSONFile filename content = exportJSONFileWith toJSON filename content
 
exportJSONFileWith :: !(a -> JSONNode) !FilePath a -> Task a
exportJSONFileWith encoder filename content = mkInstantTask ("JSON file export", ("Export of JSON file " +++ filename)) (fileTask filename content (writeJSON encoder))

fileTask filename content f tst=:{TSt|iworld=iworld=:{IWorld|world}}
	# (ok,file,world)	= fopen filename FWriteData world
	| not ok			= (openException filename,{TSt|tst & iworld={IWorld|iworld & world = world}})
	# file				= f content file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{TSt|tst & iworld={IWorld|iworld & world = world}})
	= (TaskFinished content, {TSt|tst & iworld={IWorld|iworld & world = world}})
	
writeAll content file
	= fwrites content file

writeJSON encoder content file
	= fwrites (toString (encoder content)) file

writeDocument filename document tst
	# (mbContent,tst=:{TSt|iworld=iworld=:{IWorld|world}})
							= getDocumentContent document.Document.documentId tst
	| isNothing mbContent	= (ioException filename, {TSt|tst & iworld={IWorld|iworld & world = world}})
	# (ok,file,world)		= fopen filename FWriteData world
	| not ok				= (openException filename,{TSt|tst & iworld={IWorld|iworld & world = world}})
	# file					= fwrites (fromJust mbContent) file
	# (ok,world)			= fclose file world
	| not ok				= (closeException filename,{TSt|tst & iworld={IWorld|iworld & world = world}})	
	= (TaskFinished document, {TSt|tst & iworld={IWorld|iworld & world = world}})

ioException s		= taskException (FileException s IOError)
openException s		= taskException (FileException s CannotOpen)
closeException s	= taskException (FileException s CannotClose)
