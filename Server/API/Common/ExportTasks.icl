implementation module ExportTasks

import StdBool, FilePath, CSV, File, Map, IWorld, Task, TaskContext, DocumentStore, ExceptionCombinators

exportDocument :: !FilePath !Document -> Task Document
exportDocument filename document = mkInstantTask ("Document export", ("Export of document " +++ filename)) eval
where
	eval taskNr iworld = writeDocument filename document iworld
	
exportTextFile :: !FilePath !String -> Task String
exportTextFile filename content = mkInstantTask ("Text file export", ("Export of text file " +++ filename)) eval
where
	eval taskNr iworld = fileTask filename content writeAll iworld

createCSVFile :: !String ![[String]] -> Task Document
createCSVFile filename content = mkInstantTask ("CSV file creation", ("Export of CSV file " +++ filename)) eval
where
	eval taskNr iworld
		# (doc,iworld)	= createDocumentWith filename "text/csv" (writeCSVFile content) iworld
		= (TaskFinished doc, iworld)

exportCSVFile :: !FilePath ![[String]] -> Task [[String]]
exportCSVFile filename content = mkInstantTask ("CSV file export", ("Export of CSV file " +++ filename)) eval
where
	eval taskNr iworld = fileTask filename content writeCSVFile iworld

exportCSVFileWith :: !Char !Char !Char !FilePath ![[String]] -> Task [[String]]
exportCSVFileWith delimitChar quoteChar escapeChar filename content = mkInstantTask ("CSV file export", ("Export of CSV file " +++ filename)) eval
where
	eval taskNr iworld = fileTask filename content (writeCSVFileWith delimitChar quoteChar escapeChar) iworld

exportJSONFile :: !FilePath a -> Task a | JSONEncode{|*|} a
exportJSONFile filename content = exportJSONFileWith toJSON filename content
 
exportJSONFileWith :: !(a -> JSONNode) !FilePath a -> Task a
exportJSONFileWith encoder filename content = mkInstantTask ("JSON file export", ("Export of JSON file " +++ filename)) eval
where
	eval taskNr iworld = fileTask filename content (writeJSON encoder) iworld

fileTask filename content f iworld=:{IWorld|world}
	# (ok,file,world)	= fopen filename FWriteData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# file				= f content file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	= (TaskFinished content, {IWorld|iworld & world = world})
	
writeAll content file
	= fwrites content file

writeJSON encoder content file
	= fwrites (toString (encoder content)) file

writeDocument filename document iworld
	# (mbContent,iworld=:{IWorld|world})
							= getDocumentContent document.Document.documentId iworld
	| isNothing mbContent	= (ioException filename, {IWorld|iworld & world = world})
	# (ok,file,world)		= fopen filename FWriteData world
	| not ok				= (openException filename,{IWorld|iworld & world = world})
	# file					= fwrites (fromJust mbContent) file
	# (ok,world)			= fclose file world
	| not ok				= (closeException filename,{IWorld|iworld & world = world})	
	= (TaskFinished document, {IWorld|iworld & world = world})

ioException s		= taskException (FileException s IOError)
openException s		= taskException (FileException s CannotOpen)
closeException s	= taskException (FileException s CannotClose)
