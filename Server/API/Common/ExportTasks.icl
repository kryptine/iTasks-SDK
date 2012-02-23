implementation module ExportTasks

import StdBool, FilePath, CSV, File, Map, IWorld, Task, TaskContext, DocumentStore

exportDocument :: !FilePath !Document -> Task Document
exportDocument filename document = mkInstantTask eval
where
	eval taskId iworld = writeDocument taskId filename document iworld
	
exportTextFile :: !FilePath !String -> Task String
exportTextFile filename content = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename content writeAll iworld

createCSVFile :: !String ![[String]] -> Task Document
createCSVFile filename content = mkInstantTask eval
where
	eval taskId iworld
		# (doc,iworld)	= createDocumentWith filename "text/csv" (writeCSVFile content) iworld
		= (TaskStable doc NoRep (TCEmpty taskId), iworld)

exportCSVFile :: !FilePath ![[String]] -> Task [[String]]
exportCSVFile filename content = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename content writeCSVFile iworld

exportCSVFileWith :: !Char !Char !Char !FilePath ![[String]] -> Task [[String]]
exportCSVFileWith delimitChar quoteChar escapeChar filename content = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename content (writeCSVFileWith delimitChar quoteChar escapeChar) iworld

exportJSONFile :: !FilePath a -> Task a | iTask a
exportJSONFile filename content = exportJSONFileWith toJSON filename content
 
exportJSONFileWith :: !(a -> JSONNode) !FilePath a -> Task a | iTask a
exportJSONFileWith encoder filename content = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename content (writeJSON encoder) iworld

fileTask taskId filename content f iworld=:{IWorld|world}
	# (ok,file,world)	= fopen filename FWriteData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# file				= f content file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	= (TaskStable content NoRep (TCEmpty taskId), {IWorld|iworld & world = world})
	
writeAll content file
	= fwrites content file

writeJSON encoder content file
	= fwrites (toString (encoder content)) file

writeDocument taskId filename document iworld
	# (mbContent,iworld=:{IWorld|world})
							= getDocumentContent document.Document.documentId iworld
	| isNothing mbContent	= (ioException filename, {IWorld|iworld & world = world})
	# (ok,file,world)		= fopen filename FWriteData world
	| not ok				= (openException filename,{IWorld|iworld & world = world})
	# file					= fwrites (fromJust mbContent) file
	# (ok,world)			= fclose file world
	| not ok				= (closeException filename,{IWorld|iworld & world = world})	
	= (TaskStable document NoRep (TCEmpty taskId), {IWorld|iworld & world = world})

ioException s		= taskException (FileException s IOError)
openException s		= taskException (FileException s CannotOpen)
closeException s	= taskException (FileException s CannotClose)
