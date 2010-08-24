implementation module ExportTasks

import iTasks
import TSt, DocumentDB
import StdFile, CSV

exportDocument :: !String !Document -> Task Document
exportDocument filename document = mkInstantTask "Document export" ("Export of document " +++ filename) (writeDocument filename document)

exportTextFile :: !String !String -> Task String
exportTextFile filename content = mkInstantTask "Text file export" ("Export of text file " +++ filename) (fileTask filename content writeAll)

exportCSVFile :: !String ![[String]] -> Task [[String]]
exportCSVFile filename content = mkInstantTask "CSV file export" ("Export of CSV file " +++ filename) (fileTask filename content writeCSVFile)

exportCSVFileWith :: !Char !Char !Char !String ![[String]] -> Task [[String]]
exportCSVFileWith delimitChar quoteChar escapeChar filename content = mkInstantTask "CSV file export" ("Export of CSV file " +++ filename) (fileTask filename content (writeCSVFileWith delimitChar quoteChar escapeChar))

exportJSONFile :: !String a -> Task a | JSONEncode{|*|} a
exportJSONFile filename content = exportJSONFileWith toJSON filename content
 
exportJSONFileWith :: !(a -> JSONNode) !String a -> Task a
exportJSONFileWith encoder filename content = mkInstantTask "JSON file export" ("Export of JSON file " +++ filename) (fileTask filename content (writeJSON encoder))

fileTask filename content f tst=:{TSt|iworld=iworld=:{IWorld|world}}
	# (ok,file,world)	= fopen filename FWriteData world
	| not ok			= (TaskException (openException filename),{TSt|tst & iworld={IWorld|iworld & world = world}})
	# file				= f content file
	# (ok,world)		= fclose file world
	| not ok			= (TaskException (closeException filename),{TSt|tst & iworld={IWorld|iworld & world = world}})
	= (TaskFinished content, {TSt|tst & iworld={IWorld|iworld & world = world}})
	
writeAll content file
	= fwrites content file

writeJSON encoder content file
	= fwrites (toString (encoder content)) file

writeDocument filename document tst
	# (mbContent,tst=:{TSt|iworld=iworld=:{IWorld|world}})
							= getDocumentContent document.Document.documentId tst
	| isNothing mbContent	= (TaskException documentException, {TSt|tst & iworld={IWorld|iworld & world = world}})
	# (ok,file,world)		= fopen filename FWriteData world
	| not ok				= (TaskException (openException filename),{TSt|tst & iworld={IWorld|iworld & world = world}})
	# file					= fwrites (fromJust mbContent) file
	# (ok,world)			= fclose file world
	| not ok				= (TaskException (closeException filename),{TSt|tst & iworld={IWorld|iworld & world = world}})	
	= (TaskFinished document, {TSt|tst & iworld={IWorld|iworld & world = world}})

documentException = (dynamic "Could not read document content")
openException s = (dynamic ("Could not open file: " +++ s))
closeException s = (dynamic ("Could not close file: " +++ s))



 