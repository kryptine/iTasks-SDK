implementation module ImportTasks

import iTasks
import CSV
import TSt

CHUNK_SIZE :== 1024

importTextFile :: !String -> Task String
importTextFile filename = mkInstantTask ("Import of text file " +++ filename) (fileTask filename readAll)

importCSVFile :: !String -> Task [[String]]
importCSVFile filename = mkInstantTask ("Import of CSV file " +++ filename) (fileTask filename readCSVFile)

importCSVFileWith :: !Char !Char !Char !String -> Task [[String]]
importCSVFileWith delimitChar quoteChar escapeChar filename = mkInstantTask ("Import of CSV file " +++ filename) (fileTask filename (readCSVFileWith delimitChar quoteChar escapeChar))

fileTask filename f tst=:{TSt|world}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (TaskException (openException filename),{TSt|tst & world = world})
	# (res,file)		= f file
	# (ok,world)		= fclose file world
	| not ok			= (TaskException (closeException filename),{TSt|tst & world = world})
	= (TaskFinished res, {TSt|tst & world = world})
		
readAll file
	# (chunk,file) = freads file CHUNK_SIZE
	| size chunk < CHUNK_SIZE
		= (chunk,file)
	| otherwise
		# (rest,file) = readAll file
		= (chunk +++ rest,file)
		
openException s = (dynamic ("Could not open file: " +++ s))
closeException s = (dynamic ("Could not close file: " +++ s))