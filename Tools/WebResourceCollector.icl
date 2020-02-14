module WebResourceCollector
/**
* This program collects and creates all the necessary public web resources that an iTasks program
* needs to run. These are for example, static javascript files, images and the html launch page.
*
* To find bundled public resources it looks in a project file which modules are
* used by a program and then checks for each module if a directory called "<modulename>-webpublic" exists
* if it exists, the full contents of that directory are copied to the collection directory called "<applicationname>-webpublic"
*
* It also creates an aggregated css file with additional style rules that are needed by Clean modules.
* If for an imported Clean module a file exists with the same name, but a .css extension it is included in the aggregation.
* The total collected css is written to "<applicationname>-webpublic/css/itasks-modules.css"
*
* To always bundle the right versions of web resources, this program must be run after every build in the link phase.
*/

import StdEnv
import System.CommandLine
import System.File, System.FilePath, System.Directory
import Data.Error
import Data.Tuple
import System.OS, System.OSError
import Text

FILE_EXISTS :== IF_WINDOWS 183 17

Start :: *World -> *World
Start world
	//Figure out input and output dirs
	# (argv,world) = getCommandLine world
	| length argv <> 5 = world //Fail
	# linkopts_file = argv !! 2 //When called by the IDE or cpm the second argument will be the 'linkopts' file
	# linkerrs_file = argv !! 4 //When called by the IDE or cpm the second argument will be the 'linkerrs' file
	# (content,world) = readFile linkopts_file world
	| isError content
		= writeErrorFile linkerrs_file ("Error opening " +++ linkopts_file +++ ": " +++ toString (fromError content) +++ "\n") world
	# content = fromOk content
	# outDir = exePathToOutputDir (lookupExePath content)
	# inDirs = objectPathsToInputDirs (lookupObjectPaths content)
	# cssParts = objectPathsToCSSFiles (lookupObjectPaths content)
	# cssFile = outDir </> "css" </> "itasks-modules.css"
	# world = print ("Output css file " +++ cssFile) world
	//Create output dir and 'css' dir in it
	# (mbErr,world) = createDirectory outDir world
	| not (mbErr =: (Ok _) || mbErr =: (Error (FILE_EXISTS,_))) //Ignore 'File exists' errors
		= writeErrorFile linkerrs_file ("Error creating directory " +++ outDir +++ ": " +++ toString (fromError mbErr) +++ "\n") world
	# (mbErr,world) = createDirectory (outDir </> "css") world
	| not (mbErr =: (Ok _) || mbErr =: (Error (FILE_EXISTS,_))) //Ignore 'File exists' errors
		= writeErrorFile linkerrs_file ("Error creating directory " +++ outDir </> "css" +++ ": " +++ toString (fromError mbErr) +++ "\n") world
	//Create the aggregated css file
	# (mbErr,world) = writeFile cssFile "" world
	# world = foldr (\f w -> aggregateCSS f cssFile w) world cssParts
	//Copy the contents of the input dirs if they exist
	# world = foldr (\d w -> copyWebResources d outDir w) world inDirs
	//Write an empty error file when
	= writeErrorFile linkerrs_file "" world

writeErrorFile errorfile message world
	# (mbErr,world) = writeFile errorfile message world
	| mbErr =: (Ok _) = world
	| otherwise = abort (toString (fromError mbErr)) world

//Print a debug message
print msg world
	# (console,world) = stdio world
	# console = fwrites "iTasks Resource Collector: " console
	# console = fwrites msg console
	# console = fwrites "\n" console
	# (_,world) = fclose console world
	= world

//Brute force 'parsing' functions that find the necessary information in a linkopts file
lookupExePath :: String -> String
lookupExePath linkopts
	= last (split "\t" (hd (filter (startsWith "ExePath") (split "\n" linkopts))))

lookupObjectPaths :: String -> [String]
lookupObjectPaths linkopts
	= map (last o split "\t") (takeWhile (startsWith "\tPath") (tl (dropWhile (not o (startsWith "ObjectPaths")) (split "\n" linkopts))))

//Determine the output folder where the web resources should be copied to
exePathToOutputDir :: FilePath-> FilePath
exePathToOutputDir path = dropExtension path +++ "-www"

//Determine the potential input folders and css fragments
objectPathsToInputDirs :: [FilePath] -> [FilePath]
objectPathsToInputDirs paths = removeDup (flatten (map rewrite paths))
where
	rewrite path = [join {pathSeparator} ((filter ((<>) "Clean System Files") (split {pathSeparator} (dropExtension path)))) +++ "-WebPublic"
				   //Transitional location of WebPublic files, they should eventually be linked directory to specific modules dash
				   ,join {pathSeparator} ((filter ((<>) "Clean System Files") (split {pathSeparator} (takeDirectory path)))) </> "WebPublic"
				   ]

objectPathsToCSSFiles :: [FilePath] -> [FilePath]
objectPathsToCSSFiles paths = map rewrite paths
where
	rewrite path = addExtension (join {pathSeparator} ((filter ((<>) "Clean System Files") (split {pathSeparator} (dropExtension path))))) "css"

//Copy the web resources if the input directory exists
copyWebResources :: !FilePath !FilePath !*World -> *World
copyWebResources indir outdir world
	# (dir,world) = isDirectory indir world
	| dir
		# world = print ("Copying resources from "+++indir) world
		= copyDirectoryContent indir outdir world
	| otherwise = world

aggregateCSS :: !FilePath !FilePath !*World -> *World
aggregateCSS inFile outFile world
	# (exists,world) = fileExists inFile world
	| exists
		# world = print ("Adding css file "+++inFile) world
		= copyFile inFile outFile True world
	| otherwise = world

//GENERAL UTIL, SHOULD BE PART OF PLATFORM
isDirectory :: !FilePath !*World -> (!Bool, !*World)
isDirectory path world = case getFileInfo path world of
	(Ok {FileInfo|directory},world) = (directory,world)
	(_,world)                       = (False,world)

copyDirectoryContent :: !FilePath !FilePath !*World -> *World
copyDirectoryContent indir outdir world
	= case readDirectory indir world of
		(Ok items,world) = foldr (copyItem indir outdir) world items
		(_,world) = world
where
	copyItem indir outdir item world
		| item == "." || item == ".." = world
		# (dir,world) = isDirectory (indir </> item) world
		| dir //Create the target directory and recursively copy content
			# (mbErr,world) = createDirectory (outdir </> item) world
			| isError mbErr && not (mbErr =: (Error (FILE_EXISTS, _)))
				= abort ("Error creating directory " +++ outdir </> item +++ ": " +++ toString (fromError mbErr) +++ "\n")
			= copyDirectoryContent (indir </> item) (outdir </> item) world
		| otherwise //Copy the file
			= copyFile (indir </> item) (outdir </> item) False world

copyFile :: !FilePath !FilePath !Bool !*World -> *World
copyFile inf outf append world
	# (ok,inh,world)    = fopen inf FReadData world
	| not ok
		# (e, w) = getLastOSError world
		= abort ("Error opening " +++ outf +++ ": " +++ toString (fromError e) +++ "\n")
	# (ok,outh,world)   = fopen outf (if append FAppendData FWriteData) world
	| not ok
		# (e, w) = getLastOSError world
		= abort ("Error opening " +++ outf +++ ": " +++ toString (fromError e) +++ "\n")
	# (inh,outh) = copy inh outh
	# (_,world) = fclose inh world
	# (_,world) = fclose outh world
	= world
where
	copy inh outh
		# (string, inh) = freads inh 1000
		| string == "" = (inh, outh)
		| otherwise
			# outh = fwrites string outh
			= copy inh outh

