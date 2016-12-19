module WebResourceCollector
/**
* This program collects all the necessary public web resources that an iTasks program 
* needs to run. These are for example, static javascript files, images and the html launch page.
*
* The way this collector works is straightforward. It looks in a project file which modules are
* used by a program and then checks for each module if a directory called "<modulename>-webpublic" exists
* if it exists, the full contents of that directory are copied to the collection directory called "<applicationname>-webpublic"
* 
* To always bundle the right versions of web resources, this program must be run after every build
* in the link phase.
*/

import StdEnv
import System.CommandLine
import System.File, System.FilePath, System.Directory
import Data.Error
import Text

Start :: *World -> *World
Start world
	//Figure out input and output dirs
	# (argv,world) = getCommandLine world
	| length argv <> 5 = world //Fail
	# (content,world) = readFile (argv !! 2) world //When called by the IDE or cpm the second argument will be the 'linkopts' file
	| isError content 
		= print (toString (fromError content)) world 
	# content = fromOk content
	# outDir = exePathToOutputDir (lookupExePath content)
	# inDirs = objectPathsToInputDirs (lookupObjectPaths content)
	//Create output dir
	# (mbErr,world) = createDirectory outDir world
	| not (mbErr =: (Ok _) || mbErr =: (Error (17,_))) //Ignore 'File exists' errors
		# (errorcode,errormsg) = fromError mbErr
		= print errormsg world
	//Copy the contents of the input dirs if they exist
	# world = foldr (\d w -> copyWebResources d outDir w) world inDirs
	= world

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

//Determine the potential input folders 
objectPathsToInputDirs :: [FilePath] -> [FilePath]
objectPathsToInputDirs paths = flatten (map rewrite paths)
where
	rewrite path = [join {pathSeparator} ((filter ((<>) "Clean System Files") (split {pathSeparator} (dropExtension path)))) +++ "-WebPublic"
				   //Transitional location of WebPublic files, they should eventually be linked directory to specific modules dash
				   ,join {pathSeparator} ((filter ((<>) "Clean System Files") (split {pathSeparator} (takeDirectory path)))) </> "WebPublic"
				   ]

//Copy the web resources if the input directory exists
copyWebResources :: !FilePath !FilePath !*World -> *World
copyWebResources indir outdir world
	# (dir,world) = isDirectory indir world
	| dir       
		# world = print ("Copying resources from "+++indir) world
		= copyDirectoryContent indir outdir world
	| otherwise = world

//GENERAL UTIL, SHOULD BE PART OF PLATFORM
isDirectory :: !FilePath !*World -> (!Bool, !*World)
isDirectory path world = case getFileInfo path world of
	(Ok {FileInfo|directory},world) = (directory,world)
	(_,world) 						= (False,world)

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
			# (_,world) = createDirectory (outdir </> item) world
			= copyDirectoryContent (indir </> item) (outdir </> item) world
		| otherwise //Copy the file
			= copyFile (indir </> item) (outdir </> item) world

copyFile :: !FilePath !FilePath !*World -> *World
copyFile inf outf world
	# (ok,inh,world)    = fopen inf FReadData world
	| not ok
		= world 
	# (ok,outh,world)   = fopen outf FWriteData world   
    | not ok
		= world 
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

