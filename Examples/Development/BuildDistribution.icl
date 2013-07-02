module BuildDistribution
/**
* This tool automates the task of creating a zip package of the iTask System.
* Not all steps can be automated, so there are some manual steps involved.
*
* It is experimental and incomplete... (but beats making packages completely by hand...)
*/
import iTasks
import System.Directory, System.File, System.FilePath, Data.Tuple, Text
from iTasks.Framework.Util import pad

instance toString (OSErrorCode,String)
where
    toString (_,e) = e


:: Platform		= Windows32 | Windows64 | Linux32 | Linux64 | Mac

:: DistroOptions =
	{ targetPlatform		:: Platform
	, iTasksBranch		    :: String		//Which branch
    , packageName           :: String
	}

derive class iTask Platform, DistroOptions

makeDistribution :: Task Document
makeDistribution
    = catchAll (
		editOptions
	>>= buildDistro
    >>- downloadDistro
    ) (\e -> viewInformation (Title "Oops!") [] e >>| makeDistribution)

editOptions :: Task DistroOptions
editOptions
	=		viewTitle "Build an iTasks distribution"
	||-	    updateChoice ("Target platform","Choose a target platform to make a distribution for") [ChooseWith ChooseFromComboBox id]
				[Windows32,Windows64,Linux32,Linux64,Mac] Windows32
	-&&-	updateInformation ("Svn branch","Choose which svn branch you want to make a distribution from") [] "trunk"	
    -&&-    (get currentDate @ defaultPackageName >>= updateInformation ("Package name","Specify the name of the package") [])
	@ 	\(platform,(branch,packageName)) ->
			{targetPlatform=platform,iTasksBranch=branch,packageName=packageName}
where
    defaultPackageName date
        = "CleanWithiTasks-" <+++ pad 4 date.year <+++ pad 2 date.mon <+++ pad 2 date.day

buildDistro:: DistroOptions -> Task Document
buildDistro options = withTemporaryDirectory build
where
    build tmpDir = autoSequence ("Building distro","Building iTasks distribution...")
        [createTargetFolder targetDir
        ,addCleanSystem options.targetPlatform True targetDir
        ,addITasksSDK options.iTasksBranch targetDir
        ,unpackSAPLFiles targetDir
        ,prepareIDE targetDir
        ,cleanupDistro options.targetPlatform targetDir
        ,zipDistro tmpDir targetDir targetDoc
        ] >>- \_ -> importDocument targetDoc
    where
        targetDir = options.packageName
        targetDoc = addExtension options.packageName "zip"

    autoSequence :: d [Task a] -> Task a | iTask a & descr d
    autoSequence prompt tasks = withShared 0
        \progress ->
                viewSharedInformation prompt [ViewWith (toPrj (length tasks))] progress
            ||- sequence progress [(i,t) \\ t <- tasks & i <- [1..]]
    where
        sequence progress [(i,t)] = set i progress >>| t
        sequence progress [(i,t):ts] = set i progress >>| t>>- \_ -> sequence progress ts

        toPrj num cur
            = {progress = ProgressRatio (toReal cur /  toReal num) , description ="Step " <+++ cur <+++ " of " <+++ num}

	createTargetFolder target
        = worldIO (createDirectory target) @ const Void

	//Download and add Clean system, remove unnecessary files and libraries
	addCleanSystem platform include target
        =   httpDownloadDocumentTo (downloadUrl platform) zipFile
        >>- \_ -> callProcess "Unzipping Clean" [] zipExe zipArgs Nothing
        @   const Void
	where
		zipFile	= target </> "Clean_2.4.zip"
		zipTarget = target </> "Clean 2.4"
		downloadUrl _ = "http://clean.cs.ru.nl/download/Clean24/windows/Clean_2.4.zip"
	
		zipExe = IF_POSIX_OR_WINDOWS "/usr/bin/unzip" "C:\\Program Files\\7-Zip\\7z.exe"
		zipArgs = IF_POSIX_OR_WINDOWS ["-q", zipFile,"-d", target] ["-o"+++target,"x", zipFile]
		
	//Export iTasks SDK from subversion
	addITasksSDK branch target
		=   callProcess "Exporting iTasks from subversion" [] svnExe svnArgs Nothing
        @   const Void
	where
		svnTarget = target </> "Clean 2.4" </> "iTasks-SDK"
		
		svnUrl	= "https://svn.cs.ru.nl/repos/iTask-system/" +++ branch
		svnExe = IF_POSIX_OR_WINDOWS "/usr/bin/svn" "C:\\Program Files\\Subversion\\bin\\svn.exe"
		svnArgs = ["export","--native-eol","CRLF",svnUrl,svnTarget]
	
	//Unpack Sapl libraries
	unpackSAPLFiles target
        =   callProcess "Unpacking SAPL files" [] zipExe zipArgs Nothing
        @   const Void
    where
		zipFile	= target </>"Clean 2.4"</>"iTasks-SDK"</>"Compiler"</>"StdEnv-Sapl.zip"
		zipTarget = target </>"Clean 2.4"</>"Libraries"</>"StdEnv"

		zipExe = IF_POSIX_OR_WINDOWS "/usr/bin/unzip" "C:\\Program Files\\7-Zip\\7z.exe"
		zipArgs = IF_POSIX_OR_WINDOWS ["-q", zipFile,"-d", zipTarget] ["-o"+++target,"x", zipFile]

    //Prepare the IDE
    prepareIDE target
        =   copyITasksIDE
        >>| importITasksEnvironment
        >>| updateDefaultSettings
        @   const Void
    where
        copyITasksIDE
            =   importDocument (target</>"Clean 2.4"</>"iTasks-SDK"</>"Compiler"</>"CleanIDE.exe") //UGLY COPY
            >>- exportDocument (target</>"Clean 2.4"</>"CleanIDE.exe")

        importITasksEnvironment
            =   importTextFile envsFile
            >>- \envs ->
                importTextFile (target</>"Clean 2.4"</>"iTasks-SDK"</>"Server"</>"iTasks.env")
            >>- \iTasksEnv ->
                exportTextFile envsFile (envs +++ removeFirstLines iTasksEnv)
        where
            envsFile = target</>"Clean 2.4"</>"Config"</>"IDEEnvs"
            removeFirstLines s = join "\r\n" (drop 2 (split "\r\n" s))

        updateDefaultSettings
            =   importTextFile prefsFile
            >>- \prefs ->
                exportTextFile prefsFile (setHeapSize 41943040 prefs) //40M
        where
            prefsFile =  target</>"Clean 2.4"</>"Config"</>"IDEPrefs"
            setHeapSize n s = replaceSubString "\tHeapSize:\t2097152" ("\tHeapSize:\t" +++ toString n) s

	//Remove files not required for the target platform
	cleanupDistro platform target
        = worldIO (deleteFile (target </> "Clean_2.4.zip")) @ const Void //Delete Clean zip archive
        //Remove unneeded platform file
        //Remove unneeded Clean Libraries
        //Remove unneeded ExtJS files

	//Create a zipped version 
	zipDistro tmpDir targetDir targetDoc
        = callProcess ("Creating package " +++ targetDoc) [] zipExe zipArgs (Just tmpDir) @ const Void
    where
		zipExe = IF_POSIX_OR_WINDOWS "/usr/bin/zip" "C:\\Program Files\\7-Zip\\7z.exe"
		zipArgs = IF_POSIX_OR_WINDOWS ["-r", targetDoc,targetDir] [] 

downloadDistro:: Document -> Task Document
downloadDistro doc
    = viewInformation ("Done!", "Your package has successfully been created.") [] doc

Start :: *World -> *World
Start world = startEngine makeDistribution world
