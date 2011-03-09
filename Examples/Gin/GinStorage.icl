implementation module GinStorage

import StdFile
from 	StdFunc import o, seqList, ::St
import StdMisc

from File import qualified fileExists, readFile
import FilePath
import Directory
import Text
import OSError

import 	iTasks

import GinConfig
import GinSyntax
import GinFlowLibrary

EXTENSION = "gcl"

derive class iTask MaybeError

listDirectory :: !String !*World -> (MaybeOSError [String], *World)
listDirectory path world
	# (res, world) = readDirectory path world
	| isError res = (res, world)
	= (Ok [ dropExtension m \\ m <- (fromOk res) | toLowerCase (takeExtension m) == EXTENSION ], world)

listModules :: !GinConfig !*World -> (MaybeOSError [String], *World)
listModules config world = listDirectory config.userPath world

searchPathModules :: !GinConfig !*World -> ([String], *World)
searchPathModules config world = sp [config.userPath : config.searchPaths] world
where
	sp :: [String] *World -> ([String], *World)
	sp [] world = ([], world)
	sp [path:paths] world
		# (mFiles, world) = listDirectory path world
		| isError mFiles = sp paths world
		# (files`, world) = sp paths world
		= (fromOk mFiles ++ files`, world)

modulePath :: !GinConfig !String !*World -> (Maybe String, *World)
modulePath config name world = mp [config.userPath : config.searchPaths] world
where
	mp :: [String] *World -> (Maybe String, *World)
	mp []           world = (Nothing, world)
	mp [path:paths] world 
		# filepath = (addExtension (path </> name) EXTENSION)
		# (exists, world) = 'File'.fileExists filepath world
		| exists	= (Just filepath, world)
		| otherwise	= mp paths world

readModule :: !GinConfig !String !*World -> (MaybeErrorString GModule, *World)
readModule config name world
	# (mPath, world) = modulePath config name world
	| isNothing mPath = (Error ("Module " +++ name +++ " not found in search path"), world)
	# (res, world) = 'File'.readFile (fromJust mPath) world
	| isError res = (Error ("Failed to read file " +++ fromJust mPath), world)
	# mJson = readJSON (fromOk res)
	| isNothing mJson = (Error ("Failed to parse file " +++ fromJust mPath), world)
	= (Ok (fromJust mJson), world)
where
	readJSON :: !String -> Maybe GModule
	readJSON s = fromJSON (fromString s)

readModules :: !GinConfig ![String] !*World -> (MaybeErrorString [GModule], *World)
readModules config [] world = (Ok [], world)
readModules config [name:names] world
	# (mMod, world) = readModule config name world
	| isError mMod = (liftError mMod, world)
	# (mMods, world) = readModules config names world
	| isError mMods = (mMods, world)
	= (Ok [fromOk mMod : fromOk mMods], world)
	
importModules :: !GinConfig ![String] !*World -> (MaybeErrorString [GModule], *World)
importModules config [] world = (Ok [predefinedModule], world)
importModules config names world
	# (mMods, world) = readModules config names world
	| isError mMods = (mMods, world)
	= (Ok [predefinedModule : fromOk mMods], world)
	
writeModule :: !GinConfig !String !GModule -> Task Void
writeModule config name gMod = exportJSONFile (addExtension (config.userPath </> name) EXTENSION) gMod >>| stop

moduleExists :: !GinConfig !String -> Task Bool
moduleExists config name = accWorld (modulePath config name) >>= \path -> return (isJust path)

newModuleName :: !GinConfig -> Task String
newModuleName config
	=						enterInformation "Give name of new module:"
		>>= \name ->		moduleExists config name
		>>= \exists ->		if exists
								( requestConfirmation ("Module " +++ name +++ " already exists, do you want to overwrite?")
								  >>= \ok -> if ok (return name) (newModuleName config)
								)
								( return name )

chooseModule :: !GinConfig -> Task (Maybe (!String, !GModule))
chooseModule config  
	=				accWorldOSError (listModules config)
		>>= \all ->	case all of
						[] ->		showMessage "Choose module" "No modules stored !" >>| return Nothing
						names ->	enterChoice "Choose module you want to use:" names
									>>= \choice -> accWorld (readModule config choice)
									>>= \mg -> case mg of
										Error e = showMessage "Error" ("Failed to read module " +++ choice +++ ":" +++ e) >>| return Nothing
										Ok gMod = return (Just (choice, gMod))

