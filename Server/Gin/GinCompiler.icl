implementation module GinCompiler

import StdDynamicFileIO
import StdMaybe
from StdSystem import dirseparator

import GinSyntax
import GinAbstractSyntax
import GinParser

import GinCompilerLogParser
import GinOSUtils
import GinConfig
import Text

derive class iTask CompileResult

runCompiler :: !GModule (String String GinConfig *World -> (CompileResult a, *World)) *World -> (CompileResult a, *World)
runCompiler gMod compiler world
//1. Parse and transform GModule
#result = runParse (gToAModule gMod)
| isParseError result = (CompilePathError (map (\(path,msg) = (toString path,msg)) (getParseError result)), world)
#aMod = expandModule (getParseSuccess result)
//2. Load configuration
#(config,world) = ginLoadConfig world 
| isNothing config = (CompileGlobalError "Configuration not found", world)
#config = fromJust config
//3. Pretty-print module
#basename = "test" /* TODO: Generate unique name */
#source = renderAModule [PathContexts] { AModule | aMod & name = basename }
//4. Write source code to file
#(result, world) = osWriteTextFile (filenameFromConfig config basename "icl") source world
| isOSError result = (CompileGlobalError ("Write icl file failed: " +++ formatOSError result), world)
//5. Call compiler function
= compiler source basename config world

batchBuild :: !GModule *World -> (CompileResult Dynamic, *World)
batchBuild gMod world = runCompiler gMod compiler world 
where
	compiler :: !String !String !GinConfig *World -> (CompileResult Dynamic, *World)
	compiler source basename config world
	#(result, world) = osCallProcessBlocking (quote (config.cleanPath +/+ "CleanIDE.exe") +++ " --batch-build " +++ quote (filenameFromConfig config basename "prj")) world
	| isOSError result = (CompileGlobalError ("Calling Clean IDE failed: " +++ formatOSError result), world)
	| getOSResult result == 0
//      # (result, world) = osCallProcessBlocking (filenameFromConfig config basename "bat") world
//      | isOSError result = (CompileGlobalError ("Failed to run dynamic linker batch file: " +++ formatOSError result), world)
//      # (ok,dyn,world) = readDynamic basename world
//      | not ok = (CompileGlobalError "Failed to read dynamic", world)
//	  = (CompileSuccess dyn, world)
      = (CompileSuccess (dynamic Void), world)
	#(result, world) = osReadTextFile (filenameFromConfig config basename "log") world
	| isOSError result = (CompileGlobalError ("Read log file failed: " +++ formatOSError result), world)
	#log = getOSResult result
	= (CompilePathError (findPathErrors (parseCleanIDELog log) source), world)

syntaxCheck :: !GModule *World -> (CompileResult Void, *World)
syntaxCheck gMod world = runCompiler gMod compiler world
where
	compiler :: !String !String !GinConfig *World -> (CompileResult Void, *World)
	compiler source basename config world
	#(result, world) = osCallProcessBlocking (syntaxCheckCommand (filenameFromConfig config basename "icl") config) world
	| isOSError result = (CompileGlobalError ("Calling Clean compiler failed: " +++ formatOSError result), world)
	| getOSResult result == 0 = (CompileSuccess Void, world)
	#(result, world) = osReadTextFile (errorFileName config) world
	| isOSError result = (CompileGlobalError ("Read log file failed: " +++ formatOSError result), world)
	#log = getOSResult result
	= (CompilePathError (findPathErrors (parseCleanCompilerLog log) source), world)
	
syntaxCheckCommand :: !String !GinConfig -> String
syntaxCheckCommand sourcefilename config = 
	quote (config.cleanPath +/+ "tools" +/+ "Clean System" +/+ "CleanCompiler.exe")
	+++ " -h 64M  -dynamics -generics -wmt  -desc -exl -dynamics -lat " 
	+++ "-ou \"" +++ sourcefilename
	+++ "\" -P \"" +++ join ";" (searchPaths config)
	+++ "\" -RE \"" +++ errorFileName config
	+++ "\" -RO \"" +++ config.cleanPath +/+ "Temp" +/+ "out\"" 
	
errorFileName :: !GinConfig -> String
errorFileName config = config.cleanPath +/+ "Temp" +/+ "errors"

filenameFromConfig :: !GinConfig !String !String -> String
filenameFromConfig config basename extension = config.projectPath +/+ basename +++ "." +++ extension

searchPaths :: GinConfig -> [String]
searchPaths config = 
	[ config.projectPath
	: prefix (appendTrailingSeparator config.iTasksPath) iTasksPaths
	  ++ prefix (appendTrailingSeparator config.cleanPath) cleanPaths
	] where
		prefix p l = map (\x = p +++ x) l

iTasksPaths :: [String]
iTasksPaths = 
	[ "Server"
	, "Server\\API\\Core"
	, "Server\\API\\Core\\BasicTasks"
	, "Server\\API\\Core\\TaskCombinators"
	, "Server\\API\\Domains"
	, "Server\\API\\Extensions"
	, "Server\\API\\Extensions\\Admin"
	, "Server\\Framework"
	, "Server\\Framework\\Data"
	, "Server\\Framework\\Handlers"
	, "Server\\Gin"
	, "Server\\lib"
	, "Server\\lib\\Platform"
	, "Server\\lib\\Http"
	, "Server\\lib\\graph_copy"
	]

cleanPaths :: [String]
cleanPaths = 
	[ "Libraries\\StdEnv"
	, "Libraries\\StdLib"
	, "Libraries\\Generics"
	, "Libraries\\ObjectIO"
	, "Libraries\\ObjectIO\\OS Windows"
	, "Libraries\\Dynamics"
	, "Libraries\\Dynamics\\extension"
	, "Libraries\\Dynamics\\general"
	, "Libraries\\Dynamics\\implementation"
	, "Libraries\\Dynamics\\implementation\\windows"
	, "Libraries\\Directory"
	, "Libraries\\TCPIP"
	, "libraries\\hilde"
	, "libraries\\exceptionswindows"
	, "libraries\\hilde\\parser combinators 2002"
	, "libraries\\wrapdebug"
	]
