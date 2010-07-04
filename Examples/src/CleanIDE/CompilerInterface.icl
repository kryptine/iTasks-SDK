implementation module CompilerInterface

import iTasks, DocumentDomain, Text
import Configuration

derive gPrint		CompilerException
derive gParse		CompilerException
derive gVisualize	CompilerException
derive gUpdate		CompilerException
derive gHint		CompilerException
derive gError		CompilerException
derive bimap		Maybe, (,)

compileToExe :: !(DBid AppState) -> Task Document
compileToExe sid = try compileToExe` handleCallExceptions
where
	compileToExe` =
						getConfig sid
		>>= \config.	getAppPath
		>>= \appPath.	callProcess (config.oldIDEPath +++ " --batch-build \"" +++ appPath +++ (config.projectsPath +++ "\\test\\test.prj\""))
		>>= \ret.		case ret of
							0	= 				importDocument "projects\\test\\test.exe"
									>>=			return
							_	=				try (readTextFile (config.projectsPath +++ "\\test\\test.log")) readLogError
									>>= \log.	throw (CompilerErrors (filter ((<>) "") (split "\n" log)))
									
	handleCallExceptions (CallFailed path)	= throw (CannotCallCompiler path)
	readLogError (FileException path _)		= return ("Unable to retrieve compiler errors from '" +++ path +++ "'")