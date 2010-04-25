implementation module CompilerInterface

import iTasks, DocumentDomain, Text
import Configuration

derive gPrint		CompilerException
derive gParse		CompilerException
derive gVisualize	CompilerException
derive gUpdate		CompilerException
derive bimap		Maybe, (,)

compileToExe :: !(DBid AppState) -> Task Document
compileToExe sid = try compileToExe` handleCallExceptions
where
	compileToExe` =
						getConfig sid
		>>= \config.	getAppPath
		>>= \appPath.	callProcess (config.oldIDEPath +++ " --batch-build " +++ appPath +++ (config.projectsPath +++ "\\test\\test.prj"))
		>>= \ret.		case ret of
							0	= 				loadDocumentFromFile "test.exe" "projects\\test"
									>>= \mbExe.	case mbExe of
													Nothing		= throw (CompilerErrors ["Unable to read executable"])
													Just exe	= return exe
							_	=				try (readTextFile (config.projectsPath +++ "\\test\\test.log")) readLogError
									>>= \log.	throw (CompilerErrors (filter ((<>) "") (split "\n" log)))
									
	handleCallExceptions (CallFailed path)	= throw (CannotCallCompiler path)
	readLogError (FileException path _)			= return ("Unable to retrieve compiler errors from '" +++ path +++ "'")