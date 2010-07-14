implementation module CompilerInterface

import iTasks, DocumentDomain, Text
import Configuration

derive class iTask	CompilerException
derive bimap		Maybe, (,)

compileToExe :: !(DBid AppState) -> Task Document
compileToExe sid
	# compileToExe` = try compileToExe` handleCallException
	# compileToExe` = try compileToExe` handleReadLogException
	# compileToExe` = try compileToExe` handleStringExceptions
	= compileToExe`
where
	compileToExe` =
						getConfig sid
		>>= \config.	getAppPath
		>>= \appPath.	callProcess (config.oldIDEPath +++ " --batch-build \"" +++ appPath +++ (config.projectsPath +++ "\\test\\test.prj\""))
		>>= \ret.		case ret of
							0	= 				importDocument "projects\\test\\test.exe"
									>>=			return
							_	=				readTextFile (config.projectsPath +++ "\\test\\test.logd")
									>>= \log.	throw (CompilerErrors (filter ((<>) "") (split "\n" log)))
									
	handleCallException (CallFailed path)			= throw (CannotRunCompiler ("Error creating process '" +++ path +++ "'"))
	handleReadLogException (FileException path _)	= throw (CannotRunCompiler ("Unable to retrieve compiler errors from '" +++ path +++ "'"))
	handleStringExceptions str						= throw (CannotRunCompiler str)