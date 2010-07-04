definition module CompilerInterface

import iTasks, AppState

:: CompilerException = CannotRunCompiler !String | CompilerErrors ![String]

derive gPrint		CompilerException
derive gParse		CompilerException
derive gVisualize	CompilerException
derive gUpdate		CompilerException
derive gHint		CompilerException
derive gError		CompilerException

compileToExe :: !(DBid AppState) -> Task Document