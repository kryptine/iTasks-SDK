definition module CompilerInterface

import iTasks, AppState

:: CompilerException = CannotRunCompiler !String | CompilerErrors ![String]

derive class iTask CompilerException

compileToExe :: !(DBid AppState) -> Task Document