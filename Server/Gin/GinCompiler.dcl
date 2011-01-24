definition module GinCompiler

import iTasks
from GinSyntax import ::GModule

:: CompileResult a = CompileSuccess a | CompileGlobalError String | CompilePathError [(String,String)]

derive class iTask CompileResult

batchBuild :: !GModule *World -> (CompileResult Void, *World)

syntaxCheck :: !GModule *World -> (CompileResult Void, *World)

