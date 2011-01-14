definition module GinCompiler

import GenEq, GenPrint, GenParse, GenVisualize, GenUpdate
from GinSyntax import ::GModule

:: CompileResult a = CompileSuccess a | CompileGlobalError String | CompilePathError [(String,String)]

derive class iTask CompileResult

batchBuild :: !GModule *World -> (CompileResult Dynamic, *World)

syntaxCheck :: !GModule *World -> (CompileResult Void, *World)

