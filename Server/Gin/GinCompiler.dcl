definition module GinCompiler

from iTasks import class iTask, :: IWorld
import GinSyntax
import GinParser

:: CompileResult a = CompileSuccess a | CompileGlobalError String | CompilePathError [(GPath,String)]

derive class iTask CompileResult

batchBuild :: !GModule *IWorld -> (CompileResult String, *IWorld)

syntaxCheck :: !GModule *IWorld -> (CompileResult Void, *IWorld)

exitCompiler :: *IWorld -> *IWorld

