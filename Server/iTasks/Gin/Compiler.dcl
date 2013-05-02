definition module iTasks.Gin.Compiler

from iTasks import class iTask, :: IWorld
import iTasks.Gin.Syntax
import iTasks.Gin.Parser

:: CompileResult a = CompileSuccess a | CompileGlobalError String | CompilePathError [(GPath,String)]

derive class iTask CompileResult

batchBuild :: !GModule *IWorld -> (CompileResult String, *IWorld)

syntaxCheck :: !GModule *IWorld -> (CompileResult Void, *IWorld)

exitCompiler :: *IWorld -> *IWorld

