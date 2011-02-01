definition module PmCleanSystem

import StdFile
import StdOverloaded

import UtilStrictLists
import PmCompilerOptions
import PmTypes
import StdPathname

::	CompileOrCheckSyntax	= SyntaxCheck | Compilation
instance == CompileOrCheckSyntax

::	CodeGenerateAsmOrCode	= AsmGeneration | CodeGeneration
instance == CodeGenerateAsmOrCode

:: CompilingInfo = NotCompiling | CompilingInfo !CompilerProcess;

InitCompilingInfo :: *CompilingInfo

ExitCleanCompiler :: !(!CompilingInfo,*env) -> (!CompilingInfo,*env)

:: CompilerProcess = NoCompiler | CompilerProcess !Int !Int !Int; // thread_id thread_handle process_handle

::	CompilerMsg
	= 	CompilerOK
	| 	SyntaxError
	|   GlobalError

instance == CompilerMsg

::	WindowFun env :== ([String]) -> env -> env

CompilePersistent ::
	!String !Bool !(WindowFun *env) !(WindowFun *env) !CompileOrCheckSyntax !Pathname
	!(List Pathname) !Bool !Bool !Bool !CompilerOptions !Pathname !CompilingInfo !*env
	-> (!CompilingInfo,!(!*env, !Pathname, !CompilerMsg))
	| FileEnv env

CodeGen	::						// Generates code for the given file:
	!String						// generator exe name and options
								// !! should be full path so that cg generates diagnostics in logical place...
								// should be quoted if required
	!(WindowFun *env)			// error display fun
	!CodeGenerateAsmOrCode		// generate assembly only?
	!Pathname					// full .abc pathname of module to be compiled
	!Bool						// time profiling...
	!CodeGenOptions				// code generator options
	!Processor					// target processor
	!ApplicationOptions			// application options
	!Pathname					// startup directory
	!*env						// state
	->
	( !Pathname					// full pathname of generated object file
//					Note: on the macintosh the .o file is generated in the standard Clean System
//					Files Folder. On Unix, however, the location of the .o depends on the user
//					settings.
	, !Bool							// success status
	, !*env							// state
	)
	| FileEnv env

Link ::							// Links the given file:
	!String						// linker exe name
								// !! should be full path so that linker generates diagnostics in logical place...
								// this is quoted by the Link function because it first needs to decompose it...
	!(WindowFun *env)			// error display fun
	!Pathname					// full pathname of the executable
	!ApplicationOptions			// application options
	!Pathname					// options pathname
	!(List Pathname)			// dynamic library file names
	!(List Pathname)			// object file names
	!(List Pathname)			// static library file names
	!Bool						// link statically?
	!Bool						// generate relocations?
	!Bool						// generate link map?
	!Bool						// link in resources?
	!String						// source of resources to link in
	!Bool						// generate dll?
	!String						// name of file containing symbols to be exported from dll
	!Pathname					// startup directory
	!String						// path to dynamic linker
	!Processor					// target processor
	!Bool						// 64 bit target processor
	!*env						// state
	->
	( !*env						// state
	, !Bool						// success status
	)
	| FileEnv env
