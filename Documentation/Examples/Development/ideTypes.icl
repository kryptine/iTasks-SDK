implementation module ideTypes

import iTasks
import PmTypes, PmProject, PmParse, PmEnvironment

derive class iTask IDE_State

IDE_Store :: Shared IDE_State
IDE_Store = sharedStore "IDE_State" init_IDE_State
where
	init_IDE_State :: IDE_State			
	init_IDE_State
		= 	{ projectName		= ""
			, projectPath 		= projectPath
			, project			= initProject "" 
			, cleanPath			= cleanPath
			, openedFiles		= []
			, recentFiles 		= []
			, recentProjects	= []
			, idx				= 0
			, envTargets		= [initTarget]
			}
	initIDEenv
		=	{ environmentName		= "IDE environment"	
			, paths					= environment
			, toolsOption			= initTools
			}
	initStdEnv
		=	{ environmentName		= "StdEnv"	
			, paths					= [stdenv]
			, toolsOption			= initTools
			}
	initTools 	
		= 	{ compiler				= compilerPath +++ "CleanCompiler.exe : -h 64M : -dynamics -generics"
			, codeGenerator			= compilerPath +++ "CodeGenerator.exe"
			, staticLinker			= compilerPath +++ "StaticLinker.exe : -h 64M"
			, dynamicLinker			= compilerPath +++ "DynamicLinker.exe"
			, versionOfAbcCode		= 920
			, runOn64BitProcessor	= False
			}

initProject :: !ModuleName -> Project
initProject main_module_file_name
	= PR_NewProject	main_module_file_name editWdOptions compilerOptions codeGenOptions applicationOptions list linkOptions
where
	editWdOptions 		= 	{	eo 			= { newlines = NewlineConventionNone }
							,	pos_size	= NoWindowPosAndSize 
							}
	compilerOptions		= DefaultCompilerOptions
	codeGenOptions		= DefCodeGenOptions
	applicationOptions	= DefApplicationOptions
	list				= [!!]
	linkOptions			= DefaultLinkOptions
