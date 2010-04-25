module CleanIDE

import iTasks, CommonDomain, Text
from StdFunc import flip
import PmProject, UtilStrictLists
import CompilerInterface, AppState, Configuration, GUI

Start world = startEngine [workflow "Clean IDE" cleanIDE] world

cleanIDE :: Task Void
cleanIDE = try cleanIDE` handleErrors
where
	cleanIDE` =
						setMenus menuStructure
		>>|				loadConfig
		>>= \mbConfig.	case mbConfig of
							Just config =	
												createDB (initAppState config)
								>>= \sid.		isDirectory (config.projectsPath +++ "\\test")
								>>= \prjExists.	if prjExists (return Void) (createTestPrj sid)
								>>|				openFile (config.projectsPath +++ "\\test\\test.icl") sid
								>>|				dynamicGroupAOnly [srcEditor sid] (actions sid)
								>>|				deleteDB sid
							Nothing = stop
	where
		menuStructure =	[ Menu "File"		[ MenuItem "Save" ActionSave
											, MenuItem "Save & Compile..." ActionCompile
											, MenuSeparator
											, MenuItem "Quit" ActionQuit
											]
						, Menu "Options"	[ MenuItem "Application..."		ActionEditAppOptions
											, MenuItem "Code Generation..."	ActionEditCodeGenOptions
											, MenuItem "Linker..."			ActionEditLinkOptions
											]
						]
	
		actions :: !(DBid AppState) -> [GroupAction GOnlyAction Void Void]
		actions sid =	[ GroupAction ActionQuit				GOStop GroupAlways
						, GroupAction ActionSave				(GOExtend [save sid]) GroupAlways
						, GroupAction ActionCompile				(GOExtend [saveAndCompile sid <<@ GBModal]) GroupAlways
						, GroupAction ActionEditCodeGenOptions	(GOExtend [editProjectOptions	"Code Generation Options"	PR_GetCodeGenOptions		PR_SetCodeGenOptions		sid <<@ GBAlwaysFloating]) GroupAlways
						, GroupAction ActionEditAppOptions		(GOExtend [editProjectOptions	"Application Options"		PR_GetApplicationOptions	PR_SetApplicationOptions	sid <<@ GBAlwaysFloating]) GroupAlways
						, GroupAction ActionEditLinkOptions		(GOExtend [editProjectOptions	"Linker Options"			PR_GetLinkOptions			(flip PR_SetLinkOptions)	sid <<@ GBAlwaysFloating]) GroupAlways
						]
						
		editProjectOptions desc get putback sid =
								readDB sid
			>>= \state.			accWorld (accFiles (ReadProjectFile (state.config.projectsPath +++ "\\test\\test.prj") ""))
			>>= \(prj,ok,err).	editOptions desc prj get putback
			>>= \prj.			accWorld (accFiles (SaveProjectFile (state.config.projectsPath +++ "\\test\\test.prj") prj ""))
			>>|					stop
	
	handleErrors :: !FileException -> Task Void
	handleErrors (FileException path _) = showMessageAbout "Error" ("Could not open '" +++ path +++ "'!")
	
ActionCompile				:== ActionLabel "compile"
ActionEditCodeGenOptions	:== ActionLabel "codeGenOpts"
ActionEditAppOptions		:== ActionLabel "appOpts"
ActionEditLinkOptions		:== ActionLabel "linkOpts"
	
srcEditor :: !(DBid AppState) -> Task Void
srcEditor sid = ignoreResult (updateShared "Clean Source" [] sid [srcEditorView])
where
	srcEditorView = editor	{ editorFrom	= \state		-> state.srcEditorContent
							, editorTo		= \src state	-> {state & srcEditorContent = src}
							}

save :: !(DBid AppState) -> Task Void
save sid =
				readDB sid
	>>= \state.	writeTextFile (state.config.projectsPath +++ "\\test\\test.icl") (toUnformattedString state.srcEditorContent)

saveAndCompile :: !(DBid AppState) -> Task Void
saveAndCompile sid
	# compile` = try compile` handleCompilerExceptions
	# compile` = try compile` handleFileExceptions
	= compile`
where
	compile` =
						save sid
		>>|				compileToExe sid
		>>= \exeDoc.	showMessageAbout "Download Executable" exeDoc
	
	handleCompilerExceptions e = showMessageAbout "Compiler Errors" msg
	where
		msg = case e of
			CannotCallCompiler path		= ["Unable to run compiler: '" +++ path +++ "'"]
			CompilerErrors errs			= errs
			
	handleFileExceptions (FileException path _) = showMessageAbout "Save Error" ("Unnable to write to '" +++ path +++ "'")

openFile :: !Path !(DBid AppState) -> Task Void
openFile path sid =
				readDB sid
	>>= \state.	readTextFile path
	>>= \src.	writeDB sid {state & srcEditorContent = setFormattedTextSrc (mkFormattedSrc src) state.srcEditorContent}
	>>|			stop
where
	mkFormattedSrc src = replaceSubString "\n" "<br>" src

createTestPrj :: !(DBid AppState) -> Task Void
createTestPrj sid =
				readDB sid
	>>= \state.	createDirectory (state.config.projectsPath +++ "\\test")
	>>|			accWorld (createPrjFile state.config.projectsPath)
	>>= \ok.	writeTextFile (state.config.projectsPath +++ "\\test\\test.icl") "module test\n\nimport StdEnv\n\nStart = "
where
	createPrjFile path world = accFiles (\f -> SaveProjectFile (path +++ "\\test\\test.prj") initProj "" f) world
	initProj = PR_NewProject "test" editOptions compilerOptions codeGenOptions appOptions ("{Project}" :! Nil) linkOptions
	where
		editOptions		= {eo = {newlines = NewlineConventionNone}, pos_size = NoWindowPosAndSize}
		compilerOptions	= DefaultCompilerOptions
		codeGenOptions	= DefCodeGenOptions
		appOptions		= DefApplicationOptions
		linkOptions		= DefaultLinkOptions