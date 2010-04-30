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

ActionSyntax :== ActionLabel "Do Synatx Highlighting"
	
srcEditor :: !(DBid AppState) -> Task Void
srcEditor sid =
				readDB sid
	>>= \state.	writeDB sid {state & srcEditorContent = highlightSyntax state.srcEditorContent}
	>>|			updateShared "Clean Source" [ButtonAction (ActionSyntax, Always)] sid [srcEditorView]
	>>|			srcEditor sid
	>>|			return Void
where
	srcEditorView = editor	{ editorFrom	= \state		-> state.srcEditorContent
							, editorTo		= \src state	-> {state & srcEditorContent = src}
							}

:: Mode = InCode | InString | InChar | InMLComment |InSLComment | InNum
						
highlightSyntax ft
	# tags	= highlightSyntax` 0 InCode '\0' "" []
	# src	= foldl (+++) "" (map toString tags)
	= setFormattedTextSrc src ft
where
	highlightSyntax` n mode prevChar curString acc
		| n < textSize src
			# curChar	= select src n
			# m			= inc n
			# (pos, mode, curString, acc) = case mode of
				InCode
					| curChar == '"'								= (m,							InString,	"\"",			addToAcc 1)
					| curChar == '\''								= (m,							InChar,		"'",			addToAcc 1)
					| prevChar == '/' && curChar == '*'				= (m,							InMLComment,"/*",			addToAcc 2)
					| prevChar == '/' && curChar == '/'				= (m,							InSLComment,"//",			addToAcc 2)
					| isDigit curChar && not (isAlphanum prevChar)	= (m,							InNum,		if (prevChar == '-' || prevChar == '+') (toString [prevChar,curChar]) (toString curChar),	addToAcc (if (prevChar == '-' || prevChar == '+') 2 1))
				InString | (curChar == '"' && prevChar <> '\\') || curChar == '\n'
																	= (if (curChar == '\n') n m,	InCode,		"",				addToAcc (if (curChar == '\n') 1 0))
				InChar | (curChar == '\'' && prevChar <> '\\') || curChar == '\n'
																	= (if (curChar == '\n') n m,	InCode,		"",				addToAcc (if (curChar == '\n') 1 0))
				InMLComment | prevChar == '*' && curChar == '/'
																	= (m,							InCode,		"",				addToAcc 0)
				InSLComment | curChar == '\n'
																	= (n,							InCode,		"",				addToAcc 1)
				InNum | not (isHexDigit curChar || curChar == 'E' || curChar == 'x' || curChar == '.')
																	= (n,							InCode,		"",				addToAcc 1)
				_													= (m,							mode,		appToCurStr,	acc)
			= highlightSyntax` pos mode curChar curString acc
		| otherwise = reverse (addToAcc 0)
	where
		src	= toUnformattedString ft
		appToCurStr
			| n < textSize src	= curString +++ toString (select src n)
			| otherwise			= curString
		addToAcc removeChars
			# string = case removeChars of
				0	= appToCurStr
				1	= curString
				n	= subString 0 (textSize curString - 1) curString
			= case mode of
				InString	= [SpanTag [StyleAttr "color: green"] 	(mkTextTags string False):acc]
				InChar		= [SpanTag [StyleAttr "color: purple"]	(mkTextTags string False):acc]
				InNum		= [SpanTag [StyleAttr "color: orange"]	(mkTextTags string False):acc]
				InSLComment	= [SpanTag [StyleAttr "color: blue"]	(mkTextTags string False):acc]
				InMLComment	= [SpanTag [StyleAttr "color: blue"]	(createLineBreaks string False):acc]
				InCode		= reverse (createLineBreaks string True) ++ acc
				
	createLineBreaks src processContent
		# lines	= split "\n" src
		= init (flatten (map (\line -> mkTextTags line processContent ++ [BrTag []]) lines))
			
	mkTextTags str processContent
		#tags = init (flatten [[wordTag word, RawText "&nbsp;"] \\ word <- split " " str])
		| processContent && indexOf "::" str <> -1	= [SpanTag [StyleAttr "color: red"] tags]
		| otherwise									= tags
	where
		wordTag word
			| processContent && isMember word keywords	= SpanTag [StyleAttr "color: purple"] [Text word]
			| otherwise									= Text word
		keywords = ["where", "import", "from", "let", "in", "module", "definition", "implementation", "derive", "class", "True", "False"]

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
	>>= \src.	writeDB sid {state & srcEditorContent = setFormattedTextSrc src state.srcEditorContent}
	>>|			stop

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