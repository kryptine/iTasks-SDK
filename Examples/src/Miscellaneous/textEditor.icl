implementation module textEditor

import iTasks, CommonDomain, StdMisc, Text

textEditor :: [Workflow]
textEditor = [workflow "Examples/Miscellaneous/Text Editor" (textEditorApp <<@ Subject "Text Editor")]

// global workflows
textEditorApp :: Task Void
textEditorApp = 
				setMenus
					[ Menu "File"	[ MenuItem "New"			ActionNew
									, MenuItem "Open..."		ActionOpen
									, MenuName recOpenedMenu	(SubMenu "Recently Opened" [])
									, MenuSeparator
									, MenuItem "Save"			ActionSave
									, MenuItem "Save As..."		ActionSaveAs
									, MenuSeparator
									, MenuItem "Close"			ActionClose
									, MenuItem "Quit"			ActionQuit
					]
					, Menu "Edit"	[ MenuItem "Replace..."		ActionReplace ]
					, Menu "Tools"	[ MenuItem "Statistics..."	ActionStats ]
					, Menu "Help"	[ MenuItem "About"			ActionShowAbout ]
					]
	>>|			mdiApplication 0 groupActions
where
	groupActions :: !(DBid Int) !(MDITasks EditorState Bool) -> [GroupAction GAction Void Int]
	groupActions aid mdiTasks=:{createEditor, iterateEditors} =
		[ GroupAction		ActionNew		(GExtend [modifyDB aid inc >>= \newNum. createEditor (EditorState (Note "") (NewFile newNum)) textEditorFile])	GroupAlways
		, GroupAction		ActionOpen		(GExtend [openDialog mdiTasks <<@ GBFloating])																	GroupAlways
		, GroupActionParam	actionOpenFile	(\fid -> GExtend [open (DBRef (toInt fid)) mdiTasks Nothing])													GroupAlways
		, GroupAction		ActionShowAbout	(GExtend [about <<@ GBAlwaysFloating])																			GroupAlways
		, GroupAction		ActionQuit		(GExtend [quit iterateEditors <<@ GBModal])																		GroupAlways
		]									
						
ActionReplace	:== ActionLabel "replace"
ActionStats		:== ActionLabel "stats"
recOpenedMenu	:== "recOpened"
actionOpenFile	:== "openFile"

openDialog :: !(MDITasks EditorState a) -> Task GAction
openDialog mdiTasks =
				getAllFileNames
	>>= \files.	if (isEmpty files)
					(showMessageAbout "Open File" "No files to open!" <<@ ExcludeGroupActions >>| continue)
					(										enterChoiceA "Open File" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] files <<@ ExcludeGroupActions
						>>= \(action,(name, Hidden fid)).	case action of
					 										ActionOk	=	open fid mdiTasks (Just name)
					 										_			=	continue
					)
					
open :: !(DBRef TextFile) !(MDITasks EditorState a) !(Maybe String) -> Task GAction
open fid {createEditor, existsEditor} mbAddToRecOpenedName =
				existsEditor checkIfAlreadyOpened
	>>= \mbEid.	case mbEid of
		Nothing =
				case mbAddToRecOpenedName of
					Just name	= addToRecentlyOpened name fid
					Nothing		= return Void
			>>|	getFile fid
			>>= \file.	return (GExtend [createEditor (EditorState file.TextFile.content (OpenedFile file)) textEditorFile  <<@ GBFloating])
		(Just eid) = return (GFocus eid)
where
	checkIfAlreadyOpened :: !EditorState -> Bool
	checkIfAlreadyOpened (EditorState _ file) =	case file of
		NewFile _		= False
		OpenedFile file	= (fid == file.fileId)
					
	addToRecentlyOpened :: !String !(DBRef TextFile) -> Task Void
	addToRecentlyOpened name (DBRef id) =
					getMenuItem recOpenedMenu
		>>= \item.	case item of
						Just (SubMenu label entries)	= setMenuItem recOpenedMenu (SubMenu label (take 5 [MenuItem name (ActionParam actionOpenFile (toString id)):entries]))
						_								= stop

about :: Task GAction
about =
		showMessageAbout "About" "iTextEditor June 2010" <<@ ExcludeGroupActions
	>>|	continue

quit :: !(Bool -> (Bool -> (String -> Task Bool)) -> (Task Bool)) -> Task GAction	
quit iterateEditors =
					iterateEditors False checkForUnsavedData
	>>= \cancel.	if cancel continue stopGroup
where
	checkForUnsavedData :: !Bool !String -> Task Bool
	checkForUnsavedData True e	= return True
	checkForUnsavedData False e	= requestClosingFile e
	
// editor workflows
textEditorFile :: !EditorStateRef -> Task Void
textEditorFile eid = dynamicGroupA [editorWindow eid <<@ GBFloating] (actions eid)
where
	actions :: !EditorStateRef -> [GroupAction GAction Void EditorState]
	actions eid =	[ GroupAction ActionSave	(GExtend [save eid])						(SharedPredicate eid noNewFile)
					, GroupAction ActionSaveAs	(GExtend [saveAs eid <<@ GBModal])			GroupAlways
					, GroupAction ActionReplace	(GExtend [replaceT eid  <<@ GBFloating])	(SharedPredicate eid contNotEmpty)
					, GroupAction ActionStats	(GExtend [statistics eid  <<@ GBFloating])	GroupAlways
					, GroupAction ActionClose	(GExtend [close eid <<@ GBModal])			GroupAlways
					]
	
	noNewFile :: !(SharedValue EditorState) -> Bool					
	noNewFile SharedDeleted = abort "editor state deleted"
	noNewFile (SharedValue (EditorState _ file)) = case file of
		(OpenedFile _)	= True
		_				= False
	
	contNotEmpty :: !(SharedValue EditorState) -> Bool
	contNotEmpty SharedDeleted = abort "editor state deleted"
	contNotEmpty (SharedValue (EditorState (Note cont) _)) = cont <> ""
	
editorWindow :: !EditorStateRef -> Task GAction
editorWindow eid =
		updateShared "Text Editor" [] eid [titleListener, mainEditor] <<@ Tag eid
	>>|	continue
where		
	mainEditor = editor	{ editorFrom	= \(EditorState continue _)			-> continue
						, editorTo		= \newCont (EditorState _ file)	-> EditorState newCont file
						}
						
save :: !EditorStateRef -> Task GAction
save eid =
					readDB eid
	>>= \editor.	case editor of
						(EditorState txt (OpenedFile file)) =
										setFileContent txt file
							>>= \file.	writeDB eid (EditorState file.TextFile.content (OpenedFile file))
							>>|			continue
						_ = saveAs eid

saveAs :: !EditorStateRef -> Task GAction
saveAs eid =
						enterInformationA "Save As: enter name" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] <<@ ExcludeGroupActions
	>>= \(action,name).	case action of
							ActionOk =
															readDB eid
								>>= \(EditorState txt _).	storeFile name txt
								>>= \file.					writeDB eid (EditorState file.TextFile.content (OpenedFile file))
								>>|							continue
							_ = continue
							
:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}

replaceT :: !EditorStateRef -> Task GAction
replaceT eid = replace` {searchFor = "", replaceWith = ""}
where
	replace` :: !Replace -> Task GAction
	replace` repl =
								updateInformationA "Replace" [ButtonAction (ActionClose, Always), ButtonAction (ActionReplaceAll, IfValid)] repl <<@ ExcludeGroupActions
		>>= \(action, repl).	case action of
									ActionReplaceAll =
											modifyDB eid (\(EditorState (Note txt) file) -> (EditorState (Note (replaceSubString repl.searchFor repl.replaceWith txt)) file))
										>>|	replace` repl
									_ = continue
								
ActionReplaceAll :== ActionLabel "Replace all"
												
:: TextStatistics =	{ lines			:: Int
					, words			:: Int
					, characters	:: Int
					}

statistics :: !EditorStateRef  -> Task GAction
statistics eid = 
		updateShared "Statistics" [ButtonAction (ActionOk, Always)] eid [titleListener, statsListener] <<@ ExcludeGroupActions
	>>|	continue
where
	statsListener = listener {listenerFrom =  \(EditorState (Note text) _) ->
			let txt = trim text
			in
				{lines = length (split "\n" txt), words = length (split " " (replaceSubString "\n" " " txt)), characters = textSize txt}
		}
					
titleListener :: View EditorState					
titleListener = listener	{ listenerFrom = \st=:(EditorState _ file) ->
								if (hasUnsavedData st) "*" "" 
								+++
								getFileName file
							}
								
close :: !EditorStateRef -> Task GAction
close eid =
					requestClosingFile eid
	>>= \cancel.	if cancel continue stopGroup
	
// helper functions
continue :: Task GAction
continue = return GContinue

stopGroup :: Task GAction
stopGroup = return GStop

hasUnsavedData :: !EditorState -> Bool
hasUnsavedData (EditorState continue file) = case file of
	NewFile _		= True
	OpenedFile file	= continue <> file.TextFile.content
	
requestClosingFile :: !EditorStateRef -> Task Bool
requestClosingFile eid =
	readDB eid
	>>= \state=:(EditorState _ file).	if (hasUnsavedData state)
										(					showMessageAboutA "Save changes?" [ButtonAction (ActionCancel, Always), ButtonAction (ActionNo, Always), ButtonAction (ActionYes, Always)] ("Save changes to '" +++ getFileName file +++ "'?") <<@ ExcludeGroupActions
											>>= \action.	case action of
																ActionCancel	= return True
																ActionNo		= return False
																ActionYes		= save eid >>| return False
										)
										(return False)

// global application state
:: EditorState = EditorState !Note !EditorFile
:: EditorFile = NewFile !Int | OpenedFile !TextFile
:: EditorStateRef :== DBid EditorState

// text files database
:: FileName :==	String
:: TextFile =	{ fileId	:: !(DBRef TextFile)
				, name		:: !FileName
				, content	:: !Note
				}

instance DB TextFile where
	databaseId			= mkDBid "TextFiles"
	getItemId file		= file.fileId
	setItemId id file	= {file & fileId = id}
	
storeFile :: !FileName !Note -> Task TextFile
storeFile name txt =
				getDefaultValue
	>>= \file.	dbCreateItem {TextFile| file & name = name, content = txt}
	>>= \file.	return file
	
setFileContent :: !Note !TextFile -> Task TextFile
setFileContent continue file = dbUpdateItem {TextFile | file & content = continue}
	
getFile :: !(DBRef TextFile) -> Task TextFile
getFile id =
				dbReadItem id
	>>= \res.	case res of
					Nothing		= undef
					(Just file)	= return file
					
getFileName :: !EditorFile -> String
getFileName file = case file of
	NewFile idx		= "New Text Document " +++ toString idx
	OpenedFile file	= file.TextFile.name

getAllFileNames :: Task [(FileName, Hidden (DBRef TextFile))]
getAllFileNames =
				dbReadAll
	>>= \files.	return (map (\f -> (f.TextFile.name, Hidden f.fileId)) files)
	
derive gPrint			EditorState, EditorFile, TextFile, TextStatistics, Replace
derive gParse			EditorState, EditorFile, TextFile, TextStatistics, Replace
derive gVisualize		EditorState, EditorFile, TextFile, TextStatistics, Replace
derive gUpdate			EditorState, EditorFile, TextFile, TextStatistics, Replace
derive gMerge			EditorState, EditorFile, TextFile, TextStatistics, Replace
derive gMakeSharedCopy	EditorState, EditorFile, TextFile
derive gMakeLocalCopy	EditorState, EditorFile, TextFile
derive gError			EditorState, EditorFile, TextFile, TextStatistics, Replace
derive gHint			EditorState, EditorFile, TextFile, TextStatistics, Replace
derive bimap			Maybe, (,)