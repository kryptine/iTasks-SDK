implementation module textEditor

import iTasks, StdMisc, Text

textEditor :: [Workflow]
textEditor = [workflow "Examples/Miscellaneous/Text Editor" "A simple text editor, demonstrating the 'application'-like capabilities of iTasks" (textEditorApp <<@ Subject "Text Editor")]

// global workflows
textEditorApp :: Task Void
textEditorApp = mdiApplication (AppState 0 []) groupActions menuGenFunc
where
	groupActions :: !(DBId AppState) !(MDITasks EditorState Bool) -> [GroupAction GAction Void AppState]
	groupActions gid mdiTasks=:{createEditor, iterateEditors} =
		[ GroupAction		ActionNew		(GExtend [newFile gid createEditor])							GroupAlways
		, GroupAction		ActionOpen		(GExtend [openDialog gid mdiTasks <<@ GBFloating])				GroupAlways
		, GroupActionParam	actionOpenFile	(\fid -> GExtend [open (DBRef (toInt fid)) mdiTasks Nothing])	GroupAlways
		, GroupAction		ActionAbout		(GExtend [about <<@ GBAlwaysFloating])							GroupAlways
		, GroupAction		ActionQuit		(GExtend [quit iterateEditors <<@ GBModal])						GroupAlways
		]
		
	hotkey :: !Key -> Maybe Hotkey
	hotkey key = Just {ctrl = True, alt = False, shift = True, key = key}
	
	menuGenFunc :: AppState -> Menus
	menuGenFunc (AppState _ recOpenedFiles) =
		[ Menu "File"	[ MenuItem "New"			ActionNew		(hotkey N)
						, MenuItem "Open..."		ActionOpen		(hotkey O)
						, SubMenu "Recently Opened" recentlyOpenedMenu
						, MenuSeparator
						, MenuItem "Save"			ActionSave		(hotkey S)
						, MenuItem "Save As..."		ActionSaveAs	(hotkey A)
						, MenuSeparator
						, MenuItem "Close"			ActionClose		(hotkey C)
						, MenuItem "Quit"			ActionQuit		(hotkey Q)
						]
		, Menu "Edit"	[ MenuItem "Replace..."		ActionReplace	(hotkey R)
						]
		, Menu "Tools"	[ MenuItem "Statistics..."	ActionStats		(hotkey T)
						]
		, Menu "Help"	[ MenuItem "About"			ActionAbout		Nothing
						]
		]
	where
		recentlyOpenedMenu = [MenuItem name (ActionParam "openFile" actionOpenFile (toString id)) Nothing \\ (DBRef id, name) <- recOpenedFiles]
						
ActionReplace	:== Action "replace" "replace"
ActionStats		:== Action "stats" "stats"
recOpenedMenu	:== "recOpened"
actionOpenFile	:== "openFile"

newFile :: !AppStateRef !(MDICreateEditor EditorState) -> Task GAction
newFile aid createEditor =
								modifyDB aid (\(AppState num recOpened) -> AppState (inc num) recOpened)
	>>= \(AppState newNum _).	createEditor (EditorState (Note "") (NewFile newNum)) textEditorFile

openDialog :: !AppStateRef !(MDITasks EditorState a) -> Task GAction
openDialog gid mdiTasks =
				getAllFileNames
	>>= \files.	if (isEmpty files)
					(showMessage "Open File" "No files to open!" GContinue)
					(										enterChoiceA "Open file" "Open File" buttons files
						>>= \(action,(name, Hidden fid)).	case action of
					 										ActionOk	=	open fid mdiTasks (Just gid)
					 										_			=	continue
					)
where
	buttons = [(ActionCancel, always, AsButton), (ActionOk, ifvalid, AsButton)]
					
open :: !(DBRef TextFile) !(MDITasks EditorState a) !(Maybe AppStateRef) -> Task GAction
open fid {createEditor, existsEditor} mbGid =
				existsEditor isEditingOpenendFile
	>>= \mbEid.	case mbEid of
		Nothing =
						getFile fid
			>>= \file.	case mbGid of // determine if to add to list of recently opened files
							Nothing = stop
							Just gid =
									modifyDB gid (\(AppState n files) -> AppState n (take 5 [(fid, file.TextFile.name):files]))
								>>| stop
			>>|			return (GExtend [editor file])
		Just (DBId eid) = return (GFocus eid)
where
	isEditingOpenendFile :: !EditorState -> Bool
	isEditingOpenendFile (EditorState _ file) =	case file of
		NewFile _		= False
		OpenedFile file	= (fid == file.fileId)
	
	editor :: !TextFile -> Task GAction					
	editor file = createEditor (EditorState file.TextFile.content (OpenedFile file)) textEditorFile  <<@ GBFloating

about :: Task GAction
about = showMessage "About" "iTextEditor July 2010" GContinue

quit :: !(MDIIterateEditors EditorState Bool) -> Task GAction	
quit iterateEditors =
					iterateEditors False checkForUnsavedData
	>>= \cancel.	if cancel continue stopGroup
where
	checkForUnsavedData :: !Bool !EditorStateRef -> Task Bool
	checkForUnsavedData True editor		= return True
	checkForUnsavedData False editor	= requestClosingFile editor
	
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
		updateShared "Text Editor" "You can edit the text." [] eid [titleListener, mainEditor] <<@ Tag eid
	>>|	continue
where		
	mainEditor = editor	{ editorFrom	= \(EditorState cont _)			-> cont
						, editorTo		= \newCont (EditorState _ file)	-> EditorState newCont file
						}
						
save :: !EditorStateRef -> Task GAction
save eid =
					readDB eid
	>>= \editor.	case editor of
						EditorState txt (OpenedFile file) =
										setFileContent txt file
							>>= \file.	writeDB eid (EditorState file.TextFile.content (OpenedFile file))
							>>|			continue
						_ = saveAs eid

saveAs :: !EditorStateRef -> Task GAction
saveAs eid =
						enterInformationA "Save as" "Save As: enter name" buttons <<@ NoMenus
	>>= \(action,name).	case action of
							ActionOk =
																readDB eid
								>>= \(EditorState txt _).		storeFile name txt
								>>= \file=:{TextFile|content}.	writeDB eid (EditorState content (OpenedFile file))
								>>|								continue
							_ = continue
where
	buttons = [(ActionCancel, always, AsButton),(ActionOk, ifvalid, AsButton)]
							
:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}

replaceT :: !EditorStateRef -> Task GAction
replaceT eid = replaceT` {searchFor = "", replaceWith = ""}
where
	replaceT` :: !Replace -> Task GAction
	replaceT` repl =
								updateInformationA "Replace" "Replace" buttons repl <<@ NoMenus
		>>= \(action, repl).	case action of
									ActionReplaceAll =
											modifyDB eid (dbReplaceFunc repl)
										>>|	replaceT` repl
									_ = continue
									
	buttons = [(ActionClose, always, AsButton), (ActionReplaceAll, ifvalid, AsButton)]
	
	dbReplaceFunc repl (EditorState (Note txt) file) = EditorState (Note (replaceSubString repl.searchFor repl.replaceWith txt)) file
								
ActionReplaceAll :== Action "replace-all" "Replace all"
												
:: TextStatistics =	{ lines			:: Int
					, words			:: Int
					, characters	:: Int
					}

statistics :: !EditorStateRef  -> Task GAction
statistics eid = 
		updateShared "Statistics" "Statistics of your document" [(ActionOk, always, AsButton)] eid [titleListener, statsListener] <<@ NoMenus
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
										(					showMessageAboutA "Save changes" "Save changes?" buttons (question file) <<@ NoMenus
											>>= \action.	case action of
																(ActionCancel,_)	= return True
																(ActionNo,_)		= return False
																(ActionYes,_)		= save eid >>| return False
										)
										(return False)
where
	buttons = [(ActionCancel, always, AsButton), (ActionNo, always, AsButton), (ActionYes, always, AsButton)]
	question file = "Save changes to '" +++ getFileName file +++ "'?"
	
// global application state
:: AppState = AppState !Int ![(!(DBRef TextFile), !String)]
:: AppStateRef :== DBId AppState
:: EditorState = EditorState !Note !EditorFile
:: EditorFile = NewFile !Int | OpenedFile !TextFile
:: EditorStateRef :== DBId EditorState

// text files database
:: FileName :==	String
:: TextFile =	{ fileId	:: !(DBRef TextFile)
				, name		:: !FileName
				, content	:: !Note
				}

instance DB TextFile where
	databaseId			= mkDBId "TextFiles"
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
	
derive class iTask			AppState, EditorState, EditorFile, TextFile, TextStatistics, Replace
derive class SharedVariable	EditorState, EditorFile, TextFile
derive bimap				Maybe, (,)