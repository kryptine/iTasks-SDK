implementation module textEditor

import iTasks, StdMisc, Text

textEditor :: [Workflow]
textEditor = [workflow "Examples/Miscellaneous/Text Editor" "A simple text editor, demonstrating the 'application'-like capabilities of iTasks" (textEditorApp <<@ Subject "Text Editor")]

// global workflows
textEditorApp :: Task Void
textEditorApp = mdiApplication (AppState 0 []) groupActions actionGenFunc menuGenFunc
where
	groupActions = [(ActionNew, Always), (ActionOpen, Always), (Action OpenFile "", Always), (ActionAbout, Always), (ActionQuit, Always)]
	actionGenFunc gid mdiTasks=:{createEditor, iterateEditors} (action, data) = case action of
		ActionNew			= GExtend [newFile gid createEditor]
		ActionOpen			= GExtend [openDialog gid mdiTasks <<@ Floating]
		Action OpenFile _	= GExtend [open (DBRef (toInt data)) mdiTasks Nothing]
		ActionAbout			= GExtend [about <<@ Floating]
		ActionQuit			= GExtend [quit iterateEditors <<@ Modal]
		
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
						, MenuItem "Ok"				ActionOk		Nothing
						]
		, Menu "Edit"	[ MenuItem "Replace..."		(Action Replace "")	(hotkey R)
						]
		, Menu "Tools"	[ MenuItem "Statistics..."	(Action Stats "")		(hotkey T)
						]
		, Menu "Help"	[ MenuItem "About"			ActionAbout		Nothing
						]
		]
	where
		recentlyOpenedMenu = [MenuItem name (Action OpenFile "Open File"/*(toString id)*/) Nothing \\ (DBRef id, name) <- recOpenedFiles]
						
Replace			:== "replace"
Stats			:== "stats"
RecOpenedMenu	:== "recOpened"
OpenFile		:== "openFile"

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
						>>= \(action,(name, Hidden fid)).	case fst action of
					 										ActionOk	=	open fid mdiTasks (Just gid)
					 										_			=	continue
					)
where
	buttons = [(ActionCancel, always), (ActionOk, ifvalid)]
					
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
		Just eid = return (GFocus (Tag eid))
where
	isEditingOpenendFile :: !EditorState -> Bool
	isEditingOpenendFile (EditorState _ file) =	case file of
		NewFile _		= False
		OpenedFile file	= (fid == file.fileId)
	
	editor :: !TextFile -> Task GAction					
	editor file = createEditor (EditorState file.TextFile.content (OpenedFile file)) textEditorFile  <<@ Floating

about :: Task GAction
about = showMessageA "About" "iTextEditor July 2010" [(ActionOk,always)] GContinue >>= transform snd

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
textEditorFile eid = dynamicGroupA [editorWindow eid <<@ Floating] actions actionsGenFunc
where
	actions =	[ (ActionSave, SharedPredicate eid noNewFile), (ActionSaveAs, Always)
				, (Action Replace "Replace", SharedPredicate eid contNotEmpty)
				, (Action Stats "Statistics", Always), (ActionClose, Always)
				]
	actionsGenFunc (action, _) = case action of
		ActionSave			= GExtend [save eid]
		ActionSaveAs		= GExtend [saveAs eid <<@ Modal]
		Action Replace _	= GExtend [replaceT eid  <<@ Floating]
		Action Stats _		= GExtend [statistics eid  <<@ Floating]
		ActionClose			= GExtend [close eid <<@ Modal]
	
	noNewFile :: !(Maybe EditorState) -> Bool					
	noNewFile Nothing = abort "editor state deleted"
	noNewFile (Just (EditorState _ file)) = case file of
		(OpenedFile _)	= True
		_				= False
	
	contNotEmpty :: !(Maybe EditorState) -> Bool
	contNotEmpty Nothing = abort "editor state deleted"
	contNotEmpty (Just (EditorState (Note cont) _)) = cont <> ""
	
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
	>>= \(action,name).	case fst action of
							ActionOk =
																readDB eid
								>>= \(EditorState txt _).		storeFile name txt
								>>= \file=:{TextFile|content}.	writeDB eid (EditorState content (OpenedFile file))
								>>|								continue
							_ = continue
where
	buttons = [(ActionCancel, always),(ActionOk, ifvalid)]
							
:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}

replaceT :: !EditorStateRef -> Task GAction
replaceT eid = replaceT` {searchFor = "", replaceWith = ""}
where
	replaceT` :: !Replace -> Task GAction
	replaceT` repl =
								updateInformationA "Replace" "Replace" buttons repl <<@ NoMenus
		>>= \(action, repl).	case fst action of
									ActionReplaceAll =
											modifyDB eid (dbReplaceFunc repl)
										>>|	replaceT` repl
									_ = continue
									
	buttons = [(ActionClose, always), (ActionReplaceAll, ifvalid)]
	
	dbReplaceFunc repl (EditorState (Note txt) file) = EditorState (Note (replaceSubString repl.searchFor repl.replaceWith txt)) file
								
ActionReplaceAll :== Action "replace-all" "Replace all"
												
:: TextStatistics =	{ lines			:: Int
					, words			:: Int
					, characters	:: Int
					}

statistics :: !EditorStateRef  -> Task GAction
statistics eid = 
		updateShared "Statistics" "Statistics of your document" [(ActionOk, always)] eid [titleListener, statsListener] <<@ NoMenus
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
											>>= \action.	case fst action of
																(ActionCancel,_)	= return True
																(ActionNo,_)		= return False
																(ActionYes,_)		= save eid >>| return False
										)
										(return False)
where
	buttons = [(ActionCancel, always), (ActionNo, always), (ActionYes, always)]
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