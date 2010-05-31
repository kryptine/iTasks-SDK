implementation module textEditor

import iTasks, CommonDomain, StdMisc, Text, Map

textEditor :: [Workflow]
textEditor = [workflow "Examples/Miscellaneous/Text Editor" textEditorApp]

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
	>>|			createDB (AppState 0 1 newMap)
	>>= \sid.	dynamicGroupA [] (groupActions sid)
	>>|			deleteDB sid
where
	groupActions :: !AppStateRef -> [GroupAction GAction Void Void]
	groupActions sid =	[ GroupAction		ActionNew		(GExtend [textEditorFile Nothing sid]) 								GroupAlways
						, GroupAction		ActionOpen		(GExtend [openDialog sid <<@ GBFloating])							GroupAlways
						, GroupActionParam	actionOpenFile	(\fid -> GExtend [open (DBRef (toInt fid)) sid Nothing])			GroupAlways
						, GroupAction		ActionShowAbout	(GExtend [about <<@ GBAlwaysFloating])								GroupAlways
						, GroupAction		ActionQuit		(GExtend [quit sid <<@ GBModal])									GroupAlways
						]
						
ActionReplace	:== ActionLabel "replace"
ActionStats		:== ActionLabel "stats"
recOpenedMenu	:== "recOpened"
actionOpenFile	:== "openFile"

openDialog :: !AppStateRef -> Task GAction
openDialog sid =
				getAllFileNames
	>>= \files.	if (isEmpty files)
					(showMessageAbout "Open File" "No files to open!" <<@ ExcludeGroupActions >>| continue)
					(										enterChoiceA "Open File" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] files <<@ ExcludeGroupActions
						>>= \(action,(name, Hidden fid)).	case action of
					 										ActionOk	=	open fid sid (Just name)
					 										_			=	continue
					)
		
open :: !(DBRef TextFile) !AppStateRef !(Maybe String) -> Task GAction
open fid sid mbAddToRecOpenedName =						
										readDB sid
	>>= \(AppState _ _ openedFiles).	if (isEmpty (filter (editorOf fid) (toList openedFiles)))
					 						(		case mbAddToRecOpenedName of
					 									Just name	= addToRecentlyOpened name fid
					 									Nothing		= return Void
					 							>>|	return (GExtend [textEditorFile (Just fid) sid <<@ GBFloating])
					 						)
					 						continue
where
	editorOf :: !(DBRef TextFile) !(!EditorId, !EditorState) -> Bool
	editorOf fid (_, EditorState _ file) = case file of
		NewFile _		= False
		OpenedFile file	= fid == file.fileId
					
	addToRecentlyOpened :: !String !(DBRef TextFile) -> Task Void
	addToRecentlyOpened name (DBRef id) =
					getMenuItem recOpenedMenu
		>>= \item.	case item of
						Just (SubMenu label entries)	= setMenuItem recOpenedMenu (SubMenu label (take 5 [MenuItem name (ActionParam actionOpenFile (toString id)):entries]))
						_								= stop

about :: Task GAction
about =
		showMessageAbout "About" "iTextEditor May 2010" <<@ ExcludeGroupActions
	>>|	continue
	
quit :: !AppStateRef -> Task GAction
quit sid =
										readDB sid
	>>= \(AppState _ _ openedFiles).	closeAllFiles (toList openedFiles)
where
	closeAllFiles [] = return GStop
	closeAllFiles [(eid,_):openedFiles] =
						requestClosingFile eid sid
		>>= \cancel.	if cancel (return GContinue) (closeAllFiles openedFiles)
	
// editor workflows
textEditorFile :: !(Maybe (DBRef TextFile)) !AppStateRef -> Task GAction
textEditorFile mbFile sid = 
												readDB sid
	>>= \(AppState edIdx newIdx openedFiles).	case mbFile of
													Nothing		= return (edIdx, (AppState (inc edIdx) (inc newIdx) (put edIdx (EditorState (Note "") (NewFile newIdx)) openedFiles)))
													Just fid	=
																	getFile fid
														>>=	\file.	return (edIdx, (AppState (inc edIdx) newIdx (put edIdx (EditorState file.TextFile.content (OpenedFile file)) openedFiles)))
	>>= \(editorId, state).						writeDB sid state
	>>= \state.									dynamicGroupA [editorWindow editorId sid <<@ GBFloating] (actions editorId sid)
	>>|											readDB sid
	>>= \(AppState edIdx newIdx openedFiles).	writeDB sid (AppState edIdx newIdx (del editorId openedFiles))
	>>|											continue
where
	actions :: !EditorId !AppStateRef -> [GroupAction GAction Void AppState]
	actions eid sid =	[ GroupAction ActionSave	(GExtend [save eid sid])						(SharedPredicate sid (\v -> case v of SharedValue (AppState _ _ files) = let (Just (EditorState _ file)) = get eid files in case file of (OpenedFile _) = True; _ = False; _ = False))
						, GroupAction ActionSaveAs	(GExtend [saveAs eid sid <<@ GBModal])			GroupAlways
						, GroupAction ActionReplace	(GExtend [replaceT eid sid <<@ GBFloating])		(SharedPredicate sid (\v -> case v of SharedValue (AppState _ _ files) = let (Just (EditorState (Note txt) _)) = get eid files in txt <> ""; _ = False))
						, GroupAction ActionStats	(GExtend [statistics eid sid <<@ GBFloating])	GroupAlways
						, GroupAction ActionClose	(GExtend [close eid sid <<@ GBModal])			GroupAlways
						]

editorWindow :: !EditorId !AppStateRef -> Task GAction
editorWindow eid sid =
		updateShared "Text Editor" [] sid [titleListener eid, mainEditor]
	>>|	continue
where		
	mainEditor = editor	{ editorFrom	= \(AppState _ _ files)						-> let (Just (EditorState cont _)) = get eid files in cont
						, editorTo		= \newCont (AppState edIdx newIdx files)	-> let (Just (EditorState _ file)) = get eid files in AppState edIdx newIdx (put eid (EditorState newCont file) files)
						}
						
save :: !EditorId !AppStateRef -> Task GAction
save eid sid =
												getEditorState eid sid
	>>= \(EditorState txt (OpenedFile file)).	setFileContent txt file
	>>= \file.									setEditorState (EditorState file.TextFile.content (OpenedFile file)) eid sid
	>>|											continue

saveAs :: !EditorId !AppStateRef -> Task GAction
saveAs eid sid =
						enterInformationA "Save As: enter name" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] <<@ ExcludeGroupActions
	>>= \(action,name).	case action of
							ActionOk	=								getEditorState eid sid
											>>= \(EditorState txt _).	storeFile name txt
											>>= \file.					setEditorState (EditorState file.TextFile.content (OpenedFile file)) eid sid
											>>|							continue
							_			=								continue

:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}

replaceT :: !EditorId !AppStateRef -> Task GAction
replaceT eid sid = replace` {searchFor = "", replaceWith = ""}
where
	replace` repl =
							updateInformationA "Replace" [ButtonAction (ActionClose, Always), ButtonAction (ActionReplaceAll, IfValid)] repl <<@ ExcludeGroupActions
		>>= \(action, v).	case action of
								ActionReplaceAll	=										getEditorState eid sid
														>>= \(EditorState (Note txt) file).	setEditorState (EditorState (Note (replaceSubString v.searchFor v.replaceWith txt)) file) eid sid
														>>|									replace` v
								_					= 										continue
								
ActionReplaceAll :== ActionLabel "Replace all"
												
:: TextStatistics =	{ lines			:: Int
					, words			:: Int
					, characters	:: Int
					}

statistics :: !EditorId !AppStateRef  -> Task GAction
statistics eid sid = 
		updateShared "Statistics" [ButtonAction (ActionOk, Always)] sid [titleListener eid, statsListener] <<@ ExcludeGroupActions
	>>|	continue
where
	statsListener = listener {listenerFrom =  \(AppState _ _ files) ->
			let
				(Just (EditorState (Note text) _)) = get eid files
				txt = trim text
			in
				{lines = length (split "\n" txt), words = length (split " " (replaceSubString "\n" " " txt)), characters = textSize txt}
		}

titleListener :: !EditorId -> View AppState						
titleListener eid = listener	{ listenerFrom = \(AppState _ _ files) -> 
									let (Just st=:(EditorState _ file)) = get eid files in
									if (hasUnsavedData st) "*" "" 
									+++
									getFileName file
								}
								
close :: !EditorId !AppStateRef -> Task GAction
close eid sid =
					requestClosingFile eid sid
	>>= \cancel.	if cancel (return GContinue) (return GStop)

// helper functions
continue :: Task GAction
continue = return GContinue

getEditorState :: !EditorId !AppStateRef -> Task EditorState
getEditorState eid sid =
										readDB sid
	>>= \(AppState edIdx newIdx files).	return (get eid files)
	>>= \mbEditorState.					case mbEditorState of
											Just editorState	= return editorState
											Nothing				= throw "cannot get editor state"
											
setEditorState :: !EditorState !EditorId !AppStateRef -> Task Void
setEditorState state eid sid =
										readDB sid
	>>= \(AppState edIdx newIdx files).	writeDB sid (AppState edIdx newIdx (put eid state files))
	>>|									stop
	
hasUnsavedData :: !EditorState -> Bool
hasUnsavedData (EditorState cont file) = case file of
	NewFile _			= True
	OpenedFile file	= cont <> file.TextFile.content



requestClosingFile :: !EditorId !AppStateRef -> Task Bool
requestClosingFile eid sid =
											getEditorState eid sid
	>>= \state=:(EditorState cont file).	if (hasUnsavedData state)
												(					showMessageAboutA "Save changes?" [ButtonAction (ActionCancel, Always), ButtonAction (ActionNo, Always), ButtonAction (ActionYes, Always)] ("Save changes to '" +++ getFileName file +++ "'?") <<@ ExcludeGroupActions
													>>= \action.	case action of
																		ActionCancel	= return True
																		ActionNo		= return False
																		ActionYes =
																				case file of
																					NewFile _		= ignoreResult (saveAs eid sid)
																					OpenedFile file	= ignoreResult (save eid sid)
																			>>|	return False
												)
												(return False)

// global application state
:: EditorState = EditorState !Note !EditorFile
:: EditorFile = NewFile !Int | OpenedFile !TextFile
:: AppState = AppState !Int !Int !(Map EditorId EditorState)
:: EditorId :== Int
:: AppStateRef :== DBid AppState

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
setFileContent cont file = dbUpdateItem {TextFile | file & content = cont}
	
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
	
derive gPrint			AppState, EditorState, EditorFile, TextFile, Map, TextStatistics, Replace
derive gParse			AppState, EditorState, EditorFile, TextFile, Map, TextStatistics, Replace
derive gVisualize		AppState, EditorState, EditorFile, TextFile, Map, TextStatistics, Replace
derive gUpdate			AppState, EditorState, EditorFile, TextFile, Map, TextStatistics, Replace
derive gMerge			AppState, EditorState, EditorFile, TextFile, Map, TextStatistics, Replace
derive gMakeSharedCopy	AppState, EditorState, EditorFile, TextFile, Map
derive gMakeLocalCopy	AppState, EditorState, EditorFile, TextFile, Map
derive gError			AppState, EditorState, EditorFile, TextFile, Map, TextStatistics, Replace
derive gHint			AppState, EditorState, EditorFile, TextFile, Map, TextStatistics, Replace
derive bimap			Maybe, (,)