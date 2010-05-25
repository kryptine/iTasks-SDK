implementation module textEditor

import iTasks, CommonDomain, StdMisc, Text

derive bimap Maybe, (,)

:: FileName :==	String
:: TextFile =	{ fileId	:: !(DBRef TextFile)
				, name		:: FileName
				, content	:: Note
				}

instance DB TextFile where
	databaseId			= mkDBid "TextFiles"
	getItemId file		= file.fileId
	setItemId id file	= {file & fileId = id}
	
storeFile :: FileName Note -> Task TextFile
storeFile name txt =
				getDefaultValue
	>>= \file.	dbCreateItem {TextFile| file & name = name, content = txt}
	>>= \file.	return file
	
getFile :: (DBRef TextFile) -> Task TextFile
getFile id =
				dbReadItem id
	>>= \res.	case res of
					Nothing		= undef
					(Just file)	= return file
					
getAllFileNames :: Task [(FileName, Hidden (DBRef TextFile))]
getAllFileNames =
				dbReadAll
	>>= \files.	return (map (\f -> (f.TextFile.name, Hidden f.fileId)) files)
			
:: EditorState = EditorState Note (Maybe TextFile)

derive gPrint EditorState, TextFile
derive gParse EditorState, TextFile
derive gVisualize EditorState, TextFile
derive gUpdate EditorState, TextFile
derive gMerge EditorState, TextFile
derive gMakeSharedCopy EditorState, TextFile
derive gMakeLocalCopy EditorState, TextFile
derive gError EditorState, TextFile
derive gHint EditorState, TextFile

openFile :: !(DBRef TextFile) -> Task EditorState
openFile id =
				getFile id
	>>= \file.	return (EditorState file.TextFile.content (Just file))
	
open :: Task GAction
open =
				getAllFileNames
	>>= \files.	if (isEmpty files)
					(showMessageAbout "Open File" "No files to open!" >>| return GContinue)
					(										enterChoiceA "Open File" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] files
						>>= \(action,(name, Hidden fid)).	case action of
					 										ActionOk	=				addToRecentlyOpened name fid
					 														>>|			return (GExtend [textEditorFile (Just fid) <<@ GBFloating])
					 										_			=				return GContinue
					)

save :: (DBid EditorState) -> Task Void
save sid =
										readDB sid
	>>= \(EditorState ntxt (Just file)).	dbUpdateItem {TextFile| file & content = ntxt}
	>>= \file.							writeDB sid (EditorState ntxt (Just file))
	>>|									stop
					
saveAs :: (DBid EditorState) -> Task Void
saveAs sid =
						enterInformationA "Save As: enter name" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)]
	>>= \(action,name).	case action of
							ActionOk	=							readDB sid
											>>= \(EditorState txt _).	storeFile name txt
											>>=	\file.				writeDB sid (EditorState file.TextFile.content (Just file))
											>>|						stop
							_			=							stop

:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}
derive gPrint Replace
derive gParse Replace
derive gVisualize Replace
derive gUpdate Replace
derive gError Replace
derive gHint Replace

ActionReplaceAll	:== ActionLabel "Replace All"

replaceT :: (DBid EditorState) -> Task GAction
replaceT sid = replaceT` {searchFor = "", replaceWith = ""}
where
	replaceT` repl =
							updateInformationA "Replace" [ButtonAction (ActionClose, Always), ButtonAction (ActionReplaceAll, IfValid)] repl
		>>= \(action, v).	case action of
								ActionReplaceAll	=										readDB sid
														>>= \(EditorState (Note txt) file).	writeDB sid (EditorState (Note (replaceSubString v.searchFor v.replaceWith txt)) file)
														>>|									replaceT` v
								_					= 										return GContinue

:: TextStatistics =	{ lines			:: Int
					, words			:: Int
					, characters	:: Int
					}
derive gPrint TextStatistics
derive gParse TextStatistics
derive gVisualize TextStatistics
derive gUpdate TextStatistics
derive gError TextStatistics
derive gHint TextStatistics

statistics :: (DBid EditorState)  -> Task GAction
statistics sid =
		updateShared "Statistics" [ButtonAction (ActionOk, Always)] sid [titleListener, statsListener]
	>>|	return GContinue
where
	statsListener = listener {listenerFrom = \(EditorState (Note text) _) -> let txt = trim text in {lines = length (split "\n" txt), words = length (split " " (replaceSubString "\n" " " txt)), characters = textSize txt}}

initState :: EditorState
initState = EditorState (Note "") Nothing

actionOpenFile = "openFile"
recOpenedMenu = "recOpened"

addToRecentlyOpened :: String (DBRef TextFile) -> Task Void
addToRecentlyOpened name (DBRef id) =
				getMenuItem recOpenedMenu
	>>= \item.	case item of
					Just (SubMenu label entries)	= setMenuItem recOpenedMenu (SubMenu label (take 5[MenuItem name (ActionParam actionOpenFile (toString id)):entries]))
					_								= return Void

ActionReplace	:== ActionLabel "replace"
ActionStats		:== ActionLabel "stats"

textEditorFile :: !(Maybe (DBRef TextFile)) -> Task GAction
textEditorFile mbFile  =
					case mbFile of
						Just file	= openFile file
						Nothing		= return initState
	>>= \state.		createDB state
	>>= \sid.		textEditorFile` sid
	>>=	\gAction.	return gAction//deleteDB sid
	//>>|				return gAction
where
	textEditorFile` sid =
							updateShared "Text Editor" actions sid [titleListener,mainEditor]
		>>= \(action,_).	case action of
								ActionSave		= save sid >>| textEditorFile` sid
								ActionSaveAs	= saveAs sid >>| textEditorFile` sid
								ActionReplace	= return (GExtend [textEditorFile` sid <<@ GBFloating, replaceT sid <<@ GBModal])
								ActionStats		= return (GExtend [textEditorFile` sid <<@ GBFloating, statistics sid <<@ GBAlwaysFloating])
								ActionClose		= return GContinue

	mainEditor = editor	{ editorFrom	= \(EditorState txt _) -> txt
						, editorTo		= \ntxt (EditorState _ file) -> EditorState ntxt file
						}
						
	actions =	[ MenuActionWithHotkey 	(ActionSave,	Predicate \v -> case v of Valid (EditorState _ (Just _)) = True; _ = False)			{keys = "s", ctrl = False, alt = True, shift = False}
				, MenuActionWithHotkey 	(ActionSaveAs,	Always)																				{keys = "a", ctrl = False, alt = True, shift = False}
				, MenuActionWithHotkey 	(ActionReplace,	Predicate \v -> case v of Valid (EditorState (Note txt) _) = txt <> ""; _ = False)	{keys = "r", ctrl = False, alt = True, shift = False}
				, MenuActionWithHotkey 	(ActionStats,	Always)																				{keys = "t", ctrl = False, alt = True, shift = False}
				, MenuActionWithHotkey 	(ActionClose,	Always)																				{keys = "c", ctrl = False, alt = True, shift = False}
				]
				
titleListener = listener	{ listenerFrom = \(EditorState _ file) -> case file of
								Nothing		= "New Text Document"
								Just f		= f.TextFile.name
							}

textEditorApp :: Task Void
textEditorApp = dynamicGroupA [] groupActions
where
	groupActions :: [GroupAction GAction Void Void]
	groupActions =	[ GroupAction		ActionNew		(GExtend [textEditorFile Nothing <<@ GBFloating])								GroupAlways
					, GroupAction		ActionOpen		(GExtend [open <<@ GBModal])												GroupAlways
					, GroupActionParam	actionOpenFile	(\fid -> GExtend [textEditorFile (Just (DBRef (toInt fid))) <<@ GBFloating])							GroupAlways
					, GroupAction		ActionShowAbout	(GExtend [showMessageAbout "About" "iTextEditor V0.01" <<@ GBAlwaysFloating >>| return GContinue])	GroupAlways
					, GroupAction		ActionQuit		GStop																			GroupAlways
					]
			
initTextEditor :: Task Void
initTextEditor = setMenus
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

textEditor :: [Workflow]
textEditor = [workflow "Examples/Miscellaneous/Text Editor" (initTextEditor >>| textEditorApp)]