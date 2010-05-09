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
			
:: AppState = AppState Note (Maybe TextFile)

derive gPrint AppState, TextFile
derive gParse AppState, TextFile
derive gVisualize AppState, TextFile
derive gUpdate AppState, TextFile
derive gMerge AppState, TextFile
derive gMakeSharedCopy AppState, TextFile
derive gMakeLocalCopy AppState, TextFile

subtaskBehaviour = GBFloating

openFile :: (DBRef TextFile) (DBid AppState) -> Task Void
openFile id sid =
				getFile id
	>>= \file.	writeDB sid (AppState file.TextFile.content (Just file))
	>>|			stop
	
open :: (DBid AppState) -> Task Void
open sid =
				getAllFileNames
	>>= \files.	if (isEmpty files)
					(showMessageAbout "Open File" "No files to open!")
					(										enterChoiceA "Open File" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] files
						>>= \(action,(name, Hidden fid)).	case action of
					 										ActionOk	=				addToRecentlyOpened name fid
					 														>>|			openFile fid sid
					 										_			=				stop
					)

save :: (DBid AppState) -> Task Void
save sid =
										readDB sid
	>>= \(AppState ntxt (Just file)).	dbUpdateItem {TextFile| file & content = ntxt}
	>>= \file.							writeDB sid (AppState ntxt (Just file))
	>>|									stop
					
saveAs :: (DBid AppState) -> Task Void
saveAs sid =
						enterInformationA "Save As: enter name" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)]
	>>= \(action,name).	case action of
							ActionOk	=							readDB sid
											>>= \(AppState txt _).	storeFile name txt
											>>=	\file.				writeDB sid (AppState file.TextFile.content (Just file))
											>>|						stop
							_			=							stop

:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}
derive gPrint Replace
derive gParse Replace
derive gVisualize Replace
derive gUpdate Replace

ActionReplaceAll	:== ActionLabel "Replace All"

replaceT :: (DBid AppState) -> Task Void
replaceT sid = replaceT` {searchFor = "", replaceWith = ""}
where
	replaceT` repl =
							updateInformationA "Replace" [ButtonAction (ActionClose, Always), ButtonAction (ActionReplaceAll, IfValid)] repl
		>>= \(action, v).	case action of
								ActionReplaceAll	=										readDB sid
														>>= \(AppState (Note txt) file).	writeDB sid (AppState (Note (replaceSubString v.searchFor v.replaceWith txt)) file)
														>>|									replaceT` v
								_					= 										stop

:: TextStatistics =	{ lines			:: Int
					, words			:: Int
					, characters	:: Int
					}
derive gPrint TextStatistics
derive gParse TextStatistics
derive gVisualize TextStatistics
derive gUpdate TextStatistics

statistics :: (DBid AppState)  -> Task Void
statistics sid = ignoreResult (updateShared "Statistics" [ButtonAction (ActionOk, Always)] sid [statsListener])
where
	statsListener = listener {listenerFrom = \(AppState (Note text) _) -> let txt = trim text in {lines = length (split "\n" txt), words = length (split " " (replaceSubString "\n" " " txt)), characters = textSize txt}}

initState :: AppState
initState = AppState (Note "") Nothing

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

textEditorMain :: (DBid AppState) -> Task Void
textEditorMain sid  = ignoreResult (updateShared "Text Editor" [] sid [titleListener,mainEditor])
where
	titleListener = listener	{ listenerFrom = \(AppState _ file) ->  case file of
																			Nothing		= "New Text Document"
																			Just f		= f.TextFile.name
								}
	mainEditor = editor	{ editorFrom	= \(AppState txt _) -> txt
						, editorTo		= \ntxt (AppState _ file) -> AppState ntxt file
						}

textEditorApp :: Task Void
textEditorApp =
				createDB initState
	>>= \sid.	dynamicGroupAOnly [textEditorMain sid <<@ GBFixed] (groupActions sid)
	>>|			deleteDB sid
where
	groupActions sid =	[ GroupAction		ActionNew		(GOExtend [ignoreResult (writeDB sid initState)])								GroupAlways
						, GroupAction		ActionOpen		(GOExtend [open sid <<@ GBModal])												GroupAlways
						, GroupActionParam	actionOpenFile	(\fid -> GOExtend [openFile (DBRef (toInt fid)) sid])							GroupAlways
						, GroupAction		ActionSave		(GOExtend [save sid])															(SharedPredicate sid (\(SharedValue (AppState _ file)) -> isJust file))
						, GroupAction		ActionSaveAs	(GOExtend [saveAs sid <<@ GBModal])												GroupAlways
						, GroupAction		ActionReplace	(GOExtend [replaceT sid <<@ subtaskBehaviour])									(SharedPredicate sid (\(SharedValue (AppState (Note txt) _)) -> txt <> ""))
						, GroupAction		ActionStats		(GOExtend [statistics sid <<@ subtaskBehaviour])								GroupAlways
						, GroupAction		ActionShowAbout	(GOExtend [showMessageAbout "About" "iTextEditor V0.01" <<@ subtaskBehaviour])	GroupAlways
						, GroupAction		ActionQuit		GOStop																			GroupAlways
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
					, MenuItem "Quit"			ActionQuit
					]
	, Menu "Edit"	[ MenuItem "Replace..."		ActionReplace ]
	, Menu "Tools"	[ MenuItem "Statistics..."	ActionStats ]
	, Menu "Help"	[ MenuItem "About"			ActionShowAbout ]
	]

textEditor :: [Workflow]
textEditor = [workflow "Examples/Miscellaneous/Text Editor" (initTextEditor >>| textEditorApp)]