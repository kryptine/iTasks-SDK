implementation module textEditor

import iTasks, CommonDomain, StdMisc, Text

derive bimap Maybe, (,)

:: FileName :==	String
:: TextFile =	{ fileId	:: !(DBRef TextFile)
				, name		:: FileName
				, content	:: Note
				}
derive gPrint TextFile
derive gParse TextFile
derive gVisualize TextFile
derive gUpdate TextFile

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
					
getAllFileNames :: Task [((DBRef TextFile), FileName)]
getAllFileNames =
				dbReadAll
	>>= \files.	return (map (\f -> (f.fileId, f.TextFile.name)) files)
			
:: AppState = AppState Note (Maybe TextFile)
:: AppAction = AppAction (PAction (Task AppAction)) // AppAction (ParallelAction AppAction)

derive gPrint AppState, AppAction, PAction
derive gParse AppState, AppAction, PAction
derive gVisualize AppState, AppAction, PAction
derive gUpdate AppState, AppAction, PAction
derive gMerge AppState, TextFile
derive gMakeSharedCopy AppState, TextFile
derive gMakeLocalCopy AppState, TextFile

openFile :: (DBRef TextFile) (DBid AppState) -> Task Void
openFile id sid =
				getFile id
	>>= \file.	writeDB sid (AppState file.TextFile.content (Just file))
	>>|			stop
	
open :: (DBid AppState) -> Task AppAction
open sid =
				getAllFileNames
	>>= \files.	if (isEmpty files)
					(		showMessage "No files to open!"
					 >>|	return (AppAction Continue)
					)
					(							enterChoiceA "Open File" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] files
					 >>= \(action,(fid,name)).	case action of
					 								ActionOk	=				addToRecentlyOpened name fid
					 												>>|			openFile fid sid
					 												>>|			return (AppAction Continue)
					 								_			=				return (AppAction Continue)
					)

save :: (DBid AppState) -> Task Void
save sid =
										readDB sid
	>>= \(AppState ntxt (Just file)).	dbUpdateItem {TextFile| file & content = ntxt}
	>>= \file.							writeDB sid (AppState ntxt (Just file))
	>>|									stop
					
saveAs :: (DBid AppState) -> Task AppAction
saveAs sid =
						enterInformationA "Save As: enter name" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)]
	>>= \(action,name).	case action of
							ActionOk	=							readDB sid
											>>= \(AppState txt _).	storeFile name txt
											>>=	\file.				writeDB sid (AppState file.TextFile.content (Just file))
											>>|						return (AppAction Continue)
							_			=							return (AppAction Continue)

:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}
derive gPrint Replace
derive gParse Replace
derive gVisualize Replace
derive gUpdate Replace

ActionReplaceAll	:== ActionLabel "Replace All"
ActionClose			:== ActionLabel "Close"

replaceT :: (DBid AppState) -> Task AppAction
replaceT sid =
						enterInformationA "Replace..." [ButtonAction (ActionClose, Always), ButtonAction (ActionReplaceAll, IfValid)]
	>>= \(action, v).	case action of
							ActionReplaceAll	=										readDB sid
													>>= \(AppState (Note txt) file).	writeDB sid (AppState (Note (replaceSubString v.searchFor v.replaceWith txt)) file)
													>>|									return (AppAction (Extend [replaceT sid]))
							_					= 										return (AppAction Continue)

:: TextStatistics =	{ lines			:: Int
					, words			:: Int
					, characters	:: Int
					}
derive gPrint TextStatistics
derive gParse TextStatistics
derive gVisualize TextStatistics
derive gUpdate TextStatistics

statistics :: (DBid AppState)  -> Task AppAction
statistics sid =
		updateShared "Statistics" [ButtonAction (ActionOk, Always)] sid [statsListener]
	>>| return (AppAction Continue)
where
	statsListener = listener {listenerFrom = \(AppState (Note txt) _) -> {lines = length (split "\n" txt), words = length (split " " (replaceSubString "\n" " " txt)), characters = textSize txt}}

about :: Task AppAction
about =
		showMessage "iTextEditor V0.01"
	>>|	return (AppAction Continue)

initState :: AppState
initState = AppState (Note "") Nothing

addToRecentlyOpened :: String (DBRef TextFile) -> Task Void
addToRecentlyOpened name (DBRef id) =
				getMenuItem "recOpened"
	>>= \item.	case item of
					Just (SubMenu label entries)	= setMenuItem "recOpened" (SubMenu label (take 5[MenuItem name (ActionParam "openFile" (toString id)):entries]))
					_								= return Void

ActionReplace	:== ActionLabel "replace"
ActionStats		:== ActionLabel "stats"

textEditorMain :: (DBid AppState) -> Task AppAction
textEditorMain sid  =
						updateShared "Text Editor" [MenuParamAction ("openFile", Always):(map MenuAction actions)] sid [titleListener,mainEditor]
	>>= \(action, _).	case action of
							ActionNew					= writeDB sid initState >>|				return (AppAction (Extend [textEditorMain sid]))
							ActionOpen					=										return (AppAction (Extend [textEditorMain sid, open sid]))
							ActionParam "openFile" fid	= openFile (DBRef (toInt fid)) sid >>|	return (AppAction (Extend [textEditorMain sid]))
							ActionSave					= save sid >>|							return (AppAction (Extend [textEditorMain sid]))
							ActionSaveAs				=										return (AppAction (Extend [textEditorMain sid, saveAs sid]))
							ActionReplace				= 										return (AppAction (Extend [textEditorMain sid, replaceT sid]))
							ActionStats					=										return (AppAction (Extend [textEditorMain sid, statistics sid]))
							ActionShowAbout				= 										return (AppAction (Extend [textEditorMain sid, about]))
							_							=										return (AppAction Stop)
where
	actions =	[ (ActionNew,		Always)
				, (ActionOpen,		Always)
				, (ActionSave,		(Predicate (\(Valid (AppState _ file)) -> isJust file)))
				, (ActionSaveAs,	Always)
				, (ActionQuit,		Always)
				, (ActionReplace,	(Predicate (\(Valid (AppState (Note txt) _)) -> txt <> "")))
				, (ActionStats,		Always)
				, (ActionShowAbout,	Always)
				]
	titleListener = listener {listenerFrom = \(AppState _ file) -> mkTitle file}
	mainEditor = editor	{ editorFrom	= \(AppState txt _) -> txt
						, editorTo		= \ntxt (AppState _ file) -> AppState ntxt file
						}
	mkTitle file = case file of
		Nothing		= "New Text Document"
		(Just f)	= f.TextFile.name

textEditorApp :: Task Void
textEditorApp =
				createDB initState
	>>= \sid.	group "TextEditor" "" (\(AppAction action,_) _ -> (Void,action)) id Void [textEditorMain sid]
			
initTextEditor :: Task Void
initTextEditor = setMenus
	[ Menu "File"	[ MenuItem "New"			ActionNew
					, MenuItem "Open..."		ActionOpen
					, MenuName "recOpened"		(SubMenu "Recently Opened" [])
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