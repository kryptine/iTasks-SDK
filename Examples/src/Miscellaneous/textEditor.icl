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
					
getAllFileNames :: Task [((DBRef TextFile), FileName)]
getAllFileNames =
				dbReadAll
	>>= \files.	return (map (\f -> (f.fileId, f.TextFile.name)) files)
			
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
	
open :: (DBid AppState) -> Task GAction
open sid =
				getAllFileNames
	>>= \files.	if (isEmpty files)
					(		showMessage "No files to open!"
					 >>|	return GContinue
					)
					(							enterChoiceA "Open File" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] files
					 >>= \(action,(fid,name)).	case action of
					 								ActionOk	=				addToRecentlyOpened name fid
					 												>>|			openFile fid sid
					 												>>|			return GContinue
					 								_			=				return GContinue
					)

save :: (DBid AppState) -> Task Void
save sid =
										readDB sid
	>>= \(AppState ntxt (Just file)).	dbUpdateItem {TextFile| file & content = ntxt}
	>>= \file.							writeDB sid (AppState ntxt (Just file))
	>>|									stop
					
saveAs :: (DBid AppState) -> Task GAction
saveAs sid =
						enterInformationA "Save As: enter name" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)]
	>>= \(action,name).	case action of
							ActionOk	=							readDB sid
											>>= \(AppState txt _).	storeFile name txt
											>>=	\file.				writeDB sid (AppState file.TextFile.content (Just file))
											>>|						return GContinue
							_			=							return GContinue

:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}
derive gPrint Replace
derive gParse Replace
derive gVisualize Replace
derive gUpdate Replace

ActionReplaceAll	:== ActionLabel "Replace All"
ActionClose			:== ActionLabel "Close"

replaceT :: (DBid AppState) -> Task GAction
replaceT sid =
						enterInformationA "Replace..." [ButtonAction (ActionClose, Always), ButtonAction (ActionReplaceAll, IfValid)]
	>>= \(action, v).	case action of
							ActionReplaceAll	=										readDB sid
													>>= \(AppState (Note txt) file).	writeDB sid (AppState (Note (replaceSubString v.searchFor v.replaceWith txt)) file)
													>>|									replaceT sid <<@ subtaskBehaviour
							_					= 										return GContinue

:: TextStatistics =	{ lines			:: Int
					, words			:: Int
					, characters	:: Int
					}
derive gPrint TextStatistics
derive gParse TextStatistics
derive gVisualize TextStatistics
derive gUpdate TextStatistics

statistics :: (DBid AppState)  -> Task GAction
statistics sid =
		updateShared "Statistics" [ButtonAction (ActionOk, Always)] sid [statsListener]
	>>| return GContinue
where
	statsListener = listener {listenerFrom = \(AppState (Note txt) _) -> {lines = length (split "\n" txt), words = length (split " " (replaceSubString "\n" " " txt)), characters = textSize txt}}

about :: Task GAction
about =
		showMessage "iTextEditor V0.01"
	>>|	return GContinue

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

textEditorMain :: (DBid AppState) -> Task GAction
textEditorMain sid  =	GBFixed @>> (
						updateShared "Text Editor" [MenuParamAction ("openFile", Always):map MenuAction actions] sid [titleListener,mainEditor]
	>>= \(action, _).	case action of
							ActionNew					= writeDB sid initState >>|				return (GExtend [textEditorMain sid])
							ActionOpen					=										return (GExtend [textEditorMain sid, open sid <<@ GBModal])
							ActionParam "openFile" fid	= openFile (DBRef (toInt fid)) sid >>|	return (GExtend [textEditorMain sid])
							ActionSave					= save sid >>|							return (GExtend [textEditorMain sid])
							ActionSaveAs				=										return (GExtend [textEditorMain sid, saveAs sid <<@ GBModal])
							ActionReplace				= 										return (GExtend [textEditorMain sid, replaceT sid <<@ subtaskBehaviour])
							ActionStats					=										return (GExtend [textEditorMain sid, statistics sid <<@ subtaskBehaviour])
							ActionShowAbout				= 										return (GExtend [textEditorMain sid, about <<@ subtaskBehaviour])
							_							=										return GStop)
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
	>>= \sid.	dynamicGroup [textEditorMain sid]
	>>|			deleteDB sid
			
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