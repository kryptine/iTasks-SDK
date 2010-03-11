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
	>>= \file.	dbCreateItem {file & name = name, content = txt}
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
derive gPrint AppState
derive gParse AppState
derive gVisualize AppState
derive gUpdate AppState

open :: AppState Note -> Task AppState
open st=:(AppState _ file) ntxt =
				getAllFileNames
	>>= \files.	if (isEmpty files)
					(		showMessage "No files to open!"
					 >>|	return (AppState ntxt file)
					)
					(							enterChoiceA "Open File" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] files
					 >>= \(action,(id,name)).	case action of
					 								ActionOk	=		addToRecentlyOpened name id
					 												>>|	openFile id st ntxt
					 								_			=		return (AppState ntxt file)
					)

openFile :: (DBRef TextFile) AppState Note -> Task AppState
openFile id _ _ =
				getFile id
	>>= \file.	return (AppState file.content (Just file))

save :: AppState Note -> Task AppState
save (AppState otxt (Just file)) ntxt =
				dbUpdateItem {file & content = ntxt}
	>>= \file.	return (AppState ntxt (Just file))
save st txt = saveAs st txt
					
saveAs :: AppState Note -> Task AppState
saveAs (AppState otxt file) ntxt =
						enterInformationA "Save As: enter name" [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)]
	>>= \(action,name).	case action of
							ActionOk	=				storeFile name ntxt
											>>=	\file.	return (AppState file.content (Just file))
							_			=				return (AppState ntxt file)

:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}
derive gPrint Replace
derive gParse Replace
derive gVisualize Replace
derive gUpdate Replace

replaceT :: AppState Note -> Task AppState
replaceT (AppState _ file) (Note txt) =
						enterInformationA "Replace..." [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)]
	>>= \(action, v).	case action of
							ActionOk	= return (AppState (Note (replaceSubString v.searchFor v.replaceWith txt)) file)
							_			= return (AppState (Note txt) file)

about :: Task Void
about = showMessage "iTextEditor V0.01"

initState :: Task AppState
initState = return (AppState (Note "") Nothing)

addToRecentlyOpened :: String (DBRef TextFile) -> Task Void
addToRecentlyOpened name (DBRef id) =
				getMenuItem "recOpened"
	>>= \item.	case item of
					Just (SubMenu label entries)	= setMenuItem "recOpened" (SubMenu label (take 5[MenuItem name (ActionParam "openFile" (toString id)):entries]))
					_								= return Void

textEditorApp :: Task Void
textEditorApp = initState >>= textEditor` 
where
	textEditor` st=:(AppState txt file) =
								updateInformationA title [MenuParamAction ("openFile", Always):(map MenuAction actions)] txt
		>>= \(action, ntxt).	case action of
									ActionNew					= initState										>>= textEditor`
									ActionOpen					= open st ntxt									>>= textEditor`
									ActionParam "openFile" fid	= openFile (DBRef (toInt fid)) st ntxt			>>= textEditor`
									ActionSave					= save st ntxt									>>= textEditor`
									ActionSaveAs				= saveAs st ntxt								>>= textEditor`
									ActionLabel "replace"		= replaceT st ntxt								>>= textEditor`
									ActionShowAbout				= about >>| return (AppState ntxt file)			>>= textEditor`
									_							= return Void
	where
		title = case file of
			Nothing		= "New Text Document"
			(Just f)	= f.TextFile.name
		actions =	[ (ActionNew,					Always)
					, (ActionOpen,					Always)
					, (ActionSave,					(Predicate (\_ -> isJust file)))
					, (ActionSaveAs,				Always)
					, (ActionQuit,					Always)
					, (ActionLabel "replace",		(Predicate (\v -> case v of Invalid = False; Valid (Note v) = v <> "")))
					, (ActionShowAbout,				Always)
					]
			
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
	, Menu "Edit"	[ MenuItem "Replace..."		(ActionLabel "replace") ]
	, Menu "Help"	[ MenuItem "About"			ActionShowAbout ]
	]

textEditor :: [Workflow]
textEditor = [{ name = "Examples/Miscellaneous/Text Editor"
		, label = "Text Editor"
		, roles = []
		, mainTask = initTextEditor >>| textEditorApp
		}]