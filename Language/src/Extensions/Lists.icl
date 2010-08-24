implementation module Lists

import iTasks, CommonDomain

derive class iTask List, ListMeta, AnyList
derive bimap Maybe, (,)

instance DB ListMeta
where
	databaseId 					= mkDBid "Lists"
	getItemId l					= DBRef l.ListMeta.listId
	setItemId (DBRef listId) l	= {ListMeta| l & listId = listId}

manageLists :: Task Void
manageLists = stop

manageList :: AnyList -> Task Void
manageList list = stop

createList :: !String -> Task AnyList
createList name = getDefaultValue

getAllLists :: Task [AnyList]
getAllLists = return []

getMyLists :: Task [AnyList]
getMyLists = return []

deleteList :: !AnyList -> Task AnyList
deleteList list = return list

/*
:: ListDB :== [DBid ListDBItem]
:: ListDBItem = NoteList (List Note) | DateList (List Date) | DocList (List Document)

:: List a = 
	{ title			:: String
	, description	:: (Maybe Note)
	, owners		:: [User]
	, items			:: [ListItem a]
	}
	
:: ListItem a =
	{ title			:: (Maybe String)
	, item			:: a
	, flagged		:: Bool
	}

derive class iTask ListDBItem, List, ListItem
derive gMerge ListDBItem, List, ListItem

derive bimap Maybe, (,)

getListDB :: DBid ListDB
getListDB = mkDBid "ListDB"

newList :: Task Void
newList = readDB getListDB
	>>= \ldb -> 	getCurrentUser
	>>= \me -> 		enterChoice "Choose list" "What kind of list do you want to create?" ["Note","Date","Document"]
	>>= \ltype ->	initList ltype
	>>= \list ->	createDB list
	>>= \dbid ->	writeDB getListDB [dbid:ldb]
	>>| showMessage "Success" "List is succesfully created." Void
where
	initList :: String -> Task ListDBItem
	initList type = case type of
		"Note"
			= initListNote >>= \l -> return (NoteList l)
		"Date"
			= initListDate >>= \l -> return (DateList l)
		"Document"
			= initListDoc >>= \l -> return (DocList l)
		_ 
			= initListNote >>= \l -> return (NoteList l)
	
	initListNote = enterInformation "Note list" "Edit List"
	initListDate = enterInformation "Date list" "Edit List"
	initListDoc  = enterInformation "Document list" "Edit List"

editList :: Task Void
editList = getCurrentUser
	>>= \me -> selectList me
	>>= \listId  -> listEditor listId	

pushList :: Task Void
pushList = getCurrentUser
	>>= \me -> 	selectList me
	>>= \id -> 	enterInformation "User" "To whom do you want to push this list?"
	>>= \usr ->	enterMsg usr 
	>>= \msg -> usr @: ((showInstructionAbout ("Request to edit list from "+++toString me) "Press 'Done' to continue to the list editor" msg) >>| listEditor id)
where
	enterMsg :: User -> Task Note
	enterMsg user = enterInformation "Message" ("What would you like ask from "+++toString user+++"?")

selectList :: User -> Task (DBid (ListDBItem))
selectList user 
	# roles = getRoles user
	= readDB getListDB
		>>= \db -> sequence "Reading DB" [getListItem id \\ id <- db]
		>>= \items -> sequence "Get Info" [getListInfo item id \\ item <- items & id <- db | isMember user (getOwners item) || isMember "chair" roles]
		>>= \info -> enterChoice "Choose list" "Please select the list you wish to edit" info
		>>= \choice -> return (fromHidden (snd choice))	
where
	getListItem :: (DBid ListDBItem) -> Task ListDBItem
	getListItem id = readDB id
				
	getListInfo :: ListDBItem (DBid ListDBItem) -> Task (String,(Hidden (DBid ListDBItem)))
	getListInfo item id = return ((getTitle item)+++" - "+++(getShortDescription item),Hidden id)

listEditor :: (DBid ListDBItem) -> Task Void
listEditor id = readDB id 
	>>= \list -> listEditor` id list
	>>| return Void
where
	listEditor` :: (DBid ListDBItem) ListDBItem -> Task(Action, ListDBItem)
	listEditor` id list =
		case list of 
			(NoteList _) = updateShared "Edit list" [ButtonAction (ActionFinish,Always)] id [editor {editorFrom = editorFromNote, editorTo = editorToNote}]
			(DateList _) = updateShared "Edit list" [ButtonAction (ActionFinish,Always)] id [editor {editorFrom = editorFromDate, editorTo = editorToDate}]
			(DocList  _) = updateShared "Edit list" [ButtonAction (ActionFinish,Always)] id [editor {editorFrom = editorFromDoc, editorTo = editorToDoc}]
	where
		editorFromNote (NoteList l) = l
		editorToNote l _ = (NoteList l)
		
		editorFromDate (DateList l) = l
		editorToDate l _ = (DateList l)
		
		editorFromDoc (DocList l) = l
		editorToDoc l _ = (DocList l)

listItemEditor :: (DBid ListDBItem) Int -> Task Void
listItemEditor id index = readDB id
	>>= \list -> listItemEditor` id list index
	>>| return Void
where
	listItemEditor` :: (DBid ListDBItem) ListDBItem Int -> Task(Action, ListDBItem)
	listItemEditor` id list index =
		case list of
			(NoteList _) = updateShared "Edit list" [ButtonAction (ActionFinish,Always)] id [editor {editorFrom = editorFromNote, editorTo = editorToNote}]
			(DateList _) = updateShared "Edit list" [ButtonAction (ActionFinish,Always)] id [editor {editorFrom = editorFromDate, editorTo = editorToDate}]
			(DocList  _) = updateShared "Edit list" [ButtonAction (ActionFinish,Always)] id [editor {editorFrom = editorFromDoc, editorTo = editorToDoc}]
	where
		editorFromNote (NoteList l) = getItem index l
		editorToNote i (NoteList l) = (NoteList (updateItemAt index i l))
		
		editorFromDate (DateList l) = getItem index l
		editorToDate i (DateList l) = (DateList (updateItemAt index i l))
		
		editorFromDoc (DocList l) = getItem index l
		editorToDoc i (DocList l) = (DocList (updateItemAt index i l))
		
		getItem :: !Int !(List a) -> (ListItem a)
		getItem index list = list.List.items !! index
		
		updateItemAt :: !Int !(ListItem a) !(List a) -> (List a)
		updateItemAt index item list 
			# items = list.List.items
			# items = updateAt index item items
			= {List | list & items = items}
		
//Utility
getOwners :: ListDBItem -> [User]
getOwners item
	= case item of
		(NoteList list) = list.List.owners
		(DateList list) = list.List.owners
		(DocList  list) = list.List.owners

getTitle :: ListDBItem -> String
getTitle item
	= case item of
		(NoteList list) = list.List.title
		(DateList list) = list.List.title
		(DocList  list) = list.List.title

getShortDescription :: ListDBItem -> String	
getShortDescription item
	# mbDesc = case item of
		(NoteList list) = list.List.description
		(DateList list) = list.List.description
		(DocList  list) = list.List.description
	= case mbDesc of
		Just (Note d) = if(size d > 27) ((d % (0,27))+++"...") d
		Nothing = "..."
*/