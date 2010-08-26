implementation module Lists

import iTasks, CommonDomain, Groups

derive class iTask	List, ListMeta, ListDescription, AnyList
derive gMerge		List, ListMeta, ListDescription, AnyList
derive bimap Maybe, (,)

//Internal administration of who owns lists and who they are shared with.
:: ListMeta	=
	{ listId		:: !Int
	, owner			:: !User
	, sharedWith	:: ![User]
	}

//Data type for form name & description
:: ListDescription =
	{ name			:: !String
	, description	:: !Maybe Note
	}

instance DB ListMeta
where
	databaseId 					= mkDBId "Lists"
	getItemId l					= DBRef l.ListMeta.listId
	setItemId (DBRef listId) l	= {ListMeta| l & listId = listId}

manageLists :: Task Void
manageLists 
	=	Subject "Manage lists" @>>
	(	getMyLists
	>>=	overview
	>>= \(action,list) -> case action of
		ActionNew				= newList >>= manageList	>>| return False
		ActionOpen				= manageList list			>>| return False
		ActionLabel	"Delete"	= delList list				>>| return False
		ActionQuit				=								return True
	) <! id
	>>| stop
where
	overview []		= getDefaultValue >>= showMessageA "My lists" "You have no lists." [aNew,aQuit]
	overview list	= enterChoiceA "My lists" "Select a list..." [aOpen,aDelete,aNew,aQuit] list
	
	aOpen 			= ButtonAction (ActionOpen, IfValid)
	aNew			= ButtonAction (ActionNew, Always)
	aQuit			= ButtonAction (ActionQuit, Always)
	aDelete			= ButtonAction (ActionLabel "Delete", Always)
	
	newList			=	enterChoice "List type" "What type of list do you want to create?"
						["Simple list", "Todo list", "Date list","Document list"]
					>>= \type ->
						enterInformation "Name" "Please enter a name, and if you like, a description for the list"
					>>= \desc ->
						createList type desc.ListDescription.name desc.ListDescription.description
	
	delList	list	=	requestConfirmation "Delete list" ("Are you sure you want to delete '" +++ nameOf list +++ "'?")
					>>= \confirm -> if confirm
						(deleteList list)
						(return list)
									
manageList :: AnyList -> Task Void
manageList list
	=	
	(	showItems list
	>>= \(action,_) -> case action of
		ActionLabel "Edit"	= editItems	list			>>| return False
		ActionLabel "Share"	= manageListSharing list	>>| return False
		ActionClose			=								return True
	) <! id
	>>| stop
where
	showItems l = case l of
		(SimpleList l)	= updateShared l.List.name l.List.description [ButtonAction (ActionClose,Always),ButtonAction (ActionLabel "Edit", Always),ButtonAction (ActionLabel "Share", Always)] (mkDBId ("List-" <+++ (fromHidden l.List.listId))) [listener {listenerFrom = simpleFrom}]
		(TodoList l)	= updateShared l.List.name l.List.description [ButtonAction (ActionClose,Always),ButtonAction (ActionLabel "Edit", Always),ButtonAction (ActionLabel "Share", Always)] (mkDBId ("List-" <+++ (fromHidden l.List.listId))) [listener {listenerFrom = todoFrom}]
		(DateList l)	= updateShared l.List.name l.List.description [ButtonAction (ActionClose,Always),ButtonAction (ActionLabel "Edit", Always),ButtonAction (ActionLabel "Share", Always)] (mkDBId ("List-" <+++ (fromHidden l.List.listId))) [listener {listenerFrom = dateFrom}]
		(DocumentList l)= updateShared l.List.name l.List.description [ButtonAction (ActionClose,Always),ButtonAction (ActionLabel "Edit", Always),ButtonAction (ActionLabel "Share", Always)] (mkDBId ("List-" <+++ (fromHidden l.List.listId))) [listener {listenerFrom = documentFrom}]

	editItems list = case list of
		(SimpleList l)	= updateShared l.List.name l.List.description [ButtonAction (ActionFinish,Always)] (mkDBId ("List-" <+++ (fromHidden l.List.listId))) [editor {editorFrom = simpleFrom, editorTo = simpleTo}]
		(TodoList l)	= updateShared l.List.name l.List.description [ButtonAction (ActionFinish,Always)] (mkDBId ("List-" <+++ (fromHidden l.List.listId))) [editor {editorFrom = todoFrom, editorTo = todoTo}]
		(DateList l)	= updateShared l.List.name l.List.description [ButtonAction (ActionFinish,Always)] (mkDBId ("List-" <+++ (fromHidden l.List.listId))) [editor {editorFrom = dateFrom, editorTo = dateTo}]
		(DocumentList l)= updateShared l.List.name l.List.description [ButtonAction (ActionFinish,Always)] (mkDBId ("List-" <+++ (fromHidden l.List.listId))) [editor {editorFrom = documentFrom, editorTo = documentTo}]

	simpleFrom (SimpleList l) 		= l.List.items
	simpleTo i (SimpleList l)		= SimpleList {List|l & items = i}
	
	todoFrom (TodoList l)			= l.List.items
	todoTo i (TodoList l)			= TodoList {List|l & items = i}
	
	dateFrom (DateList l)			= l.List.items
	dateTo i (DateList l)			= DateList {List|l & items = i}

	documentFrom (DocumentList l)	= l.List.items
	documentTo i (DocumentList l)	= DocumentList {List|l & items = i}


manageListSharing :: AnyList -> Task Void
manageListSharing list
	=
	(	dbReadItem (DBRef (listIdOf list))
	>>= \mbMeta -> case mbMeta of
		Nothing		= throw "Could not find list meta data"
		Just meta
			= (case meta.ListMeta.sharedWith of
				[]		= showMessageA "Sharing" "This list is not shared" [aPrevious,aAddPerson,aAddGroup] [] 
				users	= enterMultipleChoiceA "Sharing" "This list is shared with the following people" [aPrevious,aRemove,aAddPerson,aAddGroup] users
			  )
			>>= \(action,users) -> case action of
				ActionPrevious				=						return True
				ActionLabel "Delete"		= removeUsers users >>| return False
				ActionLabel "Add person(s)"	= addUsers list		>>| return False
				ActionLabel "Add group"		= addGroup list		>>| return False
	) <! id >>| stop

where
	aPrevious	= ButtonAction (ActionPrevious, Always)
	aRemove		= ButtonAction (ActionLabel "Delete", IfValid)
	aAddPerson	= ButtonAction (ActionLabel "Add person(s)", IfValid)
	aAddGroup	= ButtonAction (ActionLabel "Add group", IfValid)

	removeUsers users	= 	removeSharingForList list users
	addUsers list		=	enterInformation "Add person(s)" "Enter the person(s) you want to share this list with"
						>>= addSharingForList list 
					
	addGroup list		= 	getMyGroups
						>>= \groups -> case groups of
							[]		= showMessage "Add group" "You have no groups that you are member of" list
							groups	= enterChoice "Add group" "Which group do you want to share this list with?" groups
									>>= \group ->
										addSharingForList list group.members

createList :: !String !String !(Maybe Note) -> Task AnyList
createList type name description
	=	storeMeta
	>>= \meta ->
		storeList meta.ListMeta.listId (makeList type name description meta.ListMeta.listId)
where 
	storeMeta :: Task ListMeta
	storeMeta
		=	getContextWorker
		>>= \owner ->
			dbCreateItem {ListMeta| listId = 0, owner = owner, sharedWith =[]}
			
	makeList :: !String !String !(Maybe Note) !Int -> AnyList
	makeList "Simple list" name	desc listId		= SimpleList	{List|listId = (Hidden listId), name = name, description = desc, items = [] }
	makeList "Todo list" name desc listId		= TodoList		{List|listId = (Hidden listId), name = name, description = desc, items = [] }
	makeList "Date list" name desc listId		= DateList		{List|listId = (Hidden listId), name = name, description = desc, items = [] }
	makeList "Document list" name desc listId	= DocumentList	{List|listId = (Hidden listId), name = name, description = desc, items = [] }
	
	storeList :: !Int !AnyList -> Task AnyList 
	storeList listId list = writeDB (mkDBId ("List-" <+++ listId)) list
	
getAllLists :: Task [AnyList]
getAllLists = dbReadAll >>= getLists

getMyLists :: Task [AnyList]
getMyLists
	=	getContextWorker >>= \user -> dbReadAll >>= transform (filter (hasAccess user)) >>= getLists 
where
	hasAccess user meta = user == meta.ListMeta.owner || isMember user meta.ListMeta.sharedWith
	
getLists :: [ListMeta] -> Task [AnyList]
getLists [] 	= return []
getLists meta	= allTasks [readDB (mkDBId ("List-" <+++ m.ListMeta.listId)) \\ m <- meta]

deleteList :: !AnyList -> Task AnyList
deleteList list = deleteMeta listId  >>| deleteList listId >>| return list
where
	listId	= listIdOf list
	
	deleteMeta :: Int -> Task (Maybe ListMeta)
	deleteMeta listId = dbDeleteItem (DBRef listId)
	
	deleteList :: Int -> Task (Maybe AnyList)
	deleteList listId = deleteDB (mkDBId ("List-" <+++ listId))

addSharingForList :: !AnyList ![User] -> Task AnyList
addSharingForList list users
	= dbReadItem (DBRef (listIdOf list))
	>>= \mbMeta -> case mbMeta of
		Nothing 	= throw "List meta data not found"
		Just meta	= dbUpdateItem {ListMeta| meta & sharedWith = meta.ListMeta.sharedWith ++ users} >>| return list

removeSharingForList :: !AnyList ![User] -> Task AnyList
removeSharingForList list users
	= dbReadItem (DBRef (listIdOf list))
	>>= \mbMeta -> case mbMeta of
		Nothing 	= throw "List meta data not found"
		Just meta	= dbUpdateItem {ListMeta| meta & sharedWith = [u \\ u <- meta.ListMeta.sharedWith | not (isMember u users)]} >>| return list

listIdOf :: !AnyList -> Int
listIdOf (SimpleList l)		= fromHidden l.List.listId
listIdOf (TodoList l)		= fromHidden l.List.listId
listIdOf (DateList l)		= fromHidden l.List.listId
listIdOf (DocumentList l)	= fromHidden l.List.listId

nameOf :: !AnyList -> String
nameOf (SimpleList l)		= l.List.name
nameOf (TodoList l)			= l.List.name
nameOf (DateList l)			= l.List.name
nameOf (DocumentList l)		= l.List.name
