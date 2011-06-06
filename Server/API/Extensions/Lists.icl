implementation module Lists

import iTasks, Groups

derive class iTask List, ListMeta, ListDescription, AnyList
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
	databaseId 					= sharedStore "Lists" []
	getItemId l					= DBRef l.ListMeta.listId
	setItemId (DBRef listId) l	= {ListMeta| l & listId = listId}

manageLists :: Task Void
manageLists 
	=	Title "Manage lists" @>>
	(	getMyLists
	>>=	overview
	>>= \res -> case res of
		(ActionOpen,Just list)		= manageList list			>>| return False
		(ActionDelete,Just list)	= delList list				>>| return False
		(ActionNew,_)				= newList >>= manageList	>>| return False
		(ActionQuit,_)				=								return True
	) <! id
	>>| stop
where
	overview []		= showInformation ("My lists","You have no lists.") [] Void >>+ \_ -> UserActions [(ActionNew,Just (ActionNew,Nothing)),(ActionQuit,Just (ActionQuit,Nothing))]
	overview list	= enterChoice ("My lists","Select a list...") [] list >>+ \{modelValue,localValid} -> let mbL = if localValid (Just modelValue) Nothing in UserActions [aOpen mbL,aDelete mbL,aNew,aQuit]
	
	aOpen mbL 		= (ActionOpen, maybe Nothing (\l -> Just (ActionOpen,Just l)) mbL)
	aNew			= (ActionNew, Just (ActionNew,Nothing))
	aQuit			= (ActionQuit, Just (ActionQuit,Nothing))
	aDelete mbL		= (ActionDelete, maybe Nothing (\l -> Just (ActionDelete,Just l)) mbL)
	
	newList			=	enterChoice ("List type","What type of list do you want to create?") []
						["Simple list", "Todo list", "Date list","Document list"]
					>>= \type ->
						enterInformation ("Name","Please enter a name, and if you like, a description for the list") []
					>>= \desc ->
						createList type desc.ListDescription.name desc.ListDescription.description
	
	delList	list	=		showInformation ("Delete list","Are you sure you want to delete '" +++ nameOf list +++ "'?") [] Void
						>?*	[ (ActionNo,	Always (return list))
							, (ActionYes,	Always (deleteList list))
							]
									
manageList :: AnyList -> Task Void
manageList list
	=	
	(	showItems list
	>>= \action -> case action of
		ActionEdit			= editItems	list			>>| return False
		Action "share" _	= manageListSharing list	>>| return False
		ActionClose			=								return True
	) <! id
	>>| stop
where
	showItems l = case l of
		(SimpleList l)	= monitor (l.List.name,l.List.description) [Get simpleFrom]		(sharedStore ("List-" <+++ (fromHidden l.List.listId)) defaultValue) >>+ \_ -> UserActions [(ActionClose,Just ActionClose),(ActionEdit,Just ActionEdit),(Action "share" "Share",Just (Action "share" "Share"))]
		(TodoList l)	= monitor (l.List.name,l.List.description) [Get todoFrom]		(sharedStore ("List-" <+++ (fromHidden l.List.listId)) defaultValue) >>+ \_ -> UserActions [(ActionClose,Just ActionClose),(ActionEdit,Just ActionEdit),(Action "share" "Share",Just (Action "share" "Share"))]
		(DateList l)	= monitor (l.List.name,l.List.description) [Get dateFrom]		(sharedStore ("List-" <+++ (fromHidden l.List.listId)) defaultValue) >>+ \_ -> UserActions [(ActionClose,Just ActionClose),(ActionEdit,Just ActionEdit),(Action "share" "Share",Just (Action "share" "Share"))]
		(DocumentList l)= monitor (l.List.name,l.List.description) [Get documentFrom]	(sharedStore ("List-" <+++ (fromHidden l.List.listId)) defaultValue) >>+ \_ -> UserActions [(ActionClose,Just ActionClose),(ActionEdit,Just ActionEdit),(Action "share" "Share",Just (Action "share" "Share"))]

	editItems list = case list of
		(SimpleList l)	= updateSharedInformation (l.List.name,l.List.description) [View (simpleFrom,simpleTo)]		(sharedStore ("List-" <+++ (fromHidden l.List.listId)) defaultValue) >>+ \_ -> UserActions [(ActionFinish,Just Void)]
		(TodoList l)	= updateSharedInformation (l.List.name,l.List.description) [View (todoFrom,todoTo)]			(sharedStore ("List-" <+++ (fromHidden l.List.listId)) defaultValue) >>+ \_ -> UserActions [(ActionFinish,Just Void)]
		(DateList l)	= updateSharedInformation (l.List.name,l.List.description) [View (dateFrom,dateTo)]			(sharedStore ("List-" <+++ (fromHidden l.List.listId)) defaultValue) >>+ \_ -> UserActions [(ActionFinish,Just Void)]
		(DocumentList l)= updateSharedInformation (l.List.name,l.List.description) [View (documentFrom,documentTo)]	(sharedStore ("List-" <+++ (fromHidden l.List.listId)) defaultValue) >>+ \_ -> UserActions [(ActionFinish,Just Void)]

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
				[]		= showInformation ("Sharing","This list is not shared") [] Void >>+ \_ -> UserActions [(aPrevious,Just (aPrevious,[])),(aAddPerson,Just (aAddPerson,[])),(aAddGroup,Just (aAddGroup,[]))]
				users	= enterMultipleChoice ("Sharing","This list is shared with the following people") [] users >>+ \{modelValue = users} -> UserActions [(aPrevious,Just (aPrevious,users)),(aRemove,Just (aRemove,users)),(aAddPerson,Just (aAddPerson,users)),(aAddGroup,Just (aAddGroup,users))]
			  )
			>>= \res -> case res of
				(ActionDelete,users)		= removeUsers users >>| return False
				(Action "add-person" _,_)	= addUsers list		>>| return False
				(Action "add-group" _,_)	= addGroup list		>>| return False
				(ActionPrevious,_)			=						return True
	) <! id >>| stop

where
	aPrevious	= ActionPrevious
	aRemove		= ActionDelete
	aAddPerson	= Action "add-person" "Add person(s)"
	aAddGroup	= Action "add-group" "Add group"

	removeUsers users	= 	removeSharingForList list users
	addUsers list		=	enterInformation ("Add person(s)","Enter the person(s) you want to share this list with") []
						>>= addSharingForList list 
					
	addGroup list		= 	getMyGroups
						>>= \groups -> case groups of
							[]		= showInformation ("Add group","You have no groups that you are member of") [] list
							groups	= enterChoice ("Add group","Which group do you want to share this list with?") [] groups
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
		=	get currentUser
		>>= \owner ->
			dbCreateItem {ListMeta| listId = 0, owner = owner, sharedWith =[]}
			
	makeList :: !String !String !(Maybe Note) !Int -> AnyList
	makeList "Todo list" name desc listId		= TodoList		{List|listId = (Hidden listId), name = name, description = desc, items = [] }
	makeList "Date list" name desc listId		= DateList		{List|listId = (Hidden listId), name = name, description = desc, items = [] }
	makeList "Document list" name desc listId	= DocumentList	{List|listId = (Hidden listId), name = name, description = desc, items = [] }
	makeList _ name	desc listId					= SimpleList	{List|listId = (Hidden listId), name = name, description = desc, items = [] }
	
	storeList :: !Int !AnyList -> Task AnyList 
	storeList listId list = set (sharedStore ("List-" <+++ listId) defaultValue) list
	
getAllLists :: Task [AnyList]
getAllLists = dbReadAll >>= getLists

getMyLists :: Task [AnyList]
getMyLists
	=	get currentUser >>= \user -> dbReadAll >>= transform (filter (hasAccess user)) >>= getLists 
where
	hasAccess user meta = user == meta.ListMeta.owner || isMember user meta.ListMeta.sharedWith
	
getLists :: [ListMeta] -> Task [AnyList]
getLists [] 	= return []
getLists meta	= allTasks [get (sharedStore ("List-" <+++ m.ListMeta.listId) defaultValue) \\ m <- meta]

deleteList :: !AnyList -> Task AnyList
deleteList list = return list/*deleteMeta listId  >>| deleteList listId >>| return list
where
	listId	= listIdOf list
	
	deleteMeta :: Int -> Task (Maybe ListMeta)
	deleteMeta listId = dbDeleteItem (DBRef listId)
	
	deleteList :: Int -> Task (Maybe AnyList)
	deleteList listId = deleteShared (sharedStore ("List-" <+++ listId))*/

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
