definition module Lists

import iTasks

:: List a =
	{ name			:: !String
	, description	:: !Maybe Note
	, items			:: ![a]
	}

:: ListMeta	=
	{ listId		:: !Int
	, owner			:: !User
	, sharedWith	:: !Maybe [User]
	}

:: SimpleList	:== List String
:: TodoList		:== List (Bool, String)
:: DateList		:== List (Date, String)
:: DocumentList	:== List (Document, String)

:: AnyList	= SimpleList SimpleList
			| TodoList TodoList
			| DateList DateList
			| DocumentList DocumentList

derive class iTask List, ListMeta, AnyList
instance DB ListMeta


manageLists 	:: 				Task Void
manageList		:: AnyList	->	Task Void

createList		:: !String	->	Task AnyList
getAllLists		::				Task [AnyList]
getMyLists		::				Task [AnyList]
deleteList		:: !AnyList	->	Task AnyList

//Never mind this...
/*
:: ListDB
getListDB :: DBid ListDB

pushList :: Task Void

editList :: Task Void

newList :: Task Void
*/