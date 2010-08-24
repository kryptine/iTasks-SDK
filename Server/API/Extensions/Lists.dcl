definition module Lists

import iTasks

:: List a =
	{ listId		:: !Int
	, name			:: !String
	, description	:: !Maybe Note
	, items			:: ![a]
	}

:: SimpleList	:== List String
:: TodoList		:== List (Bool, String)
:: DateList		:== List (Date, String)
:: DocumentList	:== List (Document, String)

:: AnyList	= SimpleList SimpleList
			| TodoList TodoList
			| DateList DateList
			| DocumentList DocumentList

derive class iTask List, AnyList

manageLists 	:: 				Task Void
manageList		:: AnyList	->	Task Void

createList		:: !String !String !(Maybe Note)	->	Task AnyList
getAllLists		::										Task [AnyList]
getMyLists		::										Task [AnyList]
deleteList		:: !AnyList							->	Task AnyList