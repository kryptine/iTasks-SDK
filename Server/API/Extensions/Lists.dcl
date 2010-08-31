definition module Lists
/**
* This extension provides the ability to create lists
* (shopping lists, todo lists, meeting agendas etc) and
* share them with other users.
*/
import iTasks


:: List a =
	{ listId		:: !Hidden Int
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

/**
* Top level workflow for creating, viewing and sharing lists.
*/
manageLists 	:: 				Task Void
/**
* Top level flow for a single list.
*/
manageList		:: AnyList	->	Task Void
/**
* Create a new list.
* 
* @param type of list. Possible values "Simple list" "Todo list" "Date list" "Document list".
* @param A name
* @param An optional description
* 
* @return The new list
*/
createList		:: !String !String !(Maybe Note)	->	Task AnyList
/**
* Retrieve all lists stored in the system.
*
* @return The list of lists
*/
getAllLists		::										Task [AnyList]
/**
* Retrieve all lists that are created by, or shared with the current user.
* @return The list of lists
*/
getMyLists		::										Task [AnyList]
/**
* Delete a list
*
* @param The list to delete
* @return The deleted list
*/
deleteList		:: !AnyList							->	Task AnyList