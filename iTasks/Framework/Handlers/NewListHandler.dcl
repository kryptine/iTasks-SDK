definition module NewListHandler //iTasks.Handlers.NewListHandler

import Http, Session
import InternaliTasksCommon

/**
* Handles the ajax requests from the 'start new work' panel.
*/
handleNewListRequest :: !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt)