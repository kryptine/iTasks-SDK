definition module NewStartHandler //iTasks.Handlers.NewListHandler

import Http, Session
import InternaliTasksCommon

/**
* Handles the ajax requests from the 'start new work' panel.
*/
handleNewStartRequest :: !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt)