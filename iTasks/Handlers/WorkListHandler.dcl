definition module WorkListHandler //iTasks.Handlers.WorkListHandler

import Http
//import TaskTree
import InternaliTasksCommon

/**
* Handles the ajax requests from the current work filter panel.
*/
handleWorkListRequest :: !(Task a) !HTTPRequest *World -> (!HTTPResponse, !*World) | iData a