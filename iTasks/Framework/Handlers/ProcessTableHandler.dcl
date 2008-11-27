definition module ProcessTableHandler //iTasks.Handlers.ProcessTableHandler

import Http, Session
import InternaliTasksCommon

/**
* Handles the ajax requests for a ProcessTable tab panel.
*/
handleProcessTableRequest :: !(Task a) !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a