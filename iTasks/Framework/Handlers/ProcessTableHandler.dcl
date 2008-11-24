definition module ProcessTableHandler //iTasks.Handlers.ProcessTableHandler

import Http
import InternaliTasksCommon

/**
* Handles the ajax requests for a ProcessTable tab panel.
*/
handleProcessTableRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a