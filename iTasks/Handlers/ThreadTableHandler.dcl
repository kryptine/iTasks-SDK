definition module ThreadTableHandler //iTasks.Handlers.ThreadTableHandler

import Http
import InternaliTasksCommon

/**
* Handles the ajax requests for a ThreadTable tab panel.
*/
handleThreadTableRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a