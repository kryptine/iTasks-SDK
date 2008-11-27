definition module ThreadTableHandler //iTasks.Handlers.ThreadTableHandler

import Http, Session
import InternaliTasksCommon

/**
* Handles the ajax requests for a ThreadTable tab panel.
*/
handleThreadTableRequest :: !(Task a) !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a