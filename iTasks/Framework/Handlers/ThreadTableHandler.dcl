definition module ThreadTableHandler //iTasks.Handlers.ThreadTableHandler

import Http, Session
import InternaliTasksCommon

/**
* Handles the ajax requests for a ThreadTable tab panel.
*/
handleThreadTableRequest :: !(LabeledTask a) !Int !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a