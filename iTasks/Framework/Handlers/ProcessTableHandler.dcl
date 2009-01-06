definition module ProcessTableHandler //iTasks.Handlers.ProcessTableHandler

import Http, Session
import InternaliTasksCommon

/**
* Handles the ajax requests for a ProcessTable tab panel.
*/
handleProcessTableRequest :: !(LabeledTask a) !Int !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a