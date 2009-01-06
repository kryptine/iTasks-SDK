definition module WorkTabHandler //iTasks.Handlers.WorkTabHandler

import Http, Session
import InternaliTasksCommon

/**
* Handles the ajax requests for a work tab panel.
*/
handleWorkTabRequest :: !(LabeledTask a) !Int !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a