definition module WorkTabHandler //iTasks.Handlers.WorkTabHandler

import Http
import InternaliTasksCommon

/**
* Handles the ajax requests for a work tab panel.
*/
handleWorkTabRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a