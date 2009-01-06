definition module NewStartHandler //iTasks.Handlers.NewListHandler

import Http, Session
import InternaliTasksCommon

/**
* Handles the ajax requests from the 'start new work' panel.
*/
handleNewStartRequest :: !(LabeledTask a) !Int !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a