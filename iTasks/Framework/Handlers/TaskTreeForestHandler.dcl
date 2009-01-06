definition module TaskTreeForestHandler //iTasks.Handlers.TaskTreeForestHandler

import Http, Session
import InternaliTasksCommon

/**
* Handles the ajax requests for a TaskTreeForest tab panel.
*/
handleTaskTreeForestRequest :: !(LabeledTask a) !Int !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a