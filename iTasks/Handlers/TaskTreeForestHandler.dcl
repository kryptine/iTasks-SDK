definition module TaskTreeForestHandler //iTasks.Handlers.TaskTreeForestHandler

import Http
import InternaliTasksCommon

/**
* Handles the ajax requests for a TaskTreeForest tab panel.
*/
handleTaskTreeForestRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a