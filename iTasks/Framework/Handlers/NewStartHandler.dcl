definition module NewStartHandler

import Http, Session
import InternaliTasksCommon

/**
* Handles the ajax requests from the 'start new work' panel.
*/
handleNewStartRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)