definition module ThreadTableHandler

import Http, TSt

/**
* Handles the ajax requests for a ThreadTable tab panel.
*/
handleThreadTableRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)