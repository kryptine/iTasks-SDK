definition module iTasks.Framework.Client.Tasklet

import StdString, Data.Void, iTasks.API.Core.Client.Tasklet

// Low level functions implemented on the client to handle ...

// ... JavaScript events
handleJSEvent :: (TaskletEventHandlerFunc a) !TaskId *JSWorld -> Void

// ... interface calls
handleInterfaceCall :: !f !String !arg -> Void 




