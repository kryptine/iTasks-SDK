definition module iTasks.Framework.Client.Tasklet

import StdString, Data.Void, iTasks.API.Core.Client.Tasklet

// Low level functions implemented on the client to handle ...

// ... JavaScript events
handleJSEvent :: (HtmlEventHandlerFunc a e) !String *JSWorld -> Void

// ... interface calls
handleInterfaceCall :: !f !String !arg -> Void 




