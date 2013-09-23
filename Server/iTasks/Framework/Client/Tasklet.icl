implementation module iTasks.Framework.Client.Tasklet

import StdEnv, Data.Void, iTasks.API.Core.Client.Tasklet

:: EventQueue :== Void

handleJSEvent :: (HtmlEventHandlerFunc a e) !String *JSWorld -> Void
handleJSEvent origHandler taskId event = undef

handleInterfaceCall :: !f !String !arg -> Void 
handleInterfaceCall origHandler taskId arg = undef
