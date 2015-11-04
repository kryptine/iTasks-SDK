definition module iTasks._Framework.WebService
/**
* This module provides the web service that gives access to tasks via the web.
* It also provides access to upload/download of blob content.
*/
from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks._Framework.Engine		import :: ServiceFormat, :: ConnectionType
from iTasks._Framework.IWorld		import :: IWorld
from iTasks._Framework.Task 	    import :: Task, :: ConnectionTask
from iTasks._Framework.TaskState 	import :: TIUIState
from iTasks._Framework.SDS 			import :: RWShared
from iTasks.UI.Diff 				import :: UIChangeDef
from iTasks.API.Core.Types	        import :: InstanceNo
from Data.Queue 					import :: Queue

import iTasks._Framework.Generic

httpServer :: !Int !Int ![(!String -> Bool
				,!Bool
				,!(HTTPRequest r *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !Maybe w, !*IWorld))
				,!(HTTPRequest r (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !Maybe w, !*IWorld))
				,!(HTTPRequest r ConnectionType *IWorld -> (!Maybe w, !*IWorld))
				)] (RWShared () r w) -> ConnectionTask | TC r & TC w

webService :: !String !(HTTPRequest -> Task a) !ServiceFormat ->
                 (!(String -> Bool)
                 ,!(HTTPRequest (Map InstanceNo (Queue UIChangeDef)) *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !Maybe (Map InstanceNo (Queue UIChangeDef)), !*IWorld))
                 ,!(HTTPRequest (Map InstanceNo (Queue UIChangeDef)) (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !Maybe (Map InstanceNo (Queue UIChangeDef)), !*IWorld))
                 ,!(HTTPRequest (Map InstanceNo (Queue UIChangeDef)) ConnectionType *IWorld -> (!Maybe (Map InstanceNo (Queue UIChangeDef)), !*IWorld))
                 ) | iTask a
