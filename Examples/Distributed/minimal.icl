module minimal

import iTasks

import iTasks.Internal.Distributed.Instance
import qualified iTasks.Extensions.Distributed.SDS as D

import Data.Func
import Data.Tuple
import Data.Maybe
import Data.Functor

sharedShare = sharedStore "sharedshare" 42

mainTask = viewInformation "Choose your role" [] ()
	>>* [OnAction (Action "Domain server") $ always domainServer
	    ,OnAction (Action "Client") $ always client
	    ]
where
	domainServer = instanceServer 8123 (Domain "localhost")
		||- (updateSharedInformation "This share is shared between machines" [] sharedShare @! ())

	client = instanceClient "localhost" 8123 (Domain "localhost")
		||- (forever
			$ 'D'.get sharedShare
			>>= viewInformation "This share is shared between machines" []
			>>* [OnAction (Action "Refresh") $ always $ return ()])

Start :: *World -> *World
Start world
	= startEngineWithOptions opts mainTask world
where
	opts [] = \op->(Just {op&distributed=True}, ["Started server on port: " +++ toString op.serverPort])
	opts ["-p",p:as] = appFst (fmap (\o->{o & serverPort=toInt p})) o opts as
	opts [a:as] = opts as
