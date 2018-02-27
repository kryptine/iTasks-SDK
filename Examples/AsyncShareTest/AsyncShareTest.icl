module AsyncShareTest

import iTasks

import iTasks.Internal.Distributed.Instance

:: ServerRole = DomainServer | Client

testShare :: SDS () Int Int
testShare = sharedStore "sharedStoreName" 0

Start w = startEngine task w 

task = viewInformation "Choose your role" [] ()
	>>* [OnAction (Action "Domain server") (always domainServer)
		,OnAction (Action "Client") (always client)
		]
where
	domainServer :: Task Int
	domainServer = instanceServer 8765 (Domain "TEST")
		||- (updateSharedInformation "This share is shared in the domain" [] testShare)

	client :: Task Int
	client = instanceClient "TEST" 8765 (Domain "TEST")
		||- (updateSharedInformation "This share is stored somewhere else in the domain" [] (remote testShare (DomainShare {domain="TEST", port = 8765, name = "sharedStoreName"})))




