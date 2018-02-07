module examples

import iTasks

myTasks :: Bool -> [Workflow]
startMode :: String -> Task ()
startMode executable
	=   get serverRoleShare 
	>>- \role = case role of
			DomainServer domain -> startAuthEngine domain >>| loginAndManageWorkList "Service engineer application" (myTasks True)
			Server domain -> startAuthEngine domain >>| loginRemote (myTasks False)
			_ -> viewInformation "Welcome" [] "Chose what this iTasks instance is."
		             >>* [ OnAction (Action "Domain server") (always (domainServer))
            			 , OnAction (Action "Server") (always (server))
            			 ]
where
	server :: Task ()
	server
		= enterDomain 
		>>= \domain -> set (Server domain) serverRoleShare
		>>| startAuthEngine domain >>| loginRemote (myTasks False)

	domainServer :: Task ()
	domainServer
		= enterDomain
		>>= \domain -> set (DomainServer domain) serverRoleShare
		>>| startAuthEngine domain
		>>| loginAndManageWorkList "Service engineer application" (myTasks True)

mainTask = viewInformation "Choose your role"
	>>* [OnAction (Action "Domain server") $ always domainServer
	    ,OnAction (Action "Client") $ always client
	    ]
where
	domainServer = updateSharedInformation "This share is shared between machines" []
		$ sharedStore "someShare" 42

	client = updateSharedInformation "This share is shared between machines" []
		$ sharedShare

Start :: *World -> *World
Start world
	= startEngineWithOptions opts mainTask world
where
	opts [] = \op->(Just {op&distributed=True}, ["Started server on port: " +++ toString op.serverPort])
	opts ["-p",p:as] = appFst (fmap (\o->{o & serverPort=toInt p})) o opts as
	opts [a:as] = opts as
