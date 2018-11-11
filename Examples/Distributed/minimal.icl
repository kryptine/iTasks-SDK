module minimal

import iTasks
import iTasks.Extensions.Distributed.API

import qualified Data.Map
import StdMisc

derive class iTask DummyTask

Start iworld = doTasks task iworld
where
	task :: Task ()
	task = viewInformation "Choose role" [] "choose"
		>>* [ OnAction (Action "Domain server") (always server)
		    , OnAction (Action "Client") (always client)]

	server :: Task ()
	server = enterDomain
		>>= \domain. set (Just domain) domainName
		>>= \_. (viewSharedInformation "Tasks in the domain" [] (domainTasks domain)
		-&&- viewSharedInformation "Users in the domain" [] (domainUsers domain)
		-&&- viewSharedInformation "Hosting domain" [] domainName
		-&&- viewSharedInformation "Devices in domain" [] (domainDevices domain))
		<<@ ArrangeHorizontal @! ()

	client :: Task ()
	client = enterDomain
		>>= \domain. (addTasks domain -&&- selectTask domain) @! ()

	addTasks domain = forever (enterChoice "Choose a task to add in the domain" [] [T1, T2, T3]
		>>= \task. case task of
			T1 = appendDomainTask domain attributes (enterInformation "HUEHUE1" [] >>= \number. return (5 * number))
				>>= \_. viewInformation "Done" [] "Added task to domain!" >>= \_. return ()
			T2 = appendDomainTask domain attributes (enterInformation "HUEHUE2" [] >>= \number. return (6 * number))
				>>= \_. viewInformation "Done" [] "Added task to domain!" >>= \_. return ()
			T3 = appendDomainTask domain attributes (enterInformation "HUEHUE3" [] >>= \number. return (7 * number))
				>>= \_. viewInformation "Done" [] "Added task to domain!" >>= \_. return ())

	selectTask :: Domain -> Task ()
	selectTask domain = forever (undef)

	enterDomain :: Task Domain
	enterDomain = enterInformation "Enter domain" []

:: DummyTask = T1 | T2 | T3

attributes = 'Data.Map'.fromList
			[ ("title",      "None")
			, ("createdBy",  "me")
			, ("createdAt",  "now")
			, ("priority",   toString 5)
			]