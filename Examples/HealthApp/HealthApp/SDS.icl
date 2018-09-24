implementation module HealthApp.SDS

import iTasks

import Data.Func

import HealthApp.Definition

dClient1 =
	{Client| name = "Don Quichot"
	, address = {Address| street = "unknown"
		, number = "15"
		, zipCode = "111111"
		, city = "Unnamed"
		, state = "La Mancha"
		, country = "Spain"
		}
	, ssn = SSN "50416"
	, appointments = [{Appointment|description = "Fighting a windmill"
			, time=fromOk $ parseDateTime "1601-02-01 12:00:00"
			, length={hours=0, minutes = 1}
			, location = "Spain"
			}
		]
	, medication = [{Medication| name= "LSD", description = "Psychedelic medication", amount = "5mg/day"}]
	, photo = ClientPhoto
	}

dClient2 =
	{Client| name = "Jean-Luc Picard"
	, address = {Address| street = "Deck 9"
		, number = "3601"
		, zipCode = ""
		, city = "Enterprise"
		, state = ""
		, country = ""
		}
	, ssn = SSN "99999999"
	, appointments = [{Appointment|description = "Klingon peace talks"
			, time=fromOk $ parseDateTime "2331-02-01 12:00:00"
			, length={hours= 1, minutes = 0}
			, location = "Space"
			}
		]
	, medication = []
	, photo = ClientPhoto
	}

clientsShare :: SDSLens () [Client] [Client]
clientsShare = sharedStore "clientsShare" [dClient1, dClient2]

clientsIndexedShare :: SDSLens () [(ClientId, Client)] [(ClientId, Client)]
clientsIndexedShare = sdsLens "clientsIndexedShare" (const ()) reader writer notifier reducer clientsShare
where
	reader = SDSRead \p rs. Ok (zip2 (indexList rs) rs)
	writer = SDSWrite \p rs w. Ok (Just $ snd $ unzip w)
	notifier = SDSNotify \_ _ _ _ _. True
	reducer = Just \p ws. Ok (zip2 (indexList ws) ws)

clientShare :: ClientId -> SDSLens () Client Client
clientShare clientId = sdsFocus clientId (sdsLens "clientShare" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) (Just reducer) clientsIndexedShare)
where
	read clientId clients = Ok (snd $ clients !! clientId)
	write clientId clients client = Ok (Just (updateAt clientId (clientId, client) clients))
	notify p indexedClients client ts pp = p == pp
	reducer _ w = Ok (snd $ w !! clientId)

clientMedicationShare :: ClientId -> SDSLens () [Medication] [Medication]
clientMedicationShare clientId = mapReadWrite
	(\c. c.medication, \medication r. Just {r & medication = medication})
	(Just \p w. Ok (w.medication))
	(clientShare clientId)

clientAddresShare :: ClientId -> SDSLens () Address Address
clientAddresShare clientId = mapReadWrite
	(\c. c.address, \address client. Just {client & address = address})
	(Just \p w. Ok (w.address))
	(clientShare clientId)
