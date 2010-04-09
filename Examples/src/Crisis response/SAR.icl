implementation module SAR

import iTasks
import CommonDomain

Start :: *World -> *World
Start world = startEngine searchAndRescueExample world

searchAndRescueExample :: [Workflow]
searchAndRescueExample
	= [workflow "New Incident" manageIncident
	  ]

:: IncidentNR :== Int
:: Incident =
	{ incidentNr	:: IncidentNR
	, details		:: Note
	}

:: LogEntry =
	{ incident	:: IncidentNR
	, date		:: Date
	, time		:: Time
	, user		:: UserName
	, message	:: Note
	}

// Incident management
manageIncident :: Task Void
manageIncident = return Void

// Response decision

// SAR Heli deployment
deploySARHeli :: Task Void
deploySARHeli = return Void


// Logging
addLogEntry :: IncidentNR Note -> Task Void
addLogEntry incident message = return Void

viewLog :: IncidentNR -> Task Void
viewLog incident = return Void

// Databases