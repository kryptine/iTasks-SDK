implementation module SAR

import iTasks
import CommonDomain

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine searchAndRescueExample world

:: IncidentEntry =
	{ type        :: IncidentType
	, description :: Note
//	, location    :: MapCoordinates
	}

:: IncidentType = MedicRequest | MalFunction Note | Other String
	

:: MapCoordinates =
	{ lat			:: Real
	, lon			:: Real
	}

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
	{ logNr		:: Int
	, incident	:: IncidentNR
	, date		:: Date
	, time		:: Time
	, user		:: UserName
	, message	:: Note
	}

derive gPrint 		Incident, LogEntry, IncidentEntry, IncidentType
derive gParse		Incident, LogEntry, IncidentEntry, IncidentType
derive gVisualize	Incident, LogEntry, IncidentEntry, IncidentType
derive gUpdate		Incident, LogEntry, IncidentEntry, IncidentType

// Incident management
manageIncident :: Task Void
manageIncident 
= 		enterInformation "Enter Information about the Incident"
	>>= \incident -> createIncident
	>>= \icNR     -> addLogEntry icNR incident.IncidentEntry.description
	>>| showMessageAbout "Incident Data" incident
	>>| viewLog icNR
	>>| return Void

// Response decision

// SAR Heli deployment
deploySARHeli :: Task Void
deploySARHeli = return Void


// Incident database
createIncident :: Task IncidentNR
createIncident = getDefaultValue >>= dbCreateItem >>= \i -> return i.incidentNr

// Logging
addLogEntry :: IncidentNR Note -> Task Void
addLogEntry incident message
	=	getContextWorker -&&- (getCurrentDate -&&- getCurrentTime)
	>>= \(user,(date,time)) ->
		dbCreateItem {LogEntry | logNr = 0, incident = incident, date = date, time = time, user = user, message = message}
	>>| stop

viewLog :: IncidentNR -> Task Void
viewLog incident
	= 	readDB databaseId
	>>= \logs ->
		showMessageAbout ("Log for incident " +++ toString incident) [l \\ l <- logs | l.LogEntry.incident == incident]

// Databases
instance DB Incident
where
	databaseId				= mkDBid "Incidents"
	getItemId i				= DBRef i.Incident.incidentNr
	setItemId (DBRef r) i	= {Incident| i & incidentNr = r} 

instance DB LogEntry
where
	databaseId				= mkDBid "Log"
	getItemId l				= DBRef l.LogEntry.logNr
	setItemId (DBRef r) l	= {LogEntry| l & logNr = r}




