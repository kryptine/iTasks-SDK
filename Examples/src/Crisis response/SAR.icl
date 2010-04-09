implementation module SAR

import iTasks
import CommonDomain

derive bimap (,), Maybe

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
	{ logNr		:: Int
	, incident	:: IncidentNR
	, date		:: Date
	, time		:: Time
	, user		:: UserName
	, message	:: Note
	}

derive gPrint 		Incident, LogEntry
derive gParse		Incident, LogEntry
derive gVisualize	Incident, LogEntry
derive gUpdate		Incident, LogEntry

// Incident management
manageIncident :: Task Void
manageIncident = return Void

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




