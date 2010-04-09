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

:: IncidentType = MedicRequest | MedicEvacuation | Evacuation | FireOnboard | Other String
	
:: MapCoordinates =
	{ lat			:: Real
	, lon			:: Real
	}

searchAndRescueExample :: [Workflow]
searchAndRescueExample
	= [workflow "New Incident" (manageIncident Nothing)
	  ,workflow "Logs/View log" viewLogWF
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

:: HeliFlightDetails =
	{ targetLocation	:: MapCoordinates
	, purpose			:: Note
	}

derive gPrint 		Incident, LogEntry, IncidentEntry, IncidentType, HeliFlightDetails, MapCoordinates
derive gParse		Incident, LogEntry, IncidentEntry, IncidentType, HeliFlightDetails, MapCoordinates
derive gVisualize	Incident, LogEntry, IncidentEntry, IncidentType, HeliFlightDetails, MapCoordinates
derive gUpdate		Incident, LogEntry, IncidentEntry, IncidentType, HeliFlightDetails, MapCoordinates

// Incident management
manageIncident :: (Maybe Note) -> Task Void
manageIncident mbDesc
= 	(if (isNothing mbDesc)
		(enterInformation "Enter Information about the Incident")
		(updateInformation "Enter Information about the Incident" {IncidentEntry|type = MedicRequest,description = (fromJust mbDesc)}) )
	>>= \incident -> createIncident
	>>= \icNR     -> addLogEntry icNR incident.IncidentEntry.description
	>>|              chooseResponse icNR incident
	>>=              allTasks
	>>| stop
where
  enterIncident :: Task Incident
  enterIncident = enterInformation "Describe the incident"

  chooseResponse :: IncidentNR IncidentEntry -> Task [Task Void]
  chooseResponse icNR incident 
  	= updateMultipleChoice "Choose response actions" options (suggestion incident.IncidentEntry.type)
  where
      options = [f icNR \\ f <- [deploySARHeli
      							,deploySalvageVessel
      							,deployMultiFunctionVessel
      							,deployPolicePatrolVessel
      							,deploySurveillancePlane] ]

      //Compute the indexes in the options list that are initially selected
      suggestion MedicRequest 	  = [0]
      suggestion MedicEvacuation  = [0]
      suggestion Evacuation       = [0,2]
      suggestion _                = []

// SAR Heli deployment
deploySARHeli :: IncidentNR -> Task Void
deploySARHeli incident
	= "Deploy Search and Rescue Helicopter (SAR-XZ)"
	@>> (
			//Request flight
				enterFlightDetails
			>>= \details ->
				showInstructionAbout "Flight request" "Request the following flight of SAR-XZ at airbase" details
			>>|
				addLogEntry incident (Note ("SAR-XZ: Flight requested with details: " +++ visualizeAsTextDisplay details))
			>>| waitForEvents ["Take-off at base","Arrival at scene","Take-off at scene","Arrival at base"]
			>>| addLogEntry incident (Note ("SAR-XZ: deployment completed"))
		)
where
	enterFlightDetails :: Task HeliFlightDetails
	enterFlightDetails = enterInformation "Enter flight details for (SAR XZ)"
	
	waitForEvents [] = return Void
	waitForEvents [e:es]
		= 	enterChoiceAbout "The next expected event of SAR-XZ is: " e [EVT_OK,EVT_OTHER]
		>>= \choice -> case choice of
			EVT_OK
				= (addLogEntry incident (Note ("SAR-XZ: " +++ e)) >>| waitForEvents es)
			EVT_OTHER
				= (
						addLogEntry incident (Note "SAR-XZ deviated from expected plan.")
					>>| enterInformation "What happened?"
					>>= \reason ->
						addLogEntry incident reason
					>>| requestConfirmation "Do you want to report this as a new incident?"
					>>= \newinc ->
						if newinc
							(newIncident reason)
							stop
				  )

	newIncident reason
		=	getContextWorker
		>>= \user ->
			spawnProcess user True ( "New Incident" @>> manageIncident (Just reason))
		>>| stop
		
EVT_OK :== "This happened, continue"
EVT_OTHER :== "Something else happened, abort normal plan"

// Dummy deployment options
deploySalvageVessel :: IncidentNR -> Task Void
deploySalvageVessel incident = deployUnit "Salvage vessel (Seagull)" "Seagull" incident

deployMultiFunctionVessel :: IncidentNR -> Task Void
deployMultiFunctionVessel incident = deployUnit "Multi-function vessel (Orca)" "Orca" incident

deployPolicePatrolVessel :: IncidentNR -> Task Void
deployPolicePatrolVessel incident = deployUnit "Police patrol vessel (P-23)" "P-23" incident

deploySurveillancePlane :: IncidentNR -> Task Void
deploySurveillancePlane incident = deployUnit "Surveillance plane (PH-234)" "PH-234" incident

deployUnit :: String String IncidentNR -> Task Void
deployUnit longName shortName incident
	=	("Deploy " +++ longName)
	@>> (	addLogEntry incident (Note (shortName +++ " deployed"))
	    >>| showInstruction (shortName +++ " deployment") ("Deploy the " +++ longName +++ ". This task is done when the " +++ shortName +++ " has returned.")
	    >>| addLogEntry incident (Note (shortName +++ " returned"))
		)

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

viewLogWF :: Task Void
viewLogWF
	=	readDB databaseId
	>>=	enterChoice "Of which incident do you want to view the log?"
	>>= \incident ->
		viewLog incident.Incident.incidentNr

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