implementation module AmbulanceDispatchMap

import iTasks
import CommonDomain
import GeoDomain

derive gPrint		Incident, IncidentType
derive gParse		Incident, IncidentType
derive gVisualize 	Incident, IncidentType
derive gUpdate		Incident, IncidentType


ambulanceDispatchMapExamples :: [Workflow]
ambulanceDispatchMapExamples = flows
where
	flows = [ workflow "Examples/Crisis response/Report incident (Map)" reportIncident ]

:: Incident =
	{ location		:: Coordinate
	, type			:: IncidentType
	, time			:: Time
	, nrInjured		:: Int
	, description	:: String
	}
	
:: IncidentType = Accident | Fire | Fight | Other String
	
reportIncident :: Task Void
reportIncident
	=		markLocations >>=
	\map -> specifiyIncidents map
	>>| return Void
	
markLocations :: Task Map
markLocations = 
	enterInformation "Mark all locations where incidents have occurred"
	
specifiyIncidents :: Map -> Task [Incident]
specifiyIncidents map = sequence "Specify individual incident details" [ (specifyIncident m) \\ m <- map.Map.markers ]

specifyIncident :: MapMarker -> Task Incident
specifyIncident marker
# smap = convertToStaticMap {Map | mkMap & center = marker.position, width = 200, height = 200, zoom = 15, markers = [marker]}
# incident = { Incident
			 | location = marker.position
			 , type = Accident
			 , time = {Time | hour = 0, min = 0, sec = 0}
			 , nrInjured = 0
			 , description = ""
			 }
= showStickyMessage smap ||- updateInformation "Specify incident details" incident 