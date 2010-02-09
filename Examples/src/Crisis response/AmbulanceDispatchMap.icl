implementation module AmbulanceDispatchMap

import iTasks
import CommonDomain
import GeoDomain
import google_maps_services
import Base64
import JSONTree
import DocumentDomain

derive gPrint		Incident, IncidentType
derive gParse		Incident, IncidentType
derive gVisualize 	Incident, IncidentType
derive gUpdate		Incident, IncidentType


ambulanceDispatchMapExamples :: [Workflow]
ambulanceDispatchMapExamples = flows
where
	flows = [ workflow "Examples/Crisis response/Report incident (Map)" (reportIncident -|| showSources) ]

:: Incident =
	{ location		:: Coordinate
	, address		:: Note
	, type			:: IncidentType
	, time			:: Time
	, nrInjured		:: Int
	, description	:: Note
	}
	
:: IncidentType = Accident | Fire | Fight | Other String
	
reportIncident :: Task Void
reportIncident
	=		markLocations >>=
	\map -> specifiyIncidents map
	>>| return Void
	
markLocations :: Task GoogleMap
markLocations = 
	enterInformation "Mark all locations where incidents have occurred"
	
specifiyIncidents :: GoogleMap -> Task [Incident]
specifiyIncidents map = sequence "Specify individual incident details" [ (addressLookup m) >>= \addr -> (specifyIncident addr m) \\ m <- (reverse map.GoogleMap.markers) ]

addressLookup :: GoogleMapMarker -> Task String
addressLookup marker
	# (lat,lng) = marker.position
	= showStickyMessage ("Address is being retrieved for coordinates: ("+++toString lat+++", "+++toString lng+++")") 
	  ||- reverse_geocoding (toString lat+++","+++toString lng) "json" False GOOGLE_API_KEY parseJSON
where
	parseJSON info 
	= case toJSONTree (base64Decode info) of
		(Just tree)
			= case queryJSONTree "Placemark\\1\\address" tree of
				(Just addr) = addr
				_			= "Address Unknown"
		_	= "Address Unknown"

specifyIncident :: String GoogleMapMarker -> Task Incident
specifyIncident addr marker
# smap = convertToStaticMap {GoogleMap | mkMap & center = marker.position, width = 200, height = 200, zoom = 15, markers = [marker]}
# incident = { Incident
			 | location = marker.position
			 , address = Note addr
			 , type = Accident
			 , time = {Time | hour = 0, min = 0, sec = 0}
			 , nrInjured = 0
			 , description = Note ""
			 }
= showStickyMessageAbout "Incident location:" smap ||- updateInformation "Specify incident details" incident 


//====
showSources ::  Task Void
showSources  
	=       loadDocumentFromFile "AmbulanceDispatchMap.icl" "Crisis Response/" >>=
	\icl -> loadDocumentFromFile "AmbulanceDispatchMap.dcl" "Crisis Response/" >>=
	\dcl -> showStickyMessageAbout "Source Codes" [icl,dcl]