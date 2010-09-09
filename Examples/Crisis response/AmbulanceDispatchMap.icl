implementation module AmbulanceDispatchMap

import iTasks
import GoogleMaps
import google_maps_services
import Base64

derive class iTask	Incident, IncidentType
derive bimap (,), Maybe

ambulanceDispatchMapExamples :: [Workflow]
ambulanceDispatchMapExamples = flows
where
	flows = [ workflow "Examples/Crisis response/Report incident (Map)" "" reportIncident]

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
	enterInformation "Locations" "Mark all locations where incidents have occurred"
	
specifiyIncidents :: GoogleMap -> Task [Incident]
specifiyIncidents map = sequence "Specify individual incident details" [ (addressLookup m) >>= \addr -> (specifyIncident addr m) \\ m <- (reverse map.GoogleMap.markers) ]

addressLookup :: GoogleMapMarker -> Task String
addressLookup marker
	# (lat,lng) = marker.position
	= showStickyMessage "Address lookup" ("Address is being retrieved for coordinates: ("+++toString lat+++", "+++toString lng+++")") Void
	  ||- reverse_geocoding (toString lat+++","+++toString lng) "json" False GOOGLE_API_KEY parseJSON
where
	parseJSON info 
	= case fromString (base64Decode info) of
		(obj =:(JSONObject f))
			= case jsonQuery "Placemark/1/address" obj of
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
= showStickyMessageAbout "Location" "Incident location:" smap ||- updateInformation "Details" "Specify incident details" incident 


//====
showSources ::  Task Void
showSources
	=       importDocument "Crisis Response\\AmbulanceDispatchMap.icl" >>=
	\icl -> importDocument "Crisis Response\\AmbulanceDispatchMap.dcl" >>=
	\dcl -> showStickyMessageAbout "Sources" "View the source code of this example" [icl,dcl] >>| stop