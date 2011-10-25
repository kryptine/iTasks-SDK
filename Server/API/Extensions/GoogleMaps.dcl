definition module GoogleMaps
/*
* This extension allows the use of GoogleMaps in editors
*/
import HTML, GenVisualize

derive gVisualizeText	GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gVisualizeHtml	GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gVisualizeEditor	GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gUpdate	  		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gDefaultMask		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gVerify			GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive JSONEncode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive JSONDecode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gEq				GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType

//API Key for http://localhost
GOOGLE_API_KEY :== "ABQIAAAAaZ6XgbNqm4h_DL45IQMnSRT2yXp_ZAY8_ufC3CFXhHIE1NvwkxT4lboFdTKu2o9gr_i8kRV0Pn1fNw"

:: GoogleMap = 
	{ perspective			:: GoogleMapPerspective
	, settings				:: GoogleMapSettings
	, markers				:: [GoogleMapMarker]		// Markers placed on the map
	}
	
:: GoogleMapPerspective =
	{ type					:: GoogleMapType			// The map type
	, center				:: GoogleMapPosition 		// Coordinate of the center point (Required by maps)
	, zoom					:: Int	      				// The zoom level (Required by maps)
	}

:: GoogleMapSettings =
	{ mapTypeControl		:: Bool		  				// Show the control for switching between map types
	, panControl			:: Bool		  				// Show the control for panning
	, zoomControl			:: Bool						// Show the control for zooming
	, streetViewControl		:: Bool						// Show the control for street view
	, scaleControl			:: Bool		  				// Show the scale of the map
	, scrollwheel			:: Bool						// Scrollwheel zooming on the map
	, draggable				:: Bool						// Map can be dragged
	}

:: GoogleMapPosition = 
	{ lat		:: !Real	//Lattitude
	, lng		:: !Real	//Longitude
	}
	
:: GoogleMapType = ROADMAP | SATELLITE | HYBRID | TERRAIN
	
:: GoogleMapMarker =
	{ position				:: !GoogleMapPosition			// Position of the marker
	, title					:: !Maybe String				// Title of the marker
	, infoWindow			:: !Maybe GoogleMapInfoWindow	// Information which is shown on click
	, draggable				:: !Bool						// Can the marker be dragged
	, selected				:: !Bool
	}
	
:: GoogleMapInfoWindow =
	{ content				:: !String						// Contents of the window
	}

/**
* Create a default map
*
* @return A default map
*/
defaultMap :: GoogleMap
/**
* Create a default map without any control options switched on
*
* @return A minimal default map 
*/
minimalMap :: GoogleMap

instance toString GoogleMapType
