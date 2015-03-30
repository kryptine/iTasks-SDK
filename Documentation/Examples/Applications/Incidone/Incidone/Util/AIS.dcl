definition module Incidone.Util.AIS
/**
* This module provides functions for decoding a stream of AIVDM/AIVDO messages.
* These messages transmitted by AIS (Automatic Identification System) radio receivers.
*
* This module is based on the information provided at:
* http://gpsd.berlios.de/AIVDM.html
* and the python ais.py decoder of the gpsd project
*
* Currently this module only decodes type 1-5 messages. All other message types are ignored
*/

// AIVDM Message data

:: AIVDM
	= AIVDM1 AIVDMCNB
	| AIVDM2 AIVDMCNB
	| AIVDM3 AIVDMCNB
	| AIVDM4 AIVDM4
	| AIVDM5 AIVDM5
	| AIVDM  Int //An unsupported message type

:: AIVDMCNB = //Common Navigation Block (data for messages type 1,2 and 3)
	{ msgtype	:: Int			// Message Type
	, repeat	:: Int			// Repeat Indicator
	, mmsi		:: Int			// MMSI
	, status	:: Int			// Navigation Status
	, turn		:: Int			// Rate of Turn
	, speed		:: Int			// Speed Over Ground
	, accuracy	:: Bool		// Position Accuracy
	, lon		:: Int			// Longitude
	, lat		:: Int			// Latitude
	, course	:: Int			// Course Over Ground
	, heading	:: Int			// True Heading
	, second	:: Int			// Time Stamp
	, maneuver	:: Int			// Manuever Indicator
	, raim		:: Bool		// RAIM flag
	, radio		:: Int			// Radio status
	}

:: AIVDM4 = //Type 4 message (Base station report)
	{ msgtype		:: Int		// Message Type
	, repeat		:: Int		// Repeat Indicator
	, mmsi			:: Int		// MMSI
	, year			:: Int		// Year
	, month			:: Int		// Month
	, day			:: Int		// Day
	, hour			:: Int		// Hour
	, minute		:: Int		// Minute
	, second		:: Int		// Second 
	, accuracy		:: Bool		// Fix quality
	, lon			:: Int		// Longitude
	, lat			:: Int		// Latitude
	, epfd			:: Int		// Type of EPFD
	, raim			:: Bool		// RAIM flag
	, radio			:: Int		// SOTDMA state
	}

:: AIVDM5 = //Type 5 message (Static and Voyage Related data)
	{ msgtype		:: Int		// Message type
	, repeat		:: Int		// Repeat Indicator
	, mmsi			:: Int		// MMSI
	, ais_version	:: Int		// AIS Version
	, imo_id		:: Int		// IMO Identification Number
	, callsign		:: String	// Call Sign
	, shipname		:: String	// Vessel Name
	, shiptype		:: Int		// Ship Type
	, to_bow		:: Int		// Dimension to Bow
	, to_stern		:: Int		// Dimension to Stern
    , to_port		:: Int		// Dimension to Port
	, to_starboard	:: Int		// Dimension to Starboard
    , epfd			:: Int		// Position Fix Type
	, month			:: Int		// ETA month
	, day			:: Int		// ETA day
	, hour			:: Int		// ETA hour
	, minute		:: Int		// ETA minute
	, draught		:: Int 		// Draught
	, destination	:: String	// Destination
	, dte			:: Bool		// DTE
	}

/**
* Decodes a series of encoded AIVDM sentences to a series of
* decoded messages. AIVDM is the message format used by AIS
* AIVDM messages can consist of multiple sentences, so only full messages
* are parsed. If there are sentences of yet incomplete messages they are returned
*/
decodeAIVDM :: [String] -> ([AIVDM],[String])
