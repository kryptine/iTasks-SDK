implementation module Incidone.Util.AIS
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
import StdBool, StdInt, StdString, StdTuple, StdArray, StdClass, StdList, Text, Data.Maybe

TESTMESSAGES :==
	["!AIVDM,1,1,,,339K5fUP080PrBtNQQjua7Qn0000,0*57"
	,"!AIVDM,2,1,1,,539K5fP00000@95>221M=236222222222222220o1@D222v0051CA1C`8888,0*2C"
	,"!AIVDM,2,2,1,,88888888888,2*6C"
	,"!AIVDM,1,1,,,13aBLohP000DCV4MdGlf3@020000,0*1D"
	,"!AIVDM,2,1,1,,53aBLoh0SPpE0K;33N0lE8=E8UE>04m=@E8@4n169P:382Ec08lSm51DQ0CH,0*34"
	,"!AIVDM,2,2,1,,88888888888,2*6C"
	]

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
	, accuracy	:: Bool			// Position Accuracy
	, lon		:: Int			// Longitude
	, lat		:: Int			// Latitude
	, course	:: Int			// Course Over Ground
	, heading	:: Int			// True Heading
	, second	:: Int			// Time Stamp
	, maneuver	:: Int			// Manuever Indicator
	, raim		:: Bool			// RAIM flag
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

decodeAIVDM :: [String] -> ([AIVDM],[String])
decodeAIVDM [] = ([],[])
decodeAIVDM sentences
	# (payload, remainder)	= decodeWrapper sentences
    = case payload of
        []  = ([],remainder)
        parts
            # cnb = decodePayload (concat parts)
	        # (cnbs,remainder)	= decodeAIVDM remainder
            = ([cnb:cnbs],remainder)
where
	//Take a number of fragments from the stream to unwrap and reconstruct the message payload
	decodeWrapper :: [String] -> ([{#Char}], [String])
	decodeWrapper [] = ([],[])
	decodeWrapper [sentence:remainder]
		= case split "," sentence of
			[f1,f2,f3,f4,f5,f6,f7]
				| f1 <> "!AIVDM"
					= ([],remainder)
				# fragmentCount = toInt f2
				# fragmentNum = toInt f3
				| fragmentCount == fragmentNum
					= ([f6],remainder)
				| otherwise
					# (fragments,remainder) = decodeWrapper remainder
					= ([f6:fragments],remainder)	
			_ 	= ([],remainder)

	//Decode the message data
	decodePayload :: {#Char} -> AIVDM
	decodePayload bits
		# bits = bv_unarmor bits
		= snd ((decodeDyn decodeMsgType decoderByType) bits 0 (AIVDM 0))
	where
		decodeMsgType = decodeUnsigned 6 (\t _ -> initMessage t)

		decoderByType msg =	case msg of
			AIVDM1 _	= decodeCNB
			AIVDM2 _	= decodeCNB
			AIVDM3 _	= decodeCNB
			AIVDM4 _	= decodeType4
			AIVDM5 _	= decodeType5
			_			= decodeSpare 0 //Nop

		initMessage 1 = AIVDM1 (initCNB 1)
		initMessage 2 = AIVDM2 (initCNB 2)
		initMessage 3 = AIVDM3 (initCNB 3)
		initMessage 4 = AIVDM4 initType4
		initMessage 5 = AIVDM5 initType5
		initMessage i = AIVDM i
		
		initCNB type = {msgtype=type,repeat=0,mmsi=0,status=0,turn=0,speed=0,accuracy=False
					   ,lon=0,lat=0,course=0,heading=0,second=0,maneuver=0,raim=False,radio=0}
		initType4    = {msgtype=4,repeat=0,mmsi=0,year=0,month=0,day=0,hour=0,minute=0
					   ,second=0,accuracy=False,lon=0,lat=0,epfd=0,raim=False,radio=0}
		initType5    = {msgtype=5,repeat=0,mmsi=0,ais_version=0,imo_id=0,callsign="",shipname=""
					   ,shiptype=0,to_bow=0,to_stern=0,to_port=0,to_starboard=0,epfd=0,month=0
					   ,day=0,hour=0,minute=0,draught=0,destination="",dte=False}

		decodeCNB = decodeMultiple
			[decodeUnsigned 2 updRepeat
			,decodeUnsigned 30 updMMSI
			,decodeUnsigned 4 (updCNB (\i cnb -> {AIVDMCNB|cnb & status = i}))
			,decodeSigned 8 (updCNB (\i cnb -> {AIVDMCNB|cnb & turn = i}))
			,decodeUnsigned 10 (updCNB (\i cnb -> {AIVDMCNB|cnb & speed = i}))
			,decodeUnsigned 1 (updCNB (\i cnb -> {AIVDMCNB|cnb & accuracy = i > 0}))
			,decodeSigned 28 (updCNB (\i cnb -> {AIVDMCNB|cnb & lon = i}))
			,decodeSigned 27 (updCNB (\i cnb -> {AIVDMCNB|cnb & lat = i}))
			,decodeUnsigned 12 (updCNB (\i cnb -> {AIVDMCNB|cnb & course = i}))
			,decodeUnsigned 9 (updCNB (\i cnb -> {AIVDMCNB|cnb & heading = i rem 360}))
			,decodeUnsigned 6 (updCNB (\i cnb -> {AIVDMCNB|cnb & second = i}))
			,decodeUnsigned 2 (updCNB (\i cnb -> {AIVDMCNB|cnb & maneuver = i}))
			,decodeSpare 3
			,decodeUnsigned 1 (updCNB (\i cnb -> {AIVDMCNB|cnb & raim = i > 0}))
			,decodeUnsigned 19 (updCNB (\i cnb -> {AIVDMCNB|cnb & radio = i}))
			]

		decodeType4 = decodeMultiple
			[decodeUnsigned 2 updRepeat
			,decodeUnsigned 30 updMMSI
			]

		decodeType5 = decodeMultiple
			[decodeUnsigned 2 updRepeat
			,decodeUnsigned 30 updMMSI
			,decodeUnsigned 2 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & ais_version = i})
			,decodeUnsigned 30 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & imo_id = i})
			,decodeString 42 (\s (AIVDM5 m) -> AIVDM5 {AIVDM5|m & callsign = s})
			,decodeString 120 (\s (AIVDM5 m) -> AIVDM5 {AIVDM5|m & shipname= s})
			,decodeUnsigned 8 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & shiptype = i})
			,decodeUnsigned 9 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & to_bow = i})
			,decodeUnsigned 9 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & to_stern = i})
			,decodeUnsigned 6 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & to_port = i})
			,decodeUnsigned 6 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & to_starboard= i})
			,decodeUnsigned 4 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & epfd = i})
			,decodeUnsigned 4 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & month = i})
			,decodeUnsigned 5 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & day = i})
			,decodeUnsigned 5 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & hour = i})
			,decodeUnsigned 6 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & minute = i})
			,decodeUnsigned 8 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & draught = i})
			,decodeString 120 (\s (AIVDM5 m) -> AIVDM5 {AIVDM5|m & destination = s})
			,decodeUnsigned 1 (\i (AIVDM5 m) -> AIVDM5 {AIVDM5|m & dte = i > 0})
			,decodeSpare 1
			]

		updRepeat i (AIVDM1 m) = AIVDM1 {AIVDMCNB|m & repeat = i}
		updRepeat i (AIVDM2 m) = AIVDM2 {AIVDMCNB|m & repeat = i}
		updRepeat i (AIVDM3 m) = AIVDM3 {AIVDMCNB|m & repeat = i}
		updRepeat i (AIVDM4 m) = AIVDM4 {AIVDM4|m & repeat = i}
		updRepeat i (AIVDM5 m) = AIVDM5 {AIVDM5|m & repeat = i}
		updRepeat i msg        = msg

		updMMSI i (AIVDM1 m) = AIVDM1 {AIVDMCNB|m & mmsi = i}
		updMMSI i (AIVDM2 m) = AIVDM2 {AIVDMCNB|m & mmsi = i}
		updMMSI i (AIVDM3 m) = AIVDM3 {AIVDMCNB|m & mmsi = i}
		updMMSI i (AIVDM4 m) = AIVDM4 {AIVDM4|m & mmsi = i}
		updMMSI i (AIVDM5 m) = AIVDM5 {AIVDM5|m & mmsi = i}
		updMMSI i msg        = msg
		
		updCNB f i (AIVDM1 m) = AIVDM1 (f i m)
		updCNB f i (AIVDM2 m) = AIVDM2 (f i m)
		updCNB f i (AIVDM3 m) = AIVDM3 (f i m)
		updCNB f i msg        = msg

	decodeUnsigned :: Int (Int AIVDM -> AIVDM) {#Char} Int AIVDM -> (Int,AIVDM)
	decodeUnsigned width modifier bits pos msg
		= (pos + width, modifier (bv_ubits pos width bits) msg)

	decodeSigned :: Int (Int AIVDM -> AIVDM) {#Char} Int AIVDM -> (Int,AIVDM)
	decodeSigned width modifier bits pos msg
		= (pos + width, modifier (bv_sbits pos width bits) msg)

	decodeString :: Int (String AIVDM -> AIVDM) {#Char} Int AIVDM -> (Int,AIVDM)
	decodeString width modifier bits pos msg
		 = (pos + width, modifier chars msg)
	where
		chars = rtrim {char c \\ c <- takeWhile (\i -> i > 0) [bv_ubits (pos + i * 6) 6 bits \\ i <- [0 .. (width / 6) - 1]]}
		char i = if (i == 0 || i > 64) ' ' "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^- !\"#$%&'()*+,-./0123456789:;<=>?".[i]

	decodeSpare :: Int {#Char} Int AIVDM -> (Int, AIVDM)
	decodeSpare width bits pos msg = (pos + width, msg)

	decodeDyn :: ({#Char} Int AIVDM -> (Int, AIVDM)) (AIVDM -> ({#Char} Int AIVDM -> (Int, AIVDM))) {#Char} Int AIVDM -> (Int,AIVDM)
	decodeDyn first cont bits pos msg
		# (pos,msg) = first bits pos msg
		= (cont msg) bits pos msg

	decodeMultiple :: [{#Char} Int AIVDM -> (Int, AIVDM)] {#Char} Int AIVDM -> (Int,AIVDM)
	decodeMultiple [] bits pos msg = (pos,msg)
	decodeMultiple [f:fs] bits pos msg
		# (pos,msg) = f bits pos msg
		= decodeMultiple fs bits pos msg

import StdMisc
//Lowlevel bit manipulation
bv_unarmor :: {#Char} -> {#Char}
bv_unarmor src = unarmor_and_copy src 0 0 0 0 (createArray required_bytes '\0')
where
	//The number of 8-bit bytes needed to hold all the bits
	num_bits = size src * 6
	required_bytes = num_bits / 8 + (if (num_bits rem 8 > 0) 1 0)

	unarmor_and_copy :: {#Char} Int Int Int Int *{#Char} -> *{#Char}
	unarmor_and_copy src srci desti buf bufwidth dest
		| srci >= size src
			| bufwidth > 0	//Add remaining buffer bits to dest
				# byte	= toChar (buf bitand ((2 ^ bufwidth) - 1))
				= {dest & [desti] = byte}
			| otherwise
				= dest //Done.
		# buf = (buf << 6) bitor (unarmor_char src.[srci])
		# bufwidth = bufwidth + 6
		| bufwidth >= 8	//Copy another byte to the destination array
			# byte		= toChar (buf >> (bufwidth - 8))
			# bufwidth	= bufwidth - 8
			= (unarmor_and_copy src (srci + 1) (desti + 1) buf bufwidth {dest & [desti] = byte})
		| otherwise
			= unarmor_and_copy src (srci + 1) desti buf bufwidth dest
	where
		//Extract the six bits from an armored ASCII character
		unarmor_char :: Char -> Int
		unarmor_char char = let bits = toInt char - 48 in (if (bits > 40) (bits - 8) bits)

bv_ubits :: Int Int {#Char} -> Int
bv_ubits start width bytes
    | start + width > (size bytes * 8) = -1 //abort ("NOT ENOUGH BYTES: " +++ toString (start + width - (size bytes * 8)))
	# field = copy_bytes (start / 8) ((start + width - 1) / 8) 0	//Copy all bytes that contain the bitfield
	# align = (start + width) rem 8
	# field = if (align > 0) (field >> (8 - align)) field			//Align the field
	# field = field bitand ((2 ^ width) - 1)						//Mask the field
	= field
where
	copy_bytes firstbyte lastbyte field
		| firstbyte > lastbyte	= field
		| otherwise				= copy_bytes (firstbyte + 1) lastbyte ((field << 8) bitor (toInt bytes.[firstbyte]))

bv_sbits :: Int Int {#Char} -> Int
bv_sbits start width bits
	# field = bv_ubits start width bits
	| field bitand (1 << (width - 1)) == 0	= field
											= ~(2 ^ width - field)
