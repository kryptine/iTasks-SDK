implementation module Incidone.OP.ConceptsSQL

import Incidone.OP.Concepts, Incidone.OP.ConceptsTOP
import Text, StdMisc

toSQL :: !a -> [SQLValue] | mbToSQL a
toSQL x = mbToSQL (Just x)

fromSQL :: ![SQLValue] -> a | mbFromSQL a
fromSQL cols = case (mbFromSQL cols) of
    Just x  = x
    _       = abort ("fromSQL did not match for row: " +++ (join "," (map toString cols)))

instance mbToSQL Note
where
	mbToSQL (Just (Note x))   = [SQLVText x]
    mbToSQL Nothing           = [SQLVNull]

instance mbToSQL EmailAddress
where
	mbToSQL (Just (EmailAddress x)) = [SQLVText x]
    mbToSQL Nothing                 = [SQLVNull]

instance mbToSQL DateTime
where
	mbToSQL (Just (DateTime {Date|day,mon,year} {Time|hour,min,sec}))
		= [SQLVDatetime {SQLDate|day=day,month=mon,year=year} {SQLTime|hour=hour,minute=min,second=sec}]
    mbToSQL Nothing
        = [SQLVNull]

instance mbToSQL Document
where
	mbToSQL (Just x)  = [SQLVText (toString (toJSON x))]
    mbToSQL Nothing   = [SQLVNull]

instance mbToSQL Credentials
where
	mbToSQL (Just x) = [SQLVText (toString (toJSON x))]
    mbToSQL Nothing  = [SQLVNull]

instance mbFromSQL Note
where
	mbFromSQL [SQLVText x]	= Just (Note x)
	mbFromSQL _	= Nothing

instance mbFromSQL EmailAddress
where
	mbFromSQL [SQLVText x]	= Just (EmailAddress x)
	mbFromSQL _	= Nothing

instance mbFromSQL DateTime
where
	mbFromSQL [SQLVDatetime {SQLDate|day,month,year} {SQLTime|hour,minute,second}]
		= Just (DateTime {Date|day=day,mon=month,year=year} {Time|hour=hour,min=minute,sec=second})
	mbFromSQL [SQLVText dt]
        = Just (fromString dt)
	mbFromSQL _	= Nothing

instance mbFromSQL Document
where
	mbFromSQL [SQLVText x]  = fromJSON (fromString x)
    mbFromSQL _             = Nothing

instance mbFromSQL Credentials
where
	mbFromSQL [SQLVText x]  = fromJSON (fromString x)
    mbFromSQL _             = Nothing

instance mbToSQL Degrees
where
    mbToSQL (Just (Degrees x)) = [SQLVInteger x]
    mbToSQL Nothing            = [SQLVNull]

instance mbToSQL Feet
where
    mbToSQL (Just (Feet x))   = [SQLVInteger x]
    mbToSQL Nothing           = [SQLVNull]

instance mbToSQL Meters
where
    mbToSQL (Just (Meters x)) = [SQLVInteger x]
    mbToSQL Nothing           = [SQLVNull]

instance mbToSQL Miles
where
    mbToSQL (Just (Miles x))  = [SQLVInteger x]
    mbToSQL Nothing           = [SQLVNull]

instance mbToSQL Temperature
where
    mbToSQL (Just (Temperature x)) = [SQLVInteger x]
    mbToSQL Nothing                = [SQLVNull]

instance mbToSQL Knots
where
    mbToSQL (Just (Knots x))  = [SQLVInteger x]
    mbToSQL Nothing           = [SQLVNull]

instance mbToSQL ContactPosition
where
    mbToSQL (Just (PositionDescription desc Nothing))
        = [SQLVNull,SQLVNull,SQLVText desc]
    mbToSQL (Just (PositionDescription desc (Just (lat,lon))))
        = [SQLVDouble lat,SQLVDouble lon,SQLVText desc]
    mbToSQL (Just (PositionLatLng (lat,lon)))
        = [SQLVDouble lat,SQLVDouble lon,SQLVNull]
    mbToSQL Nothing
        = [SQLVNull,SQLVNull,SQLVNull]

instance mbToSQL ContactTrack
where
	mbToSQL (Just x)  = [SQLVText (toString (toJSON x))]
    mbToSQL Nothing   = [SQLVNull]

instance mbToSQL AIVDMCNB
where
	mbToSQL (Just x)  = [SQLVText (toString (toJSON x))]
    mbToSQL Nothing   = [SQLVNull]

instance mbToSQL AIVDM5
where
	mbToSQL (Just x)  = [SQLVText (toString (toJSON x))]
    mbToSQL Nothing   = [SQLVNull]

instance mbFromSQL ContactPosition
where
    mbFromSQL [SQLVNull,SQLVNull,SQLVText desc]
        = Just (PositionDescription desc Nothing)
    mbFromSQL [SQLVDouble lat,SQLVDouble lon,SQLVText desc]
        = Just (PositionDescription desc (Just (lat,lon)))
    mbFromSQL [SQLVReal lat,SQLVReal lon,SQLVText desc]
        = Just (PositionDescription desc (Just (lat,lon)))
    mbFromSQL [SQLVFloat lat,SQLVFloat lon,SQLVText desc]
        = Just (PositionDescription desc (Just (lat,lon)))

    mbFromSQL [SQLVDouble lat,SQLVDouble lon,SQLVNull]
        = Just (PositionLatLng (lat,lon))
    mbFromSQL [SQLVReal lat,SQLVReal lon,SQLVNull]
        = Just (PositionLatLng (lat,lon))
    mbFromSQL [SQLVFloat lat,SQLVFloat lon,SQLVNull]
        = Just (PositionLatLng (lat,lon))
    mbFromSQL _
        = Nothing

instance mbFromSQL ContactTrack
where
	mbFromSQL [SQLVText x] = fromJSON (fromString x)
    mbFromSQL _ = Nothing

instance mbFromSQL AIVDMCNB
where
	mbFromSQL [SQLVText x] = fromJSON (fromString x)
    mbFromSQL _ = Nothing

instance mbFromSQL AIVDM5
where
	mbFromSQL [SQLVText x]  = fromJSON (fromString x)
    mbFromSQL _             = Nothing

instance mbFromSQL User
where
	mbFromSQL [SQLVText x]  = fromJSON (fromString x)
    mbFromSQL _             = Nothing

instance mbFromSQL Degrees
where
	mbFromSQL [SQLVInteger x]   = Just (Degrees x)
    mbFromSQL [SQLVText x]      = Just (Degrees (toInt x))
    mbFromSQL _                 = Nothing

instance mbFromSQL Feet
where
	mbFromSQL [SQLVInteger x]   = Just (Feet x)
	mbFromSQL [SQLVText x]      = Just (Feet (toInt x))
    mbFromSQL _                 = Nothing

instance mbFromSQL Meters
where
	mbFromSQL [SQLVInteger x]   = Just (Meters x)
	mbFromSQL [SQLVText x]      = Just (Meters (toInt x))
    mbFromSQL _                 = Nothing

instance mbFromSQL Miles
where
	mbFromSQL [SQLVInteger x]   = Just (Miles x)
	mbFromSQL [SQLVText x]      = Just (Miles (toInt x))
    mbFromSQL _                 = Nothing

instance mbFromSQL Temperature
where
	mbFromSQL [SQLVInteger x]   = Just (Temperature x)
	mbFromSQL [SQLVText x]      = Just (Temperature (toInt x))
    mbFromSQL _                 = Nothing

instance mbFromSQL Knots
where
	mbFromSQL [SQLVInteger x]   = Just (Knots x)
	mbFromSQL [SQLVText x]      = Just (Knots (toInt x))
    mbFromSQL _                 = Nothing

columnsCommunication :: ColumnSourceDef
columnsCommunication
    = BaseTable {name="Communication",alias="Communication",columns=["communicationNo","time","type","direction","status","handledBy","withContact"]}

instance mbFromSQL Communication
where
    mbFromSQL [communicationNo,time,type,direction,status,handledBy,withContact] = Just
        {Communication
        |communicationNo=fromSQL [communicationNo]
        ,time=fromSQL [time]
        ,type=fromSQL [type]
        ,direction=fromSQL [direction]
        ,status=mbFromSQL [status]
        ,withContact= mbFromSQL [withContact]
        ,handledBy = mbFromSQL [handledBy]
        ,aboutIncidents= []
        }

columnsCommunicationDetails :: ColumnSourceDef
columnsCommunicationDetails
    = LeftJoin (LeftJoin (LeftJoin (BaseTable
        {name="Communication",alias="Communication",columns=["communicationNo","time","type","direction","status"]})
        {name="PhoneCall",alias="PhoneCall",columns=["externalNo"]} ("Communication","communicationNo") ("PhoneCall","communicationNo"))
        {name="Contact",alias="handledBy_Contact",columns=["contactNo","type","name","group"]} ("Communication","handledBy") ("handledBy_Contact","contactNo"))
        {name="Contact",alias="withContact_Contact",columns=["contactNo","type","name","group"]} ("Communication","withContact") ("withContact_Contact","contactNo")

instance mbFromSQL CommunicationDetails
where
    mbFromSQL [communicationNo,time,type,direction,status,externalNo
              ,handledBy_contactNo,handledBy_type,handledBy_name,handledBy_group
              ,withContact_contactNo,withContact_type,withContact_name,withContact_group
              ] = Just
        {CommunicationDetails
        |communicationNo=fromSQL [communicationNo]
        ,time=fromSQL [time]
        ,type=fromSQL [type]
        ,direction=fromSQL [direction]
        ,status=mbFromSQL [status]
        ,withContact= mbFromSQL [withContact_contactNo,withContact_type,withContact_name,withContact_group]
        ,handledBy = mbFromSQL [handledBy_contactNo,handledBy_type,handledBy_name,handledBy_group]
        ,aboutIncidents= []
        ,externalNo = mbFromSQL [externalNo]
        }

columnsContactShort :: ColumnSourceDef
columnsContactShort
    = BaseTable {name="Contact",alias="Contact",columns=["contactNo","type","name","group"]}

instance mbFromSQL ContactShort
where
    mbFromSQL [contactNo=:SQLVInteger _,type,name,group] = Just
        {ContactShort
        |contactNo=fromSQL [contactNo]
        ,type=mbFromSQL [type]
        ,name=mbFromSQL [name]
        ,group=mbFromSQL [group]
        }
    mbFromSQL _ = Nothing

columnsContactShortWithIncidents :: ColumnSourceDef
columnsContactShortWithIncidents
    = BaseTable {name="Contact",alias="Contact",columns=["contactNo","type","name","group"]}

instance mbFromSQL ContactShortWithIncidents
where
    mbFromSQL [contactNo=:SQLVInteger _,type,name,group] = Just
        {ContactShortWithIncidents
        |contactNo=fromSQL [contactNo]
        ,type=mbFromSQL [type]
        ,name=mbFromSQL [name]
        ,group=mbFromSQL [group]
        ,incidents=[]
        }
    mbFromSQL _ = Nothing

columnsContactGeo :: ColumnSourceDef
columnsContactGeo
    = BaseTable {name="Contact",alias="Contact",columns=["contactNo","type","name","group","needsHelp","providesHelp","position_lat","position_lon","position_desc","heading","track","positionUpdated"]}

instance mbFromSQL ContactGeo
where
    mbFromSQL [contactNo=:SQLVInteger _,type,name,group,needsHelp,providesHelp,position_lat,position_lon,position_desc,heading,track,positionUpdated] = Just
        {ContactGeo
        |contactNo=fromSQL [contactNo]
        ,type=mbFromSQL [type]
        ,name=mbFromSQL [name]
        ,group=mbFromSQL [group]
        ,needsHelp= fromMaybe False (mbFromSQL [needsHelp])
        ,providesHelp= fromMaybe False (mbFromSQL [providesHelp])
        ,position=mbFromSQL [position_lat,position_lon,position_desc]
        ,heading=mbFromSQL [heading]
        ,track=mbFromSQL [track]
        ,positionUpdated=mbFromSQL [positionUpdated]
        }
    mbFromSQL _ = Nothing

columnsPhoneCall :: ColumnSourceDef
columnsPhoneCall
    = BaseTable {name="PhoneCall",alias="PhoneCall",columns=["callNotes","externalNo","externalRef","communicationNo"]}

instance mbFromSQL PhoneCall
where
    mbFromSQL [callNotes,externalNo,externalRef,communicationNo] = Just
        {PhoneCall
        |callNotes = mbFromSQL [callNotes]
        ,externalNo = mbFromSQL [externalNo]
        ,externalRef = mbFromSQL [externalRef]
        ,communicationNo = fromSQL [communicationNo]
        }
instance mbToSQL PhoneCall
where
    mbToSQL (Just {PhoneCall|callNotes,externalNo,externalRef,communicationNo})
        = flatten [mbToSQL callNotes,mbToSQL externalNo,mbToSQL externalRef,toSQL communicationNo]
    mbToSQL Nothing
        = repeatn 4 SQLVNull

columnsRadioCall :: ColumnSourceDef
columnsRadioCall
    = BaseTable {name="RadioCall",alias="RadioCall",columns=["channel","callNotes","communicationNo"]}

instance mbFromSQL RadioCall
where
    mbFromSQL [channel,callNotes,communicationNo] = Just
        {RadioCall
        |channel = mbFromSQL [channel]
        ,callNotes = mbFromSQL [callNotes]
        ,communicationNo = fromSQL [communicationNo]
        }
instance mbToSQL RadioCall
where
    mbToSQL (Just {RadioCall|channel,callNotes,communicationNo})
        = flatten [mbToSQL channel,mbToSQL callNotes,toSQL communicationNo]
    mbToSQL Nothing
        = repeatn 3 SQLVNull

columnsEmailMessage :: ColumnSourceDef
columnsEmailMessage
    = BaseTable {name="EmailMessage",alias="EmailMessage",columns=["recipient","sender","subject","body"]}

instance mbFromSQL EmailMessage
where
    mbFromSQL [recipient,sender,subject,body] = Just
        {EmailMessage
        |recipient=fromSQL [recipient]
        ,sender=fromSQL [sender]
        ,subject=fromSQL [subject]
        ,body=fromSQL [body]
        }

instance mbToSQL EmailMessage
where
    mbToSQL (Just {EmailMessage|recipient,sender,subject,body})
        = flatten [toSQL recipient,toSQL sender,toSQL subject,toSQL body]
    mbToSQL Nothing
        = repeatn 4 SQLVNull

columnsP2000Message :: ColumnSourceDef
columnsP2000Message
    = BaseTable {name="P2000Message",alias="P2000Message",columns=["prio","capCode","body"]}

instance mbFromSQL P2000Message
where
    mbFromSQL [prio,capCode,body] = Just
        {P2000Message
        |prio=fromSQL [prio]
        ,capCode=fromSQL [capCode]
        ,body=fromSQL [body]
        }
instance mbToSQL P2000Message
where
    mbToSQL (Just {P2000Message|prio,capCode,body})
        = toSQL prio ++ toSQL capCode ++ toSQL body
    mbToSQL Nothing
        = repeatn 3 SQLVNull

columnsIncident :: ColumnSourceDef
columnsIncident
    = LeftJoin (BaseTable
         {name="Incident",alias="Incident",columns=["incidentNo","title","summary","type","phase","closed"]})
         {name="WeatherData",alias="WeatherData",columns= ["weatherType","windDirection","windSpeed","visibility","seaState","swellDirection","waveHeight","airTemp","seaTemp","cloudBase","barometric"]} ("Incident","incidentNo") ("WeatherData","incidentNo")
instance mbFromSQL Incident
where
    mbFromSQL
        [incidentNo,title,summary,type,phase,closed
        ,weatherType,windDirection,windSpeed,visibility,seaState,swellDirection,waveHeight,airTemp,seaTemp,cloudBase,barometric
        ] = Just
        {Incident
        |incidentNo = fromSQL [incidentNo]
        ,title = mbFromSQL [title]
        ,summary = mbFromSQL [summary]
        ,type = mbFromSQL [type]
        ,phase = mbFromSQL [phase]
        ,weather = fromSQL [weatherType,windDirection,windSpeed,visibility,seaState,swellDirection,waveHeight,airTemp,seaTemp,cloudBase,barometric]
        ,log = []
        ,closed = fromMaybe False (mbFromSQL [closed])
        ,contacts = []
        ,communications = []
        }
instance mbToSQL Incident
where
    mbToSQL (Just {Incident|incidentNo,title,summary,type,phase,weather,closed})
        = flatten [toSQL incidentNo,mbToSQL title,mbToSQL summary,mbToSQL type, mbToSQL type,toSQL closed] ++ toSQL weather
    mbToSQL Nothing
        = repeatn 17 SQLVNull

columnsIncidentShort :: ColumnSourceDef
columnsIncidentShort
    = BaseTable {name="Incident",alias="Incident",columns=["incidentNo","title"]}

instance mbFromSQL IncidentShort
where
    mbFromSQL [incidentNo,title] = Just
        {IncidentShort
        |incidentNo=fromSQL [incidentNo]
        ,title=mbFromSQL [title]
        }

columnsIncidentDetails :: ColumnSourceDef
columnsIncidentDetails
    = BaseTable {name="Incident",alias="Incident",columns=["incidentNo","title","summary","type","phase"]}

instance mbFromSQL IncidentDetails
where
    mbFromSQL [incidentNo,title,summary,type,phase] = Just
        {IncidentDetails
        |incidentNo=fromSQL [incidentNo]
        ,title=mbFromSQL [title]
        ,summary=mbFromSQL [summary]
        ,type=mbFromSQL [type]
        ,phase=mbFromSQL [phase]
        }

columnsWeatherData :: ColumnSourceDef
columnsWeatherData
    = BaseTable
        {name="WeatherData",alias="WeatherData"
        ,columns=["weatherType","windDirection","windSpeed","visibility","seaState","swellDirection"
                 ,"waveHeight","airTemp","seaTemp","cloudBase","barometric"]}

instance mbFromSQL WeatherData
where
    mbFromSQL [weatherType,windDirection,windSpeed,visibility,seaState,swellDirection,waveHeight,airTemp,seaTemp,cloudBase,barometric] = Just
        {WeatherData
        |weatherType=mbFromSQL [weatherType]
        ,windDirection=mbFromSQL [windDirection]
        ,windSpeed=mbFromSQL [windSpeed]
        ,visibility=mbFromSQL [visibility]
        ,seaState=mbFromSQL [seaState]
        ,swellDirection=mbFromSQL [swellDirection]
        ,waveHeight=mbFromSQL [waveHeight]
        ,airTemp=mbFromSQL [airTemp]
        ,seaTemp=mbFromSQL [seaTemp]
        ,cloudBase=mbFromSQL [cloudBase]
        ,barometric=mbFromSQL [barometric]
        }

instance mbToSQL WeatherData
where
    mbToSQL (Just {WeatherData|weatherType,windDirection,windSpeed,visibility,seaState,swellDirection,waveHeight,airTemp,seaTemp,cloudBase,barometric})
        = flatten [mbToSQL weatherType,mbToSQL windDirection,mbToSQL windSpeed,mbToSQL visibility,mbToSQL seaState,mbToSQL swellDirection
                  ,mbToSQL waveHeight,mbToSQL airTemp,mbToSQL seaTemp,mbToSQL cloudBase,mbToSQL barometric]
    mbToSQL Nothing
        = repeatn 11 SQLVNull

columnsLogEntry :: ColumnSourceDef
columnsLogEntry
    = LeftJoin (BaseTable
        {name="LogEntry",alias="LogEntry",columns=["incident","eventAt","loggedAt","loggedBy","message"]})
        {name="Contact",alias="Contact_loggedBy",columns=["name","type"]} ("LogEntry","loggedBy") ("Contact_loggedBy","contactNo")

instance mbFromSQL LogEntry
where
    mbFromSQL [incident,eventAt,loggedAt,loggedBy,message,name,type] = Just
        {LogEntry
        |incident = fromSQL [incident]
        ,eventAt = fromSQL [eventAt]
        ,loggedAt = fromSQL [loggedAt]
        ,loggedBy = mbFromSQL [loggedBy,name,type]
        ,message = fromSQL [message]
        }
instance mbToSQL LogEntry
where
    mbToSQL (Just {LogEntry|incident,eventAt,loggedAt,loggedBy,message})
        # [loggedBy,name,type:_] = mbToSQL loggedBy
        = flatten [toSQL incident, toSQL eventAt,toSQL loggedAt,[loggedBy],toSQL message,[name],[type]]
    mbToSQL Nothing
        = repeatn 7 SQLVNull

instance mbFromSQL ContactAvatar
where
    mbFromSQL [contactNo=:(SQLVInteger _),name,type] = Just
        {ContactAvatar
        |contactNo=fromSQL [contactNo]
        ,name=mbFromSQL [name]
        ,type=mbFromSQL [type]
        ,photos=[]
        }
    mbFromSQL _ = Nothing

instance mbToSQL ContactAvatar
where
    mbToSQL (Just {ContactAvatar|contactNo,name,type})
        = flatten [toSQL contactNo,mbToSQL name, mbToSQL type]
    mbToSQL Nothing
        = repeatn 3 SQLVNull

columnsContact :: ColumnSourceDef
columnsContact
    = BaseTable
        {name="Contact",alias="Contact"
        ,columns=["contactNo","type","name","group","position_lat","position_lon","position_desc"
                 ,"heading","track","positionUpdated","needsHelp","providesHelp","notes","account","access","status"]}

instance mbFromSQL Contact
where
    mbFromSQL [contactNo,type,name,group,position_lat,position_lon,position_desc,heading,track,positionUpdated,needsHelp,providesHelp,notes,account,access,status] = Just
        { Contact
        | contactNo	            = fromSQL [contactNo]
        , type                  = mbFromSQL [type]
        , name                  = mbFromSQL [name]
        , group                 = mbFromSQL [group]
        , position              = mbFromSQL [position_lat,position_lon,position_desc]
        , heading               = mbFromSQL [heading]
        , track                 = mbFromSQL [track]
        , positionUpdated       = mbFromSQL [positionUpdated]
        , needsHelp             = fromMaybe False (mbFromSQL [needsHelp])
        , providesHelp          = fromMaybe False (mbFromSQL [providesHelp])
        , communicationMeans    = []
        , photos                = []
        , notes                 = mbFromSQL [notes]
        , account               = mbFromSQL [account]
        , access                = mbFromSQL [access]
        , status                = mbFromSQL [status]
        , incidents			    = []
        , communicationsWith	= []
        }

columnsCommunicationMean :: ColumnSourceDef
columnsCommunicationMean
    = LeftJoin (LeftJoin (LeftJoin (LeftJoin (BaseTable
        {name="CommunicationMean",alias="CommunicationMean",columns=["id","type"]})
        {name="P2000Receiver",alias="P2000Receiver",columns=["capCode"]} ("CommunicationMean","id") ("P2000Receiver","id"))
        {name="EmailAccount",alias="EmailAccount",columns=["emailAddress"]} ("CommunicationMean","id") ("EmailAccount","id"))
        {name="VHFRadio",alias="VHFRadio",columns=["callSign","mmsi"]} ("CommunicationMean","id") ("VHFRadio","id"))
        {name="Telephone",alias="Telephone",columns=["phoneNo"]} ("CommunicationMean","id") ("Telephone","id")

instance mbFromSQL CommunicationMean
where
    mbFromSQL [id,type,capCode,emailAddress,callSign,mmsi,phoneNo] = Just
        {CommunicationMean
        |id = fromSQL [id]
        ,type = fromSQL [type]
        ,capCode = mbFromSQL [capCode]
        ,emailAddress = mbFromSQL [emailAddress]
        ,callSign = mbFromSQL [callSign]
        ,mmsi = mbFromSQL [mmsi]
        ,phoneNo = mbFromSQL [phoneNo]
        }

columnsPersonDetails :: ColumnSourceDef
columnsPersonDetails
    = BaseTable {name="Person",alias="Person",columns=["age","gender","nationality","injuries","stateOfMind","medicalHistory"]}

instance mbFromSQL PersonDetails
where
    mbFromSQL [age,gender,nationality,injuries,stateOfMind,medicalHistory] = Just
        {PersonDetails
        |age=mbFromSQL [age]
        ,gender=mbFromSQL [gender]
        ,nationality=mbFromSQL [nationality]
        ,injuries=mbFromSQL [injuries]
        ,stateOfMind=mbFromSQL [stateOfMind]
        ,medicalHistory=mbFromSQL [medicalHistory]
        }
instance mbToSQL PersonDetails
where
    mbToSQL (Just {PersonDetails|age,gender,nationality,injuries,stateOfMind,medicalHistory})
        = flatten [mbToSQL age,mbToSQL gender,mbToSQL nationality,mbToSQL injuries,mbToSQL stateOfMind,mbToSQL medicalHistory]
    mbToSQL Nothing
        = repeatn 5 SQLVNull

columnsVesselDetails :: ColumnSourceDef
columnsVesselDetails
    = BaseTable {name="Vessel",alias="Vessel"
                ,columns=["vesselType","imo","inmarsatNo","description","pob","engineType","fuel"
                         ,"destination","course","speed","lpc","npc","range","lseOnBoard","navaidsOnBoard"]}

instance mbFromSQL VesselDetails
where
    mbFromSQL [vesselType,imo,inmarsatNo,description,pob,engineType,fuel,destination,course,speed,lpc,npc,range,lseOnBoard,navaidsOnBoard] = Just
        {VesselDetails
        |vesselType=mbFromSQL [vesselType]
        ,imo=mbFromSQL [imo]
        ,inmarsatNo=mbFromSQL [inmarsatNo]
        ,description=mbFromSQL [description]
        ,pob=mbFromSQL [pob]
        ,engineType=mbFromSQL [engineType]
        ,fuel=mbFromSQL [fuel]
        ,destination=mbFromSQL [destination]
        ,course=mbFromSQL [course]
        ,speed=mbFromSQL [speed]
        ,lpc=mbFromSQL [lpc]
        ,npc=mbFromSQL [npc]
        ,range=mbFromSQL [range]
        ,lseOnBoard=fromSQL [lseOnBoard]
        ,navaidsOnBoard=fromSQL [navaidsOnBoard]
        }
instance mbToSQL VesselDetails
where
    mbToSQL (Just {VesselDetails|vesselType,imo,inmarsatNo,description,pob,engineType,fuel,destination,course
                  ,speed,lpc,npc,range,lseOnBoard,navaidsOnBoard})
        = flatten [mbToSQL vesselType,mbToSQL imo,mbToSQL inmarsatNo,mbToSQL description,mbToSQL pob
                  ,mbToSQL engineType,mbToSQL fuel,mbToSQL destination,mbToSQL course,mbToSQL speed,mbToSQL lpc
                  ,mbToSQL npc,mbToSQL range,toSQL lseOnBoard,toSQL navaidsOnBoard]
    mbToSQL Nothing
        = repeatn 15 SQLVNull

columnsSurferDetails :: ColumnSourceDef
columnsSurferDetails
    = BaseTable {name="Surfer",alias="Surfer"
                ,columns=["surfboardDescription","suitDescription","age","gender","nationality","injuries","stateOfMind","medicalHistory"]}

instance mbFromSQL SurferDetails
where
    mbFromSQL [surfboardDescription,suitDescription,age,gender,nationality,injuries,stateOfMind,medicalHistory] = Just
        {SurferDetails
        |surfboardDescription=mbFromSQL [surfboardDescription]
        ,suitDescription= mbFromSQL [suitDescription]
        ,age=mbFromSQL [age]
        ,gender=mbFromSQL [gender]
        ,nationality=mbFromSQL [nationality]
        ,injuries=mbFromSQL [injuries]
        ,stateOfMind=mbFromSQL [stateOfMind]
        ,medicalHistory=mbFromSQL [medicalHistory]
        }
instance mbToSQL SurferDetails
where
    mbToSQL (Just {SurferDetails|surfboardDescription,suitDescription,age,gender,nationality,injuries,stateOfMind,medicalHistory})
        = flatten [mbToSQL surfboardDescription,mbToSQL suitDescription,mbToSQL age,mbToSQL gender,mbToSQL nationality
                  ,mbToSQL injuries,mbToSQL stateOfMind,mbToSQL medicalHistory]
    mbToSQL Nothing
        = repeatn 8 SQLVNull

columnsDiverDetails :: ColumnSourceDef
columnsDiverDetails
    = BaseTable {name="Diver",alias="Diver",columns=["description"]}

instance mbFromSQL DiverDetails
where
    mbFromSQL [description] = Just
        {DiverDetails
        |description=mbFromSQL [description]
        }
instance mbToSQL DiverDetails
where
    mbToSQL (Just {DiverDetails|description})
        = flatten [mbToSQL description]
    mbToSQL Nothing
        = repeatn 1 SQLVNull

columnsAirplaneDetails :: ColumnSourceDef
columnsAirplaneDetails
    = BaseTable {name="Airplane",alias="Airplane",columns=["callsign","planeType"]}

instance mbFromSQL AirplaneDetails
where
    mbFromSQL [callsign,planeType] = Just
        {AirplaneDetails
        |callsign=mbFromSQL [callsign]
        ,planeType=mbFromSQL [planeType]
        }
instance mbToSQL AirplaneDetails
where
    mbToSQL (Just {AirplaneDetails|callsign,planeType})
        = flatten [mbToSQL callsign,mbToSQL planeType]
    mbToSQL Nothing
        = repeatn 2 SQLVNull

columnsHelicopterDetails :: ColumnSourceDef
columnsHelicopterDetails
    = BaseTable {name="Helicopter",alias="Helicopter",columns=["callsign","helicopterType"]}

instance mbFromSQL HelicopterDetails
where
    mbFromSQL [callsign,helicopterType] = Just
        {HelicopterDetails
        |callsign = mbFromSQL [callsign]
        ,helicopterType = mbFromSQL [helicopterType]
        }
instance mbToSQL HelicopterDetails
where
    mbToSQL (Just {HelicopterDetails|callsign,helicopterType})
        = flatten [mbToSQL callsign,mbToSQL helicopterType]
    mbToSQL Nothing
        = repeatn 2 SQLVNull

columnsAISContact :: ColumnSourceDef
columnsAISContact
    = BaseTable {name="AISContact",alias="AISContact"
                ,columns=["mmsi","position_lat","position_lon","position_desc","heading","track","lastPositionMsg","lastInfoMsg","positionUpdated","infoUpdated"]}

instance mbFromSQL AISContact
where
    mbFromSQL [mmsi,position_lat,position_lon,position_desc,heading,track,lastPositionMsg,lastInfoMsg,positionUpdated,infoUpdated] = Just
        {AISContact
        |mmsi = fromSQL [mmsi]
        ,position = mbFromSQL [position_lat,position_lon,position_desc]
        ,heading = mbFromSQL [heading]
        ,track = mbFromSQL [track]
        ,lastPositionMsg = mbFromSQL [lastPositionMsg]
        ,lastInfoMsg = mbFromSQL [lastInfoMsg]
        ,positionUpdated = mbFromSQL [positionUpdated]
        ,infoUpdated = mbFromSQL [infoUpdated]
        }

instance mbFromSQL [SQLValue]
where
    mbFromSQL row = Just row

