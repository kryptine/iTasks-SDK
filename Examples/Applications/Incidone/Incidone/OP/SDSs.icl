implementation module Incidone.OP.SDSs

import iTasks, iTasks.API.Extensions.SQLDatabase
import qualified Data.Map as DM
import Incidone.Configuration
import Incidone.OP.Concepts, Incidone.OP.Conversions
import Incidone.Util.SQLSDS

derive class iTask ContactFilter

dbReadSDS :: String -> ROShared QueryDef [r] | mbFromSQL r
dbReadSDS notifyId = databaseDef >++> sqlReadSDS notifyId

dbReadWriteOneSDS :: String -> RWShared QueryDef r r | mbFromSQL, mbToSQL r & gDefault{|*|} r
dbReadWriteOneSDS notifyId = databaseDef >++> sqlReadWriteOneSDS notifyId

dbLinkSDS :: String String String String -> RWShared (Maybe [Int]) [(Int,Int)] [(Int,Int)]
dbLinkSDS notifyId table col1 col2 = databaseDef >++> sqlLinkSDS notifyId table col1 col2

//ACCESS SDS

allContactPhotos :: Shared (Map ContactNo [ContactPhoto]) //TODO: Also store in database
allContactPhotos = sharedStore "ContactPhotos" 'DM'.newMap

lastAISImport :: Shared (Maybe (DateTime,String,Int))
lastAISImport = sharedStore "lastAISImport" Nothing

allCommunications :: ROShared () [CommunicationDetails]
allCommunications = sdsFocus Nothing filteredCommunications

filteredCommunications :: ROShared (Maybe RowFilterDef) [CommunicationDetails]
filteredCommunications
    = mapRead (\(communication,aboutIncidents) -> map (addAboutIncidentsToCommunication aboutIncidents) communication)
                        (filteredCommunicationsBase |+| sdsFocus () communicationAboutIncidentsIndexed)
where
    addAboutIncidentsToCommunication aboutIncidents communication=:{CommunicationDetails|communicationNo}
        = {CommunicationDetails|communication
          &aboutIncidents= fromMaybe [] ('DM'.get communicationNo aboutIncidents)
          }
    communicationAboutIncidentsIndexed = mapRead groupByFst communicationAboutIncidents

filteredCommunicationsBase :: ROShared (Maybe RowFilterDef) [CommunicationDetails]
filteredCommunicationsBase = sdsTranslate "filteredCommunicationsBase" query (dbReadSDS "filteredCommunicationsBase")
where
    query rows = {columns = columnsCommunicationDetails, rows = rows , order = Just [OrderDesc ("Communication","communicationNo")]}

communicationAboutIncidents :: ROShared () [(CommunicationNo,IncidentShort)]
communicationAboutIncidents = mapRead (map fromSQLWithId) (sdsFocus query (dbReadSDS "communicationAboutIncidents"))
where
 query =
    { columns    = InnerJoin columnsIncidentShort
                    {name="communications_aboutIncidents",alias="communications_aboutIncidents",columns=["aboutIncidents"]}
                    ("Incident","incidentNo") ("communications_aboutIncidents","communications")
    , rows       = Nothing
    , order      = Nothing
    }

communicationByNo :: RWShared CommunicationNo Communication Communication
communicationByNo = mapReadWrite (readPrj,writePrj)
    (communicationByNoBase
     >+< sdsTranslate "communicationByNoIncidents" (\p -> Just [p]) incidentNosByCommunicationNosIndexed)
where
	readPrj (communication=:{Communication|communicationNo},ilinks)
        = {Communication
          |communication
          & aboutIncidents = fromMaybe [] ('DM'.get communicationNo ilinks)
          }
	writePrj communication=:{Communication|communicationNo,aboutIncidents} (_,ilinks)
		= Just (communication,'DM'.put communicationNo aboutIncidents ilinks)
	writePrj _ _ = Nothing

communicationByNoBase :: RWShared CommunicationNo Communication Communication
communicationByNoBase = databaseDef >++> sqlShare "communicationByNo" readFun writeFun
where
    query communicationNo
        = {columns=columnsCommunication,rows=Just (EqualsValue ("Communication","communicationNo") (toSQL communicationNo)), order = Nothing}

	readFun communicationNo cur
        # (sql,vals)        = toReadSQL (query communicationNo)
        # (err,cur)         = execute sql vals cur
		| isJust err		= (Error (toString (fromJust err)),cur)
		# (err,mbRow,cur)	= fetchOne cur
		| isJust err		= (Error (toString (fromJust err)),cur)
        | isNothing mbRow   = (Error ("Could not find communication number " +++ toString communicationNo),cur)
		= (Ok (fromSQL (fromJust mbRow)), cur)

	writeFun communicationNo {Communication|time,type,direction,status,handledBy,withContact} cur
		//Update Contact table
		# (err,cur) = execute "UPDATE Communication SET `time` = ?, `type` = ?, `direction` = ?, `status` = ?, `handledBy` = ?, `withContact` = ? WHERE `communicationNo` = ?"
				(flatten
                [toSQL time
				,toSQL type
                ,toSQL direction
                ,mbToSQL status
                ,mbToSQL handledBy
				,mbToSQL withContact
				,toSQL   communicationNo
				]) cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		                = (Ok (), cur)

communicationDetailsByNo :: RWShared CommunicationNo CommunicationDetails CommunicationDetails
communicationDetailsByNo = sdsParallel "communicationDetailsByNo" param read (SDSWriteConst writel) (SDSWriteConst writer) communicationDetailsByNoBase incidentsByCommunicationShort
where
    param p = (p,p)
    read (communication,aboutIncidents) = {CommunicationDetails|communication & aboutIncidents = aboutIncidents}
    writel _ communication = Ok (Just communication)
    writer _ communication = Ok (Just [incidentNo \\ {IncidentShort|incidentNo} <- communication.CommunicationDetails.aboutIncidents])

communicationDetailsByNoBase ::  RWShared CommunicationNo CommunicationDetails CommunicationDetails
communicationDetailsByNoBase = databaseDef >++> sqlShare "communicationByNo" readFun writeFun
where
    //TODO use a write query that does multiple table updates
    query communicationNo = {columns=columnsCommunicationDetails,rows=Just (EqualsValue ("Communication","communicationNo") [SQLVInteger communicationNo]),order=Nothing}

	readFun communicationNo cur
        # (sql,vals)        = toReadSQL (query communicationNo)
        # (err,cur)         = execute sql vals cur
		| isJust err		= (Error (toString (fromJust err)),cur)
		# (err,mbRow,cur)	= fetchOne cur
		| isJust err		= (Error (toString (fromJust err)),cur)
        | isNothing mbRow   = (Error ("Communication number " <+++ communicationNo <+++ " could not be found"),cur)
	                    	= (Ok (fromSQL (fromJust mbRow)), cur)

	writeFun communicationNo (communication=:{CommunicationDetails|time,type,direction,status,withContact,handledBy,externalNo}) cur
		//Update Contact table
		# (err,cur) = execute "UPDATE Communication SET time = ?, type = ?, direction = ?, status = ?, withContact = ?, handledBy = ? WHERE communicationNo = ?"
				(flatten
                [toSQL time
				,toSQL type
				,toSQL direction
				,mbToSQL status
				,mbToSQL (fmap (\{ContactShort|contactNo}->contactNo) withContact)
				,mbToSQL (fmap (\{ContactShort|contactNo}->contactNo) handledBy)
				,toSQL communicationNo
				]) cur
		| isJust err = (Error (toString (fromJust err)),cur)
        = case type of
            PhoneCall
		//Brute force upsert, try insert, if it fails, try update
                //# (err,cur) = execute "INSERT INTO PhoneCall (communicationNo,externalNo) VALUES (?,?) ON DUPLICATE KEY UPDATE externalNo = VALUES(externalNo)"
                # (err,cur) = execute "INSERT INTO PhoneCall (communicationNo,externalNo) VALUES (?,?)"
                    (flatten [toSQL communicationNo,mbToSQL externalNo]) cur
				| isJust err
                	# (err,cur) = execute "UPDATE PhoneCall SET externalNo = ? WHERE communicationNo = ?" 
                    	(flatten [mbToSQL externalNo,toSQL communicationNo]) cur
			        | isJust err  = (Error (toString (fromJust err)),cur)
			        = (Ok (), cur)
			    = (Ok (), cur)
            _
		        = (Ok (), cur)

phoneCallByNo :: RWShared CommunicationNo PhoneCall PhoneCall
phoneCallByNo = sdsTranslate "phoneCallByNo" query (dbReadWriteOneSDS "phoneCallByNo")
where
    query communicationNo = {columns=columnsPhoneCall,rows=Just (EqualsValue ("PhoneCall","communicationNo") [SQLVInteger communicationNo]),order=Nothing}

phoneCallByReference :: RWShared PhoneCallReference PhoneCall PhoneCall
phoneCallByReference = sdsTranslate "phoneCallByReference" query (dbReadWriteOneSDS "phoneCallByReference")
where
    query ref = {columns=columnsPhoneCall,rows=Just (EqualsValue ("PhoneCall","externalRef") [SQLVText ref]),order=Nothing}

radioCallByNo :: RWShared CommunicationNo RadioCall RadioCall
radioCallByNo = sdsTranslate "radioCallByNo" query (dbReadWriteOneSDS "radioCallByNo")
where
    query communicationNo = {columns=columnsRadioCall,rows=Just (EqualsValue ("RadioCall","communicationNo") [SQLVInteger communicationNo]),order=Nothing}

emailMessageByNo :: RWShared CommunicationNo EmailMessage EmailMessage
emailMessageByNo = sdsTranslate "emailMessageByNo" query (dbReadWriteOneSDS "emailMessageByNo")
where
    query communicationNo = {columns=columnsEmailMessage,rows=Just (EqualsValue ("EmailMessage","communicationNo") [SQLVInteger communicationNo]),order=Nothing}

p2000MessageByNo :: RWShared CommunicationNo P2000Message P2000Message
p2000MessageByNo = sdsTranslate "p2000MessageByNo" query (dbReadWriteOneSDS "P2000MessageByNo")
where
    query communicationNo = {columns=columnsP2000Message,rows=Just (EqualsValue ("P2000Message","communicationNo") [SQLVInteger communicationNo]),order=Nothing}

allIncidents :: ROShared () [Incident]
allIncidents = filteredIncidents Nothing

filteredIncidents :: (Maybe RowFilterDef) -> ReadOnlyShared [Incident]
filteredIncidents mbWhere = mapRead prj (baseIncidents mbWhere |+| sdsFocus Nothing contactNosByIncidentNosIndexed |+| sdsFocus Nothing communicationNosByIncidentNosIndexed)
where
	prj ((incidents,cnlinks),cmlinks) = map (addLinks cnlinks cmlinks) incidents

	addLinks cnlinks cmlinks incident=:{Incident|incidentNo}
		= {Incident
		  | incident
		  & contacts = fromMaybe [] ('DM'.get incidentNo cnlinks)
		  , communications = fromMaybe [] ('DM'.get incidentNo cmlinks)
		  }

detailsIncidents :: (Maybe RowFilterDef) -> ReadOnlyShared [IncidentDetails]
detailsIncidents mbWhere = mapRead (map prj) (baseIncidents mbWhere)
where	
    prj {Incident|incidentNo,title,summary,type,phase}
      = {IncidentDetails|incidentNo=incidentNo,title=title,summary=summary,type=type,phase=phase}

baseIncidents :: (Maybe RowFilterDef) -> ReadOnlyShared [Incident]
baseIncidents rows = sdsFocus query (dbReadSDS "allIncidents")
where
 query = {columns = columnsIncident, rows = rows, order = Nothing}

filteredIncidentsShort :: ROShared (Maybe RowFilterDef) [IncidentShort]
filteredIncidentsShort = sdsTranslate "filteredIncidentsShort" query (dbReadSDS "allIncidents")
where
    query rows = {columns = columnsIncidentShort, rows = rows, order = Nothing}

allIncidentsShort :: ROShared () [IncidentShort]
allIncidentsShort = sdsFocus Nothing filteredIncidentsShort

openIncidents :: ROShared () [Incident]
openIncidents =	filteredIncidents (Just openIncidentsCond)

openIncidentsShort :: ROShared () [IncidentShort]
openIncidentsShort = sdsFocus (Just openIncidentsCond) filteredIncidentsShort

openIncidentsDetails ::	ROShared () [IncidentDetails]
openIncidentsDetails = detailsIncidents (Just openIncidentsCond)

openIncidentsCond :: RowFilterDef
openIncidentsCond = OrCondition (EqualsValue ("Incident","closed") [SQLVInteger 0]) (EqualsNull ("Incident","closed"))

closedIncidentsCond :: RowFilterDef
closedIncidentsCond = EqualsValue ("Incident","closed") [SQLVInteger 1]

recentIncidents :: ROShared () [Incident]
recentIncidents = filteredIncidents (Just closedIncidentsCond)

recentIncidentsDetails :: ROShared () [IncidentDetails]
recentIncidentsDetails = detailsIncidents (Just closedIncidentsCond)

incidentsByContactShort	:: RWShared ContactNo [IncidentShort] [IncidentNo]
incidentsByContactShort = databaseDef>++> sqlShare "incidentsByContact" readFun writeFun
where
    query contactNo =
        {columns = InnerJoin columnsIncidentShort {name="contacts_incidents",alias="contacts_incidents",columns=[]} ("Incident","incidentNo") ("contacts_incidents","contacts")
        ,rows    = Just (EqualsValue ("contacts_incidents","incidents") [SQLVInteger contactNo])
        ,order = Nothing}

	readFun contactNo cur
        # (sql,vals) = toReadSQL (query contactNo)
        # (res,cur) = execSelect sql vals cur
		= case res of
			Error e	= (Error e,cur)
			Ok rows = (Ok (map fromSQL rows),cur)

	writeFun contactNo links cur
		//Unlink old
		# (err,cur) 	= execute "DELETE FROM contacts_incidents WHERE incidents = ? " (toSQL contactNo) cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		//Relink new
		# (err,cur)		= executeMany "INSERT INTO contacts_incidents (contacts,incidents) VALUES (?,?)"
							[toSQL incidentNo ++ toSQL contactNo \\ incidentNo <- links] cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		= (Ok (),cur)

incidentsByContactDetails :: RWShared ContactNo [IncidentDetails] [IncidentNo]
incidentsByContactDetails = databaseDef  >++> sqlShare "incidentsByContact" readFun writeFun
where
    query contactNo =
        {columns = InnerJoin columnsIncidentDetails {name="contacts_incidents",alias="contacts_incidents",columns=[]}
                ("Incident","incidentNo") ("contacts_incidents","contacts")
        ,rows = Just (EqualsValue ("contacts_incidents","incidents") [SQLVInteger contactNo])
        ,order = Nothing
        }
    readFun contactNo cur
        # (sql,vals) = toReadSQL (query contactNo)
        # (res,cur) = execSelect sql vals cur
		= case res of
			Error e	= (Error e,cur)
			Ok rows = (Ok (map fromSQL rows),cur)

	writeFun contactNo links cur
		//Unlink old
		# (err,cur) 	= execute "DELETE FROM contacts_incidents WHERE incidents = ? " (toSQL contactNo) cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		//Relink new
		# (err,cur)		= executeMany "INSERT INTO contacts_incidents (contacts,incidents) VALUES (?,?)"
							[toSQL incidentNo ++ toSQL contactNo \\ incidentNo <- links] cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		= (Ok (),cur)

incidentsByCommunicationShort :: RWShared CommunicationNo [IncidentShort] [IncidentNo]
incidentsByCommunicationShort = databaseDef >++> sqlShare "incidentsByCommunication" readFun writeFun
where
    columns = InnerJoin columnsIncidentShort {name="communications_aboutIncidents",alias="communications_aboutIncidents",columns=[]}
                ("Incident","incidentNo") ("communications_aboutIncidents","communications")
    rows communicationNo
            = Just (EqualsValue ("communications_aboutIncidents","aboutIncidents") [SQLVInteger communicationNo])

	readFun communicationNo cur
        # (sql,vals) = toReadSQL {columns=columns,rows=rows communicationNo,order=Nothing}
        # (res,cur) = execSelect sql vals cur
		= case res of
			Error e	= (Error e,cur)
			Ok rows = (Ok (map fromSQL rows),cur)

	writeFun communicationNo links cur
		//Unlink old
		# (err,cur) 	= execute "DELETE FROM communications_aboutIncidents WHERE aboutIncidents = ? " (toSQL communicationNo) cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		//Relink new
		# (err,cur)		= executeMany "INSERT INTO communications_aboutIncidents (communications,aboutIncidents) VALUES (?,?)"
							[toSQL incidentNo ++ toSQL communicationNo \\ incidentNo <- links] cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		= (Ok (),cur)

incidentsByNosShort :: ROShared [IncidentNo] [IncidentShort]
incidentsByNosShort = sdsTranslate "incidentsByNosShort" cond filteredIncidentsShort
where
    cond []  = Just (EqualsValue ("Incident","incidentNo") [SQLVInteger 0]) //Don't match anythig
    cond nos = Just (EqualsValue ("Incident","incidentNo") (map SQLVInteger nos))

incidentByNo :: RWShared IncidentNo Incident Incident
incidentByNo = mapReadWrite (readPrj,writePrj)
    (incidentByNoBase
     >+< (sdsTranslate "incidentByNoContacts" (\p -> Just [p]) contactNosByIncidentNosIndexed)
     >+< (sdsTranslate "incidentByNoCommunications" (\p -> Just [p]) communicationNosByIncidentNosIndexed)
     >+| incidentLog)
where
	readPrj (((incident,cnlinks),cmlinks),log)
        = {Incident
		  | incident
		  & contacts = fromMaybe [] ('DM'.get incident.Incident.incidentNo cnlinks)
		  , communications = fromMaybe [] ('DM'.get incident.Incident.incidentNo cmlinks)
          , log = log
		  }
	
	writePrj (incident=:{Incident|incidentNo,contacts,communications}) (((_,cnlinks),cmlinks),_)
		= Just ((incident,'DM'.put incidentNo contacts cnlinks),'DM'.put incidentNo communications cmlinks)
	writePrj _ _ = Nothing

incidentByNoBase :: RWShared IncidentNo Incident Incident
incidentByNoBase = databaseDef >++> sqlShare "incidentByNo" readFun writeFun
where
    query incidentNo = {columns=columnsIncident,rows=Just (EqualsValue ("Incident","incidentNo") [SQLVInteger incidentNo]),order=Nothing}

	readFun incidentNo cur
        # (sql,vals)        = toReadSQL (query incidentNo)
		# (err,cur)			= execute sql vals cur
		| isJust err		= (Error (toString (fromJust err)),cur)
		# (err,mbRow,cur)	= fetchOne cur
		| isJust err		= (Error (toString (fromJust err)),cur)
		| isNothing mbRow   = (Error ("Could not find incident number " +++ toString incidentNo),cur)
		= (Ok (fromSQL (fromJust mbRow)), cur)

	writeFun incidentNo incident=:{Incident|title,summary,type,phase,closed} cur
		//Update Incident table
		# (err,cur) = execute "UPDATE Incident SET title = ?, summary = ?, type = ?, phase = ?, closed = ? WHERE incidentNo = ?"
			(flatten
            [mbToSQL title
			,mbToSQL summary
			,mbToSQL type
			,mbToSQL phase
			,toSQL closed
			,toSQL incidentNo
			]) cur
		| isJust err	= (Error (toString (fromJust err)),cur)
						= (Ok (),cur)

incidentTitleByNo :: RWShared IncidentNo String String
incidentTitleByNo = sdsTranslate "incidentTitleByNo" query (dbReadWriteOneSDS "incidentTitleByNo")
where
    query incidentNo = {columns=columnsIncidentTitle,rows=Just (EqualsValue ("Incident","incidentNo") [SQLVInteger incidentNo]),order=Nothing}
    columnsIncidentTitle = BaseTable {name="Incident",alias="Incident",columns=["title"]}

incidentWeather :: RWShared IncidentNo WeatherData WeatherData
incidentWeather = sdsTranslate "incidentWeather" query (dbReadWriteOneSDS "incidentByWeather")
where
    query incidentNo = {columns=columnsWeatherData,rows=Just (EqualsValue ("WeatherData","incidentNo") [SQLVInteger incidentNo]),order=Nothing}

incidentLog :: RWShared IncidentNo [LogEntry] LogEntry
incidentLog = sdsParallel "incidentLog" param read (SDSWriteConst (\_ w -> Ok (Just w))) (SDSWriteConst (\_ _ -> Ok Nothing)) incidentLogBase allContactPhotos
where
    param p = (p,())
    read (logEntries,photos) = [{LogEntry|e & loggedBy = fmap (addAvatarPhotos photos) e.LogEntry.loggedBy} \\ e <- logEntries]
    addAvatarPhotos photos a=:{ContactAvatar|contactNo} = {ContactAvatar|a & photos = fromMaybe [] ('DM'.get contactNo photos)}

incidentLogBase :: RWShared IncidentNo [LogEntry] LogEntry
incidentLogBase = databaseDef >++> sqlShare "incidentLog" readFun writeFun
where
    query incidentNo = {columns=columnsLogEntry,rows=Just (EqualsValue ("LogEntry","incident") [SQLVInteger incidentNo]),order = Just [OrderDesc ("LogEntry","loggedAt")]}

    readFun incidentNo cur
        # (sql,vals) = toReadSQL (query incidentNo)
        # (res,cur) = execSelect sql vals cur
        = case res of
            Error e	= (Error e, cur)
            Ok rows = (Ok (map fromSQL rows),cur)

    writeFun incidentNo entry=:{LogEntry|eventAt,loggedAt,loggedBy,message} cur
        # (_,cur) = execInsert "INSERT INTO LogEntry (incident,eventAt,loggedAt,loggedBy,message) VALUES (?,?,?,?,?)"
            (take 5 (toSQL entry)) cur
        = (Ok (),cur)

incidentOverview :: ROShared IncidentNo IncidentOverview
incidentOverview = mapRead prj (incidentByNo |+| contactsByIncident)
where
    prj ({Incident|title,summary,type},contacts)
        = {IncidentOverview|title=title,summary=summary,type=type
          ,contactsNeedingHelp=[{ContactNameTypePosition|name=name,type=type,position=position} \\ {Contact|name,type,position,needsHelp} <- contacts | needsHelp]
        }

contactNosByIncidentNosIndexed :: RWShared (Maybe [IncidentNo]) (Map IncidentNo [ContactNo]) (Map IncidentNo [ContactNo])
contactNosByIncidentNosIndexed = mapReadWrite (groupByFst,\w _ -> Just (ungroupByFst w)) contactNosByIncidentNos

contactNosByIncidentNos :: RWShared (Maybe [IncidentNo]) [(IncidentNo,ContactNo)] [(IncidentNo,ContactNo)]
contactNosByIncidentNos = dbLinkSDS "contactNosByIncidentNos" "contacts_incidents" "contacts" "incidents"

communicationNosByIncidentNosIndexed :: RWShared (Maybe [IncidentNo]) (Map IncidentNo [CommunicationNo]) (Map IncidentNo [CommunicationNo])
communicationNosByIncidentNosIndexed = mapReadWrite (groupByFst,\w _ -> Just (ungroupByFst w)) communicationNosByIncidentNos

communicationNosByIncidentNos :: RWShared (Maybe [IncidentNo]) [(IncidentNo,CommunicationNo)] [(IncidentNo,CommunicationNo)]
communicationNosByIncidentNos = dbLinkSDS "communicationNosByIncidentNos" "communications_aboutIncidents" "communications" "aboutIncidents"

allContacts :: ROShared () [Contact]
allContacts = sdsFocus Nothing filteredContacts

filteredContacts :: ROShared (Maybe RowFilterDef) [Contact]
filteredContacts = mapRead prj
    (    allContactsBase
     |+| sdsFocus Nothing incidentNosByContactNosIndexed
     |+| sdsFocus Nothing communicationNosByContactNosIndexed
     |+| sdsFocus () allContactPhotos)
where
	prj (((contacts,ilinks),clinks),photos) = [addPhotos photos (addLinks ilinks clinks c) \\ c <- contacts]

	addLinks ilinks clinks contact=:{Contact|contactNo}
		= {Contact
		  |contact
		  & incidents			= fromMaybe [] ('DM'.get contactNo ilinks)
		  , communicationsWith	= fromMaybe [] ('DM'.get contactNo clinks)
		  }
    addPhotos photos contact=:{Contact|contactNo}
        = {Contact
          |contact
          & photos              = fromMaybe [] ('DM'.get contactNo photos)
          }

allContactsBase :: ROShared (Maybe RowFilterDef) [Contact]
allContactsBase = sdsTranslate "allContactsBase" query (dbReadSDS "allContacts")
where
    query rows =
       { columns = columnsContact
       , rows = rows
       , order = Just [OrderAsc ("Contact","name")]
       }

sqlFilteredContactsShort :: ROShared (Maybe RowFilterDef) [ContactShort]
sqlFilteredContactsShort = sdsTranslate "sqlFilteredContactsShort" query (dbReadSDS "allContacts")
where
    query rows =
        { columns   = columnsContactShort
        , rows      = rows
        , order     = Just [OrderAsc ("Contact","name")]
        }

filteredContactsGeo :: ROShared (Maybe RowFilterDef) [ContactGeo]
filteredContactsGeo = sdsTranslate "filteredContactsGeo" query (dbReadSDS "allContacts")
where
    query rows =
        { columns   = columnsContactGeo
        , rows      = rows
        , order     = Just [OrderAsc ("Contact","name")]
        }

allContactsShort :: ROShared () [ContactShort]
allContactsShort = sdsFocus Nothing sqlFilteredContactsShort

filteredContactsShort :: ROShared ContactFilter [ContactShort]
filteredContactsShort = sdsTranslate "filteredContactsShort" param sqlFilteredContactsShort
where
    param {filterByName=Just name}  = Just (LikeValue ("Contact","name") (name+++"%"))
    param _                         = Nothing

contactsWithGroupShort :: ROShared String [ContactShort]
contactsWithGroupShort = sdsTranslate "contactsWithGroupShort" query sqlFilteredContactsShort
where
    query group = Just (EqualsValue ("Contact","group") (toSQL group))

contactsOfOpenIncidentsShort :: ROShared () [ContactShortWithIncidents]
contactsOfOpenIncidentsShort = sdsSequence "contactsOfOpenIncidentsShort" param read writel writer contactsOfOpenIncidentsShortBase openIncidentsByContactsShortIndexed
where
    writel = SDSWriteConst (\_ _ -> Ok Nothing)
    writer = SDSWriteConst (\_ _ -> Ok Nothing)
    param _ contacts = [contactNo \\ {ContactShortWithIncidents|contactNo} <- contacts]
    read (contacts,incidents) = [{ContactShortWithIncidents|c & incidents = fromMaybe [] ('DM'.get contactNo incidents)}
                                \\ c=:{ContactShortWithIncidents|contactNo} <- contacts]

    openIncidentsByContactsShortIndexed = mapRead groupByFst openIncidentsByContactsShort

contactsOfOpenIncidentsShortBase :: ROShared () [ContactShortWithIncidents]
contactsOfOpenIncidentsShortBase = sdsFocus query (dbReadSDS "contactsOfOpenIncidentsShort")
where
/*
    query =
        { columns   = LeftJoin (RightJoin columnsContactShortWithIncidents
                        {name="contacts_incidents",alias="contacts_incidents",columns=[]} ("contacts_incidents","incidents") ("Contact","contactNo"))
                        {name="Incident",alias="Incident",columns=[]} ("contacts_incidents","contacts") ("Incident","incidentNo")
        , rows      = Just (OrCondition (EqualsValue ("Incident","closed") [SQLVInteger 0]) (EqualsNull ("Incident","closed")))
        , order     = Nothing
        }
*/
    //PREVENT RIGHT JOIN BECAUSE OF SQLITE
    query
      # (BaseTable tblContactCols) = columnsContactShortWithIncidents
      = { columns   = LeftJoin (LeftJoin (BaseTable {name="Incident",alias="Incident",columns=[]})
                    {name="contacts_incidents",alias="contacts_incidents",columns=[]} ("Incident","incidentNo") ("contacts_incidents","contacts"))
                    tblContactCols ("contacts_incidents","incidents") ("Contact","contactNo")
        , rows      = Just (AndCondition (NotCondition (EqualsNull ("Contact","contactNo"))) (OrCondition (EqualsValue ("Incident","closed") [SQLVInteger 0]) (EqualsNull ("Incident","closed"))))
        , order     = Nothing
        }
contactsOfOpenIncidentsGeo :: ROShared () [ContactGeo]
contactsOfOpenIncidentsGeo = sdsFocus query (dbReadSDS "contactsOfOpenIncidentsGeo")
where
    /*
    query =
        { columns   = LeftJoin (RightJoin columnsContactGeo
                        {name="contacts_incidents",alias="contacts_incidents",columns=[]} ("contacts_incidents","incidents") ("Contact","contactNo"))
                        {name="Incident",alias="Incident",columns=[]} ("contacts_incidents","contacts") ("Incident","incidentNo")
        , rows      = Just (OrCondition (EqualsValue ("Incident","closed") [SQLVInteger 0]) (EqualsNull ("Incident","closed")))
        , order     = Nothing
        }
    */
    //PREVENT RIGHT JOIN BECAUSE OF SQLITE
    query
      # (BaseTable tblContactCols) = columnsContactGeo
      = { columns   = LeftJoin (LeftJoin (BaseTable {name="Incident",alias="Incident",columns=[]})
                    {name="contacts_incidents",alias="contacts_incidents",columns=[]} ("Incident","incidentNo") ("contacts_incidents","contacts"))
                    tblContactCols ("contacts_incidents","incidents") ("Contact","contactNo")
        , rows      = Just (AndCondition (NotCondition (EqualsNull ("Contact","contactNo"))) (OrCondition (EqualsValue ("Incident","closed") [SQLVInteger 0]) (EqualsNull ("Incident","closed"))))
        , order     = Nothing
        }

openIncidentsByContactsShort :: ROShared [ContactNo] [(ContactNo,IncidentShort)]
openIncidentsByContactsShort = databaseDef >++> sqlShare "openIncidentsByContacts" readFun writeFun
where
    query contactNo =
        {columns = InnerJoin columnsIncidentShort {name="contacts_incidents",alias="contacts_incidents",columns=["incidents"]} ("Incident","incidentNo") ("contacts_incidents","contacts")
        ,rows    = Just (AndCondition
            (EqualsValue ("contacts_incidents","incidents") (map SQLVInteger contactNo))
            (OrCondition (EqualsValue ("Incident","closed") [SQLVInteger 0]) (EqualsNull ("Incident","closed")))
            )
        ,order   = Nothing
        }

	readFun contactNo cur
        # (sql,vals) = toReadSQL (query contactNo)
        # (res,cur) = execSelect sql vals cur
		= case res of
			Error e	= (Error e,cur)
			Ok rows = (Ok (map fromSQLWithId rows),cur)

	writeFun contactNo _ cur
		= (Ok (),cur)

contactsOfOpenIncidents :: ROShared () [Contact]
contactsOfOpenIncidents = sdsFocus query (dbReadSDS "contactsOfOpenIncidents") //TODO: Add incidents and communications fields, and use select distinct
where
 /*
 query =
    { columns   = LeftJoin (RightJoin columnsContact
                    {name="contacts_incidents",alias="contacts_incidents",columns=[]} ("contacts_incidents","incidents") ("Contact","contactNo"))
                    {name="Incident",alias="Incident",columns=[]} ("contacts_incidents","contacts") ("Incident","incidentNo")
    , rows      = Just (OrCondition (EqualsValue ("Incident","closed") [SQLVInteger 0]) (EqualsNull ("Incident","closed")))
    , order     = Just [OrderAsc ("Contact","name")]
    }
*/
    //PREVENT RIGHT JOIN BECAUSE OF SQLITE
    query
      # (BaseTable tblContactCols) = columnsContact
      = { columns   = LeftJoin (LeftJoin (BaseTable {name="Incident",alias="Incident",columns=[]})
                    {name="contacts_incidents",alias="contacts_incidents",columns=[]} ("Incident","incidentNo") ("contacts_incidents","contacts"))
                    tblContactCols ("contacts_incidents","incidents") ("Contact","contactNo")
        , rows      = Just (AndCondition (NotCondition (EqualsNull ("Contact","contactNo"))) (OrCondition (EqualsValue ("Incident","closed") [SQLVInteger 0]) (EqualsNull ("Incident","closed"))))
        , order     = Just [OrderAsc ("Contact","name")]
        }

contactsNeedingHelpShort :: ROShared () [ContactShort]
contactsNeedingHelpShort = sdsTranslate "contactsNeedingHelpShort" query sqlFilteredContactsShort
where
    query group = Just (EqualsValue ("Contact","needsHelp") (toSQL True))

contactsProvidingHelpShort :: ROShared () [ContactShort]
contactsProvidingHelpShort = sdsTranslate "contactsProvidingHelpShort" query sqlFilteredContactsShort
where
    query group = Just (EqualsValue ("Contact","providesHelp") (toSQL True))

contactsProvidingHelpGeo :: ROShared () [ContactGeo]
contactsProvidingHelpGeo = sdsTranslate "contactsProvidingHelpGeo" query filteredContactsGeo
where
    query group = Just (EqualsValue ("Contact","providesHelp") (toSQL True))

contactsByNos :: ROShared [ContactNo] [Contact]
contactsByNos = sdsTranslate "contactsByNos" cond filteredContacts
where
    cond []  = Just (EqualsValue ("Contact","contactNo") [SQLVInteger 0]) //Don't match anythig
    cond nos = Just (EqualsValue ("Contact","contactNo") (map SQLVInteger nos))

contactsByNosShort :: ROShared [ContactNo] [ContactShort]
contactsByNosShort = sdsTranslate "contactsByNosShort" cond sqlFilteredContactsShort
where
    cond []  = Just (EqualsValue ("Contact","contactNo") [SQLVInteger 0]) //Don't match anythig
    cond nos = Just (EqualsValue ("Contact","contactNo") (map SQLVInteger nos))


contactsByIncident :: RWShared IncidentNo [Contact] [ContactNo]
contactsByIncident = databaseDef >++> sqlShare "allContacts" readFun writeFun
where
    query incidentNo =
        {columns = InnerJoin columnsContact {name="contacts_incidents",alias="contacts_incidents",columns=[]} ("Contact","contactNo") ("contacts_incidents","incidents")
        ,rows    = Just (EqualsValue ("contacts_incidents","contacts") [SQLVInteger incidentNo])
        ,order   = Just [OrderAsc ("Contact","name")]
        }

	readFun incidentNo cur
        # (sql,vals) = toReadSQL (query incidentNo)
        # (res,cur) = execSelect sql vals cur
		= case res of
			Error e	= (Error e,cur)
			Ok rows = (Ok (map fromSQL rows),cur)

	writeFun incidentNo links cur
		//Unlink old
		# (err,cur) 	= execute "DELETE FROM contacts_incidents WHERE contacts = ? " (toSQL incidentNo) cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		//Relink new
		# (err,cur)		= executeMany "INSERT INTO contacts_incidents (contacts,incidents) VALUES (?,?)"
							[toSQL incidentNo ++ toSQL contactNo \\ contactNo <- links] cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		= (Ok (),cur)

contactsByIncidentShort	:: RWShared IncidentNo [ContactShort] [ContactNo]
contactsByIncidentShort = databaseDef >++> sqlShare "contactsByIncidentShort" readFun writeFun
where
    query incidentNo =
        {columns = InnerJoin columnsContactShort {name="contacts_incidents",alias="contacts_incidents",columns=[]} ("Contact","contactNo") ("contacts_incidents","incidents")
        ,rows    = Just (EqualsValue ("contacts_incidents","contacts") [SQLVInteger incidentNo])
        ,order   = Just [OrderAsc ("Contact","name")]
        }

	readFun incidentNo cur
        # (sql,vals) = toReadSQL (query incidentNo)
        # (res,cur)  = execSelect sql vals cur
		= case res of
			Error e	= (Error e,cur)
			Ok rows = (Ok (map fromSQL rows),cur)

	writeFun incidentNo links cur
		//Unlink old
		# (err,cur) 	= execute "DELETE FROM contacts_incidents WHERE contacts = ? " (toSQL incidentNo) cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		//Relink new
		# (err,cur)		= executeMany "INSERT INTO contacts_incidents (contacts,incidents) VALUES (?,?)"
							[toSQL incidentNo ++ toSQL contactNo \\ contactNo <- links] cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		= (Ok (),cur)

contactsByIncidentGeo :: ROShared IncidentNo [ContactGeo]
contactsByIncidentGeo = sdsTranslate "contactsByIncidentGeo" query (dbReadSDS "contactsByIncidentShort")
where
    query incidentNo =
        {columns = InnerJoin columnsContactGeo {name="contacts_incidents",alias="contacts_incidents",columns=[]} ("Contact","contactNo") ("contacts_incidents","incidents")
        ,rows    = Just (EqualsValue ("contacts_incidents","contacts") [SQLVInteger incidentNo])
        ,order   = Just [OrderAsc ("Contact","name")]
        }

contactByNo :: RWShared ContactNo Contact Contact
contactByNo = mapReadWrite (readPrj,writePrj)
    (contactByNoBase
     >+< sdsTranslate "contactByNoIncident" (\p -> Just [p]) incidentNosByContactNosIndexed
     >+< sdsTranslate "contactByNoCommunications" (\p -> Just [p]) communicationNosByContactNosIndexed
     >+| contactPhotos)
where
	readPrj (((contact,ilinks),clinks),photos) = (addPhotos photos o addLinks ilinks clinks) contact

	addLinks ilinks clinks contact=:{Contact|contactNo}
		= {Contact
		  |contact
		  & incidents			= fromMaybe [] ('DM'.get contactNo ilinks)
		  , communicationsWith	= fromMaybe [] ('DM'.get contactNo clinks)
		  }
    addPhotos photos contact=:{Contact|contactNo}
        = {Contact
          |contact
          & photos              = photos
          }

	writePrj (contact=:{Contact|contactNo,incidents,communicationsWith}) (((_,ilinks),clinks),_)
		= Just ((contact,'DM'.put contactNo incidents ilinks),'DM'.put contactNo communicationsWith clinks)
	writePrj _ _ = Nothing

contactByNoBase :: RWShared ContactNo Contact Contact
contactByNoBase = databaseDef >++> sqlShare "contactByNo" readFun writeFun
where
    query contactNo
        = {columns=columnsContact,rows=Just (EqualsValue ("Contact","contactNo") (toSQL contactNo)), order = Nothing}

	readFun contactNo cur
        # (sql,vals)        = toReadSQL (query contactNo)
        # (err,cur)         = execute sql vals cur
		| isJust err		= (Error (toString (fromJust err)),cur)
		# (err,mbRow,cur)	= fetchOne cur
		| isJust err		= (Error (toString (fromJust err)),cur)
        | isNothing mbRow   = (Error ("Could not find contact number " +++ toString contactNo),cur)
		= (Ok (fromSQL (fromJust mbRow)), cur)

	writeFun contactNo {Contact|type,name,group,position,heading,track,positionUpdated,needsHelp,providesHelp,notes,account,access,status} cur
		//Update Contact table
		# (err,cur) = execute "UPDATE Contact SET `type` = ?, `name` = ?, `group` = ?, `position_lat` = ?, `position_lon` = ?, `position_desc` = ?, `heading` = ?, `track` = ?, `positionUpdated` = ?, `needsHelp` = ?, `providesHelp` = ?, `notes` = ?, `account` = ?, `access` = ?, `status` = ? WHERE `contactNo` = ?"
				(flatten
                [mbToSQL type
				,mbToSQL name
                ,mbToSQL group
                ,mbToSQL position
				,mbToSQL heading
				,mbToSQL track
				,mbToSQL positionUpdated
				,toSQL   needsHelp
                ,toSQL   providesHelp
				,mbToSQL notes
                ,mbToSQL account
                ,mbToSQL access
                ,mbToSQL status
				,toSQL   contactNo
				]) cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		                = (Ok (), cur)

contactByMMSI :: RWShared MMSI (Maybe Contact) (Maybe Contact)
contactByMMSI = databaseDef >++> sqlShare "contactByMMSI" readFun writeFun
where
    //Find the first contact that has a VHFRadio communication mean with matching mmsi
    /*
    query mmsi =
        { columns = RightJoin (RightJoin columnsContact
            {name="communicationMeans1_communicationMeans2",alias="communicationMeans1_communicationMeans2",columns=[]} ("Contact","contactNo") ("communicationMeans1_communicationMeans2","communicationMeans2"))
            {name="VHFRadio",alias="VHFRadio",columns=[]} ("communicationMeans1_communicationMeans2","communicationMeans1") ("VHFRadio","id")
        , rows = Just (EqualsValue ("VHFRadio","mmsi") [SQLVInteger mmsi])
        , order = Nothing
        }
    */
    //PREVENT RIGHT JOIN BECAUSE OF SQLITE
    query mmsi
      # (BaseTable tblContactCols) = columnsContact
      = { columns   = LeftJoin (LeftJoin (BaseTable {name="VHFRadio",alias="VHFRadio",columns=[]})
                        {name="communicationMeans1_communicationMeans2",alias="communicationMeans1_communicationMeans2",columns=[]} ("communicationMeans1_communicationMeans2","communicationMeans1") ("VHFRadio","id"))
                        tblContactCols ("communicationMeans1_communicationMeans2","communicationMeans2") ("Contact","contactNo")
        , rows      = Just (AndCondition (NotCondition (EqualsNull ("Contact","contactNo"))) (EqualsValue ("VHFRadio","mmsi") [SQLVInteger mmsi]))
        , order     = Nothing
        }
	readFun mmsi cur
        # (sql,vals)        = toReadSQL (query mmsi)
        # (err,cur)         = execute sql vals cur
		| isJust err		= (Error (toString (fromJust err)),cur)
		# (err,mbRow,cur)	= fetchOne cur
		| isJust err		= (Error (toString (fromJust err)),cur)
		= (Ok (fmap fromSQL mbRow), cur)
		
	writeFun mmsi Nothing cur = (Ok (), cur) //Only write on Just
	writeFun mmsi (Just contact=:{Contact|contactNo,type,name,group,position,heading,track,positionUpdated,needsHelp,providesHelp,notes,status}) cur
	    //Update contact info
	    # (err,cur) = execute "UPDATE Contact SET `type` = ?, `name` = ?, `group` = ?, `position_lat` = ?,`position_lon` = ?, `position_desc` = ?, `heading` = ?, `track` = ?, `positionUpdated` = ?, `needsHelp` = ?, `providesHelp` = ?, `notes` = ?, `status` = ? WHERE `contactNo` = ?"
	        (flatten
            [mbToSQL type
            ,mbToSQL name
            ,mbToSQL group
            ,mbToSQL position
		    ,mbToSQL heading
            ,mbToSQL track
            ,mbToSQL positionUpdated
		    ,toSQL   needsHelp
		    ,toSQL   providesHelp
		    ,mbToSQL notes
            ,mbToSQL status
		    ,toSQL   contactNo
		    ]) cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		    = (Ok (),cur)

contactByCredentials :: ROShared Credentials (Maybe Contact)
contactByCredentials = mapRead listToMaybe (sdsTranslate "contactByCredentials" query (dbReadSDS "contactByCredentials"))
where
 query credentials =
    { columns   = columnsContact
    , rows      = Just (EqualsValue ("Contact","account") (toSQL credentials))
    , order     = Nothing
    }


contactCommunicationMeans :: ROShared ContactNo [CommunicationMean]
contactCommunicationMeans = sdsTranslate "contactCommunicationMeans" query (dbReadSDS "allCommunicationMeans")
where
    query contactNo = {columns=columns,rows=rows contactNo,order = Nothing}
    rows contactNo = Just (EqualsValue ("communicationMeans1_communicationMeans2","communicationMeans2") [SQLVInteger contactNo])
    columns = InnerJoin columnsCommunicationMean 
                {name="communicationMeans1_communicationMeans2",alias="communicationMeans1_communicationMeans2",columns=[]}
                ("communicationMeans1_communicationMeans2","communicationMeans1") ("CommunicationMean","id")

communicationMeanById :: RWShared CommunicationMeanId CommunicationMean CommunicationMean
communicationMeanById = databaseDef >++> sqlShare "communicationMeanById" readFun writeFun
where
    readFun id cur
        # (sql,match)       = toReadSQL {columns=columnsCommunicationMean,rows=Just (EqualsValue ("CommunicationMean","id") [SQLVInteger id]),order=Nothing}
	    # (err,cur)			= execute sql match cur
		| isJust err		= (Error (toString (fromJust err)),cur)
		# (err,mbRow,cur)	= fetchOne cur
		| isJust err		= (Error (toString (fromJust err)),cur)
		                    = (Ok (maybe defaultValue fromSQL mbRow), cur)

    writeFun id mean=:{CommunicationMean|type,phoneNo,callSign,mmsi,emailAddress,capCode} cur
        # (query,args) = case type of
           CMPhone  = ("UPDATE Telephone SET phoneNo = ? WHERE id = ?",mbToSQL phoneNo ++ toSQL id)
           CMVHF    = ("UPDATE VHFRadio SET callSign = ?, mmsi = ? WHERE id = ?",mbToSQL callSign ++ mbToSQL mmsi ++ toSQL id)
           CMEmail  = ("UPDATE EmailAccount SET emailAddress = ? WHERE id = ?",mbToSQL emailAddress ++ toSQL id)
           CMP2000  = ("UPDATE P2000Receiver SET capCode = ? WHERE id = ?",mbToSQL capCode ++ toSQL id)
        = case execute query args cur of
            (Just e, cur) = (Error (toString e), cur)
            (_, cur)      = (Ok (), cur)

contactMMSI :: ROShared ContactNo (Maybe MMSI)
contactMMSI = mapRead toPrj contactCommunicationMeans
where
    toPrj means = case [mmsi \\{CommunicationMean|type=CMVHF,mmsi=Just mmsi} <- means] of
        [mmsi:_]    = Just mmsi
        _           = Nothing

contactAIS :: ROShared ContactNo (Maybe AISContact)
contactAIS = sdsSequence "contactAIS" (\_ mbMMSI -> mbMMSI) snd (SDSWriteConst (\_ _ -> Ok Nothing)) (SDSWriteConst (\_ w -> Ok (Just w))) contactMMSI (roMaybe (toReadOnly AISContactByMMSI))

contactCommunications :: ROShared ContactNo [CommunicationDetails]
contactCommunications = sdsTranslate "contactCommunications" cond filteredCommunications
where
    cond contactNo = Just (OrCondition (EqualsValue ("Communication","withContact") [SQLVInteger contactNo])
                            (EqualsValue ("Communication","handledBy") [SQLVInteger contactNo]))

contactPhotos :: RWShared ContactNo [ContactPhoto] [ContactPhoto]
contactPhotos = sdsSplit "contactPhotos" param read write allContactPhotos
where
    param p             = ((),p)
    read p all          = fromMaybe [] ('DM'.get p all)
    write p all photos  = ('DM'.put p photos all, (==) p)

contactAccess :: RWShared ContactNo ContactAccess ContactAccess
contactAccess = mapReadWrite (read,write) contactByNoBase
where
    read {Contact|account,access} = {ContactAccess|account=account,access=access}
    write {ContactAccess|account,access} contact = Just {Contact|contact & account = account, access=access}

contactAvatar :: ROShared ContactNo ContactAvatar
contactAvatar = mapRead toAvatar (toReadOnly contactByNo)
where
    toAvatar {Contact|contactNo,name,type,photos=photos} = {ContactAvatar|contactNo=contactNo,name=name,type=type,photos=photos}

personDetailsByNo :: RWShared ContactNo PersonDetails PersonDetails
personDetailsByNo = sdsTranslate "personDetailsByNo" query (dbReadWriteOneSDS "personDetailsByNo")
where
    query contactNo = {columns=columnsPersonDetails,rows=Just (EqualsValue ("Person","contactNo") [SQLVInteger contactNo]), order=Nothing}

vesselDetailsByNo :: RWShared ContactNo VesselDetails VesselDetails
vesselDetailsByNo = sdsTranslate "vesselDetailsByNo" query (dbReadWriteOneSDS "vesselDetailsByNo")
where
    query contactNo = {columns=columnsVesselDetails,rows=Just (EqualsValue ("Vessel","contactNo") [SQLVInteger contactNo]), order=Nothing}

surferDetailsByNo :: RWShared ContactNo SurferDetails SurferDetails
surferDetailsByNo = sdsTranslate "surferDetailsByNo" query (dbReadWriteOneSDS "surferDetailsByNo")
where
    query contactNo = {columns=columnsDiverDetails,rows=Just (EqualsValue ("Surfer","contactNo") [SQLVInteger contactNo]), order=Nothing}

diverDetailsByNo :: RWShared ContactNo DiverDetails DiverDetails
diverDetailsByNo = sdsTranslate "diverDetailsByNo" query (dbReadWriteOneSDS "diverDetailsByNo")
where
    query contactNo = {columns=columnsDiverDetails,rows=Just (EqualsValue ("Diver","contactNo") [SQLVInteger contactNo]), order=Nothing}

airplaneDetailsByNo :: RWShared ContactNo AirplaneDetails AirplaneDetails
airplaneDetailsByNo = sdsTranslate "airplaneDetailsByNo" query (dbReadWriteOneSDS "airplaneDetailsByNo")
where
    query contactNo = {columns=columnsAirplaneDetails,rows=Just (EqualsValue ("Airplane","contactNo") [SQLVInteger contactNo]), order=Nothing}

helicopterDetailsByNo :: RWShared ContactNo HelicopterDetails HelicopterDetails 
helicopterDetailsByNo = sdsTranslate "helicopterDetailsByNo" query (dbReadWriteOneSDS "helicopterDetailsByNo")
where
    query contactNo = {columns=columnsHelicopterDetails,rows=Just (EqualsValue ("Helicopter","contactNo") [SQLVInteger contactNo]), order=Nothing}

incidentNosByContactNosIndexed :: RWShared (Maybe [ContactNo]) (Map ContactNo [IncidentNo]) (Map ContactNo [IncidentNo])
incidentNosByContactNosIndexed = mapReadWrite (groupByFst,\w _ -> Just (ungroupByFst w)) incidentNosByContactNos

incidentNosByContactNos :: RWShared (Maybe [ContactNo]) [(ContactNo,IncidentNo)] [(ContactNo,IncidentNo)]
incidentNosByContactNos = dbLinkSDS "incidentNosByContactNos" "contacts_incidents" "incidents" "contacts"

incidentNosByCommunicationNosIndexed :: RWShared (Maybe [CommunicationNo]) (Map CommunicationNo [IncidentNo]) (Map CommunicationNo [IncidentNo])
incidentNosByCommunicationNosIndexed = mapReadWrite (groupByFst,\w _ -> Just (ungroupByFst w)) incidentNosByCommunicationNos

incidentNosByCommunicationNos :: RWShared (Maybe [CommunicationNo]) [(CommunicationNo,IncidentNo)] [(CommunicationNo,IncidentNo)]
incidentNosByCommunicationNos = dbLinkSDS "incidentNosByCommunicationNos" "communications_aboutIncidents" "aboutIncidents" "communications"

communicationNosByContactNosIndexed :: RWShared (Maybe [ContactNo]) (Map ContactNo [CommunicationNo]) (Map ContactNo [CommunicationNo])
communicationNosByContactNosIndexed = mapReadWrite (groupByFst,\w _ -> Just (ungroupByFst w)) communicationNosByContactNos

communicationNosByContactNos :: RWShared (Maybe [ContactNo]) [(ContactNo,CommunicationNo)] [(ContactNo,CommunicationNo)]
communicationNosByContactNos = sdsTranslate "communicationNosByContactNos" (const ()) (sharedStore "FIXME" [])
//communicationNosByContactNos = dbLinkSDS "communicationNosByContactNos" "Communication" "communicationNo" "withContact"
//TODO: This set should be merged with the "handledBy" relation betweeen communications and contacts

sqlFilteredAISContacts :: ROShared (Maybe RowFilterDef) [AISContact]
sqlFilteredAISContacts = sdsTranslate "sqlFilteredAISContacts" query (dbReadSDS "allAISContacts")
where
    query rows =
        { columns   = columnsAISContact
        , rows      = rows
        , order     = Nothing
        }

allAISContacts :: ROShared () [AISContact]
allAISContacts = sdsFocus Nothing sqlFilteredAISContacts

boundedAISContacts :: ROShared ContactBounds [AISContact]
boundedAISContacts = sdsTranslate "boundedAISContacts" query sqlFilteredAISContacts  //TODO: Filter by bounds
where
    query ((latmin,lonmin),(latmax,lonmax))
        = Just (AndCondition
            (AndCondition (GreaterEqualsValue ("AISContact","position_lat") (SQLVReal latmin))
                             (SmallerEqualsValue ("AISContact","position_lat") (SQLVReal latmax)))
            (AndCondition (GreaterEqualsValue ("AISContact","position_lon") (SQLVReal lonmin))
                             (SmallerEqualsValue ("AISContact","position_lon") (SQLVReal lonmax)))
            )

AISContactByMMSI :: RWShared MMSI (Maybe AISContact) (Maybe AISContact)
AISContactByMMSI = databaseDef >++> sqlShare "allAISContacts" readFun writeFun
where
    readFun mmsi cur
        # (sql,match)   = toReadSQL {columns = columnsAISContact, rows = Just (EqualsValue ("AISContact","mmsi") [SQLVInteger mmsi]), order = Nothing}
        # (res,cur)     = execSelect sql match cur
        = case res of
			Error e	    = (Error e,cur)
            Ok []       = (Ok Nothing, cur)
            Ok [row:_]  = (Ok (Just (fromSQL row)),cur)

    writeFun mmsi Nothing cur
        # (res,cur) = execDelete "DELETE FROM AISContact WHERE mmsi = ?" (toSQL mmsi) cur
        = case res of
			Error e	    = (Error e,cur)
            _           = (Ok (), cur)

    writeFun mmsi (Just {AISContact|position,heading,track,lastPositionMsg,lastInfoMsg,positionUpdated,infoUpdated}) cur
		//Brute force upsert, try insert, if it fails, try update
        # (res,cur) = execInsert "INSERT INTO AISContact (mmsi,position_lat,position_lon,position_desc,heading,track,lastPositionMsg,lastInfoMsg,positionUpdated,infoUpdated) VALUES (?,?,?,?,?,?,?,?,?,?)" 
                        (flatten [toSQL mmsi, mbToSQL position, mbToSQL heading, mbToSQL track, mbToSQL lastPositionMsg, mbToSQL lastInfoMsg,mbToSQL positionUpdated, mbToSQL infoUpdated]) cur

		| res=:(Error _) //Try update
        	# (mbErr,cur) = execute "UPDATE AISContact SET position_lat = ?, position_lon = ?, position_desc = ?, heading = ?, track = ?, lastPositionMsg = ?, lastInfoMsg = ?, positionUpdated = ?, infoUpdated = ? WHERE mmsi = ?"
				(flatten [mbToSQL position, mbToSQL heading, mbToSQL track, mbToSQL lastPositionMsg, mbToSQL lastInfoMsg,mbToSQL positionUpdated, mbToSQL infoUpdated,toSQL mmsi]) cur
		| mbErr=:(Just _) = (Error (toString (fromJust mbErr)),cur)
            	= (Ok (), cur)
        = (Ok (),cur)

currentUserAvatar :: ROShared () (Maybe ContactAvatar)
currentUserAvatar = sdsSequence "userContactNo" (\_ u -> userContactNo u) snd writel writer currentUser (roMaybe (mapRead Just (toReadOnly contactAvatar)))
where
    writel = SDSWriteConst (\_ _ -> Ok Nothing)
    writer = SDSWriteConst (\_ _ -> Ok Nothing)

currentUserContactNo :: ROShared () ContactNo
currentUserContactNo = mapReadError (\u -> maybe (Error (exception "User has no contact no")) Ok (userContactNo u)) (toReadOnly currentUser)

