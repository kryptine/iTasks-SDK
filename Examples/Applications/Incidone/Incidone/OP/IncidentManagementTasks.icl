implementation module Incidone.OP.IncidentManagementTasks
import iTasks, iTasks.Internal.HtmlUtil, iTasks.Extensions.SQLDatabase
import iTasks.Extensions.DateTime
import Incidone.Configuration
import Incidone.OP.Concepts, Incidone.OP.SDSs, Incidone.OP.Conversions
import Incidone.OP.ContactManagementTasks
import Incidone.Util.TaskPatterns
import Incidone.Util.Differences
import Incidone.Util.Notification
import Incidone.DeviceBased.VideoWall
import Incidone.ActionManagementTasks
import Data.List, Data.Either
import qualified Data.Map as DM
import Text.HTML

openIncidentInWorkspace :: Workspace IncidentNo -> Task ()
openIncidentInWorkspace ws incidentNo
    =   addOnceToWorkspace ("incident-"<+++incidentNo) (doOrClose (manageIncidentInformation ws incidentNo)) ws @! ()

//View and add/update all information known about the incident
//Summary in body, and special parts in separate detached tasks
manageIncidentInformation :: Workspace IncidentNo  -> Task ()
manageIncidentInformation ws incidentNo
	=	withHeader (viewSharedTitle (sdsFocus incidentNo incidentTitleByNo))
		(parallel
		    [(Embedded, \_ -> manageIncidentSituationInfo incidentNo )
		    ,(Embedded, \_ -> manageIncidentContacts ws incidentNo)
		    ,(Embedded, \_ -> manageIncidentActions incidentNo)
		    ,(Embedded, \_ -> manageIncidentWeather incidentNo)
		    ,(Embedded, \_ -> manageIncidentLog incidentNo)
		    ] [] <<@ ArrangeWithTabs False)
    @! ()

//Basic incident information (title, summary, type, phase etc..)
manageIncidentSituationInfo :: IncidentNo -> Task ()
manageIncidentSituationInfo incidentNo
    =	viewOrEdit (Icon "basic-information","General","The following general information is known about the situation") situation log
    >^* [OnAction (Action "/Share to wall") (always (shareIncident incidentNo))
        ,OnAction (Action "/Close incident") (always (confirmCloseIncident incidentNo <<@ InWindow))
        ]
where
    situation = mapReadWrite (toPrj,fromPrj) (sdsFocus incidentNo incidentByNo)
    where
        toPrj {Incident|title,summary,type,phase}
	        = {IncidentBasic|title,summary,type,phase}
        fromPrj {IncidentBasic|title,summary,type,phase} incident
	        = Just {Incident|incident&title = title, summary = summary, type = type, phase = phase}

    log = logIncidentBasicsUpdated incidentNo

    shareIncident incidentNo = set (WallIncidentSummary (Just incidentNo)) wallContent @! ()
    confirmCloseIncident incidentNo
        =  viewInformation ("Close incident","Are you sure you want to close this incident?") [] ()
        >>* [OnAction ActionYes (always (closeIncident incidentNo))
            ,OnAction ActionNo (always (return ()))
            ]

manageIncidentContacts :: Workspace IncidentNo -> Task ()
manageIncidentContacts ws incidentNo
    =   manageContacts <<@ (Icon "contacts") <<@ (Title "Involved Contacts")
    @! ()
where
	contacts        = sdsFocus incidentNo contactsByIncident
	manageContacts  = ((withShared Nothing (\sel -> (chooseFromList sel -||- chooseFromMap sel) <<@ (ArrangeWithSideBar 0 LeftSide 250 True))))
                    >^* [(OnAction (Action "/Add contact") (always (add <<@ InWindow @! ())))
                        ,(OnAction (Action "/Remove contact") (ifValue (\c -> c=:(Left _)) (\(Left c) -> (remove c <<@ InWindow @! ()))))
                        ,(OnAction (Action "/Update position") (ifValue (\c -> c=:(Left _)) (\(Left c) -> updateContactPosition c <<@ InWindow @! ())))
                        ,(OnAction (Action "/Update status") (ifValue (\c -> c=:(Left _)) (\(Left c) -> updateContactStatus c <<@ InWindow @! ())))
                        ,(OnAction (Action "/Open contact") (ifValue (\c -> c=:(Left _)) (\(Left c) -> openContactInWorkspace ws c)))
                        ]
    chooseFromList sel = editSharedChoiceWithSharedAs () [ChooseFromList listView] contacts (Left o contactIdentity) sel
    chooseFromMap sel = viewContactsOnMap (sdsFocus incidentNo contactsByIncidentGeo) sel

    listView c=:{Contact|name,type,status,photos}
        = ">" <+++ type <+++ ": " <+++ name <+++ " (" <+++ status <+++ ")"

    add	= oneOrAnother (Title "Add contact..")
            ("Known contact",enterChoiceWithSharedAs () [ChooseFromDropdown id] allContactsShort contactNo)
            ("Add new contact",enterInformation () [])
		>>? \contact ->
            createContactIfNew contact
            >>- \contactNo ->
		        upd (\cs -> map contactIdentity cs ++ [contactNo]) contacts
            >>| logContactAdded incidentNo contactNo
    where
        contactNo {ContactShort|contactNo} = contactNo

        createContactIfNew (Left no) = return no
        createContactIfNew (Right c) = createContact c

	remove sel
        = viewSharedInformation ("Remove contact from incident","Are your sure you want to remove this contact?") [] (mapRead contactTitle (sdsFocus sel contactByNo)) //TODO: Create contactTitle share
	    >>* [OnAction ActionNo (always (return ()))
	        ,OnAction ActionYes (always (upd (\cs -> [c \\ c <- map contactIdentity cs | c <> sel]) contacts >>| logContactRemoved incidentNo sel))
		    ]

manageIncidentActions :: IncidentNo -> Task ()
manageIncidentActions incidentNo
	=	selectAndWorkOnPlannedActions
    >^* [OnAction (Action "/Add action") (always (addTopActionItem [] [incidentNo]))]
	@!  ()
where
    selectAndWorkOnPlannedActions
        = (feedForward (chooseActionItem (Title "Overview") False True (sdsFocus incidentNo actionStatusesByIncident) /* <<@ AfterLayout (tweakUI fill) */)
        (\s -> whileUnchanged s
            (\t -> case t of
              Just taskId    = workOnActionItem taskId
              Nothing        = viewInformation () [] ()
            )
        )) <<@ (ArrangeWithSideBar 0 LeftSide 250 True) <<@ (Icon "actions") <<@ (Title "Incident Actions") //FIXME

manageIncidentWeather :: IncidentNo -> Task ()
manageIncidentWeather incidentNo
    =   (get webLinksConfig
    >>- \webConfig -> case webConfig.weatherWidgets of
        Just widgets = (viewWebWeather widgets ||- viewOrEdit (Title "Weather on scene") weather log) <<@ (ArrangeWithSideBar 0 RightSide 300 True)
        Nothing      = viewOrEdit (Title "Weather on scene") weather log

    ) <<@ Title "Weather" <<@ Icon "weather"
where
    weather = sdsFocus incidentNo incidentWeather
    log     = logIncidentWeatherUpdated incidentNo
    viewWebWeather widgets = viewInformation (Title "Web weather info") [] (Html widgets)

manageIncidentLog :: IncidentNo -> Task ()
manageIncidentLog incidentNo
    =     addMessages incidentNo
    ||-   viewIncidentLog incidentNo <<@ ArrangeWithSideBar 0 TopSide 100 False <<@ Title "Log" <<@ Icon "Log"
    @! ()
where
    viewIncidentLog :: IncidentNo -> Task [LogEntry]
    viewIncidentLog incident = viewSharedInformation () [ViewAs toView] (sdsFocus incidentNo incidentLog)
    where
        toView log = DivTag [ClassAttr "incident-log"] (flatten [[vizDate date:map vizEntry entries] \\ (date,entries) <- groupByDate log])

        vizDate date = H2Tag [ClassAttr "incident-log-date"] [Text (toString date)]
        vizEntry entry = DivTag [ClassAttr "incident-log-entry"]
                                (vizAvatar entry.loggedBy ++ vizName entry.loggedBy ++ vizTime entry.eventAt ++ vizMessage entry.message)
        vizName (Just {ContactAvatar|name=Just name})
            = [DivTag [ClassAttr "incident-log-name"] [Text name]]
        vizName _
            = [DivTag [ClassAttr "incident-log-name"] [Text "System message"]]
        vizAvatar (Just {ContactAvatar|photos=[p:_]})
            = [DivTag [ClassAttr "incident-log-avatar"] [ImgTag [SrcAttr p.ContactPhoto.avatar.contentUrl,HeightAttr "50",WidthAttr "50"]]]
        vizAvatar _ = []
        vizTime (datetime) = [DivTag [ClassAttr "incident-log-time"] [Text (toString (toTime datetime))]]
        vizMessage message = [DivTag [ClassAttr "incident-log-message"] [nl2br (toString message)]]

        groupByDate log = [(toDate e.eventAt,es) \\ es=:[e:_] <-  groupBy (\e1 e2 -> toDate e1.eventAt == toDate e2.eventAt) log]

    addMessages incidentNo = forever
        (   enterInformation () [] @ string
        >>* [OnAction (Action "Add log message") (hasValue (\msg -> addLogMessage msg incidentNo))]
        )

	string :: String -> String
	string x = x

viewIncidentDetails :: IncidentNo -> Task ()
viewIncidentDetails incidentNo
	= withHeader (viewSharedTitle (sdsFocus incidentNo incidentTitleByNo))
	    (viewSharedInformation () [] (mapRead incidentDetails incident)) //TODO: Create a more efficient share for the details
	@! ()
where
    incident = sdsFocus incidentNo incidentByNo

updateSharedIncidentRefList :: d Bool (RWShared () [IncidentNo] [IncidentNo]) -> Task [IncidentNo] | toPrompt d
updateSharedIncidentRefList d compact refs
    =   manageCurrentItems
    >^* [OnAction (Action "Add") (always (addItem <<@ InWindow))]
where
    manageCurrentItems
        = updateSharedInformation d [UpdateAs toPrj fromPrj] items @ map incidentIdentity
    where
        items = sdsDeref refs id incidentsByNosShort (\_ is -> is)
        toPrj l = [(incidentIdentity i,incidentTitle i) \\i <-l]
        fromPrj _ items = map fst items

    addItem
        =   selectKnownOrDefineNewIncident
        >>? (\def -> createIncidentIfNew def >>- \incidentNo -> upd (\r -> r ++ [incidentNo]) refs)

selectKnownOrDefineNewIncident :: Task (Either IncidentNo NewIncident)
selectKnownOrDefineNewIncident
    = oneOrAnother ("Add incident...","You can either select an open incident, or define a new one.")
        ("Known incident", chooseKnownIncident)
        ("Add new incident",enterNewIncident)
where
    chooseKnownIncident
        = enterChoiceWithSharedAs () [ChooseFromDropdown id] openIncidentsShort incidentIdentity
    enterNewIncident
        = enterInformation () []

createIncidentIfNew :: (Either IncidentNo NewIncident) -> Task IncidentNo
createIncidentIfNew (Left no) = return no
createIncidentIfNew (Right incident) = createIncident incident

addLogMessage :: msg IncidentNo -> Task IncidentNo | toString msg
addLogMessage message incidentNo
	=	get currentUserAvatar -&&- get currentDateTime
	>>- \(user,now) ->
        set {LogEntry|incident = incidentNo,eventAt = now, loggedAt = now, loggedBy = user, message = toString message}
            (sdsFocus incidentNo incidentLog)
    >>| addNotification (toString message)
	@!	incidentNo

addLogMessageForContact :: msg ContactNo -> Task [IncidentNo] | toString msg
addLogMessageForContact msg contactNo
    =   get (sdsFocus contactNo incidentsByContactShort) @ (\shorts -> [incidentNo \\{IncidentShort|incidentNo} <- shorts])
    >>- \incidentNos ->
        allTasks [addLogMessage msg incidentNo \\ incidentNo <- incidentNos]

derive gDifferences IncidentBasic, IncidentType, ContactBasic, ContactType, ContactPosition, WeatherData, Maybe, Feet, Temperature, Meters, Degrees, WeatherType, Knots
derive gDifferences PersonDetails, SurferDetails, VesselDetails, DiverDetails, AirplaneDetails, HelicopterDetails, EmergencyPhase, ContactStatus, Gender, Miles, VesselType

logCommunicationResponded :: CommunicationNo -> Task ()
logCommunicationResponded communicationNo
    =   get (sdsFocus communicationNo communicationDetailsByNo)
    >>- \communication=:{CommunicationDetails|aboutIncidents} ->
        allTasks [addLogMessage (message communication) incidentNo \\ {IncidentShort|incidentNo} <- aboutIncidents]
    @!  ()
where
    message {CommunicationDetails|type,withContact,handledBy}
        = "Communication: " <+++ handledBy <+++ " responded to " <+++ type <+++ " with contact " <+++ withContact

logIncidentCreated :: IncidentNo NewIncident -> Task ()
logIncidentCreated incidentNo incident
    = addLogMessage (message incident) incidentNo @! ()
where
    message {NewIncident|type,title,summary}
        = "New incident: " <+++ title <+++ "\nType: " <+++ type <+++ "\nSummary:\n" <+++ summary

logIncidentBasicsUpdated :: IncidentNo IncidentBasic IncidentBasic -> Task ()
logIncidentBasicsUpdated incidentNo old new
    = addLogMessage (message old new) incidentNo @! ()
where
    message old new
        = "Situation information updated:\n" +++ showDifferences old new

logIncidentWeatherUpdated :: IncidentNo WeatherData WeatherData -> Task ()
logIncidentWeatherUpdated incidentNo old new
    = addLogMessage (message old new) incidentNo @! ()
where
    message old new
        = "Weather on scene information updated:\n" +++ showDifferences old new

logContactAdded :: IncidentNo ContactNo -> Task ()
logContactAdded incidentNo contactNo
    =   get (sdsFocus contactNo contactByNo)
    >>- \contact ->
        addLogMessage (message contact) incidentNo @! ()
where
    message {Contact|name,type} = "Added contact to incident\nName: " <+++ name <+++ "\nType: " <+++ type

logContactRemoved :: IncidentNo ContactNo -> Task ()
logContactRemoved incidentNo contactNo
    =   get (sdsFocus contactNo contactByNo)
    >>- \contact ->
        addLogMessage (message contact) incidentNo @! ()
where
    message {Contact|name,type} = "Removed contact from incident\nName: " <+++ name <+++ "\nType: " <+++ type

logContactBasicsUpdated :: ContactNo ContactBasic ContactBasic -> Task ()
logContactBasicsUpdated contactNo old new
    = addLogMessageForContact (message old new) contactNo @! ()
where
    message old new
        = "Basic contact info updated:\n" +++ showDifferences old new

logContactPhotoAdded :: ContactNo ContactPhoto -> Task ()
logContactPhotoAdded contactNo photo
    = addLogMessageForContact (message photo) contactNo @! ()
where
    message {ContactPhoto|original={Document|name}}
        = "Contact photo added: " +++ name

logContactPositionUpdated :: ContactNo (Maybe ContactPosition) (Maybe ContactPosition) -> Task ()
logContactPositionUpdated contactNo old new
    = addLogMessageForContact (message old new) contactNo @! ()
where
    message old new
        = "Contact position updated:\n" +++ showDifferences old new

logContactStatusUpdated :: ContactNo (Maybe ContactStatus) (Maybe ContactStatus) -> Task ()// Updated status (add to all incidents in which involved)
logContactStatusUpdated contactNo old new
    = addLogMessageForContact (message old new) contactNo @! ()
where
    message old new
        = "Contact status updated:\n" +++ showDifferences old new


logPersonDetailsUpdated :: ContactNo PersonDetails PersonDetails -> Task ()
logPersonDetailsUpdated contactNo old new
    = addLogMessageForContact (message old new) contactNo @! ()
where
    message old new
        = "Person details updated:\n" +++ showDifferences old new

logVesselDetailsUpdated :: ContactNo VesselDetails VesselDetails -> Task ()
logVesselDetailsUpdated contactNo old new
    = addLogMessageForContact (message old new) contactNo @! ()
where
    message old new
        = "Vessel details updated:\n" +++ showDifferences old new

logSurferDetailsUpdated :: ContactNo SurferDetails SurferDetails -> Task ()
logSurferDetailsUpdated contactNo old new
    = addLogMessageForContact (message old new) contactNo @! ()
where
    message old new
        = "Surfer details updated:\n" +++ showDifferences old new

logDiverDetailsUpdated :: ContactNo DiverDetails DiverDetails -> Task ()
logDiverDetailsUpdated contactNo old new
    = addLogMessageForContact (message old new) contactNo @! ()
where
    message old new
        = "Diver details updated:\n" +++ showDifferences old new

logAirplaneDetailsUpdated :: ContactNo AirplaneDetails AirplaneDetails -> Task ()
logAirplaneDetailsUpdated contactNo old new
    = addLogMessageForContact (message old new) contactNo @! ()
where
    message old new
        = "Airplane details updated:\n" +++ showDifferences old new

logHelicopterDetailsUpdated :: ContactNo HelicopterDetails HelicopterDetails -> Task ()
logHelicopterDetailsUpdated contactNo old new
    = addLogMessageForContact (message old new) contactNo @! ()
where
    message old new
        = "Helicopter details updated:\n" +++ showDifferences old new

logActionAdded :: ActionStatus -> Task ()
logActionAdded {ActionStatus|title,progress,incidents}
    = allTasks [addLogMessage (message title progress) incidentNo \\ incidentNo <- incidents] @! ()
where
    message name status = "Action added to incident\nName: " <+++ name <+++ "\nInitial status: " <+++ status

logActionUpdated :: ActionStatus -> Task ()
logActionUpdated {ActionStatus|title,progress,incidents}
    = allTasks [addLogMessage (message title progress) incidentNo \\ incidentNo <- incidents] @! ()
where
    message name status = "Action status updated\nName: " <+++ name <+++ "\nNew status: " <+++ status

createIncident :: NewIncident -> Task IncidentNo
createIncident incident
	=	create incident
	>>- \incidentNo ->
		logIncidentCreated incidentNo incident
	@! 	incidentNo
where
	create :: NewIncident -> Task IncidentNo
	create {NewIncident|type,title,summary}
		=	get databaseDef
		>>- \db -> sqlExecute db ["allIncidents"] (execInsert "INSERT INTO Incident (type,title,summary) VALUES (?,?,?)"
			(flatten [mbToSQL type, mbToSQL title, mbToSQL summary]))


deleteIncident :: IncidentNo -> Task ()
deleteIncident incidentNo
	= 		delete incidentNo
	-&&-	upd (\m -> 'DM'.put incidentNo [] m) (sdsFocus (Just [incidentNo]) contactNosByIncidentNosIndexed)
	-&&-	upd (\m -> 'DM'.put incidentNo [] m) (sdsFocus (Just [incidentNo]) communicationNosByIncidentNosIndexed)
	@!  ()
where
	delete :: IncidentNo -> Task ()
	delete incidentNo
		= get databaseDef
		>>= \db -> sqlExecute db ["allIncidents"] (execDelete "DELETE FROM Incident WHERE incidentNo = ?" (toSQL incidentNo)) @! ()

closeIncident :: IncidentNo -> Task ()
closeIncident incidentNo
	=	upd (\i -> {Incident|i & closed = True}) (sdsFocus incidentNo incidentByNo)
	>>- \i ->
		addLogMessage "Incident closed" (incidentIdentity i)
    @!  ()

linkContactsToIncident  :: [ContactNo] IncidentNo -> Task IncidentNo
linkContactsToIncident contactNos incidentNo
    = upd update (sdsFocus incidentNo contactsByIncidentShort) @! incidentNo
where
    update links = removeDup (contactNos ++ map (\{ContactShort|contactNo} -> contactNo) links)


