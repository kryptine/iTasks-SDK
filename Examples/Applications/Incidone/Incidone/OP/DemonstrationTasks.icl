implementation module Incidone.OP.DemonstrationTasks
import iTasks
import Incidone.OP.Concepts, Incidone.OP.SDSs
import Incidone.OP.IncidentManagementTasks
import Incidone.OP.ContactManagementTasks
import Incidone.OP.CommunicationManagementTasks

generateTestIncident :: Bool -> Task IncidentNo
generateTestIncident closed
    =   randomChoice [YachtEngineProblems,YachtAground,Medevac]
    >>- \incidentType ->
        makeupTroubleVessel incidentType
    >>- \vesselInfo ->
        createContact vesselInfo
    >>- \vesselNo ->
        createIncident {NewIncident|type=Just incidentType,title=Just (incidentTitle vesselInfo incidentType),summary = Nothing}
    >>- \incidentNo ->
        get (sdsFocus "Watch officers" contactsWithGroupShort) >>- randomChoice
    >>- \wo ->
        linkContactsToIncident [vesselNo,wo.ContactShort.contactNo] incidentNo
    >>- \_ ->
        //Create first call
        createFirstCall vesselNo wo incidentNo
    >>- \_ ->
        if closed (closeIncident incidentNo @! incidentNo) (return incidentNo)
where
    makeupTroubleVessel :: IncidentType -> Task NewContact
    makeupTroubleVessel incidentType
        =   createName
        >>- \name ->
            createLocation
        >>- \(lat,lng) ->
            return {NewContact|defaultValue & type = Just Vessel, name = Just name, position = Just (PositionLatLng (lat,lng)), needsHelp = True}

    incidentTitle {NewContact|name=Just name} YachtEngineProblems = name +++ " heeft motorproblemen"
    incidentTitle {NewContact|name=Just name} YachtAground = name +++ " aan de grond"
    incidentTitle {NewContact|name=Just name} Medevac = "Medevac van " +++ name
    incidentTitle {NewContact|name=Just name} _ = name +++ " in trouble"

    createName = randomChoice adjectives -&&- randomChoice nouns @ (\(a,n) -> a+++" "+++n)
    where
        adjectives = ["Red","Blue","Green","Wild","Flying","Proud","Eternal"]
        nouns = ["panther","bowler","cruiser","mary","violet"]

    createLocation = (get randomInt @ coord 52.1 54.1) -&&- (get randomInt @ coord 1.7 4.0)
    where
        coord min max rnd = min + ((max - min) * (toReal ((rnd rem 100) + 1) / 100.0))

    createFirstCall caller wo incidentNo
        =   createCommunication RadioCall In (Just caller)
        >>- \communicationNo ->
            upd update (sdsFocus communicationNo communicationDetailsByNo)
        >>- \_ ->
            logCommunicationResponded communicationNo
        @!  communicationNo
    where
        update c = {CommunicationDetails|c
                & handledBy = Just wo
                , aboutIncidents = [{IncidentShort|defaultValue&incidentNo=incidentNo}]
                , status  = Just Answered
                }
