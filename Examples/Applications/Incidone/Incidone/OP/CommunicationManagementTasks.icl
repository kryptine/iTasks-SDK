implementation module Incidone.OP.CommunicationManagementTasks
import iTasks
import iTasks.API.Extensions.SQLDatabase
import Incidone.Configuration
import Incidone.OP.Concepts, Incidone.OP.SDSs
import Incidone.OP.ContactManagementTasks, Incidone.OP.IncidentManagementTasks
import Incidone.ActionManagementTasks
import Incidone.Util.TaskPatterns
import Incidone.Util.Notification
import Incidone.Integration.Asterisk

ActionDial      :== Action "Dial"   [ActionIcon "phone-call"]
ActionHangup    :== Action "Hangup" [ActionIcon "phone-call"]
ActionChange    :== Action "Change" []
ActionCreate    :== Action "Create" [ActionIcon "add"]
ActionSend      :== Action "Send" []

//Update the information about a communication
updateCommunication :: CommunicationNo -> Task CommunicationNo
updateCommunication communicationNo
    =   get (sdsFocus communicationNo communicationDetailsByNo)
    >>- \{CommunicationDetails|type,direction} -> case (type,direction) of
            (PhoneCall,In)      = answerPhoneCall communicationNo
            (PhoneCall,Out)     = initiatePhoneCall communicationNo
            (RadioCall,In)      = answerRadioCall communicationNo
            (RadioCall,Out)     = initiateRadioCall communicationNo
            (EmailMessage,Out)  = composeEmailMessage communicationNo
            (P2000Message,Out)  = composeP2000Message communicationNo
            _
                =   viewSharedInformation (Title "Communication details:") [] (sdsFocus communicationNo communicationDetailsByNo)
                >>| return communicationNo

//Answer an incoming call
answerPhoneCall :: CommunicationNo -> Task CommunicationNo
answerPhoneCall communicationNo
    =   setInitialHandledBy communicationNo
    >>| connectInboundPhoneCall communicationNo
    ||- ((manageCommunicationContact communicationNo
          -&&-
          manageVoiceCallContent PhoneCall communicationNo) <<@ ArrangeWithSideBar 0 LeftSide 300 True) 
                                                            <<@ ArrangeWithSideBar 0 TopSide 60 False
                                                            <<@ Title ("Answer phone call")
                                                            <<@ AfterLayout (uiDefSetSize (ExactSize 800) (ExactSize 600))
    @!  communicationNo

//Make an outgoing phone call
initiatePhoneCall :: CommunicationNo -> Task CommunicationNo
initiatePhoneCall communicationNo
    =   setInitialHandledBy communicationNo
    >>| connectOutboundPhoneCall communicationNo
    ||- ((manageCommunicationContact communicationNo
          -&&-
          manageVoiceCallContent PhoneCall communicationNo) <<@ ArrangeWithSideBar 0 LeftSide 300 True)
                                                            <<@ ArrangeWithSideBar 0 TopSide 60 False
                                                            <<@ Title ("Make phone call")
                                                            <<@ AfterLayout (uiDefSetSize (ExactSize 800) (ExactSize 600))
    @!  communicationNo

//Answer an incoming radio call
answerRadioCall :: CommunicationNo -> Task CommunicationNo
answerRadioCall communicationNo
    =   setInitialHandledBy communicationNo
    >>| updateRadioCallMeta communicationNo
    ||- ((manageCommunicationContact communicationNo
          -&&-
          manageVoiceCallContent RadioCall communicationNo) <<@ ArrangeWithSideBar 0 LeftSide 300 True)
                                                            <<@ ArrangeWithSideBar 0 TopSide 60 False
                                                            <<@ Title ("Answer radio call")
                                                            <<@ AfterLayout (uiDefSetSize (ExactSize 800) (ExactSize 600))
    @!  communicationNo

//Initiate an outgoing radio call
initiateRadioCall :: CommunicationNo -> Task CommunicationNo
initiateRadioCall communicationNo
    =   setInitialHandledBy communicationNo
    >>| updateRadioCallMeta communicationNo
    ||- ((manageCommunicationContact communicationNo
          -&&-
          manageVoiceCallContent RadioCall communicationNo) <<@ ArrangeWithSideBar 0 LeftSide 300 True)
                                                            <<@ ArrangeWithSideBar 0 TopSide 60 False
                                                            <<@ Title "Initiate radio call"
                                                            <<@ AfterLayout (uiDefSetSize (ExactSize 800) (ExactSize 600))
    @!  communicationNo

//Compose an outgoing e-mail message
composeEmailMessage :: CommunicationNo -> Task CommunicationNo
composeEmailMessage communicationNo
    =   setInitialHandledBy communicationNo
    >>| updateMessageMeta communicationNo
    ||- ((composeAndSendMessage communicationNo message transmitEmailMessage
          -&&-
          relateMessageToIncidents communicationNo) <<@ ArrangeWithTabs)
                                                    <<@ ArrangeWithSideBar 0 TopSide 60 False
                                                    <<@ Title "Compose E-mail"
                                                    <<@ AfterLayout (uiDefSetSize (ExactSize 800) (ExactSize 600))
    @! communicationNo
where
    message = sdsFocus communicationNo emailMessageByNo

//Compose an outgoing P2000 message
composeP2000Message :: CommunicationNo -> Task CommunicationNo
composeP2000Message communicationNo
    =   setInitialHandledBy communicationNo
    >>| updateMessageMeta communicationNo
    ||- ((composeAndSendMessage communicationNo message transmitP2000Message
          -&&-
          relateMessageToIncidents communicationNo) <<@ ArrangeWithTabs)
                                                    <<@ ArrangeWithSideBar 0 TopSide 60 False
                                                    <<@ Title "Compose P2000 message"
                                                    <<@ AfterLayout (uiDefSetSize (ExactSize 800) (ExactSize 600))
    @! communicationNo
where
    message = sdsFocus communicationNo p2000MessageByNo

composeAndSendMessage communicationNo share sendTask = forever
    (whileUnchanged (sdsFocus communicationNo communicationStatus)
            \status -> case status of
                Nothing = composeMessage
                _       = viewMessage
    )
where
    composeMessage
        =   updateSharedInformation (Title "Message") [] share <<@ FillNotes
        >>* [OnAction ActionSend (hasValue (\_ -> sendTask communicationNo))]

    viewMessage
        = viewSharedInformation (Title "Message") [] share
        @! ()

relateMessageToIncidents :: CommunicationNo -> Task ()
relateMessageToIncidents communicationNo
    = manageSharedListWithDetails details add aboutIncidents
where
    aboutIncidents = sdsFocus communicationNo communicationAboutIncidents
    communicationAboutIncidents = mapReadWrite (toPrj,fromPrj) communicationByNo
    where
        toPrj {Communication|aboutIncidents} = aboutIncidents
        fromPrj aboutIncidents c = Just {Communication|c & aboutIncidents = aboutIncidents}

    details incidentNo
        =   manageLinkedIncidentInfo incidentNo
        >>* [OnAction (Action "Remove" []) (always (upd (filter ((<>) incidentNo)) aboutIncidents))]
        @!  ()

    add =   selectKnownOrDefineNewIncident <<@ Title "+"
        >>= createIncidentIfNew

/**
* Interact with the PBX integration to establish an actual phone connection
*/
connectOutboundPhoneCall :: CommunicationNo -> Task ()
connectOutboundPhoneCall communicationNo
    =   (updatePhoneCallMeta communicationNo
    >&> watch
    >^* [OnAction ActionDial (ifValue isNothing (\_ -> initiateAsteriskChannel communicationNo))
        ,OnAction ActionCancel (ifValue (maybe False ((===) Pending)) (\_ -> destroyAsteriskChannel communicationNo))
        ,OnAction ActionHangup (ifValue (maybe False ((===) Connected)) (\_ -> destroyAsteriskChannel communicationNo))
        ]) <<@ Attribute "buttonPosition" "right"
    @! ()

updatePhoneCallMeta :: CommunicationNo -> Task CommunicationStatus
updatePhoneCallMeta communicationNo
    = ((Label "External number" @>> updateSharedInformation () [] (sdsFocus communicationNo phoneCallExternalNo))
        -&&-
       (Label "Time" @>> updateSharedInformation () [] (sdsFocus communicationNo communicationTime))
      <<@ ForceLayout)
      -&&-
      ((Label "Status" @>> editSharedChoice () [] [Pending,Ringing,Connected,Missed,Answered] (sdsFocus communicationNo communicationStatus))
        -&&-
       (Label "Handled by" @>> editSharedChoiceWithSharedAs () [] watchOfficers contactIdentity (sdsFocus communicationNo communicationHandledBy))
      <<@ ForceLayout) <<@ ArrangeSplit Horizontal False
    @ \(_,(status,_)) -> status

updateRadioCallMeta :: CommunicationNo -> Task CommunicationStatus
updateRadioCallMeta communicationNo
    = ((Label "Channel" @>> updateSharedInformation () [] (sdsFocus communicationNo radioCallChannel))
       -&&-
       (Label "Time" @>> updateSharedInformation () [] (sdsFocus communicationNo communicationTime))
 <<@ ForceLayout)
      -&&-
      ((Label "Status" @>> editSharedChoice () [] [Missed,Answered] (sdsFocus communicationNo communicationStatus))
        -&&-
       (Label "Handled by" @>> editSharedChoiceWithSharedAs () [] watchOfficers contactIdentity (sdsFocus communicationNo communicationHandledBy))
      <<@ ForceLayout) <<@ ArrangeSplit Horizontal False
    @ \(_,(status,_)) -> status

updateMessageMeta :: CommunicationNo -> Task CommunicationStatus
updateMessageMeta communicationNo
    = ((Label "Time" @>> updateSharedInformation () [] (sdsFocus communicationNo communicationTime))
      <<@ ForceLayout)
      -&&-
      ((Label "Status" @>> editSharedChoice () [] [Pending,Sent] (sdsFocus communicationNo communicationStatus))
        -&&-
       (Label "Handled by" @>> editSharedChoiceWithSharedAs () [] watchOfficers contactIdentity (sdsFocus communicationNo communicationHandledBy))
      <<@ ForceLayout) <<@ ArrangeSplit Horizontal False
    @ \(_,(status,_)) -> status

phoneCallExternalNo = mapReadWrite (toExternalNo,fromExternalNo) phoneCallByNo
where
    toExternalNo {PhoneCall|externalNo} = externalNo
    fromExternalNo nexternalNo c=:{PhoneCall|externalNo} = if (nexternalNo =!= externalNo) (Just {PhoneCall|c & externalNo = nexternalNo}) Nothing

radioCallChannel = mapReadWrite (toChannel,fromChannel) radioCallByNo
where
    toChannel {RadioCall|channel} = channel
    fromChannel nchannel c=:{RadioCall|channel} = if (nchannel =!= channel) (Just {RadioCall|c & channel = nchannel}) Nothing

communicationTime = mapReadWrite (toTime,fromTime) communicationByNo
where
    toTime {Communication|time} = time
    fromTime ntime c=:{Communication|time} = if (ntime =!= time) (Just {Communication|c & time = ntime}) Nothing

communicationStatus = mapReadWrite (toStatus,fromStatus) communicationByNo
where
    toStatus {Communication|status} = status
    fromStatus nstatus c=:{Communication|status} = if (nstatus =!= status) (Just {Communication|c & status = nstatus}) Nothing

communicationHandledBy = mapReadWrite (toHandledBy,fromHandledBy) communicationByNo
where
    toHandledBy {Communication|handledBy} = handledBy
    fromHandledBy nhandledBy c=:{Communication|handledBy} = if (nhandledBy =!= handledBy) (Just {Communication|c & handledBy = nhandledBy}) Nothing

watchOfficers = sdsFocus "NLDA" contactsWithGroupShort

connectInboundPhoneCall :: CommunicationNo -> Task CommunicationStatus
connectInboundPhoneCall communicationNo = updatePhoneCallMeta communicationNo
    //Possible actions based on the status "answer" or "decline", but for now just use the phone system for that

/**
* Determine who the communication is with.
* either by searching and selecting an existing contact
* or creating a new one.
*
* Once the contact is set, you get to access to key pieces of information
* about the contact during the call.
*/
manageCommunicationContact :: CommunicationNo -> Task ContactNo
manageCommunicationContact communicationNo = forever (
    whileUnchanged (mapRead (\{Communication|withContact} -> withContact) (sdsFocus communicationNo communicationByNo))
    ( \mbContactNo -> case mbContactNo of
        Nothing         =   determineContact Nothing >>- setContact communicationNo
        Just contactNo  =   manageSelectedContactInfo contactNo
                        >>* [OnAction ActionChange (always (determineContact (Just contactNo) >>- setContact communicationNo))]
    ))
where
    setContact communicationNo contactNo
        = upd (\c -> {Communication|c & withContact = Just contactNo}) (sdsFocus communicationNo communicationByNo) @! contactNo

determineContact :: (Maybe ContactNo) -> Task ContactNo
determineContact mbPrevious
    =  withShared {ContactFilter|filterByName=Nothing}
      \filter ->
       createNewContact filter
       -||-
       selectExistingContact filter
       <<@ ArrangeVertical
where
    createNewContact filter
        =   enterInformation (Title "Contact") [] @> (mapToFilter,filter)
        >>* [OnAction ActionCreate (hasValue (createContact))]
    selectExistingContact filter
        =   whileUnchanged filter
            \curFilter ->
            enterChoiceWithSharedAs (Title "Select contact") [ChooseWith (ChooseFromList contactTitle)] (sdsFocus curFilter filteredContactsShort) contactIdentity
        >>* [OnValue (hasValue return)
            :maybe [] (\contactNo -> [OnAction ActionCancel (always (return contactNo))]) mbPrevious]

    mapToFilter (Value {NewContact|name} _) {ContactFilter|filterByName}
        = if (name =!= filterByName) (Just {ContactFilter|filterByName = name}) Nothing
    mapToFilter _ _ = Just {ContactFilter|filterByName=Nothing}

manageSelectedContactInfo :: ContactNo -> Task ContactNo
manageSelectedContactInfo contactNo
    = (  viewContactHeader contactNo
    -&&- manageContactActions True contactNo
    -&&- manageContactCommunicationMeans True contactNo
    ) <<@ ArrangeVertical
    @! contactNo
    //TODO: manageContactPosition
    //TODO: manageContactStatus (for own resources)

manageLinkedIncidentInfo :: IncidentNo -> Task IncidentNo
manageLinkedIncidentInfo incidentNo
    = (  viewSharedTitle (sdsFocus incidentNo (mapRead incidentTitle incidentByNo))
    -&&- manageIncidentLog incidentNo
    ) <<@ ArrangeVertical
    @! incidentNo
    //TODO: manageIncidentContacts
    //TODO: manageIncidentActions

manageVoiceCallContent :: CommunicationType CommunicationNo -> Task ()
manageVoiceCallContent type communicationNo
    = updateCallNotes -|| relateMessageToIncidents communicationNo <<@ ArrangeWithTabs
where
    updateCallNotes
        =   updateSharedInformation (Title "Notes") [] (callNotes type) <<@ FillNotes
        @! ()

    callNotes PhoneCall = sdsFocus communicationNo (mapReadWrite (toPrj,fromPrj) phoneCallByNo)
    where
        toPrj {PhoneCall|callNotes} = callNotes
        fromPrj callNotes c = Just {PhoneCall|c & callNotes = callNotes}

    callNotes RadioCall = sdsFocus communicationNo (mapReadWrite (toPrj,fromPrj) radioCallByNo)
    where
        toPrj {RadioCall|callNotes} = callNotes
        fromPrj callNotes c = Just {RadioCall|c & callNotes = callNotes}

setInitialHandledBy :: CommunicationNo -> Task Communication
setInitialHandledBy communicationNo
    =   get currentUserContactNo @ Just
    >>- \curHandledBy ->
        upd (\c=:{Communication|handledBy} -> {Communication|c & handledBy =maybe curHandledBy Just handledBy})
            (sdsFocus communicationNo communicationByNo)

createPhoneCall	:: CommunicationDirection -> Task CommunicationNo
createPhoneCall direction = createCommunication PhoneCall direction Nothing

createRadioCall	:: CommunicationDirection -> Task CommunicationNo
createRadioCall direction = createCommunication RadioCall direction Nothing

createEmailMessage :: CommunicationDirection -> Task CommunicationNo
createEmailMessage direction = createCommunication EmailMessage direction Nothing

createP2000Message :: CommunicationDirection -> Task CommunicationNo
createP2000Message direction = createCommunication P2000Message direction Nothing

createCommunication	:: CommunicationType CommunicationDirection (Maybe ContactNo)-> Task CommunicationNo
createCommunication type direction mbWithContact
	=	get (currentDateTime |+| databaseDef)
    >>- \(datetime,db) ->
		sqlExecute db ["allCommunications"] (execInsert "INSERT INTO Communication (time,type,direction,withContact) VALUES (?,?,?,?)" (flatten [toSQL datetime,toSQL type,toSQL direction,mbToSQL mbWithContact]))
    >>- \communicationNo -> case type of
		PhoneCall       = sqlExecute db ["allCommunications"] (execInsert "INSERT INTO PhoneCall (communicationNo) VALUES (?)" (toSQL communicationNo)) @! communicationNo
		RadioCall       = sqlExecute db ["allCommunications"] (execInsert "INSERT INTO RadioCall (communicationNo) VALUES (?)" (toSQL communicationNo)) @! communicationNo
		EmailMessage    = sqlExecute db ["allCommunications"] (execInsert "INSERT INTO EmailMessage (communicationNo,sender,recipient,subject,body) VALUES (?,?,?,?,?)" (flatten [toSQL communicationNo,toSQL "",toSQL "",toSQL "",toSQL ""])) @! communicationNo
		P2000Message    = sqlExecute db ["allCommunications"] (execInsert "INSERT INTO EmailMessage (communicationNo) VALUES (?)" (toSQL communicationNo)) @! communicationNo

linkIncidentsToCommunication :: [IncidentNo] CommunicationNo -> Task CommunicationNo
linkIncidentsToCommunication incidentNos communicationNo
    = upd update (sdsFocus communicationNo incidentsByCommunicationShort) @! communicationNo
where
    update links = removeDup (incidentNos ++ map (\{IncidentShort|incidentNo} -> incidentNo) links)

transmitEmailMessage :: CommunicationNo -> Task ()
transmitEmailMessage communicationNo
    //TODO Actually send the message
    = upd (\c -> {Communication|c & status = Just Sent}) (sdsFocus communicationNo communicationByNo) @! ()

transmitP2000Message :: CommunicationNo -> Task ()
transmitP2000Message communicationNo
    = upd (\c -> {Communication|c & status = Just Sent}) (sdsFocus communicationNo communicationByNo) @! ()

reportPhoneCallBegin :: (Maybe String) (Maybe String) -> Task CommunicationNo
reportPhoneCallBegin externalNo externalRef
    =   traceValue ("Call started: " <+++ externalNo <+++ " " <+++ externalRef)
    >>| createPhoneCall In
    >>- \communicationNo ->
        upd (\c -> {Communication|c & status = Just Ringing}) (sdsFocus communicationNo communicationByNo)
    >>| upd (\p -> {PhoneCall|p & externalNo = externalNo, externalRef = externalRef}) (sdsFocus communicationNo phoneCallByNo)
    >>| addNotification ("Incoming call from: "+++ fromMaybe "(unknown)" externalNo)
    @!  communicationNo

reportPhoneCallConnected :: (Either CommunicationNo String) -> Task ()
reportPhoneCallConnected (Left communicationNo) = upd (\c -> {Communication|c & status = Just Connected}) (sdsFocus communicationNo communicationByNo) @! ()
reportPhoneCallConnected (Right externalRef)
    =   traceValue ("Call connected: " +++ externalRef)
    >>| updateCommunicationByCallReference externalRef updateFun
where
    updateFun c = {Communication|c & status = Just Connected}

reportPhoneCallEnd :: (Either CommunicationNo String) -> Task ()
reportPhoneCallEnd (Left communicationNo) = upd (\c -> {Communication|c & status = Just Answered}) (sdsFocus communicationNo communicationByNo) @! ()
reportPhoneCallEnd (Right externalRef)
    =   traceValue ("Call ended: " +++ externalRef)
    >>| updateCommunicationByCallReference externalRef updateFun
where
    updateFun c=:{Communication|status=Just Connected}  = {Communication|c & status = Just Answered}
    updateFun c                                         = {Communication|c & status = Just Missed}

updateCommunicationByCallReference externalRef f = catchAll
    (   get (sdsFocus externalRef phoneCallByReference)
      >>- \{PhoneCall|communicationNo} ->
        upd f (sdsFocus communicationNo communicationByNo) @! ()
    ) (\_ -> return ())

injectEmail :: EmailMessage -> Task CommunicationNo
injectEmail message
    = createEmailMessage In
    >>- \communicationNo ->
        set message (sdsFocus communicationNo emailMessageByNo)
    @!  communicationNo
