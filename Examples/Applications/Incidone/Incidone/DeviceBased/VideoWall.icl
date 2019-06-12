implementation module Incidone.DeviceBased.VideoWall
import iTasks
import iTasks.Extensions.DateTime
import Incidone.OP.Concepts, Incidone.OP.SDSs, Incidone.ActionManagementTasks
import Incidone.Util.TaskPatterns
import Text, Text.HTML, Data.List, iTasks.Internal.HtmlUtil

derive class iTask WallContent

wallContent :: SimpleSDSLens WallContent
wallContent = sharedStore "WallContent" (WallOverview defaultValue)

viewVideoWallContent :: Task WallContent
viewVideoWallContent
    = (header ||- content) <<@ (ArrangeWithHeader 0) //<<@ AfterLayout plainLayoutFinal //FIXME
where
    header
        = viewSharedInformation [ViewAs view] (currentTime |*| currentUTCTime)  //<<@ (AfterLayout (uiDefSetHalign AlignRight o uiDefSetBaseCls "wall-header")) //FIXME
    where
        view (local,utc) = "LOCAL: " + lpad (toString local.Time.hour) 2 '0' + ":" + lpad (toString local.Time.min) 2 '0' + " "
                         + "UTC: " + lpad (toString utc.Time.hour) 2 '0' + ":" + lpad (toString utc.Time.min) 2 '0'
    content
        = whileUnchanged wallContent \content -> case content of
        WallClock                               = (Title "Local Time" @>> viewSharedInformation [ViewAs formatTime] currentTime @! content)
//        WallCountDown until                     = (viewSharedInformation (Title "Countdown") [ViewAs (\t -> formatDateTime (until - t))] currentDateTime @! content)
        WallOverview perspective                = viewWallOverview perspective @! content
        WallContactSummary (Just contactNo)     = viewWallContactSummary contactNo @! content
        WallIncidentSummary (Just incidentNo)   = viewWallIncidentSummary incidentNo @! content
        _                                       = Hint "Nothing selected..." @>> viewInformation  [] () @! content

formatTime time = DivTag [StyleAttr "font-size: 80pt; text-align: center; padding-top: 200px;"] [Text (toString time)]
formatDateTime time = DivTag [StyleAttr "font-size: 80pt; text-align: center; padding-top: 200px;"] [Text (toString time)]

mapContacts = mapRead (\(x,y) -> x++y) (contactsOfOpenIncidentsGeo |*| contactsProvidingHelpGeo)

viewWallOverview perspective
    = (((Title "Open Incidents") @>> viewSharedInformation  [ViewAs formatIncidents] openIncidentsDetails)
        -&&-
       (get standardMapLayers
        >>- \baseLayers ->
        viewSharedInformation [ViewAs (toMap perspective baseLayers)] mapContacts /* <<@ AfterLayout (tweakUI (setMargins 0 0 0 0 o fill))*/ ) //FIXME
      ) <<@ ArrangeWithSideBar 0 LeftSide False
where
    toMap perspective baseLayers contacts
        = toLeafletMap {ContactMap|perspective=perspective,layers=baseLayers++[{title="Contacts",def=CMMarkersLayer [contactGeoToMapMarker False False c \\ c=:{ContactGeo|position=Just _} <- contacts]}]}

    formatIncidents incidents
        = UlTag [ClassAttr "wall-incident-list"]
            [LiTag [] [SpanTag [] [Text (fromMaybe "" title)],BrTag []
                      ,Text (maybe "-" toSingleLineText type)
                      ]
            \\ {IncidentDetails|title,type} <- incidents]

viewWallContactSummary contactNo
	= withHeader viewContactTitle
      ((viewDetails
       -&&-
        ((viewPosition -&&- viewCommunication ) <<@ArrangeWithSideBar 1 RightSide False)
       ) <<@ArrangeWithSideBar 0 LeftSide False)
where
    contact = sdsFocus contactNo contactByNo

    viewContactTitle
        = viewSharedInformation [] (mapRead contactTitle contact) /* <<@ AfterLayout (uiDefSetBaseCls "wall-contact-title") */ //FIXME
    viewDetails
        = (viewPhoto -&&- viewTypeDetails) <<@ (Title "Details")
    viewPhoto
		= viewSharedInformation [ViewAs formatPhoto] contact
    where
	    formatPhoto {Contact|photos,type,notes}
            = ImgTag [ClassAttr "wall-contact-details",WidthAttr "200",HeightAttr "200",SrcAttr (photoSrc photos)]

	    photoSrc [{ContactPhoto|thumb}:_]	= thumb.Document.contentUrl
	    photoSrc _							= "/no-photo.jpg"

    viewTypeDetails = whileUnchanged contact
        \{Contact|type} -> case type of
            Just Vessel = viewSharedInformation [] (sdsFocus contactNo vesselDetailsByNo) @! ()
            Just Person = viewSharedInformation [] (sdsFocus contactNo personDetailsByNo) @! ()
            _           = viewInformation [] ()

    viewPosition
        = (((Title "Position") @>> viewSharedInformation [ViewAs formatPosition] contact /* <<@ AfterLayout (uiDefSetBaseCls "wall-contact-position") */) //FIXME
           -&&-
           ((Title "Map") @>> viewSharedInformation [ViewAs contactMap] contact /*<<@ AfterLayout (tweakUI (setMargins 0 0 0 0 o fill)) */) //FIXME
          )
    where
        formatPosition {Contact|position=Just pos}  = toSingleLineText pos
        formatPosition _                            = "No position known"

        contactMap c=:{Contact|position}
            = toLeafletMap {ContactMap|defaultValue & perspective = perspective position, layers =[{title="Contacts",def=CMMarkersLayer (markers c position)}]}

	    perspective position = {ContactMapPerspective|defaultValue & center = fromMaybe defaultValue.ContactMapPerspective.center contactPos, zoom = 12, cursor = contactPos}
        where
            contactPos = maybe Nothing latLng position

        markers c (Just _)  = [contactToMapMarker False False c]
        markers c _         = []

    viewCommunication
        = (Title "Last communication") @>> viewSharedInformation [ViewAs (formatComms o take 5)] (sdsFocus contactNo contactCommunications)
    where
        formatComms items
            = DivTag []
                [PTag [ClassAttr "wall-communication-list"] [Text "At :",Text (toSingleLineText time),BrTag []
                          ,Text "With: ",Text (maybe "-" toSingleLineText handledBy)
                          ]
                \\ {CommunicationDetails|time,handledBy} <- items]


viewWallIncidentSummary incidentNo
	= withHeader viewIncidentTitle
      ((viewIncidentContacts -&&- viewIncidentActions -&&- viewIncidentLog) <<@ ArrangeHorizontal)
where
    incident = sdsFocus incidentNo incidentByNo

    viewIncidentTitle
        = viewSharedInformation [] (sdsFocus incidentNo incidentTitleByNo) //<<@ AfterLayout (uiDefSetBaseCls "wall-contact-title") //FIXME
    viewIncidentContacts
        = (Title "Involved Contacts") @>> viewSharedInformation [ViewAs toView] (sdsFocus incidentNo contactsByIncident)
    where
        toView contacts = DivTag [ClassAttr "wall-incident-contacts"] (map formatContact contacts)

        formatContact {Contact|name,photos,type,group,needsHelp}
            = DivTag [ClassAttr (if needsHelp "wall-incident-contact needshelp" "wall-incident-contact")]
                [ImgTag [ClassAttr "wall-contact-avatar",WidthAttr "50",HeightAttr "50",SrcAttr (photoSrc photos)]
                ,DivTag [ClassAttr "wall-contact-name"] [Text (toSingleLineText name)]
                ,DivTag [ClassAttr "wall-contact-group"] [Text (toSingleLineText group)]
                ]
	    photoSrc [{ContactPhoto|avatar}:_]	= avatar.Document.contentUrl
	    photoSrc _							= "/no-photo.jpg"

    viewIncidentActions
        = (Title "Open Actions") @>> viewSharedInformation [ViewAs toView]  (sdsFocus incidentNo actionStatusesByIncident) /* <<@ AfterLayout (tweakUI fill) */ //FIXME
    where
        toView actions = DivTag [] [vizAction a \\ (_,_,a) <- actions]
        vizAction {ActionStatus|title}
            = DivTag [ClassAttr "wall-action"] [H2Tag [ClassAttr "wall-action-title"] [Text title]]

    viewIncidentLog
        = Title "Last Log Messages" @>> viewSharedInformation [ViewAs (toView o take 5)] (sdsFocus incidentNo incidentLog)
    where
        toView log = DivTag [] (flatten [[vizDate date:map vizEntry entries] \\ (date,entries) <- groupByDate log])

        vizDate date = H2Tag [ClassAttr "wall-log-date"] [Text (toString date)]
        vizEntry entry = DivTag [ClassAttr "wall-log-entry"]
                                (vizName entry.loggedBy ++ vizTime entry.eventAt ++ vizMessage entry.message)
        vizName (Just {ContactAvatar|name=Just name}) = [DivTag [ClassAttr "wall-log-name"] [Text name]]
        vizName _ = [DivTag [ClassAttr "wall-log-name"] [Text "System message"]]
        vizTime datetime = [DivTag [ClassAttr "wall-log-time"] [Text (toString (toTime datetime))]]
        vizMessage message = [DivTag [ClassAttr "wall-log-message"] [nl2br (toString message)]]

        groupByDate log = [(toDate e.eventAt,es) \\ es=:[e:_] <-  groupBy (\e1 e2 -> toDate e1.eventAt == toDate e2.eventAt) log]
