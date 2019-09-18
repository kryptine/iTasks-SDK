implementation module Incidone.DeviceBased.Tablet
import iTasks
import Incidone.OP.Concepts, Incidone.OP.SDSs
import Incidone.DeviceBased.VideoWall
import Incidone.Util.TaskPatterns
import Text.HTML

selectVideoWallContent :: Task ()
selectVideoWallContent
    = (header ||- selectContent) <<@ (ArrangeWithHeader 0)
    @! ()
where
    header
        = viewInformation [] ("REMOTE CONTROL") //<<@ (AfterLayout (uiDefSetHalign AlignRight o uiDefSetBaseCls "wall-header")) //FIXME

    mapContacts = mapRead (\(x,y) -> x++y) (contactsOfOpenIncidentsGeo |*| contactsProvidingHelpGeo)
    selectContent
        = (switchContent >&> withSelection viewNoSelection configureContent) <<@ (ArrangeWithSideBar 0 LeftSide False)

    switchContent = (Title "Choose Content") @>> enterChoice [ChooseFromList bigLabel] contentOptions
    contentOptions
        = ["Overview","Incident","Contact","Clock","Countdown"]

    configureContent selection
        =   (configure selection
        >^* [OnAction (Action "Share to Wall") (hasValue (\c -> set c wallContent))])
    where
        title = "Configure Content"
        configure "Overview"
            = get (standardMapLayers |*| standardPerspective)
            >>- \(baseLayers,perspective) ->
                withShared perspective
                \p -> Title title @>> updateSharedInformation [UpdateSharedAs (toMap baseLayers) fromMap (const o Just)] (p >*| mapContacts) @ fst
            //<<@ AfterLayout (tweakUI fill) //FIXME
            @   WallOverview
        where
            toMap baseLayers (perspective,contacts)
                = toLeafletMap {ContactMap| perspective = perspective, layers = baseLayers ++ [{title="Contacts",def=CMMarkersLayer [contactGeoToMapMarker False False c \\ c=:{ContactGeo|position=Just _} <- contacts]}]}
            fromMap _ {LeafletMap|perspective}
                = fromLeafletPerspective perspective
        configure "Incident"
            =   Title title @>> enterChoiceWithSharedAs [ChooseFromList bigLabel] allIncidentsShort (\{IncidentShort|incidentNo} -> WallIncidentSummary (Just incidentNo))
        configure "Contact"
            =    Title title @>> enterChoiceWithSharedAs [ChooseFromList bigLabel] allContactsShort (\{ContactShort|contactNo} -> WallContactSummary (Just contactNo))
        configure "Clock"
            =   Title title @>> viewInformation [] "No configuration is needed for the clock."
            //<<@ AfterLayout (tweakUI fill) //FIXME
            @!  WallClock
        configure "Countdown"
            =   get currentDateTime
            >>- \datetime ->
				Title title @>> Hint "Set the countdown date and time" @>> updateInformation [] datetime
            @   WallCountDown
        configure _
            = Title title @>> Hint "This option is not available yet..." @>> viewInformation [] () @? const NoValue

    bigLabel l = SpanTag [StyleAttr "font-size: 24px; font-weight: bold; margin-bottom: 5px;"] [Text (toSingleLineText l)]
