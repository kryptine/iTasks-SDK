implementation module Incidone.OP.ContactManagementTasks
import iTasks, iTasks.Extensions.SQLDatabase, iTasks.Extensions.Web, iTasks.Extensions.Process
import Incidone.Configuration
import Incidone.OP.Concepts, Incidone.OP.SDSs, Incidone.OP.Conversions
import Incidone.OP.IncidentManagementTasks
import Incidone.Util.TaskPatterns
import Incidone.DeviceBased.VideoWall

import Incidone.Extensions.CrewLists //For demo

import qualified Data.Map as DM
import Text, Text.HTML, Data.Either, Data.Functor
import iTasks.UI.Editor.Controls

CONVERT_BIN :== "/opt/local/bin/convert"
//CONVERT_BIN :== "/usr/bin/convert"

openContactInWorkspace :: Workspace ContactNo -> Task ()
openContactInWorkspace ws contactNo = addOnceToWorkspace ("contact-"<+++contactNo) (doOrClose (manageContactInformation ws contactNo)) ws @! ()

selectContact :: Task (Either ContactNo MMSI)
selectContact = withShared Nothing
    \sel -> (
		(selectContactFromLists sel <<@ Title "Browse")
		 -||-
         (viewContactsOnMap mapContacts sel <<@ Title "Map")
		 <<@ ArrangeWithTabs True
        )
where	
    mapContacts = mapRead (\(x,y) -> x++y) (contactsOfOpenIncidentsGeo |+| contactsProvidingHelpGeo)

	selectContactFromLists :: (Shared (Maybe (Either ContactNo MMSI))) -> Task (Either ContactNo MMSI)
	selectContactFromLists sel
		= anyTask [editSharedSelectionWithShared (Title "Involved in open incidents") False
						(SelectInTree groupByIncident select) contactsOfOpenIncidentsShort (selIds sel)
				  ,editSharedSelectionWithShared (Title "Available for help") False
						(SelectInTree groupByGroup select) contactsProvidingHelpShort (selIds sel)
                  ,editSharedSelectionWithShared (Title "All contacts") False
                        (SelectInTree groupByGroup select) allContactsShort (selIds sel)
/*
				  ,(editSharedSelectionWithShared (Title "AIS") False
						(SelectInTree ungrouped) (mapRead (sortBy (\x y -> contactTitle x < contactTitle y) o map aisToContact) allAISContacts) (Right o contactIdentity)) sel
*/				  ] <<@ (ArrangeSplit Horizontal True) @? tvHd 

    fromOpenOption [{ContactShortWithIncidents|contactNo}] = contactNo

	selIds sel = mapReadWrite (toPrj,fromPrj) sel
	where
		toPrj Nothing = []
		toPrj (Just (Left contactNo)) = [contactNo]
		toPrj (Just (Right mmsi)) = [~ mmsi]

		fromPrj [s] _ = Just (Just (if (s > 0) (Left s) (Right (~s))))
		fromPrj _ _   = Just Nothing

	//Organize contacts into a tree
	groupByIncident contacts
        = [{ChoiceNode|id=c.ContactShortWithIncidents.contactNo, label=label c
					,icon=Just (contactIcon {Contact|defaultValue&type=c.ContactShortWithIncidents.type})
					,expanded=False,children=[]} \\ c <- contacts]
    where
        label {ContactShortWithIncidents|name,incidents}
            = fromMaybe "-" name +++ " (" +++ join "," [fromMaybe "-" title\\{IncidentShort|title}<-incidents] +++ ")"

    groupByGroup contacts
        = [{ChoiceNode|id=0,label=group,icon=Nothing,expanded=True,children=
			[{ChoiceNode|id=c.ContactShort.contactNo, label=fromMaybe "-" c.ContactShort.name
					,icon=Just (contactIcon c)
					,expanded=False,children=[]} \\ c <- items]} \\ (group,items) <- 'DM'.toList (foldl group 'DM'.newMap contacts)]
    where
        label {ContactShort|name} = fromMaybe "-" name
        group groups c=:{ContactShort|group}
            # g = fromMaybe "Uncategorized" group
            = 'DM'.put g (fromMaybe [] ('DM'.get g groups) ++ [c]) groups

	ungrouped contacts =
		[{ChoiceNode|id=c.ContactShort.contactNo, label=fromMaybe "-" c.ContactShort.name, icon=Just (contactIcon c),expanded=False,children=[]} \\ c <- contacts]

	select _ [0] = []
	select _ [s] = [if (s > 0) (Left s) (Right (~s))]
	select _ _   = []


manageContactInformation :: Workspace ContactNo -> Task ()
manageContactInformation ws contactNo
	= 	withHeader (viewTitle contactNo)
		(allTasks
			[manageContactBasics contactNo
			,manageContactDetails contactNo
			,manageContactPhotos contactNo
			,viewAISInfo contactNo
			,manageContactCommunication contactNo
			,manageContactIncidents ws contactNo
            ,manageContactActions False contactNo
            ,manageContactCrew` contactNo <<@ Title "Crew"
            ] <<@ ArrangeWithTabs True) @! ()
where
    viewTitle contactNo	= viewSharedTitle (mapRead contactTitle (sdsFocus contactNo contactByNo))

    manageContactCrew` contactNo = whileUnchanged (mapRead (\{Contact|type,providesHelp}->(type,providesHelp)) (sdsFocus contactNo contactByNo))
        \(type,providesHelp) -> if (type === Just Vessel && providesHelp)
            (manageContactCrew contactNo)
            (viewInformation () [] () <<@ NoUserInterface)

manageContactBasics :: ContactNo -> Task ()
manageContactBasics contactNo = (
    allTasks
        [viewAndEdit (viewContactBasics contactNo) (editContactBasics contactNo)
        ,manageContactAccess contactNo
        ]
    >^*[OnAction (Action "/Share to wall") (\_ -> Just (shareContact contactNo))
       ] @! ()) <<@ Title "General" <<@ (ApplyAttribute "icon" "basic-information")
where

    contactBasics contactNo = mapReadWrite (toPrj,fromPrj) (sdsFocus contactNo contactByNo)
    where
        toPrj {Contact|type,name,group,position,heading,needsHelp,providesHelp,status,notes}
            = {ContactBasic|type=type,name=name,group=group,position=position,heading=heading,needsHelp=needsHelp,providesHelp=providesHelp,status=status,notes=notes}
        fromPrj {ContactBasic|type,name,group,position,heading,needsHelp,providesHelp,status,notes} c
            = Just {Contact|c & type=type,name=name,group=group,position=position,heading=heading,needsHelp=needsHelp,providesHelp=providesHelp,status=status,notes=notes}

    viewContactBasics contactNo
        = viewSharedInformation () [] (contactBasics contactNo)

    editContactBasics contactNo
        =   (get (contactBasics contactNo)
        >>= \old ->
            updateInformation () [] old
        >>? \new -> set new (contactBasics contactNo)
        >>| logContactBasicsUpdated contactNo old new
        ) @! ()

    shareContact contactNo
        = set (WallContactSummary (Just contactNo)) wallContent @! ()

manageContactDetails :: ContactNo -> Task ()
manageContactDetails contactNo
    = whileUnchanged (mapRead (\{Contact|type} -> type) (sdsFocus contactNo contactByNo))
        \mbType -> case mbType of
            Just Person = manageDetails (sdsFocus contactNo personDetailsByNo) (logPersonDetailsUpdated contactNo)  @! ()
            Just Vessel = manageDetails (sdsFocus contactNo vesselDetailsByNo) (logVesselDetailsUpdated contactNo) @! ()
            Just Surfer = manageDetails (sdsFocus contactNo surferDetailsByNo) (logSurferDetailsUpdated contactNo) @! ()
            Just Diver  = manageDetails (sdsFocus contactNo diverDetailsByNo) (logDiverDetailsUpdated contactNo) @! ()
            Just Airplane = manageDetails (sdsFocus contactNo airplaneDetailsByNo) (logAirplaneDetailsUpdated contactNo) @! ()
            Just Helicopter = manageDetails (sdsFocus contactNo helicopterDetailsByNo) (logHelicopterDetailsUpdated contactNo) @! ()
            _               = viewInformation ("Details","No details can be entered if the contact type is unknown.") [] ()
where
    manageDetails share log = forever (
            viewSharedInformation [Att (Title "Details"),Att (Icon "details")] [] share
        >>* [OnAction (Action "/Edit") (always (editDetails share log))]
        )

    editDetails share log
        =   get share
        >>- \old -> updateInformation [Att (Title "Details (editing)"),Att (Icon "details")] [] old
        >>? \new -> set new share
        >>| log old new

manageContactPhotos :: ContactNo -> Task ()
manageContactPhotos contactNo
    =   (enterChoiceWithShared () [ChooseFromList toPrj] (sdsFocus contactNo contactPhotos)
    >^* [OnAction (Action "/Add photo") (always (addPhoto <<@ InWindow))
        ,OnAction (Action "/Delete photo") (hasValue (\p -> delPhoto p <<@ InWindow))
        ])
        <<@ Title "Photos"
        <<@ Icon "photos"
    @!  ()
where
    toPrj {original,thumb} = ATag [StyleAttr "margin: 5px; float:left",HrefAttr original.Document.contentUrl] [ImgTag [WidthAttr "200",HeightAttr "200",SrcAttr thumb.Document.contentUrl]]

    addPhoto
        =   enterInformation ("Add photo","Please select a photo to upload") []
        >>? addContactPhoto contactNo

    delPhoto photo
        =   viewInformation ("Delete photo","Do you want to delete this photo?") [ViewAs (\{original} ->original.Document.name)] photo
        >>? removeContactPhoto contactNo

manageContactCommunication :: ContactNo -> Task ()
manageContactCommunication contactNo
    = ((manageContactCommunicationMeans True contactNo) // <<@ AfterLayout (tweakUI fill)) //FIXME
       -&&-
       (viewContactCommunications contactNo) //<<@ AfterLayout (tweakUI fill)) //FIXME
      ) <<@ ArrangeWithSideBar 0 LeftSide 200 True <<@ Title "Communication" <<@ Icon "communication"
    @! ()
where
    viewContactCommunications contactNo
        = enterChoiceWithShared (Title "Communication history") [] (sdsFocus contactNo contactCommunications)

manageContactCommunicationMeans :: Bool ContactNo -> Task CommunicationMean
manageContactCommunicationMeans compact contactNo = forever (
		enterInformation "FIXME" []

//        editSelectionWithShared (Title "Communication means") [SelectInTree group] (sdsFocus contactNo contactCommunicationMeans) @? tvHd
    >^* [OnAction ActionAdd  (always (addMean contactNo <<@ InWindow @! ()))
        ,OnAction ActionEdit (hasValue (\{CommunicationMean|id} -> editMean id <<@ InWindow @! ()))
        ,OnAction ActionRemove (hasValue (\{CommunicationMean|id} -> removeMean id))
        ] 
    )
where
    ActionAdd = Action (if compact "Add" "/Add")
    ActionEdit = Action (if compact "Edit" "/Edit")
    ActionRemove = Action (if compact "Remove" "/Remove")

    group means expanded = [] /*[{ChoiceTree|label=label m,icon=Just (icon m.CommunicationMean.type),value=ChoiceNode i,type=LeafNode} \\ (i,m) <- means]
    where
        label {CommunicationMean|type=CMPhone,phoneNo} = fromMaybe "-" phoneNo
        label {CommunicationMean|type=CMVHF,callSign,mmsi} = fromMaybe "-" callSign +++ " / " +++ maybe "-" toString mmsi
        label {CommunicationMean|type=CMEmail,emailAddress} = maybe "-" toSingleLineText emailAddress
        label {CommunicationMean|type=CMP2000,capCode} = fromMaybe "-" capCode

        icon CMPhone = "phone-call"
        icon CMVHF = "vhf-call"
        icon CMEmail = "e-mail-message"
        icon CMP2000 = "p2000-message"
*/

    addMean contactNo
        =   enterInformation (Title "Add communication mean") [EnterAs toNewCommunicationMean]
        >>? createCommunicationMean contactNo

    editMean id
        = get (sdsFocus id communicationMeanById)
        >>- \c=:{CommunicationMean|type,phoneNo,callSign,mmsi,emailAddress,capCode} -> case type of
            CMPhone -> updateInformation (Title "Edit telephone details") [] {TelephoneDetails|phoneNo=phoneNo}
                       @ \{TelephoneDetails|phoneNo} -> {CommunicationMean|c & phoneNo = phoneNo}
            CMVHF   -> updateInformation (Title "Edit VHF details") [] {VHFRadioDetails|callSign=callSign,mmsi=mmsi}
                       @ \{VHFRadioDetails|callSign,mmsi} -> {CommunicationMean|c & callSign=callSign, mmsi=mmsi}
            CMEmail -> updateInformation (Title "Edit e-mail details") [] {EmailAccountDetails|emailAddress=emailAddress}
                       @ \{EmailAccountDetails|emailAddress} -> {CommunicationMean|c & emailAddress = emailAddress}
            CMP2000 -> updateInformation (Title "Edit P2000 details") [] {P2000ReceiverDetails|capCode = capCode}
                       @ \{P2000ReceiverDetails|capCode} -> {CommunicationMean|c & capCode = capCode}
        >>? \new ->
            set new (sdsFocus id communicationMeanById)

    removeMean id = deleteCommunicationMean id

manageContactActions :: Bool ContactNo -> Task ()
manageContactActions compact contactNo
	=	if compact selectActions selectAndWorkOnActions
    >^* [OnAction ActionAdd (always (addTopActionItem [contactNo] [] @! ()))
        ,OnAction ActionEdit (hasValue (\a -> editActionItem a @! ()))
        ]
	@!  ()
where
    ActionAdd = Action (if compact "Add" "/Add action")
    ActionEdit = Action (if compact "Edit" "/Edit action")

    title = (Title (if compact "Actions" "Overview"))
    selectActions
        = chooseActionItem title False False (sdsFocus contactNo actionStatusesByContact) // <<@ AfterLayout (tweakUI fill) //FIXME

    selectAndWorkOnActions
        = ( chooseActionItem title False True (sdsFocus contactNo actionStatusesByContact) // <<@ AfterLayout (tweakUI fill)) //FIXME
            >&> \s -> whileUnchanged s
                (\t -> case t of
                Just taskId    = workOnActionItem taskId @! taskId
                Nothing        = viewInformation () [] () @? const NoValue
                )
          ) <<@ (ArrangeWithSideBar 0 LeftSide 250 True) <<@ (Icon "actions") <<@ (Title "Actions")

manageContactIncidents :: Workspace ContactNo -> Task ()
manageContactIncidents ws contactNo
    =	feedForward choose
    (	\sel ->
        withSelection viewNoSelection viewIncidentDetails sel 
        -&&-
        doAddRemoveOpen (add <<@ InWindow) (\c -> (remove c) <<@ InWindow) (\c -> doOrClose (open c)) ws sel
    )	<<@ (ArrangeWithSideBar 1 RightSide 300 True) <<@ (Icon "incidents") <<@ (Title "Incidents")
    @! ()
where
    incidents   =   sdsFocus contactNo incidentsByContactDetails
    choose		=	enterChoiceWithSharedAs () [] incidents incidentDetailsIdentity
    open sel	=	manageIncidentInformation ws sel
    add			=	enterChoiceWithSharedAs ("Add contact to incident","Select an incident to add this contact to") [] allIncidentsShort incidentShortIdentity
                >>? \i ->
                    upd (\is -> [incidentNo \\{IncidentDetails|incidentNo} <-is] ++ [i]) incidents
                >>| logContactAdded i contactNo
    remove sel	=	viewSharedInformation ("Remove contact from incident","Are your sure you want this contact to be removed from this incident?") [] (sdsFocus sel incidentTitleByNo)
                >>* [OnAction ActionNo (always (return ()))
                    ,OnAction ActionYes (always (upd (\is -> [ incidentNo \\ {IncidentDetails|incidentNo} <- is | incidentNo <> sel]) incidents >>| logContactRemoved sel contactNo))
                    ]

	viewNoSelection = return () //FIXME
    incidentShortIdentity {IncidentShort|incidentNo} = incidentNo
    incidentDetailsIdentity {IncidentDetails|incidentNo} = incidentNo


viewAISInfo :: ContactNo -> Task ()
viewAISInfo contactNo = whileUnchanged (sdsFocus contactNo contactMMSI)
    \mbMMSI -> case mbMMSI of
        Nothing = viewInformation () [] () <<@ NoUserInterface
        Just mmsi = viewSharedInformation (Icon "ais","AIS","Latest AIS data") [] (sdsFocus mmsi AISContactByMMSI) @! ()

manageContactAccess :: ContactNo -> Task ()
manageContactAccess contactNo = (
        viewAndEdit (viewContactAccess contactNo) (editContactAccess contactNo)
    @!  ()
    ) <<@ Title "Partner access" <<@ Icon "access"
where
    access = sdsFocus contactNo contactAccess

    viewContactAccess contactNo
        = viewSharedInformation () [ViewAs view] access
    where
        view {ContactAccess|account=Nothing} = "This contact can not log in to Incidone"
        view {ContactAccess|account=Just {Credentials|username=Username uname}}
            = "This contact can log in to Incidone as user '"+++uname+++"'"

    editContactAccess contactNo
        =   get access
        >>- \original ->
            ( (Label "Account" @>> updateInformation () [] original.ContactAccess.account)
              -&&-
              (Label "Access level" @>> updateChoice () [ChooseFromCheckGroup viewLevel] [PartnerAccess,WOAccess] (fromMaybe PartnerAccess original.ContactAccess.access))
            )
        >>? \(updatedAccount,updatedAccess) ->
            set {ContactAccess|account=updatedAccount,access=Just updatedAccess} access

    viewLevel WOAccess = "Watch Officer access"
    viewLevel PartnerAccess = "Partner access"

viewContactDetails :: ContactNo -> Task ()
viewContactDetails contactNo
	= withHeader (viewSharedTitle (mapRead contactTitle contact))
	( viewPhoto contact
	  -&&-
	  viewSharedInformation () [] (mapRead contactDetails contact)
      -&&-
      viewContactCommunicationMeans contactNo
      -&&-
      viewSharedInformation "Actions:" [ViewAs viewActions] (sdsFocus contactNo actionStatusesByContact)
	) @! ()
where
	contact = sdsFocus contactNo contactByNo

	viewPhoto contact
		= viewSharedInformation () [ViewAs contactThumbHtml] contact

    viewActions items = case [title \\ (_,_,{ActionStatus|title}) <- items] of
        []      = ["There are no actions for this contact"]
        titles  = titles

viewContactCommunicationMeans :: ContactNo -> Task [CommunicationMean]
viewContactCommunicationMeans contactNo
    = viewSharedInformation "Communication means:" [ViewAs viewComms] (sdsFocus contactNo contactCommunicationMeans)
where
    viewComms items = TableTag [] (map viewComm items)
    where
        viewComm {CommunicationMean|type=CMPhone,phoneNo} = commRow "phone-call" ("Telephone :" +++ fromMaybe "-" phoneNo)
        viewComm {CommunicationMean|type=CMVHF,callSign,mmsi} = commRow "vhf-call" ("Callsign: " +++ fromMaybe "-" callSign +++ " / MMSI: " +++ maybe "-" toString mmsi)
        viewComm {CommunicationMean|type=CMEmail,emailAddress} = commRow "e-mail-message" ("E-mail: " +++ maybe "-" toSingleLineText emailAddress)
        viewComm {CommunicationMean|type=CMP2000,capCode} = commRow "p2000-message" ("P2000: " +++ fromMaybe "-" capCode)

        commRow icon label
            = TrTag [] [TdTag [] [DivTag [StyleAttr "width:16px;height:16px;",ClassAttr ("icon-"+++icon)] []],TdTag [] [Text label]]


viewAISContactDetails :: MMSI -> Task ()
viewAISContactDetails mmsi
    = withHeader (viewTitle (toString mmsi))
        (viewSharedInformation () [ViewAs (fmap aisToDetails)] (sdsFocus mmsi AISContactByMMSI)
         -&&-
         viewVesselWebLinks mmsi
        ) @! ()

viewContactHeader :: ContactNo -> Task ()
viewContactHeader contactNo
    = viewSharedInformation () [ViewAs toView] (sdsFocus contactNo contactByNo) @! ()
where
    toView c=:{Contact|photos,name}
        = DivTag [] [ImgTag [StyleAttr "float: left; margin-right: 5px":attributes]
                    ,H1Tag [StyleAttr "font-size: 30px; font-weight: normal"] [Text (contactTitle c)]
                    ]
    where
        (ImgTag attributes) = contactAvatarHtml c

viewVesselWebLinks :: MMSI -> Task ()
viewVesselWebLinks mmsi
    =   get webLinksConfig
    >>- \webLinks -> if (isEmpty webLinks.vesselLinks)
        (return ())
        (viewInformation "Find on the web" [ViewAs toLinks] webLinks.vesselLinks @! ())
where
    toLinks links
        = UlTag [] [LiTag [] [ATag [TargetAttr "_blank", HrefAttr (replaceSubString "{mmsi}" (toString mmsi) url)] [Text title]]\\{WebLink|title,url=URL url} <- links]

updateContactPosition :: ContactNo -> Task (Maybe (Maybe ContactPosition))
updateContactPosition contactNo
    =   get (sdsFocus contactNo contactByNo |+| standardMapLayers)
    >>- \({Contact|name,type,position},baseLayers) ->
        withShared (position,initPerspective position)
        \tmpInfo ->
        (updateSharedInformation ("Position update","Update position of contact "<+++ name) [UpdateAs fst (\(_,y) x -> (x,y))] tmpInfo
         -||-
         updateSharedInformation () [UpdateAs (toMap baseLayers) (fromMap baseLayers)] tmpInfo
         -||-
         viewSharedInformation "Search the web" [ViewAs (toSearchURLs o fst)] tmpInfo
        ) @ fst
    >>? \newPosition ->
        upd (\c -> {Contact|c&position=newPosition}) (sdsFocus contactNo contactByNo)
    >>| logContactPositionUpdated contactNo position newPosition
    @!  newPosition
where
	initPerspective position = {ContactMapPerspective|defaultValue & center = fromMaybe defaultValue.ContactMapPerspective.center contactPos, zoom = 7, cursor = contactPos}
    where
        contactPos = maybe Nothing latLng position

    toMap baseLayers (pos,perspective)
        = toLeafletMap (contactMap baseLayers (pos,perspective))
    fromMap baseLayers (pos,perspective) leafletMap
        # {ContactMap|perspective} = fromLeafletMap (contactMap baseLayers (pos,perspective)) leafletMap
        = case perspective.ContactMapPerspective.cursor of
            (Just cursor)   = (Just (PositionLatLng cursor),perspective)
            _               = (pos, perspective)

    fromMap _ info _ = info

    contactMap baseLayers (pos,perspective)
        = {ContactMap|defaultValue & perspective = {ContactMapPerspective|perspective & cursor = maybe Nothing latLng pos},layers = baseLayers}

    toSearchURLs Nothing    = UlTag [] []
    toSearchURLs (Just pos) = UlTag [] [LiTag [] [ATag [TargetAttr "_blank", HrefAttr ("http://maps.google.com/?q="+++toSingleLineText pos)] [Text "Search with Google Maps"]]
                                       ,LiTag [] [ATag [TargetAttr "_blank", HrefAttr ("http://www.bing.com/maps/?q="+++toSingleLineText pos)] [Text "Search with Bing Maps"]]
                                       ]

updateContactStatus :: ContactNo -> Task (Maybe (Maybe ContactStatus))
updateContactStatus contactNo
    =   get (sdsFocus contactNo contactByNo)
    >>- \{Contact|status} ->
        updateInformation (Title "Status") [] status
    >>? \newStatus ->
        upd (\c -> {Contact|c&status = newStatus}) (sdsFocus contactNo contactByNo)
    >>| logContactStatusUpdated contactNo status newStatus
    @!  newStatus

updateSharedContactRefList :: d (RWShared () [ContactNo] [ContactNo]) -> Task [ContactNo] | toPrompt d
updateSharedContactRefList d refs
    =   manageCurrentItems
    >^* [OnAction (Action "Add") (always (addItem <<@ InWindow))]
where
    manageCurrentItems
        = updateSharedInformation d [UpdateAs toPrj fromPrj] items @ map contactIdentity
    where
        items = sdsDeref refs id contactsByNosShort (\_ cs -> cs)
        toPrj l = [(contactIdentity c,contactTitle c) \\ c <-l]
        fromPrj _ items = map fst items

    addItem
        =   selectKnownOrDefineNewContact
        >>? (\def -> createContactIfNew def >>- \contactNo -> upd (\r -> r ++ [contactNo]) refs)

selectKnownOrDefineNewContact :: Task (Either ContactNo NewContact)
selectKnownOrDefineNewContact
    = oneOrAnother ("Add contact...","You can either select a know contact, or define a new one.")
        ("Known contact", chooseKnownContact)
        ("Add new contact",enterNewContact)
where
    chooseKnownContact
        = enterChoiceWithSharedAs () [ChooseFromDropdown id] allContactsShort contactIdentity
    enterNewContact
        = enterInformation () []

createContactIfNew :: (Either ContactNo NewContact) -> Task ContactNo
createContactIfNew (Left no) = return no
createContactIfNew (Right contact) = createContact contact

createContact :: NewContact -> Task ContactNo
createContact {NewContact|type,name,position,needsHelp}
	=   get databaseDef
	>>- \db -> sqlExecute db ["allContacts"] (
		execInsert "INSERT INTO Contact (`type`,`name`,`position_lat`,`position_lon`,`position_desc`,`needsHelp`) VALUES (?,?,?,?,?,?)"
			(flatten
            [mbToSQL type
			,mbToSQL name
			,mbToSQL position
			,toSQL   needsHelp
			]))

deleteContact :: ContactNo -> Task ()
deleteContact contactNo
	=		delete contactNo
	-&&-	upd (\m -> 'DM'.put contactNo [] m) (sdsFocus (Just [contactNo]) incidentNosByContactNosIndexed)
	-&&-	upd (\m -> 'DM'.put contactNo [] m) (sdsFocus (Just [contactNo]) communicationNosByContactNosIndexed)
	@!  ()
where
	delete :: ContactNo -> Task ()
	delete contactNo
		=	get databaseDef
		>>= \db -> sqlExecute db ["allContacts"] (execDelete "DELETE FROM Contact WHERE contactNo = ?" (toSQL contactNo)) @! ()

addContactPhoto :: ContactNo Document -> Task ContactPhoto
addContactPhoto contactNo original
    =   withTemporaryDirectory
        \tmp ->
        exportDocument (tmp</>"orig.jpg") original
    >>- \_ ->
        callProcess "Creating thumbnail..." [] CONVERT_BIN
            ["-define","jpeg:size=400x400",(tmp</>"orig.jpg"),"-thumbnail","200x200^","-gravity","center","-extent","200x200",(tmp</>"thumb.png")] Nothing False
    >>- \_ ->
        importDocument (tmp</>"thumb.png")
    >>- \thumb ->
        callProcess "Creating avatar..." [] CONVERT_BIN
            ["-define","jpeg:size=100x100",(tmp</>"orig.jpg"),"-thumbnail","50x50^","-gravity","center","-extent","50x50",(tmp</>"avatar.png")] Nothing False
    >>- \_ ->
        importDocument (tmp</>"avatar.png")
    >>- \avatar -> let photo = {ContactPhoto|original = original, thumb = thumb, avatar = avatar} in
        upd (\photos -> 'DM'.put contactNo [photo:fromMaybe [] ('DM'.get contactNo photos)] photos) allContactPhotos
    >>- \_ ->
        logContactPhotoAdded contactNo photo
    @!  photo

removeContactPhoto :: ContactNo ContactPhoto -> Task ContactPhoto
removeContactPhoto contactNo photo
    =  upd (\photos -> 'DM'.put contactNo (removePhoto photo (fromMaybe [] ('DM'.get contactNo photos))) photos) allContactPhotos
    @! photo
where
    removePhoto photo photos = [p \\ p <- photos | p =!= photo]

createCommunicationMean :: ContactNo NewCommunicationMean -> Task CommunicationMeanId
createCommunicationMean contactNo mean=:{NewCommunicationMean|type,phoneNo,callSign,mmsi,emailAddress,capCode}
    =   get databaseDef
    >>- \db ->
        sqlExecute db ["allCommunicationMeans"] (execInsert "INSERT INTO CommunicationMean (type) VALUES (?)" (toSQL type))
    >>- \id ->
        sqlExecute db [] (execInsert "INSERT INTO communicationMeans1_communicationMeans2 (communicationMeans1,communicationMeans2) VALUES (?,?)" (toSQL id ++ toSQL contactNo))
    >>- \_ -> case type of
        CMPhone = sqlExecute db [] (execInsert "INSERT INTO Telephone (id,phoneNo) VALUES (?,?)" (toSQL id ++ mbToSQL phoneNo))
        CMVHF   = sqlExecute db [] (execInsert "INSERT INTO VHFRadio (id,callSign,mmsi) VALUES (?,?,?)" (toSQL id ++ mbToSQL callSign ++ mbToSQL mmsi))
        CMEmail = sqlExecute db [] (execInsert "INSERT INTO EmailAccount (id,emailAddress) VALUES (?,?)" (toSQL id ++ mbToSQL emailAddress))
        CMP2000 = sqlExecute db [] (execInsert "INSERT INTO P2000Receiver (id,capCode) VALUES (?,?)" (toSQL id ++ mbToSQL capCode))

deleteCommunicationMean :: CommunicationMeanId -> Task ()
deleteCommunicationMean id
    =   get databaseDef
    >>- \db ->
        sqlExecute db ["allCommunicationMeans"] (execDelete "DELETE FROM communicationMeans1_communicationMeans2 WHERE communicationMeans1 = ? " (toSQL id))
    >>- \_ ->
        allTasks [sqlExecute db [] (execDelete ("DELETE FROM " +++ table +++" WHERE id = ? ") (toSQL id)) \\ table <-
                    ["CommunicationMean","Telephone","VHFRadio","EmailAccount","P2000Receiver"]]
    @!  ()

updatePosition :: ContactPosition String (Shared Contact) -> Task Contact
updatePosition newposition src contact
	= upd (update newposition src) contact
where
	update newposition src contact=:{Contact|position}
		= {Contact|contact & position = Just newposition}

verifyContactCredentials :: Credentials -> Task (Maybe User)
verifyContactCredentials credentials
    = get (sdsFocus credentials contactByCredentials)
    @ fmap contactUser

viewContactsOnMap :: (ReadWriteShared [ContactGeo] w) (Shared (Maybe (Either ContactNo MMSI))) -> Task (Either ContactNo MMSI) | iTask w
viewContactsOnMap sharedContacts sel
   =   get (standardMapLayers |+| standardPerspective)
   >>- \(baseLayers,perspective) ->
       withShared (False,perspective)
       \localState ->
            updateSharedInformation "Show AIS contacts:" [UpdateAs fst (\(_,y) x -> (x,y))] localState
            ||-
            (updateSharedInformation () [UpdateAs (toPrj baseLayers) fromPrj] (mapState localState sharedContacts sel)) @ (\(a,b,c) -> (b,c))
            >^* [OnAction (Action "/Share map to wall") (hasValue sharePerspective)
                ]
        @? selection
where
    mapState :: (Shared (Bool,ContactMapPerspective))
                (ReadWriteShared [ContactGeo] w)
                (Shared (Maybe (Either ContactNo MMSI))) ->
                ReadWriteShared ([(Bool,ContactGeo)], Maybe (Either ContactNo MMSI), ContactMapPerspective) (Maybe (Either ContactNo MMSI), ContactMapPerspective) | iTask w
    mapState local contacts sel = sdsSequence "mapState" id (\_ r -> r) (\_ _ -> Right read) writel writer (local >+< sel) mapContacts
    where
        mapContacts = sdsSelect "mapContacts" choose (SDSNotifyConst (\_ _ _ -> False)) (SDSNotifyConst (\_ _ _ -> False)) withoutAISContacts withAISContacts
        where
            choose ((withAIS,{ContactMapPerspective|bounds=Just bounds}),_) = (if withAIS (Right bounds) (Left bounds))
            choose _                                                        = (Left defaultValue)

            baseContacts = mapRead (\cs -> [(False,c) \\ c<-cs]) (toReadOnly contacts)
            aisContacts = mapRead (\cs -> [(True,c)\\c=:{ContactGeo|position=Just position}<-map aisToContactGeo cs]) (toReadOnly boundedAISContacts)

            withoutAISContacts = sdsFocus () baseContacts
            withAISContacts = mapRead (\(a,b) -> a++b) (aisContacts |+| sdsFocus () baseContacts)

        read (((showAis,perspective),mbSel),contacts) = (contacts,mbSel,perspective)
        writel = SDSWrite (\_ ((showAis,_),_) (mbSel,perspective) -> Ok (Just ((showAis,perspective),mbSel)))
        writer = SDSWriteConst (\_ _ -> Ok Nothing)

    toPrj baseLayers (contacts,sel,perspective)
        = toLeafletMap {ContactMap|perspective=perspective,layers=[{title="Contacts",def=CMMarkersLayer (toMarkers sel contacts)}:baseLayers]}

    fromPrj (contacts,sel,_) map=:{LeafletMap|perspective}
        = (maybe sel Just (updateSelection (selectionFromLeafletMap map)),fromLeafletPerspective perspective)

	selection (Value (Just no,_) stable)	= Value no stable
	selection _								= NoValue

    sharePerspective (_,perspective) = set (WallOverview perspective) wallContent @! ()
	
    toMarkers sel contacts
        = [contactGeoToMapMarker ais (isSelected contactNo sel) c \\ (ais,c=:{ContactGeo|contactNo,name=Just _,position=Just _}) <- contacts]

    isSelected contactNo (Just (Left no)) = no == contactNo
    isSelected contactNo (Just (Right no)) = no == contactNo
    isSelected _ _ = False

    updateSelection [] = Nothing
	updateSelection [markerId:ms]
        | startsWith "c" markerId   = Just (Left (toInt (subString 1 (textSize markerId) markerId)))
        | startsWith "a" markerId   = Just (Right (toInt (subString 1 (textSize markerId) markerId)))
                                    = updateSelection ms
		
	findContactNo title contacts = case [(isAis,contactNo) \\ (isAis,{ContactGeo|contactNo,name}) <- contacts | name == title] of
		[(False,contactNo)] = Just (Left contactNo)
		[(True,mmsi)]   = Just (Right mmsi)
		_			    = Nothing

