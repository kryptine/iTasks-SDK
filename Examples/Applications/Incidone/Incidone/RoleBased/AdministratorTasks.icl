implementation module Incidone.RoleBased.AdministratorTasks

import iTasks, iTasks.Extensions.SQLDatabase, iTasks.Extensions.DateTime
import iTasks.Extensions.Document, iTasks.Extensions.JSONFile
import Incidone.OP.Concepts, Incidone.OP.SDSs, Incidone.OP.DemonstrationTasks
import Incidone.OP.ContactManagementTasks
import Incidone.OP.ImportExportTasks
import Incidone.Configuration
import Incidone.Integration.AIS
import Incidone.Integration.Asterisk
import Incidone.ContactPosition
import Incidone.ActionManagementTasks
import Incidone.Util.TaskPatterns
import iTasks.Extensions.Admin.ServerAdmin
import iTasks.Extensions.Dashboard
import iTasks.UI.Layout, iTasks.UI.Definition
import Text.HTML

:: DatabaseProblem
    = NoDatabaseAccess
    | NoDatabaseTables
    | IncorrectDatabaseTables

derive class iTask DatabaseProblem
derive gDefault ContactMap, ContactMapLayer, ContactMapLayerDefinition, ContactMapMarker, ContactMapRegion, ContactTrack, ContactMapMarkerType, DateTime

configureIncidone :: [Workspace -> Task ()]
configureIncidone = map const [configureDatabase		<<@ (Title "Database")
                              ,manageUsers              <<@ (Title "Users")
                              ,manageDemoData
                              ,configureIntegration
                              ,manageUserActionCatalog  <<@ (Title "Action catalog")
                              ,configureMaps            <<@ (Title "Maps")
                              ,configureWebLinks        <<@ (Title "Web integration")
//                              ,(manageServer @! ())     <<@ (Title "Processes")
                              ]
configureDatabase :: Task ()
configureDatabase
    =   viewDatabaseConfigStatus
    >^* [OnAction (Action "Configure database") (always (doOrCancel setupDatabase <<@ InWindow))
        ,OnAction (Action "Manage database") (ifValue hasAccess (\_ -> doOrClose manageDatabase /* <<@ AfterLayout (uiDefSetSize (ExactSize 800) (ExactSize 600))*/ <<@ InWindow))
        ]
    @! ()
where
    hasAccess (Error NoDatabaseAccess)  = False
    hasAccess _                         = True

    viewDatabaseConfigStatus
        =   whileUnchanged databaseConfig
        \config ->
            checkDatabaseConfig config
        >>- \status -> Title "Database configuration" @>> viewInformation [ViewAs databaseStatusView] status

    databaseStatusView (Ok InternalSQLiteDB)            = (LightOnGreen, "Incidone is correctly configured to use an internal SQLite database.")
    databaseStatusView (Ok (ExternalMySQLDB _))         = (LightOnGreen, "Incidone is correctly configured to use an external MySQL database.")
    databaseStatusView (Error NoDatabaseAccess)         = (LightOnRed, "A database is configured, but it can not be accessed. Please reconfigure.")
    databaseStatusView (Error NoDatabaseTables)         = (LightOnRed, "A database is configured, but it contains no tables. Please reconfigure.")
    databaseStatusView (Error IncorrectDatabaseTables)  = (LightOnRed, "A database is configured, but it contains other tables than Incidone's. Please reconfigure.")

    setupDatabase
        =   get databaseConfig
        >>- editDatabaseConfig
    where
        editDatabaseConfig config
            =   Title "Database configuration" @>> Hint "Please edit settings" @>> updateInformation [] config
            >>! \newConfig ->
                checkDatabaseConfig newConfig
            >>- \mbError -> case mbError of
                Ok config
                    = set config databaseConfig @! ()
                Error NoDatabaseTables
                    =   set newConfig databaseConfig
                    >-| Title "Create tables" @>> Hint "The database you configured does not have tables set up yet." @>> viewInformation [] "Do you want to create them now?"
                    >>* [OnAction ActionYes (always (createIncidoneTables (toDatabaseDef newConfig) @! ()))
                        ,OnAction ActionNo (always (return ()))
                        ]

                Error e
                    =   Title "Warning" @>> Hint "The new configuration appears to have a problem" @>> viewInformation [ViewAs databaseStatusView] (Error e)
                    >>* [OnAction (Action "Set anyway") (always (set config databaseConfig @! ()))
                        ,OnAction (Action "Change and try again") (always (editDatabaseConfig newConfig))
                        ]

    checkDatabaseConfig config
        = catchAll
            ( get (sdsFocus (toDatabaseDef config) sqlTables)
             >>- \tables -> if (isEmpty tables)
                (return (Error NoDatabaseTables))
                (if (checkTables tables)
                    (return (Ok config))
                    (return (Error IncorrectDatabaseTables))
                )
            )
            (\e -> return (Error NoDatabaseAccess))
    where
        checkTables tables = foldl (\c t -> c && isMember t.SQLTable.name tables) True IncidoneDB

    manageDatabase
        =   get databaseDef
        >>- \db ->
          (  ((Title "Tables") @>> enterChoiceWithShared [/*ChooseFromTree group */] (sdsFocus db sqlTables)
              >^* [OnAction ActionDelete (hasValue (\table -> deleteTable db table <<@ InWindow @! ()))
                  ,OnAction (Action "Empty database") (always (emptyDatabase db <<@ InWindow @! ()))
                  ,OnAction (Action "Load Incidone tables") (always (createIncidoneTables db <<@ InWindow ))
                  ]
             )
        >&> withSelection viewNoSelection
            \table ->
            catchAll (
                (Title ("Schema of"+++ table)) @>> viewSharedInformation  [] (sdsFocus (db,table) sqlTableDefinition) @! ()
            ) (\e -> viewInformation [] e @! ())
        ) <<@ (ArrangeWithSideBar 0 LeftSide True)
    where
        //group items _ = [{ChoiceTree|defaultValue & label=o,value=ChoiceNode i}\\(i,o) <- items]

        deleteTable db table
            =   Hint "Are your sure you want to delete this table?" @>> viewInformation [] table
            >?? \_ -> sqlExecuteDropTable db table
            @! ()

    createIncidoneTables db
        =   (sequence [sqlExecuteCreateTable db table \\ table <- IncidoneDB]
             >>- \result ->
			 Hint "Incidone schema created" @>> viewInformation [] result
			) <<@ Title "Creating Incidone tables..."
        >>* [OnAction ActionOk (always (return ()))]

    emptyDatabase db
        =   Title "Empty database" @>> viewInformation [] "Are your sure you want fully remove all database tables?"
        >?? \_ ->
            get (sdsFocus db sqlTables)
        >>- \tables ->
            sequence [sqlExecuteDropTable db table \\ table <- tables]
        >>- \result -> 
			Title "Empty database" @>> Hint "All data deleted" @>> viewInformation [] result
        >>* [OnAction ActionOk (always (return ()))]

manageUsers :: Task ()
manageUsers = forever (catchAll (
        manageExistingUsers
    >^* [OnAction (Action "/Add") (always (addUser <<@ InWindow))
        ,OnAction (Action "/Import from CSV") (always (importUsers <<@ InWindow))
        ,OnAction (Action "/Set admin password") (always (setAdminPassword <<@ InWindow))
        ]
      ) (\e -> Hint "Error" @>> viewInformation [] e >!| return ()))
where
    manageExistingUsers
        =   (enterChoiceWithSharedAs [ChooseFromGrid id] allContactsShort contactIdentity
        >&> withSelection viewNoSelection manageContactAccess
        )<<@ ArrangeWithSideBar 0 LeftSide True

	viewNoSelection = Hint "Select a user" @>> viewInformation [] ()
    addUser
        =   (Title "Add user") @>> enterInformation []
        >?? \newUser -> (createContact newUser @! ())

    importUsers = doOrClose (
            Hint instructions @>> enterInformation []
        >>! \doc -> catchAll (
                importContactsFromCSVFile doc >-| viewInformation [] "Succesfully imported contacts" >!| return ()
            ) (\_ -> viewInformation [] "Failed to import contacts" @! ())
        ) <<@ Title "Import contacts"
	where
		instructions = "Please select a CSV file to upload.\n" +++
					   "The file needs to be formatted like the example /demo-content/contacts.csv file."

    setAdminPassword = (
            enterPasswords -&&- get adminPassword
        >>* [OnAction ActionCancel (always (return Nothing))
            ,OnAction (Action "Change") (hasValue updatePassword)
            ]
        ) <<@ Title "Set admin password"
    where
        enterPasswords
            =    (Label "Old password"           @>> enterInformation [])
            -&&- (Label "New password"           @>> enterInformation [])
            -&&- (Label "New password (again)"   @>> enterInformation [])
            @ \(o,(n1,n2)) -> (o,n1,n2)

        updatePassword ((old,new1,new2),current)
            | old =!= current = Hint "Error" @>> viewInformation [] "You did not enter the correct old password" >!| return Nothing
            | new1 =!= new2   = Hint "Error" @>> viewInformation [] "The new passwords are not the same" >!| return Nothing
                              = set new1 adminPassword
                              >-| viewInformation [] "The admin password has been updated" >!| return (Just ())

manageDemoData :: Task ()
manageDemoData
    = allTasks
        [forever generateTestIncidents <<@ Title "Generate demo incidents"
        ] <<@ Title "Demo data"
    @! ()
where
    generateTestIncidents
        =   (Hint "Enter the number of demo incidents that you would like to create and press continue" @>> enterInformation [])
            -&&-
            (Hint "Immediate close the incidents?" @>> enterInformation [])
        >>! \(num,closed) ->
            sequence (repeatn num (generateTestIncident closed))
        @! ()

configureIntegration :: Task ()
configureIntegration
    = anyTask [configureAISIntegration,configureAsteriskIntegration,configureEmailIntegration] <<@ Title "Integration"
    @! ()
where
    configureAISIntegration
        =   Title "AIS server configuration" @>> Hint "Please supply the host and port of the AIS server" @>>
			updateSharedInformation [] aisLinkConfig
        ||- (Hint "AIS Link Status" @>> manageBackgroundTask "ais-sync" "AIS Synchronisation" syncAISStream)

    configureAsteriskIntegration
        =   Title "Asterisk server configuration" @>> Hint "Please supply the configuration for your asterisk server" @>>
			updateSharedInformation [] asteriskLinkConfig
        ||- (Hint "Asterisk Link Status" @>> manageBackgroundTask "asterisk-sync" "Asterisk Synchronisation" syncAsteriskAMI)
    configureEmailIntegration
        =   Title "SMTP server configuration" @>> Hint "Please supply the configuration for your SMTP server" @>>
			updateSharedInformation [] smtpConfig
        @! ()

configureMaps :: Task ()
configureMaps
    =   ((Title "Default map perspective" @>> viewOrEdit standardPerspective (\_ _ -> return ()))
         -||-
         (Title "Map layer definitions" @>> viewOrEdit  standardMapLayers (\_ _ -> return ()))
         <<@ ArrangeVertical
        )
    -|| previewMapLayers
    <<@ (ArrangeSplit Horizontal True) <<@ (Title "Manage map layers")
where
    previewMapLayers :: Task ContactMapPerspective
    previewMapLayers = withShared defaultValue
        \perspective -> (Title "Preview" @>> updateSharedInformation [UpdateSharedAs toPrj fromPrj (const o Just)] (perspective >*| standardMapLayers)) <<@ ApplyLayout flexMap @ fst
    where
        toPrj (perspective,layers) = toLeafletMap {ContactMap|defaultValue & perspective=perspective,layers=layers}
        fromPrj _ {LeafletMap|perspective} = fromLeafletPerspective perspective
		flexMap = layoutSubUIs (SelectByPath [1]) (setUIAttributes (sizeAttr FlexSize FlexSize))

configureWebLinks :: Task ()
configureWebLinks
    = viewAndEdit (Hint "Web integration configuration" @>> viewSharedInformation [] webLinksConfig)
                  (get webLinksConfig >>- updateInformation [] >?? \updated -> set updated webLinksConfig)
    >^* [OnAction (Action "/Export") (always (exportConfig <<@ InWindow))
        ,OnAction (Action "/Import") (always (importConfig <<@ InWindow))]
    @! ()
where
    exportConfig
        =   doOrClose (
                get (webLinksConfig |*| currentDateTime)
            >>- \(config,now) -> createJSONFile ("Incidone-weblinks-" +++ paddedDateTimeString now +++ ".json") config
            >>- \result -> Hint "An export file has been created" @>> viewInformation [] result
            @!  ()
            ) <<@ Title "Export web links"
	where
		paddedDateTimeString {DateTime|year,mon,day,hour,min,sec}
			= toString year +++ toString mon +++ toString day +++ toString hour +++ toString min +++ toString sec

    importConfig
        =   doOrClose (
            (Hint instructions @>> enterInformation [])
            >>! \doc -> catchAll (
                    importJSONDocument doc
                >>- \config ->
                    set config webLinksConfig
                >-| Hint "Succesfully imported web links" @>> viewInformation [] () @! ()
                ) (\e -> (Hint "Failed import of web links" @>> viewInformation [] e) @! ())
            ) <<@ Title "Import web links"
    where
        instructions = toString
            (PTag [] [Text "Please select a JSON export file to upload.",BrTag []
                     ,Text "The file needs to be formatted like ",ATag [HrefAttr "/demo-content/weblinks.json",TargetAttr "_blank"] [Text "weblinks.json"]
                     ])


