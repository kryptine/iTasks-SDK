implementation module Incidone.RoleBased.AdministratorTasks

import iTasks, iTasks.API.Extensions.SQLDatabase
import Incidone.OP.Concepts, Incidone.OP.SDSs, Incidone.OP.DemonstrationTasks
import Incidone.OP.ContactManagementTasks
import Incidone.OP.ImportExportTasks
import Incidone.Configuration
import Incidone.Integration.AIS
import Incidone.Integration.Asterisk
import Incidone.ContactPosition
import Incidone.ActionManagementTasks
import Incidone.Util.TaskPatterns
import iTasks.API.Extensions.Admin.ServerAdmin
import iTasks.API.Extensions.Dashboard

:: DatabaseProblem
    = NoDatabaseAccess
    | NoDatabaseTables
    | IncorrectDatabaseTables

derive class iTask DatabaseProblem

configureIncidone :: [Workspace -> Task ()]
configureIncidone = map const [configureDatabase
                              ,manageUsers              <<@ (Title "Users")
                              ,manageDemoData
                              ,configureIntegration
                              ,manageUserActionCatalog
                              ,configureMaps            <<@ (Title "Maps")
                              ,configureWebLinks        <<@ (Title "Web integration")
//                              ,(manageServer @! ())     <<@ (Title "Processes")
                              ]
configureDatabase :: Task ()
configureDatabase
    =   viewDatabaseConfigStatus
    >^* [OnAction (Action "Configure database" []) (always (doOrCancel setupDatabase <<@ InWindow))
        ,OnAction (Action "Manage database" []) (ifValue hasAccess (\_ -> doOrClose manageDatabase <<@ AfterLayout (uiDefSetSize (ExactSize 800) (ExactSize 600)) <<@ InWindow))
        ]
    @! ()
where
    hasAccess (Error NoDatabaseAccess)  = False
    hasAccess _                         = True

    viewDatabaseConfigStatus
        =   whileUnchanged databaseConfig
        \config ->
            checkDatabaseConfig config
        >>- viewInformation (Title "Database configuration") [ViewWith databaseStatusView]

    databaseStatusView (Ok InternalSQLiteDB)            = Row (LightOnGreen, "Incidone is correctly configured to use an internal SQLite database.")
    databaseStatusView (Ok (ExternalMySQLDB _))         = Row (LightOnGreen, "Incidone is correctly configured to use an external MySQL database.")
    databaseStatusView (Error NoDatabaseAccess)         = Row (LightOnRed, "A database is configured, but it can not be accessed. Please reconfigure.")
    databaseStatusView (Error NoDatabaseTables)         = Row (LightOnRed, "A database is configured, but it contains no tables. Please reconfigure.")
    databaseStatusView (Error IncorrectDatabaseTables)  = Row (LightOnRed, "A database is configured, but it contains other tables than Incidone's. Please reconfigure.")

    setupDatabase
        =   get databaseConfig
        >>- editDatabaseConfig
    where
        editDatabaseConfig config
            =   updateInformation ("Database configuration","Please edit settings") [] config
            >>= \newConfig ->
                checkDatabaseConfig newConfig
            >>- \mbError -> case mbError of
                Ok config
                    = set config databaseConfig @! ()
                Error NoDatabaseTables
                    =   set newConfig databaseConfig
                    >>| viewInformation ("Create tables","The database you configured does not have tables set up yet.") [] "Do you want to create them now?"
                    >>* [OnAction ActionYes (always (createIncidoneTables (toDatabaseDef newConfig) @! ()))
                        ,OnAction ActionNo (always (return ()))
                        ]

                Error e
                    =   viewInformation ("Warning","The new configuration appears to have a problem") [ViewWith databaseStatusView] (Error e)
                    >>* [OnAction (Action "Set anyway" []) (always (set config databaseConfig @! ()))
                        ,OnAction (Action "Change and try again" []) (always (editDatabaseConfig newConfig))
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
          (  (enterChoiceWithShared (Title "Tables") [ChooseWith (ChooseFromTree group)] (sdsFocus db sqlTables)
              >^* [OnAction ActionDelete (hasValue (\table -> deleteTable db table <<@ InWindow @! ()))
                  ,OnAction (Action "Empty database" []) (always (emptyDatabase db <<@ InWindow @! ()))
                  ,OnAction (Action "Load Incidone tables" []) (always (createIncidoneTables db <<@ InWindow ))
                  ]
             )
        >&> withSelection viewNoSelection
            \table ->
            catchAll (
                viewSharedInformation (Title ("Schema of"+++ table)) [] (sdsFocus (db,table) sqlTableDefinition) @! ()
            ) (\e -> viewInformation () [] e @! ())
        ) <<@ (ArrangeWithSideBar 0 LeftSide 300 True)
    where
        group items _ = [{ChoiceTree|defaultValue & label=o,value=ChoiceNode i}\\(i,o) <- items]

        deleteTable db table
            =   viewInformation "Are your sure you want to delete this table?" [] table
            >>? \_ -> sqlExecuteDropTable db table
            @! ()

    createIncidoneTables db
        =   (sequence "Creating Incidone tables" [sqlExecuteCreateTable db table \\ table <- IncidoneDB]
        >>- viewInformation "Incidone schema created" []) <<@ Title "Creating Incidone tables..."
        >>* [OnAction ActionOk (always (return ()))]

    emptyDatabase db
        =   viewInformation (Title "Empty database") [] "Are your sure you want fully remove all database tables?"
        >>? \_ ->
            get (sdsFocus db sqlTables)
        >>- \tables ->
            sequence "Deleting all tables" [sqlExecuteDropTable db table \\ table <- tables]
        >>- viewInformation ("Empty database","All data deleted") []
        >>* [OnAction ActionOk (always (return ()))]

manageUsers :: Task ()
manageUsers = forever (catchAll (
        manageExistingUsers
    >^* [OnAction (Action "/Add" []) (always (addUser <<@ InWindow))
        ,OnAction (Action "/Import from CSV" []) (always (importUsers <<@ InWindow))
        ,OnAction (Action "/Set admin password" []) (always (setAdminPassword <<@ InWindow))
        ]
      ) (\e -> viewInformation "Error" [] e >>| return ()))
where
    manageExistingUsers
        =   (enterChoiceWithSharedAs () [ChooseWith (ChooseFromGrid id)] allContactsShort contactIdentity 
        >&> withSelection viewNoSelection manageContactAccess
        )<<@ ArrangeWithSideBar 0 LeftSide 200 True

	viewNoSelection = return () //FIXME
    addUser
        =   enterInformation (Title "Add user") []
        >>? \newUser -> (createContact newUser @! ())

    importUsers = doOrClose (
            enterInformation instructions []
        >>= \doc -> catchAll (
                importContactsFromCSVFile doc >>- \_ -> viewInformation () [] "Succesfully imported contacts" >>| return ()
            ) (\_ -> viewInformation "Failed to import contacts" [] ())
        ) <<@ Title "Import contacts"
	where
		instructions = toString (PTag [] [Text "Please select a CSV file to upload.",BrTag []
							,Text "The file needs to be formatted like ",ATag [HrefAttr "/demo-content/contacts.csv",TargetAttr "_blank"] [Text "this example file"]
							])

    setAdminPassword = (
            enterPasswords -&&- get adminPassword
        >>* [OnAction ActionCancel (always (return Nothing))
            ,OnAction (Action "Change" [ActionIcon "ok"]) (hasValue updatePassword)
            ]
        ) <<@ Title "Set admin password"
    where
        enterPasswords
            =    (Label "Old password"           @>> enterInformation () [])
            -&&- (Label "New password"           @>> enterInformation () [])
            -&&- (Label "New password (again)"   @>> enterInformation () [])
            @ \(o,(n1,n2)) -> (o,n1,n2)

        updatePassword ((old,new1,new2),current)
            | old =!= current = viewInformation "Error" [] "You did not enter the correct old password" >>| return Nothing
            | new1 =!= new2   = viewInformation "Error" [] "The new passwords are not the same" >>| return Nothing
                              = set new1 adminPassword
                              >>| viewInformation () [] "The admin password has been updated" >>| return (Just ())

manageDemoData :: Task ()
manageDemoData
    = allTasks
        [forever generateTestIncidents <<@ Title "Generate demo incidents"
        ] <<@ Title "Demo data"
    @! ()
where
    generateTestIncidents
        =   enterInformation "Enter the number of demo incidents that you would like to create and press continue" []
            -&&-
            enterInformation "Immediate close the incidents?" []
        >>= \(num,closed) ->
            sequence "Generating test incidents" (repeatn num (generateTestIncident closed))
        @! ()

configureIntegration :: Task ()
configureIntegration
    = anyTask [configureAISIntegration,configureAsteriskIntegration,configureEmailIntegration] <<@ ForceLayout <<@ Title "Integration"
    @! ()
where
    configureAISIntegration
        =   updateSharedInformation ("AIS server configuration","Please supply the host and port of the AIS server") [] aisLinkConfig
        ||- manageBackgroundTask "AIS Link Status" "ais-sync" "AIS Synchronisation" syncAISStream

    configureAsteriskIntegration
        = updateSharedInformation ("Asterisk server configuration","Please supply the configuration for your asterisk server") [] asteriskLinkConfig
        ||- manageBackgroundTask "Asterisk Link Status" "asterisk-sync" "Asterisk Synchronisation" syncAsteriskAMI
    configureEmailIntegration
        = updateSharedInformation ("SMTP server configuration","Please supply the configuration for your SMTP server") [] smtpConfig
        @! ()

configureMaps :: Task ()
configureMaps
    =   (viewOrEdit (Title "Default map perspective") standardPerspective (\_ _ -> return ())
         -||-
         viewOrEdit (Title "Map layer definitions") standardMapLayers (\_ _ -> return ())
         <<@ ArrangeVertical
        )
    -|| previewMapLayers
    <<@ (ArrangeSplit Horizontal True) <<@ (Title "Manage map layers")
where
    previewMapLayers :: Task ContactMapPerspective
    previewMapLayers = withShared defaultValue
        \perspective -> updateSharedInformation (Title "Preview") [UpdateWith toPrj fromPrj] (perspective >+| standardMapLayers) <<@ AfterLayout (tweakUI fill)
    where
        toPrj (perspective,layers) = toLeafletMap {ContactMap|defaultValue & perspective=perspective,layers=layers}
        fromPrj _ {LeafletMap|perspective} = fromLeafletPerspective perspective

configureWebLinks :: Task ()
configureWebLinks
    = viewAndEdit (viewSharedInformation "Web integration configuration" [] webLinksConfig)
                  (get webLinksConfig >>- updateInformation () [] >>? \updated -> set updated webLinksConfig)
    >^* [OnAction (Action "/Export" []) (always (exportConfig <<@ InWindow))
        ,OnAction (Action "/Import" []) (always (importConfig <<@ InWindow))]
    @! ()
where
    exportConfig
        =   doOrClose (
                get (webLinksConfig |+| currentDateTime)
            >>- \(config,now) -> createJSONFile ("Incidone-weblinks-" +++ paddedDateTimeString now +++ ".json") config
            >>- viewInformation "An export file has been created" []
            @!  ()
            ) <<@ Title "Export web links"

    importConfig
        =   doOrClose (
            enterInformation instructions []
            >>= \doc -> catchAll (
                    importJSONDocument doc
                >>- \config ->
                    set config webLinksConfig
                >>- \_ -> viewInformation () [] "Succesfully imported web links" @! ()
                ) (\e -> viewInformation "Failed import of web links" [] e @! ())
            ) <<@ Title "Import web links"
    where
        instructions = toString 
            (PTag [] [Text "Please select a JSON export file to upload.",BrTag []
                     ,Text "The file needs to be formatted like ",ATag [HrefAttr "/demo-content/weblinks.json",TargetAttr "_blank"] [Text "weblinks.json"]
                     ])


