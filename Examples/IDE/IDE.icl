module IDE

import iTasks
import iTasks.API.Extensions.Development.Codebase
import iTasks.API.Extensions.CodeMirror, StdFile

import FindDefinitions

import IDE_Types

//Global status (for all users! If you open a file, everybody opens a file!)
IDE_Status :: (Shared IDE_Status)
IDE_Status = sharedStore  "IDE_Status" 	{ codeBase 	    = []
                                        , codeLocations = []
                                        , openModules   = []
                                        }
initIDE
    =                   rescanCodeBase

rescanCodeBase
    =                   get IDE_Status
	>>= \status ->		codeBaseFromEnvironment status.codeLocations
	>>= \codeBase ->	upd (\status -> {status & codeBase = codeBase}) IDE_Status

Start w = startEngine workOnCleanModules w
where
    workOnCleanModules
        =   initIDE
        >>- \initState ->
            parallel [(Embedded,chooseAndAddModules)
                     ,(Embedded,addSearches)
                     :[(Embedded,closableParTask (\l -> editCleanModule cleanModule l @ IDE_ModuleEdit)) \\ cleanModule <- initState.openModules]]
                        [OnAction (Action "/Setup code locations" []) (always (Embedded,\_ -> editCodeLocations <<@ InWindow @! IDE_SettingsEdit))]
                       <<@ ArrangeCustom arrange <<@ FullScreen
        @> (updateOpenModules,IDE_Status)

    arrange [b1,b2:bs] actions
        = arrangeWithSideBar 0 LeftSide 200 True
                [b1, arrangeWithSideBar 0 TopSide 40 False [b2,arrangeWithTabs bs []] []] actions

    chooseAndAddModules list
        = 			 	(whileUnchanged (mapRead (\s -> s.codeBase) IDE_Status)
            \codeBase -> ((navigateCodebase codeBase
				        >^* [ OnAction (Action "Open .icl" []) (hasValue (\module -> openEditor (module,Icl) list))
				            , OnAction (Action "Open .dcl" []) (hasValue (\module -> openEditor (module,Dcl) list))
				            , OnAction (Action "/Setup code locations" []) (always ((editCodeLocations @! ()) <<@ InWindow)) //TODO: Remove if actions on parallel work...
				            ])
				       )) @? const NoValue

    addSearches list = forever
          (     enterInformation () []
            >>* [OnAction (Action "Search" [ActionKey {key=KEY_ENTER,ctrl=False,shift=False,alt=False}])
                    (hasValue (\query -> openSearch SearchIdentifier query list))]
          ) <<@ (Attribute "buttonPosition" "right")
          @? const NoValue

    updateOpenModules (Value results _) status
        | openModules <> status.openModules = Just {status & openModules = openModules}
                                            = Nothing
    where
        openModules = [moduleName \\ (_,Value (IDE_ModuleEdit {IDE_ModuleEdit|moduleName}) _) <- results]
    updateOpenModules _ _ = Nothing

openEditor :: CleanModule (SharedTaskList IDE_TaskResult) -> Task ()
openEditor moduleName list
    =   appendTask Embedded (closableParTask (\l -> editCleanModule moduleName l @ IDE_ModuleEdit)) list
    >>- \taskId -> focusTask taskId list
    @!  ()

editCleanModule :: CleanModule (SharedTaskList IDE_TaskResult) -> Task IDE_ModuleEdit
editCleanModule fileName=:((filePath,moduleName),ext) list
    = withShared (initCleanEditor True "")
        (\mirror -> updateCleanEditor mirror fileName
        ) <<@ Title (moduleName +++ toString ext)
    @! {IDE_ModuleEdit|moduleName=fileName}

where
	showSelection :: (Shared CodeMirror) (SharedTaskList IDE_TaskResult) -> Task String
	showSelection mirror list
	=        viewSharedInformation "Selection" [] (mapRead getSelection mirror)
        >^*	[ OnAction (Action "/Search/Search Identifier..." [])     (hasValue (\selection -> openSearch SearchIdentifier selection list))
			, OnAction (Action "/Search/Search Definition..." [])     (hasValue (\selection -> openSearch SearchDefinition selection list))
            , OnAction (Action "/Search/Search Implementation..." []) (hasValue (\selection -> openSearch SearchImplementation selection list))
            ]

openSearch what identifier list
    =   appendTask Embedded (closableParTask (\l -> searchCodebase what identifier l @ IDE_Search)) list
    >>- \taskId -> focusTask taskId list
    @!  ()

searchCodebase what initq list
    = withShared initq
    \query -> (
        (updateSharedInformation () [] query)
        ||-
        (whileUnchanged query
            \identifier ->
                get IDE_Status
            >>- \status ->
                searchForIdentifier what True identifier Nothing status.codeBase
            >>- \results ->
                viewSearchResults identifier (fst results) list
            @! {IDE_Search|query=identifier,results=fst results}
        )) <<@ (ArrangeWithSideBar 0 TopSide 50 False) <<@ (Title "Search")
where
    viewSearchResults :: !String [(!CleanModule,!IdentifierPositionList)] (SharedTaskList IDE_TaskResult) -> Task (!CleanModule,!IdentifierPositionList)
    viewSearchResults identifier [] list
        =	viewInformation (Title "Results") [] (identifier +++ " has *not* been found !") @? const NoValue
    viewSearchResults identifier found list
        =   enterChoice ("Results","\"" +++ identifier +++ "\" has been found in:") [ChooseWith (ChooseFromGrid toGrid)] found
        >^* [OnAction (Action "Open module" [ActionTrigger DoubleClick]) (hasValue (\(module,_) -> openEditor module list))]

    toGrid :: (!CleanModule,!IdentifierPositionList) -> FoundInfo
    toGrid (((path,name),ext),positions) = { fileName = name +++ toString ext, numFound =  length (toList positions) }
    toList (Pos begin end rest) = [(begin,end): toList rest]
    toList _ = []


:: FoundInfo =  { fileName	:: FileName
				, numFound		:: Int
				}

derive class iTask FoundInfo

editCodeLocations
    =   get IDE_Status
    >>= \status -> updateInformation ("Code locations","Please enter the locations where you keep your Clean code") [] status.codeLocations
    >>? \codeLocations ->
            upd (\status -> {status & codeLocations = codeLocations}) IDE_Status
        >>| rescanCodeBase

(>>?) infixl 1 :: !(Task a) !(a -> Task b) -> Task (Maybe b) | iTask a & iTask b
(>>?) taska taskbf = step taska (const Nothing)
    [OnAction ActionCancel          (always (return Nothing))
    ,OnAction ActionOk              (hasValue (\a -> taskbf a @ Just))
    ,OnValue                        (ifStable (\a -> taskbf a @ Just))
    ]

getSelection :: CodeMirror -> Identifier
getSelection {position,selection=Nothing,source} =  "nothing"
getSelection {position,selection=Just (begin,end),source}
| begin == end =  "zero"
= source%(begin,end-1)

closableParTask :: (ParallelTask a) -> (ParallelTask a) | iTask a
closableParTask task = task`
where
    task` l =   get (taskListSelfId l)
            >>- \myId ->
                (task l -|| (viewInformation () [] () >>* [OnAction ActionClose (always (removeTask myId l))]))
