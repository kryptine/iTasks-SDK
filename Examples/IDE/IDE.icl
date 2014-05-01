module IDE

import iTasks
import iTasks.API.Extensions.Development.Codebase
import iTasks.API.Extensions.CodeMirror, StdFile

import FindDefinitions

import IDE_Types

//Global status (for all users! If you open a file, everybody opens a file!)
IDE_Status :: (Shared IDE_Status)
IDE_Status = sharedStore  "IDE_Status" 	{ openedFiles   = []
			    						, codeBase 	    = []
                                        , codeLocations = []
                                        }

initIDE
    =                   rescanCodeBase

rescanCodeBase
    =                   get IDE_Status
	>>=	\status ->		codeBaseFromEnvironment status.codeLocations
	>>= \codeBase ->	upd (\status -> {status & codeBase = codeBase}) IDE_Status

Start w = startEngine workOnCleanModules w
where
    workOnCleanModules
        =   initIDE
        >>- \initState ->
            parallel [(Embedded,chooseAndAddModules)
                     :[(Embedded,cleanEditor module) \\ module <- initState.openedFiles]] [] <<@ ArrangeCustom arrange <<@ FullScreen

    arrange [b:bs] actions
        = arrangeWithSideBar 0 LeftSide 200 True [b,arrangeWithTabs bs []] actions

    chooseAndAddModules list
        = 			 	whileUnchanged IDE_Status
            \status -> ((navigateCodebase status.codeBase
				        >^* [ OnAction (Action "Open .icl" []) (hasValue (\module -> appendTask Embedded (cleanEditor (module,Icl)) list))
				            , OnAction (Action "Open .dcl" []) (hasValue (\module -> appendTask Embedded (cleanEditor (module,Dcl)) list))
				            , OnAction (Action "/Setup code locations" []) (always ((editCodeLocations @? const NoValue) <<@ InWindow))
				            ])
				       )@? const NoValue

    cleanEditor fileName=:((filePath,moduleName),ext) list
    =   (((		upd (\st -> {st & openedFiles = removeDup (st.openedFiles ++ [fileName])}) IDE_Status
         >>|	editCleanModule fileName
        )
        >&>
        (\mirror ->
                viewSharedInformation "Selected" [ViewWith (getSelection o fromJust)] mirror
            >>*	[ OnAction (Action "Search" []) (ifValue isJust (\mirror -> searchFor (getSelection (fromJust mirror))))
                ]
        )) <<@ (ArrangeWithSideBar 1 BottomSide 100 True) <<@ Title (moduleName +++ toString ext)
        ) >>* [OnAction ActionClose 			(always (closeEditor fileName list))]
	
    closeEditor fileName list
        =				upd (\st -> {st & openedFiles = removeMember fileName st.openedFiles}) IDE_Status
        >>|				get (taskListSelfId list)
        >>= \myId ->	removeTask myId list 
        @! ()

searchFor identifier
	= 					get IDE_Status
	>>= \status ->	    searchForIdentifier SearchIdentifier True identifier (hd (codeBaseToCleanModuleNames status.codeBase)) status.codeBase
	>>= \result ->		viewInformation "result" [] result 
	@! ()

editCodeLocations
    =   get IDE_Status
    >>= \status -> updateInformation "Please enter the locations where you keep your Clean code" [] status.codeLocations
    >>? \codeLocations ->
            upd (\status -> {status & codeLocations = codeLocations}) IDE_Status
        >>| rescanCodeBase

getSelection :: CodeMirror -> Identifier
getSelection {position,selection=Nothing,source} =  "nothing"
getSelection {position,selection=Just (begin,end),source}
| begin == end =  "zero"
= source%(begin,end-1)


(>>?) infixl 1 :: !(Task a) !(a -> Task b) -> Task (Maybe b) | iTask a & iTask b
(>>?) taska taskbf = step taska (const Nothing)
    [OnAction ActionCancel          (always (return Nothing))
    ,OnAction ActionOk              (hasValue (\a -> taskbf a @ Just))
    ,OnValue                        (ifStable (\a -> taskbf a @ Just))
    ]
