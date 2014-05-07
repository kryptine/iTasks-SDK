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
                     :[(Embedded,openEditor module) \\ module <- initState.openedFiles]] [] <<@ ArrangeCustom arrange <<@ FullScreen

    arrange [b:bs] actions
        = arrangeWithSideBar 0 LeftSide 200 True [b,arrangeWithTabs bs []] actions

    chooseAndAddModules list
        = 			 	whileUnchanged IDE_Status
            \status -> ((navigateCodebase status.codeBase
				        >^* [ OnAction (Action "Open .icl" []) (hasValue (\module -> appendTask Embedded (openEditor (module,Icl)) list 
				        															 >>= \id -> focusTask id list @! ()))
				            , OnAction (Action "Open .dcl" []) (hasValue (\module -> appendTask Embedded (openEditor (module,Dcl)) list
				            														 >>= \id -> focusTask id list @! ()))
				            , OnAction (Action "/Setup code locations" []) (always ((editCodeLocations @! ()) <<@ InWindow))
				            ])
				       )@? const NoValue

openEditor fileName=:((filePath,moduleName),ext) list
=   (((		upd (\st -> {st & openedFiles = removeDup (st.openedFiles ++ [fileName])}) IDE_Status
     >>|	editCleanModule True fileName
    )
    >&>
    (\mirror -> forever (
            viewSharedInformation "Selected" [ViewWith (getSelection o fromJust)] mirror
        >>*	[ OnAction (Action "/Search/Search Identifier..." []) (ifValue isJust (\mirror -> searchFor (getSelection (fromJust mirror)) <<@ InWindow ))
            ])
    )) <<@ (ArrangeWithSideBar 1 BottomSide 100 True) <<@ Title (moduleName +++ toString ext)
    ) >>* [OnAction ActionClose 			(always (closeEditor fileName list))]

closeEditor fileName list
    =				upd (\st -> {st & openedFiles = removeMember fileName st.openedFiles}) IDE_Status
    >>|				get (taskListSelfId list)
    >>= \myId ->	removeTask myId list
    @! ()

searchFor identifier
= 						updateInformation "Search" [] identifier
	>>= \identifier ->	get IDE_Status
	>>= \status ->	    searchForIdentifier SearchIdentifier True identifier Nothing status.codeBase
	>>= \result	->		showIdentifiersFound (fst result)
	>>|					return ()
where
	showIdentifiersFound :: [(!CleanModule,!IdentifierPositionList)] -> Task (CleanModule,(Int,Int))
	showIdentifiersFound found
	=					enterChoice (identifier +++ " : found in") [ChooseWith (ChooseFromGrid id)] (toGrid found)
	 >>=				viewInformation "result" []
	where
		toGrid :: [(!CleanModule,!IdentifierPositionList)] -> [(CleanModule,(Int,Int))]
		toGrid found = [(name,(begin,end)) \\ (name,positions) <- found, (begin,end) <- toList positions]
		toList (Pos begin end rest) = [(begin,end): toList rest] 
		toList _ = []

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
