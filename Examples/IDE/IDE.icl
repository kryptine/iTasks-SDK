module IDE

import iTasks
import iTasks.API.Extensions.Development.Codebase
import iTasks.API.Extensions.CodeMirror, StdFile

import FindDefinitions

import IDE_Types

//Global status (for all users! If you open a file, everybody opens a file!)
IDE_Status :: (Shared IDE_Status)
IDE_Status = sharedStore  "IDE_Status" 	{ info		    = []
			    						, codeBase 	    = []
                                        , codeLocations = []
                                        }

initIDE
    =                   rescanCodeBase

rescanCodeBase
    =                   get IDE_Status
	>>=	\status ->		upd (\status -> {status & info = map (\info -> {info & opened = False} status.info)}) 
	>>= \status ->		codeBaseFromEnvironment status.codeLocations
	>>= \codeBase ->	upd (\status -> {status & codeBase = codeBase}) IDE_Status

Start w = startEngine workOnCleanModules w
where
    workOnCleanModules
        =   initIDE
        >>- \initState ->
            parallel [(Embedded,chooseAndAddModules)
                     :[(Embedded,openEditor cleanModule) \\ {cleanModule,opened} <- initState.info | not opened]] [] <<@ ArrangeCustom arrange <<@ FullScreen

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
=   			get IDE_Status
	\status ->  case find fileName status of
					(Just i) -> if (status.info[i] = opened) 
										(return ())
										(create-an-sds-projection-on-the-status

	(		upd (\st -> {st & openedFiles = removeDup (st.openedFiles ++ [fileName])}) IDE_Status
     >>|	withShared (config True "") (\mirror -> updateCleanEditor mirror fileName -&&- forever (showSelection mirror)) 
    ) <<@ (ArrangeWithSideBar 1 BottomSide 100 True) <<@ Title (moduleName +++ toString ext)
where
	showSelection :: (Shared CodeMirror) -> Task ()
	showSelection mirror
	=        viewSharedInformation "Selected" [ViewWith getSelection] mirror
        >>*	[ OnAction (Action "/Search/Search Identifier..." [])     (hasValue (\mirror -> searchFor SearchIdentifier     (getSelection mirror) mirror <<@ InFloatingWindow ))
			, OnAction (Action "/Search/Search Definition..." [])     (hasValue (\mirror -> searchFor SearchDefinition     (getSelection mirror) mirror <<@ InFloatingWindow ))
            , OnAction (Action "/Search/Search Implementation..." []) (hasValue (\mirror -> searchFor SearchImplementation (getSelection mirror) mirror <<@ InFloatingWindow ))
            , OnAction ActionClose 			(always (closeEditor fileName list))
            ]

closeEditor fileName list
    =				upd (\st -> {st & openedFiles = removeMember fileName st.openedFiles}) IDE_Status
    >>|				get (taskListSelfId list)
    >>= \myId ->	removeTask myId list
    @! ()

searchFor what identifier mirror
= 						updateInformation "Search" [] identifier
	>>= \identifier ->	get IDE_Status
	>>= \status ->	    searchForIdentifier what True identifier Nothing status.codeBase
	>>= \result	->		showIdentifiersFound (fst result)
	>>|					return ()
where
	showIdentifiersFound :: [(!CleanModule,!IdentifierPositionList)] -> Task (Maybe FoundInfo)
	showIdentifiersFound []
	=					viewInformation (identifier +++ " has *not* been found !") [] Nothing
	  >>=				return
	showIdentifiersFound found
	=					enterChoice ("\"" +++ identifier +++ "\" has been found in:") [ChooseWith (ChooseFromGrid toGrid)] found
	 >>= 				viewInformation "result" []
	 >>=				return o Just
	where
		toGrid :: [(!CleanModule,!IdentifierPositionList)] -> [FoundInfo]
		toGrid found = [{ fileName = name +++ toString ext, found =  length (toList positions) } \\ (((path,name),ext),positions) <- found]
		toList (Pos begin end rest) = [(begin,end): toList rest] 
		toList _ = []

:: FoundInfo =  { fileName	:: FileName
				, found		:: Int
				}
derive class iTask FoundInfo


editCodeLocations
    =   get IDE_Status
    >>= \status -> updateInformation "Please enter the locations where you keep your Clean code" [] status.codeLocations
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


