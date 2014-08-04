module IDE

import iTasks
import iTasks.API.Extensions.Development.Codebase
import iTasks.API.Extensions.CodeMirror, StdFile
import qualified Data.Map
import Text

import FindDefinitions
import APIDocumentation

import IDE_Types

//Common actions
ActionSearch :== Action "Search" [ActionIcon "search",ActionKey {key=KEY_ENTER,ctrl=False,shift=False,alt=False}]
ActionAdd    :== Action "Add" []
ActionOpen   :== Action "Open" []

//Global status (for all users! If you open a file, everybody opens a file!)
IDE_Status :: (Shared IDE_Status)
IDE_Status = sharedStore  "IDE_Status" 	{ codeBase 	    = []
                                        , openModules   = []
                                        }
initIDE
    =                   updateCodeBase

updateCodeBase
    =                   get IDE_Status
	>>= \status ->		rescanCodeBase status.codeBase
	>>= \codeBase ->	upd (\status -> {status & codeBase = codeBase}) IDE_Status

Start w = startEngine workOnCleanModules w
where
    workOnCleanModules
        =   initIDE
        >>- \initState ->
            parallel [(Embedded,viewLogo) //Yes viewing a useless logo is something we enable you to do :)
                     ,(Embedded,chooseAndAddModules)
                     ,(Embedded,addSearches)
                     :[]/*[(Embedded,closableParTask (\l -> editCleanModule cleanModule l @ IDE_ModuleEdit)) \\ cleanModule <- initState.openModules]*/]
                     []
                     <<@ ArrangeCustom arrange <<@ FullScreen
        @> (updateOpenModules,IDE_Status)

    arrange [b1,b2,b3:bs] actions
        = arrangeWithSideBar 0 LeftSide 200 True
                [ arrangeWithSideBar 0 TopSide 40 False [b1,b2] []
                , arrangeWithSideBar 0 TopSide 40 False [b3,arrangeWithTabs bs []] []] actions

    viewLogo _ = viewInformation () [] (ATag [HrefAttr "http://itasks.org/",TargetAttr "_blank"] [ImgTag [SrcAttr "/img/logo.png",WidthAttr "200",HeightAttr "40"]])
                <<@ ForceLayout <<@ AfterLayout (tweakUI (setMargins 0 0 0 0))
               @? const NoValue

    chooseAndAddModules list
        = 			 	(whileUnchanged (mapRead (\s -> s.codeBase) IDE_Status)
            \codeBase -> ((navigateCodebase codeBase
				        >^* [ OnAction ActionOpen (hasValue (openSelection list))
                            , OnAction ActionAdd (always ((addSourceTree @! ()) <<@ InWindow))
				            ])
				       )) @? const NoValue

    openSelection list (SelSourceTree rootDir) = openSourceTreeSettings rootDir list
    openSelection list (SelMainModule rootDir moduleName) = openEditor rootDir MainModule moduleName list
    openSelection list (SelAuxModule rootDir moduleName) = openEditor rootDir AuxModule moduleName list

    addSourceTree :: Task (Maybe SourceTree)
    addSourceTree
        =   enterInformation ("Add","Please specify the location of the sourcecode on your disk") []
        >>* [OnAction ActionCancel (always (return Nothing))
            ,OnAction ActionAdd (hasValue (\path -> add path @ Just))
            ]
    where
        add location =
            let tree =  {SourceTree|rootDir=location,modules=[],readOnly=False} in
                    upd (\status -> {status & codeBase = status.codeBase ++ [tree]}) IDE_Status
                >>| updateCodeBase
                @! tree

    addSearches list = forever
          (((    (enterInformation () [] <<@ NoAnnotation)
            -&&-
                (updateChoice () [ChooseWith (ChooseFromComboBox view)] [SearchIdentifier,SearchDefinition,SearchImplementation] SearchIdentifier <<@ NoAnnotation)
            )   <<@ ArrangeCustom arrange)
            >>* [OnAction ActionSearch (hasValue (\(query,what) -> openSearch what query list))]
          ) <<@ (Attribute "buttonPosition" "right")
          @? const NoValue
    where
        view SearchIdentifier = "Identifier"
        view SearchDefinition = "Definition"
        view SearchImplementation = "Implementation"

        arrange [block] [] = {UIBlock|block & content = {block.UIBlock.content & direction = Horizontal}} //HACK
        arrange blocks actions = arrangeHorizontal blocks actions

    updateOpenModules (Value results _) status
        | openModules <> status.openModules = Just {status & openModules = openModules}
                                            = Nothing
    where
        openModules = [] //[moduleName \\ (_,Value (IDE_ModuleEdit {IDE_ModuleEdit|moduleName}) _) <- results]

    updateOpenModules _ _ = Nothing

openSourceTreeSettings :: FilePath (SharedTaskList IDE_TaskResult) -> Task ()
openSourceTreeSettings base list
    =   appendTask Embedded (closableParTask (\l -> editSourceTree base l @ IDE_SourceTreeEdit)) list
    >>- \taskId -> focusTask taskId list
    @!  ()

editSourceTree :: FilePath (SharedTaskList IDE_TaskResult) -> Task FilePath
editSourceTree base list
    = viewInformation ("Source tree settings","This is a placeholder for an editor for a source tree") [] base
    <<@ Icon "sourcetree"

openEditor :: FilePath ModuleType ModuleName (SharedTaskList IDE_TaskResult) -> Task ()
openEditor base type name list
    =   appendTask Embedded (closableParTask editorTask ) list
    >>- \taskId -> focusTask taskId list
    @!  ()
where
    editorTask = case type of
        MainModule = (\l -> workOnCleanMainModule base name l @ IDE_ModuleEdit)
        AuxModule = (\l -> workOnCleanAuxModule base name l @ IDE_ModuleEdit)

workOnCleanMainModule :: FilePath ModuleName (SharedTaskList IDE_TaskResult) -> Task IDE_ModuleEdit
workOnCleanMainModule base moduleName list
    = viewInformation () [ViewWith moduleTitleView] moduleName
      ||-
      (editIclFile base moduleName list
       -&&-
       buildMainModule base moduleName
        <<@ ArrangeWithTabs
      ) <<@ ArrangeWithSideBar 0 TopSide 35 False <<@ (Title moduleName)  <<@ Icon "mainmodule"
    @! {IDE_ModuleEdit|moduleName=moduleName}

workOnCleanAuxModule :: FilePath ModuleName (SharedTaskList IDE_TaskResult) -> Task IDE_ModuleEdit
workOnCleanAuxModule base moduleName list
    = viewInformation () [ViewWith moduleTitleView] moduleName
      ||-
      (editIclFile base moduleName list
       -&&-
       editDclFile base moduleName list
        <<@ ArrangeWithTabs
      ) <<@ ArrangeWithSideBar 0 TopSide 35 False <<@ (Title moduleName) <<@ Icon "auxmodule"
    @! {IDE_ModuleEdit|moduleName=moduleName}

editIclFile :: FilePath ModuleName (SharedTaskList IDE_TaskResult) -> Task ()
editIclFile base moduleName list
    = editCleanFile (foldl (</>) base (split "." moduleName) +++ ".icl") list <<@ Title "Implementation"

editDclFile :: FilePath ModuleName (SharedTaskList IDE_TaskResult) -> Task ()
editDclFile base moduleName list
    = editCleanFile (foldl (</>) base (split "." moduleName) +++ ".dcl") list <<@ Title "Definition"

editCleanFile :: FilePath (SharedTaskList IDE_TaskResult) -> Task ()
editCleanFile path list
   =    importTextFile path
   >>- \initContent ->
        withShared (initCleanEditor False initContent)
            \mirror -> updateCleanEditor (shareSearchResults path list mirror) @! ()

moduleTitleView s = SpanTag [StyleAttr "font-size: 24px"] [Text s]

buildMainModule :: FilePath ModuleName -> Task IDE_ModuleEdit
buildMainModule base moduleName
    = viewInformation () [] ("Placeholder for build task of module "+++ moduleName) <<@ Title "Build"
    @? const NoValue

shareSearchResults :: FilePath (SharedTaskList IDE_TaskResult) (Shared CodeMirror)  -> (Shared CodeMirror)
shareSearchResults path list mirror = mirror
//= 		list >+> filterSearchers //TODO Use proper share propagation to link shares
where
    filterSearchers :: (TaskList IDE_TaskResult) -> (Shared CodeMirror)
    filterSearchers {TaskList|items} 
    # highLight 		=  [ toList posList \\ {TaskListItem|value=Value (IDE_Search {results}) _} <- items
                                            ,  ((sBase,sModule,sExt),posList) <- results
                                            |  addExtension (sBase </> sModule) sExt == path
                           ]
    = case highLight of
        [] 		-> mirror
        lights  -> mapWrite (\mw mr -> Just {mr & highlighted = flatten lights}) mirror


openSearch :: SearchWhat String (SharedTaskList IDE_TaskResult) -> Task ()
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
                viewSearchResults what identifier (fst results) list
            @! {IDE_Search|query=identifier,results=fst results}
        )) <<@ (ArrangeWithSideBar 0 TopSide 50 False) <<@ (Title "Search") <<@ Icon "search"
where
    viewSearchResults :: !SearchWhat !String [(!CleanFile,!IdentifierPositionList)] (SharedTaskList IDE_TaskResult) -> Task (!CleanFile,!IdentifierPositionList)
    viewSearchResults what identifier [] list
        =	viewInformation (Title "Results") [] (toString what identifier +++ "has *not* been found !") @? const NoValue
    viewSearchResults what identifier found list
        =   enterChoice ("Results",toString what identifier +++ "has been found in:") [ChooseWith (ChooseFromGrid toGrid)] found
        >^* [OnAction (Action "Open module" [ActionTrigger DoubleClick]) (hasValue (\((base,module,_),_) -> openEditor base AuxModule module list))] //TODO: Extract module type from codeBase

	toString SearchIdentifier 	  ident  = "Identifier \""        +++ ident +++ "\" "
	toString SearchImplementation ident  = "Implementation of \"" +++ ident +++ "\" "
	toString SearchDefinition     ident  = "Definition of \""     +++ ident +++ "\" "

    toGrid :: (!CleanFile,!IdentifierPositionList) -> FoundInfo
    toGrid ((pathName,modName,ext),positions) = { moduleName = modName
    											, iclDcl	 = ext
    											, howOften 	 = length (toList positions) 
    											, positions	 = toList positions
    											, pathName 	 = pathName
    											}



:: FoundInfo =  { moduleName 	:: ModuleName
				, iclDcl		:: Extension
				, howOften		:: Int
				, positions		:: [(Int,Int)]
				, pathName		:: FilePath
				}

derive class iTask FoundInfo, SearchWhat

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

toList :: IdentifierPositionList -> [(Int,Int)]
toList (Pos begin end rest) = [(begin,end): toList rest]
toList _ = []

NoAnnotation	:== AfterLayout (tweakControls (map (\(c,_) -> (c,'Data.Map'.newMap))))

