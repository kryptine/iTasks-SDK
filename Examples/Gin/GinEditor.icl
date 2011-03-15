implementation module GinEditor

import StdFile
//import DynamicIO

import GenEq
import Text
import iTasks, TSt

import GinAbstractSyntax
import GinConfig
import GinCompiler
import GinDomain
import GinParser
import GinStorage
import GinSyntax

import FilePath, File

ginEditor :: Task Void
ginEditor = handleMenu <<@ FWFullWidth

getConfig :: Task GinConfig
getConfig = accWorld ginLoadConfig >>= \maybeConfig = 
    case maybeConfig of 
        Just config -> accWorld (ginCheckConfig config) >>= \error = (if (isNothing error) (return config) (setupDialog config)) >>^ ginCheckApplet config
        Nothing     -> accWorld ginDefaultConfig >>= \config = setupDialog config
where
	setupDialog :: GinConfig -> Task GinConfig
	setupDialog config = dialog config >>= \newconfig = appWorld (ginStoreConfig newconfig) >>| return newconfig
	where
	    dialog config = updateInformation "Gin editor setup" config >>= \config = 
	                    accWorld (ginCheckConfig config) >>= \error = if (isNothing error) (return config) (dialog config)
	
	ginCheckApplet :: !GinConfig -> Task Void
	ginCheckApplet config =
		accWorld (fileExists (config.iTasksPath </> "Client" </> "Build" </> "Gin.jar")) >>= \exists	=
		if exists stop (showInstruction "Gin Editor" instruction Void >>| ginCheckApplet config)
	where
		instruction = PTag [] [ Text "In order to run the Gin editor, make sure that"
							  , UlTag [] [ LiTag [] [Text "1. The Java JDK 1.6.x is installed on your system"]
							             , LiTag [] [Text "2. The JAVA_HOME environment variable points to the JDK directory"]
							             ]
							  , Text "Next, run the batch file "
							  , TtTag [] [Text (config.iTasksPath </> "Client\\Gin\\build.bat")]
							  , Text " to compile the Gin editor applet."
							  ]

:: Mode = ViewWorkflow | ViewImports | ViewTypes | ViewSource

:: EditorState = 
    { name			:: !Maybe String
    , changed		:: !Bool
    , mode			:: !Mode
    , checkSyntax 	:: !Bool
    , gMod			:: !GModule
    , compiled		:: !Maybe String
    , config		:: !GinConfig
    }

derive class iTask EditorState, Mode

//-----------------------------------------------------------------------------------        
ActionUpdate           :== Action "update"			"Update"
ActionRun              :== Action "run"				"Run"
ActionViewWorkflow     :== Action "viewworkflow"	"Workflow"
ActionViewImports      :== Action "viewimports"		"Imports"
ActionViewTypes        :== Action "viewtypes"		"Types"    
ActionViewSource       :== Action "viewsource"		"Generated source"      
//ActionEnableSC         :== Action "sc_on"  			"Enable syntax checking"
//ActionDisableSC        :== Action "sc_off" 			"Disable syntax checking"

initMenu :: MenuDefinition
initMenu =
    [ Menu "File"    [ MenuItem ActionNew				(Just { key = N, ctrl = True, alt = False, shift = False })
                     , MenuItem ActionOpen				(Just { key = N, ctrl = True, alt = False, shift = False })
                     , MenuSeparator
                     , MenuItem ActionSave				(Just { key = N, ctrl = True, alt = False, shift = False })
                     , MenuItem ActionSaveAs			Nothing
                     , MenuSeparator
                     , MenuItem ActionUpdate			(Just { key = U, ctrl = True, alt = False, shift = False })
                     , MenuItem ActionRun				(Just { key = R, ctrl = True, alt = False, shift = False })
                     , MenuSeparator
                     , MenuItem ActionQuit				Nothing
                     ]
    , Menu "View"    [ MenuItem ActionViewWorkflow		Nothing
                     , MenuItem ActionViewImports		Nothing
                     , MenuItem ActionViewTypes			Nothing
                     , MenuItem ActionViewSource		Nothing
                     ]
//	, Menu "Options" [ MenuItem ActionEnableSC 	        Nothing
//                     , MenuItem ActionDisableSC			Nothing
//                     ]
    , Menu "Help"    [ MenuItem ActionAbout        		Nothing
                     ]
	]
    
actions :: EditorState -> [(TaskAction b)]
actions state = [ (ActionNew,              always)
                , (ActionOpen,             always)
                , (ActionSave,             (\_ -> state.EditorState.changed ))
                , (ActionSaveAs,           always)
                , (ActionUpdate,           always)
                , (ActionRun,              (\_ -> isJust state.EditorState.compiled ))
                , (ActionQuit,             always)
                , (ActionAbout,            always)
                , (ActionViewWorkflow,     (\_ -> state.EditorState.mode =!= ViewWorkflow))
                , (ActionViewImports,      (\_ -> state.EditorState.mode =!= ViewImports))
                , (ActionViewTypes,        (\_ -> state.EditorState.mode =!= ViewTypes))
                , (ActionViewSource,         (\_ -> state.EditorState.mode =!= ViewSource))
//                , (ActionEnableSC,         (\_ -> not state.EditorState.checkSyntax))
//                , (ActionDisableSC,        (\_ -> state.EditorState.checkSyntax))
                ]

handleMenu :: Task Void
handleMenu = emptyState >>= \state -> initMenu @>> doMenu state

emptyState :: Task EditorState 
emptyState = getConfig >>= \config -> 
	return { name = Nothing, changed = False, mode = ViewWorkflow, checkSyntax = False, gMod = newModule, compiled = Nothing, config = config}
    
doMenu :: EditorState -> Task Void
doMenu state =: { EditorState | mode, config, gMod } =
	case mode of
        ViewWorkflow -> updateInformationA (getName state, "workflow view") (diagramView, diagramUpdate) (actions state) gMod
                        >>= \(action, mbGMod) -> 
                        case mbGMod of
                        	Nothing		-> return (action, state)
                        	Just gMod	-> return (action, setChanged state { EditorState | state & gMod = gMod})
        ViewImports  -> accWorld (searchPathModules config) >>= \allModules = 
        				updateInformationA (getName state, "Imports view") (importsView (sort allModules), importsUpdate) (actions state) gMod
                        >>= \(action, mbGMod) -> 
                        case mbGMod of
                        	Nothing		-> return (action, state)
                        	Just gMod	-> return (action, setChanged state { EditorState | state & gMod = gMod})
        ViewTypes    -> updateInformationA (getName state, "types view") (typeView, typeUpdate) (actions state) gMod
	                    >>= \(action, mbGMod) -> 
	                    case mbGMod of
	                       	Nothing		-> return (action, state)
	                       	Just gMod	-> return (action, setChanged state { EditorState | state & gMod = gMod})
        ViewSource  -> accWorld (tryRender gMod config POICL) >>= \source ->
        				showMessageA ("code view", formatSource source) (actions state) Void
                        >>= \(action, _) = return (action, state)                   
    >>= switchAction

//Bimaps on GModule
diagramView :: !GModule -> ORYXEditor
diagramView {imports, moduleKind = (GGraphicalModule defs)} = ginORYXEditor imports (hd defs).GDefinition.body

diagramUpdate :: !ORYXEditor !GModule -> GModule
diagramUpdate oryx gMod=:{moduleKind = (GGraphicalModule defs)} = 
	{ GModule 
	| gMod 
	& moduleKind = GGraphicalModule 
		[	{ GDefinition 
			| hd defs & body = oryx.ORYXEditor.diagram
			} 
		]
	}

importsView :: ![String] !GModule -> MultipleChoice String
importsView allModules gMod = MultipleChoice allModules 
	(catMaybes (map (listIndex allModules 0) gMod.GModule.imports))
where
	listIndex :: [a] Int a -> Maybe Int | Eq a
	listIndex []     _ _ = Nothing
	listIndex [x:xs] i a | a == x    = Just i
						 | otherwise = listIndex xs (i+1) a
importsUpdate :: (MultipleChoice String) GModule -> GModule
importsUpdate (MultipleChoice imports indices) gMod =
	updateDiagramExtensions { GModule | gMod & imports = map (\i-> imports !! i) indices }

typeView :: !GModule -> [GTypeDefinition]
typeView gMod = gMod.GModule.types
typeUpdate :: ![GTypeDefinition] !GModule -> GModule
typeUpdate types gMod = { GModule | gMod & types = types }

switchAction :: (Action, EditorState) -> Task Void
switchAction (action, state) = 
    case action of
        ActionNew              -> askSaveIfChanged state >>| emptyState >>= doMenu
        ActionOpen             -> askSaveIfChanged state >>| open state >>= doMenu 
        ActionSave             -> save state >>= doMenu
        ActionSaveAs           -> saveAs state >>= doMenu 
        ActionUpdate           -> update state >>= doMenu 
        ActionRun              -> run state >>| doMenu state
        ActionQuit             -> askSaveIfChanged state 
        ActionAbout            -> showAbout >>| doMenu state
        ActionViewWorkflow     -> doMenu { EditorState | state & mode = ViewWorkflow }
        ActionViewImports      -> doMenu { EditorState | state & mode = ViewImports }
        ActionViewTypes        -> doMenu { EditorState | state & mode = ViewTypes }
        ActionViewSource         -> doMenu { EditorState | state & mode = ViewSource }
//        ActionEnableSC         -> doMenu state { EditorState | state & editor = { GinEditor | state.EditorState.editor & checkSyntax = True } }
//        ActionDisableSC        -> doMenu state { EditorState | state & editor = { GinEditor | state.EditorState.editor & checkSyntax = False } }

getName :: EditorState -> String
getName state = case state.EditorState.name of
    Just n  -> n
    Nothing -> "(unnamed)"

setChanged :: EditorState EditorState -> EditorState
setChanged old new = if (old.EditorState.gMod =!= new.EditorState.gMod) { new & changed = True } new        

open :: EditorState -> Task EditorState 
open state = emptyState >>= \newState -> chooseModule state.EditorState.config >>= \mMod = 
	case mMod of
		Just (name, gMod) = return { EditorState | newState & name = Just name, gMod = gMod }
		Nothing			  = return state

save :: EditorState -> Task EditorState
save state = case state.EditorState.name of
    Just name  -> writeModule state.EditorState.config name state.EditorState.gMod >>| 
				  return { state & changed = False }
    Nothing    -> saveAs state

saveAs :: EditorState -> Task EditorState
saveAs state = newModuleName state.EditorState.config >>= \name = 
	save { EditorState | state & name = Just name }

askSaveIfChanged :: EditorState -> Task Void
askSaveIfChanged state = if state.changed
    (requestConfirmation ("File " +++ (getName state) +++ " has changed, save changes?") >>= \confirm =
        if confirm (save state >>| return Void) (return Void)
    )
    (return Void)

update :: EditorState -> Task EditorState
update state
# state = { state & compiled = Nothing }
= accIWorld (batchBuild state.EditorState.gMod)
  >>= \result = case result of
   				  CompileSuccess dynfile 	-> (showMessage ("Compiler output", "Compiled successfully") Void) 
   				  								>>| return { state & compiled = Just dynfile }
    			  error						-> showMessageAbout "Compiler output" error >>| return state

run :: EditorState -> Task Void
run state = showMessage ("Error", "Runninig tasks requires dynamic linker") Void
/*
run state 					= case state.compiled of
	Just dynfile -> readDynamicTask dynfile
						>>= \(ok, task) = (if ok task (showMessage ("Error", "Failed to read task") Void)) >>| stop
	Nothing 	-> showMessage ("Error", "No compiled task") Void
*/

formatSource :: String -> HtmlTag
formatSource source = TextareaTag [ColsAttr "80", RowsAttr "25"] [ Text source ]

tryRender :: GModule GinConfig PrintOption *World -> (String, *World)
tryRender gMod config printOption world
# (st, world) = gToAModule gMod config world
# source = case runParse st of
	GSuccess aMod -> renderAModule printOption aMod
	GError errors -> "Parse error:\n" +++ ((join "\n" (map (\(path,msg) = toString path +++ ":" +++ msg) errors)))
= (source, world)

showAbout :: Task Void
showAbout = showMessage ("Gin workflow editor", "version 0.1") Void

accIWorld :: !(*IWorld -> *(!a,!*IWorld)) -> Task a | iTask a
accIWorld fun = mkInstantTask ("Run Iworld function", "Run a IWorld function and get result.") (mkTaskFunction (accIWorldTSt fun))