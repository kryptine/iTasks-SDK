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
import GinStorage
import GinSyntax

import FilePath

ginEditor :: Task Void
ginEditor = (/*ginSetup >>| */handleMenu) <<@ FWFullWidth

ginSetup :: Task Void
ginSetup = accWorld ginLoadConfig >>= \maybeConfig = 
    case maybeConfig of 
        Just config -> accWorld (ginCheckConfig config) >>= \error = (if (isNothing error) stop (setupDialog config)) >>| ginCheckApplet config
        Nothing     -> accWorld ginDefaultConfig >>= \config = setupDialog config
where
	setupDialog :: GinConfig -> Task Void
	setupDialog config = dialog config >>= \newconfig = appWorld (ginStoreConfig newconfig) where
	    dialog config = updateInformation "Gin editor setup" config >>= \config = 
	                    accWorld (ginCheckConfig config) >>= \error = if (isNothing error) (return config) (dialog config)
	
	ginCheckApplet :: !GinConfig -> Task Void
	ginCheckApplet config =
		fileExists (config.iTasksPath </> "Client" </> "Build" </> "Gin.jar") >>= \exists	=
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

:: Mode = EditWorkflow | EditTypes | EditCode

:: EditorState = 
    { name			:: Maybe String
    , changed		:: Bool
    , mode			:: Mode
    , checkSyntax 	:: Bool
    , gMod			:: GModule
    , compiled		:: Maybe String
    }

derive class iTask EditorState, Mode

//-----------------------------------------------------------------------------------        
ActionUpdate           :== Action "update"			"Update"
ActionRun              :== Action "run"				"Run"
ActionEditWorkflow     :== Action "editworkflow"	"Edit Workflow" 
ActionEditTypes        :== Action "edittypes"		"Edit Types"    
ActionEditCode         :== Action "editcode"		"Edit Code"      
ActionEnableSC         :== Action "sc_on"  			"Enable syntax checking"
ActionDisableSC        :== Action "sc_off" 			"Disable syntax checking"

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
    , Menu "View"    [ MenuItem ActionEditWorkflow		Nothing
                     , MenuItem ActionEditTypes			Nothing
                     , MenuItem ActionEditCode			Nothing
                     ]
	, Menu "Options" [ MenuItem ActionEnableSC 	        Nothing
                     , MenuItem ActionDisableSC			Nothing
                     ]
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
                , (ActionEditWorkflow,     (\_ -> state.EditorState.mode =!= EditWorkflow))
                , (ActionEditTypes,        (\_ -> state.EditorState.mode =!= EditTypes))
                , (ActionEditCode,         (\_ -> state.EditorState.mode =!= EditCode))
                , (ActionEnableSC,         (\_ -> not state.EditorState.checkSyntax))
                , (ActionDisableSC,        (\_ -> state.EditorState.checkSyntax))
                ]

handleMenu :: Task Void
handleMenu 
    =   initMenu @>> doMenu emptyState

emptyState :: EditorState 
emptyState = { name = Nothing, changed = False, mode = EditWorkflow, checkSyntax = False, gMod = newModule, compiled = Nothing }
    
doMenu :: EditorState -> Task Void
doMenu state =: { EditorState | mode, gMod } = 
    case mode of
        EditWorkflow -> updateInformationA (getName state, "workflow view") (diagramView, diagramUpdate) (actions state) gMod
                        >>= \(action, mbEditor) -> 
                        case mbEditor of
                        	Nothing		-> return (action, state)
                        	Just gMod	-> return (action, setChanged state { EditorState | state & gMod = gMod})
        EditTypes    -> updateInformationA (getName state, "types view") (typeView, typeUpdate) (actions state) gMod
	                    >>= \(action, mbGMod) -> 
	                    case mbGMod of
	                       	Nothing		-> return (action, state)
	                       	Just gMod	-> return (action, setChanged state { EditorState | state & gMod = gMod})
        EditCode     -> updateInformationA (getName state, "code view") (codeView, codeUpdate) (actions state) gMod
                        >>= \(action, _) = return (action, state)                   
    >>= switchAction

//Bimaps on GModule
diagramView :: GModule -> ORYXEditor
diagramView gMod = ginORYXEditor
diagramUpdate :: ORYXEditor GModule -> GModule
diagramUpdate oryx gMod = gMod

typeView :: GModule -> [GTypeDefinition]
typeView gMod = gMod.GModule.types
typeUpdate :: [GTypeDefinition] GModule -> GModule
typeUpdate types gMod = { GModule | gMod & types = types }

codeView :: GModule -> (Note, Note)
codeView gMod = ( Note (tryRender gMod False), Note (tryRender gMod True))
codeUpdate :: (Note, Note) GModule -> GModule
codeUpdate _ gMod = gMod 

switchAction :: (Action, EditorState) -> Task Void
switchAction (action, state) = 
    case action of
        ActionNew              -> askSaveIfChanged state >>| doMenu emptyState
        ActionOpen             -> askSaveIfChanged state >>| open state >>= doMenu 
        ActionSave             -> save state >>= doMenu
        ActionSaveAs           -> saveAs state >>= doMenu 
        ActionUpdate           -> update state >>= doMenu 
        ActionRun              -> run state >>| doMenu state
        ActionQuit             -> askSaveIfChanged state 
        ActionAbout            -> showAbout >>| doMenu state
        ActionEditWorkflow     -> doMenu { EditorState | state & mode = EditWorkflow }
        ActionEditTypes        -> doMenu { EditorState | state & mode = EditTypes }
        ActionEditCode         -> doMenu { EditorState | state & mode = EditCode }
        ActionEnableSC         -> doMenu state //{ EditorState | state & editor = { GinEditor | state.EditorState.editor & checkSyntax = True } }
        ActionDisableSC        -> doMenu state //{ EditorState | state & editor = { GinEditor | state.EditorState.editor & checkSyntax = False } }
    
getName :: EditorState -> String
getName state = case state.EditorState.name of
    Just n  -> n
    Nothing -> "(unnamed)"

setChanged :: EditorState EditorState -> EditorState
setChanged old new = if (old.EditorState.gMod =!= new.EditorState.gMod) { new & changed = True } new        

open :: EditorState -> Task EditorState 
open state = chooseModule >>= \(name, gMod) = 
    return { EditorState | emptyState & name = Just name, gMod = gMod }

save :: EditorState -> Task EditorState
save state = case state.EditorState.name of
    Just n  -> storeModule (n, state.EditorState.gMod) >>| 
               return { state & changed = False }
    Nothing -> saveAs state

saveAs :: EditorState -> Task EditorState
saveAs state = newModuleName state.EditorState.gMod >>= \(name, gMod`) = 
    return { state & name = Just name, changed = False }

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

viewSource :: GModule -> Note
viewSource gMod = Note (tryRender gMod False)	

showAbout :: Task Void
showAbout = showMessage ("Gin workflow editor", "version 0.1") Void

accIWorld :: !(*IWorld -> *(!a,!*IWorld)) -> Task a | iTask a
accIWorld fun = mkInstantTask ("Run Iworld function", "Run a IWorld function and get result.") (mkTaskFunction (accIWorldTSt fun))