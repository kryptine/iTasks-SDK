implementation module GinEditor

import StdFile
import DynamicIO

import GenEq
import Text
import iTasks

import GinAbstractSyntax
import GinConfig
import GinCompiler
import GinDomain
import GinStorage
import GinSyntax

from GinOSUtils import qualified ::Path, appendTrailingSeparator
from clCCall_12 import winFileExists

ginEditor :: Task Void
ginEditor = (ginSetup >>| handleMenu) <<@ FWFullWidth

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
	ginCheckApplet config
	| not (winFileExists ('GinOSUtils'.appendTrailingSeparator config.iTasksPath +++ "Client\\Build\\Gin.jar"))
	  = showInstruction "Gin Editor" instruction Void >>| ginCheckApplet config
	= stop
	where
		instruction = PTag [] [ Text "In order to run the Gin editor, make sure that"
							  , UlTag [] [ LiTag [] [Text "1. The Java JDK 1.6.x is installed on your system"]
							             , LiTag [] [Text "2. The JAVA_HOME environment variable points to the JDK directory"]
							             ]
							  , Text "Next, run the batch file "
							  , TtTag [] [Text ('GinOSUtils'.appendTrailingSeparator config.iTasksPath +++ "Client\\Gin\\build.bat")]
							  , Text " to compile the Gin editor applet."
							  ]

:: Mode = EditWorkflow | EditTypes | EditCode
derive gEq Mode

:: EditorState = 
    { name     :: Maybe String
    , changed  :: Bool
    , mode     :: Mode
    , editor   :: GinEditor
    , compiled :: Bool
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

initMenu :: Menus
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
                , (ActionRun,              (\_ -> state.EditorState.compiled ))
                , (ActionQuit,             always)
                , (ActionAbout,            always)
                , (ActionEditWorkflow,     (\_ -> state.EditorState.mode =!= EditWorkflow))
                , (ActionEditTypes,        (\_ -> state.EditorState.mode =!= EditTypes))
                , (ActionEditCode,         (\_ -> state.EditorState.mode =!= EditCode))
                , (ActionEnableSC,         (\_ -> not state.EditorState.editor.GinEditor.checkSyntax))
                , (ActionDisableSC,        (\_ -> state.EditorState.editor.GinEditor.checkSyntax))
                ]

handleMenu :: Task Void
handleMenu 
    =   initMenu @>> doMenu emptyState

emptyState :: EditorState 
emptyState = { name = Nothing, changed = False, mode = EditWorkflow, editor = newEditor, compiled = False }
    
doMenu :: EditorState -> Task Void
doMenu state =: { EditorState | mode, editor } = 
    case mode of
        EditWorkflow -> updateInformationA (getName state, "workflow view") idBimap (actions state) editor
                        >>= \(action, mbEditor) 
                        = case mbEditor of
                        	Nothing                -> return (action, state)
                        	Just editor` -> return (action, setChanged state { EditorState | state & editor = editor`})
        EditTypes    -> updateInformationA (getName state, "types view") idBimap (actions state) editor.gMod.GModule.types
                        >>= \(action, mbTypes) 
                        = case mbTypes of
                        	Nothing     -> return (action, state)
                        	Just types` -> return (action, setChanged state { EditorState | state & editor = {GinEditor | state.editor & gMod = {GModule | state.editor.gMod & types = types` }}})
        EditCode     -> updateInformationA (getName state, "code view") idBimap (actions state) 
                                           ( Note (tryRender editor.gMod False)
                                           , Note (tryRender editor.gMod True)
                                           )
                        >>= \(action, _) = return (action, state)                   
    >>= switchAction

switchAction :: (ActionEvent, EditorState) -> Task Void
switchAction ((action, actiondata), state) = 
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
        ActionEnableSC         -> doMenu { EditorState | state & editor = { GinEditor | state.editor & checkSyntax = True } }
        ActionDisableSC        -> doMenu { EditorState | state & editor = { GinEditor | state.editor & checkSyntax = False } }
    
getName :: EditorState -> String
getName state = case state.EditorState.name of
    Just n  -> n
    Nothing -> "(unnamed)"

setChanged :: EditorState EditorState -> EditorState
setChanged old new = if (old.editor =!= new.editor) { new & changed = True } new

tryRender :: GModule Bool -> String
tryRender gMod expand = 
    case runParse (gToAModule gMod) of
        GSuccess aMod -> renderAModule [] ((if expand expandModule id) aMod) 
        GError errors -> "Parse error:\n" +++ ((join "\n" (map (\(path,msg) = toString path +++ ":" +++ msg) errors)))        
              
open :: EditorState -> Task EditorState 
open state = chooseModule >>= \(name, gMod) = 
    return { EditorState | emptyState & name = Just name, editor = { GinEditor | newEditor & gMod = gMod } }

save :: EditorState -> Task EditorState
save state = case state.EditorState.name of
    Just n  -> storeModule (n, state.EditorState.editor.GinEditor.gMod) >>| 
               return { state & changed = False }
    Nothing -> saveAs state

saveAs :: EditorState -> Task EditorState
saveAs state = newModuleName state.EditorState.editor.GinEditor.gMod >>= \(name, gMod`) = 
    return { state & name = Just name, changed = False }

askSaveIfChanged :: EditorState -> Task Void
askSaveIfChanged state = if state.changed
    (requestConfirmation ("File " +++ (getName state) +++ " has changed, save changes?") >>= \confirm =
        if confirm (save state >>| return Void) (return Void)
    )
    (return Void)

update :: EditorState -> Task EditorState
update state
# state = { state & compiled = False }
= accWorld (batchBuild state.editor.GinEditor.gMod) 
  >>= \result = case result of
   				  CompileSuccess dyn -> return { state & compiled = True }
    			  error              -> showMessageAbout "Compiler output" error >>| return state

run :: EditorState -> Task Void
run state 					= readDynamicTask "test" 
			>>= \(ok, task) = (if ok task (showMessage ("Error", "Failed to read task") Void)) >>| stop

viewSource :: GinEditor -> Note
viewSource editor = Note (tryRender editor.GinEditor.gMod False)	
    
showAbout :: Task Void
showAbout = showMessage ("Gin workflow editor", "version 0.1") Void
