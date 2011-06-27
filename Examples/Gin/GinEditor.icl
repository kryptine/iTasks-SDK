implementation module GinEditor

import StdFile

import GenEq
import Text
import iTasks

import GinAbstractSyntax
import GinConfig
import GinCompiler
import GinDomain
import GinParser
import GinStorage
import GinSyntax

import FilePath, File
from Util import appFst

//from Serialization import qualified serialize, deserialize

ginEditor :: WorkflowContainer Void
ginEditor = Workflow initManagerProperties ginEditor`

getConfig :: Task GinConfig
getConfig = accWorld ginLoadConfig >>= \maybeConfig = 
    case maybeConfig of 
        Just config = accWorld (ginCheckConfig config) >>= \error = (if (isNothing error) (return config) (setupDialog config))
        Nothing     = accWorld ginDefaultConfig >>= \config = setupDialog config
where
	setupDialog :: GinConfig -> Task GinConfig
	setupDialog config = dialog config >>= \newconfig = appWorld (ginStoreConfig newconfig) >>| return newconfig
	where
	    dialog config = updateInformation "GiN editor setup" [] config >>= \config = 
	                    accWorld (ginCheckConfig config) >>= \error = if (isNothing error) (return config) (dialog config)

:: EditorState = 
    { config		:: !GinConfig
    , name			:: !Maybe String
    , gMod			:: !GModule
    , checkSyntax	:: !Bool
    , changed		:: !Bool
    , dirty			:: !Bool
    , errors		:: ![ORYXError]
    , source		:: !String
    , compiled		:: !Maybe String
    }

derive class iTask EditorState
    
getInitialState :: Task EditorState 
getInitialState = getConfig >>= \config -> return
	{ EditorState
	| config		= config
	, name			= Nothing
	, gMod			= updateDiagramExtensions newModule
	, checkSyntax	= False
	, changed		= False
	, dirty			= False
	, errors		= []
	, source		= ""
	, compiled		= Nothing
	}

//-----------------------------------------------------------------------------------        
ActionCompile          :== Action "File/Compile"			
ActionRun              :== Action "File/Run"				
ActionViewDeclaration  :== Action "View/Declaration"		
ActionViewWorkflow     :== Action "View/Workflow"			
ActionViewImports      :== Action "View/Imports"			
ActionViewTypes        :== Action "View/Types"				   
ActionViewSource       :== Action "View/Generated source"   
ActionEnableSC         :== Action "Options/Enable syntax checking"
ActionDisableSC        :== Action "Options/Disable syntax checking"

ginEditor` :: Task Void
ginEditor` = 
	getInitialState >>= \initialState -> 
	ginParallelLayout @>> 
	parallel
		"GiN Editor"
		initialState
		(\_ _ -> Void)
		[ ShowAs BodyTask \s p -> forever (ginInteractionLayout @>>
				(updateSharedInformation "Workflow diagram" 
					[UpdateView (GetShared diagramView, PutbackShared diagramUpdate)] 
					s Void) >>+ noActions
				)
		, ShowAs HiddenTask \s p -> forever (chooseAction (actions s p) >>= id)
		, ShowAs HiddenTask activator		
		]

ginParallelLayout :: ParallelLayouter
ginParallelLayout = \par=:{TUIParallel|title,description,items}-> 
	case items of
		[(Just editor,_),(_,actions),activator]	= (editor,actions)
		_ 										= defaultParallelLayout par

ginInteractionLayout :: InteractionLayouter
ginInteractionLayout = \interaction = 
	case interaction.editorParts of
		[{TUIDef | content = TUIControl (TUIORYXControl _) _}] =
			({TUIDef | hd interaction.editorParts & width = FillParent 1 (FixedMinSize 400)},interaction.TUIInteraction.actions)
		_ 	= defaultInteractionLayout interaction

diagramView :: EditorState -> ORYXEditor
diagramView { EditorState | gMod = { moduleKind = GGraphicalModule defs }, errors } = 
	{ ORYXEditor 
	| (ginORYXEditor (hd defs).GDefinition.body)
	& errors = errors 
	}

diagramUpdate :: ORYXEditor Void EditorState -> EditorState
diagramUpdate editor _ state = { EditorState | state & gMod = setDiagram state.gMod editor, dirty = True}
where
	setDiagram :: !GModule !ORYXEditor -> GModule
	setDiagram gMod =:{moduleKind = (GGraphicalModule defs)} editor=:{diagram}
		=	{ GModule 
			| gMod
			& moduleKind = GGraphicalModule
				( [ { GDefinition 
					| hd defs 
					& body = diagram
					}
					: tl defs
				  ]
				)
			}

activator :: (Shared EditorState) (ParallelInfo s) -> Task Void
activator stateShared parallelInfo = forever activator` 
where
	activator` :: Task Void
	activator` =	(showSharedInformation "Diagram monitor" [] stateShared Void >? \(state,_) -> state.dirty) //Look for the dirty flag to become True
					>>= \(state,_) -> return { EditorState | state & dirty = False, changed = True }
					>>= generateSource
					>>= \state -> (if state.EditorState.checkSyntax 
									(checkErrors state)
									(return state))
					>>= set stateShared >>| stop

generateSource :: EditorState -> Task EditorState
generateSource state = accWorld (tryRender state.EditorState.gMod state.EditorState.config POICL) 
	>>= \source -> return { EditorState | state & source = source }


checkErrors :: EditorState -> Task EditorState
checkErrors state=:{ EditorState | gMod = { moduleKind = GGraphicalModule defs } }
	= accIWorld (syntaxCheck state.EditorState.gMod)
	  >>= transform (\compileResult -> { EditorState | state & errors = makeErrorString compileResult })
where
	makeErrorString :: (CompileResult a) -> [ORYXError]
	makeErrorString (CompileSuccess _) = []
	makeErrorString (CompileGlobalError error) = [makeORYXError ((hd defs).GDefinition.body) ([], error)]
	makeErrorString (CompilePathError errors) = map (makeORYXError ((hd defs).GDefinition.body)) errors

actions :: (Shared EditorState) (ParallelInfo EditorState) -> [(Action, Task Void)]
actions stateShared parallelInfo
	=	[ (ActionNew,              actionTask (\s -> askSaveIfChanged s >>| getInitialState))
		, (ActionOpen,             actionTask open)
		, (ActionSave,             actionTask save)
		, (ActionSaveAs,           actionTask saveAs)
		, (ActionCompile,          actionTask compile)
		, (ActionRun,              actionTask run)
		, (ActionQuit,             set parallelInfo [StopParallel] >>| stop)
		, (ActionViewDeclaration,  moduleEditor "Declaration" (declarationView, declarationUpdate))
		, (ActionViewImports,      importsEditor)
		, (ActionViewTypes,        moduleEditor "Types" (typesView, typesUpdate))
		, (ActionViewSource,       sourceView)
		, (ActionEnableSC,	       actionTask (\s -> checkErrors { s & checkSyntax = True }))
		, (ActionDisableSC,	       actionTask (\s -> return { s & checkSyntax = False }))
		, (ActionAbout,            actionTask showAbout)
		]
	where
		addTask task = defaultInteractionLayout @>>
			set parallelInfo [AppendTask (ShowAs BodyTask (\s _ -> task))] >>| stop

		actionTask task = addTask (get stateShared >>= task >>= set stateShared)

		moduleEditor title v = addTask (updateSharedInformation title [UpdateView (app2 (GetShared,\f -> PutbackShared (\a _ e -> f a e)) (liftModuleView v))] stateShared Void)
		
		declarationEditor = moduleEditor "declaration" (declarationView, declarationUpdate)
		importsEditor = addTask 
			(					get stateShared 
				>>= \state	 ->	accWorld (searchPathModules state.EditorState.config)
				>>= \modules ->	moduleEditor "imports" (importsView modules, importsUpdate)
			)
		sourceView = addTask (showSharedInformation "source view" [ShowView (GetShared (\s -> formatSource s.EditorState.source))] stateShared Void)

liftModuleView :: (GModule -> a, a GModule -> GModule) -> (EditorState -> a, a EditorState -> EditorState)
liftModuleView (toView, fromView) = 
	( \model -> toView model.gMod
	, \view model -> { model & gMod = fromView view model.gMod, changed = True }
	)
	
declarationView :: !GModule -> GDeclaration
declarationView {moduleKind = (GGraphicalModule defs)} = (hd defs).GDefinition.declaration
	
declarationUpdate :: !GDeclaration !GModule -> GModule
declarationUpdate decl gMod=:{moduleKind = (GGraphicalModule defs)} =
	{ gMod
	& moduleKind = GGraphicalModule 
		[	{ GDefinition 
			| hd defs & declaration = decl
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

typesView :: !GModule -> [GTypeDefinition]
typesView gMod = gMod.GModule.types
typesUpdate :: ![GTypeDefinition] !GModule -> GModule
typesUpdate types gMod = { GModule | gMod & types = types }

getName :: EditorState -> String
getName state = case state.EditorState.name of
    Just n  -> n
    Nothing -> "(unnamed)"

setChanged :: EditorState EditorState -> EditorState
setChanged old new = if (old.EditorState.gMod =!= new.EditorState.gMod) { new & changed = True } new        

open :: EditorState -> Task EditorState 
open state = getInitialState >>= \initialState -> chooseModule state.EditorState.config >>= \mMod = 
	case mMod of
		Just (name, gMod) = return { EditorState | initialState & name = Just name, gMod = gMod } >>= generateSource
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
    (		showInformation ("File " +++ (getName state) +++ " has changed, save changes?") [] Void
        >?*	[ (ActionNo,	Always (return Void))
        	, (ActionYes,	Always (save state >>| return Void))
        	]
    )
    (return Void)
where
	requestConfirmation :: !String -> Task Bool 
	requestConfirmation message = showInformation message Void >>+ \_ -> UserActions [(ActionYes, True), (ActionNo, False)]

compile :: EditorState -> Task EditorState
compile state
# state = { state & compiled = Nothing }
= accIWorld (batchBuild state.EditorState.gMod)
  >>= \result = case result of
   				  CompileSuccess dynfile 	-> showInformation ("Compiler output", "Compiled successfully") [] Void 
   				  								>>| return { state & compiled = Just dynfile }
    			  error						-> showInformation "Compiler output" [About error] state

run :: EditorState -> Task EditorState
run state = showInformation ("Error", "Runninig tasks requires dynamic linker") [] state
/*
run state = 
	case state.compiled of
		Nothing 	 = showInformation ("Error", "No compiled task") [] state
		Just dynfile = readDynamicTask dynfile >>= \task = catchAll task  (\error -> showInformation ("Error", error) [] Void)
						>>| return state
where
	readDynamicTask :: !String -> Task (Task a) | iTask a
	readDynamicTask filename = importTextFile filename >>= \dynString -> 
		case 'Serialization'.deserialize dynString of
			Ok value = return value
			Error errorString = throw (DynamicIOException errorString)

:: DynamicIOException = DynamicIOException !String
derive class iTask DynamicIOException

instance toString DynamicIOException
where
	toString (DynamicIOException errorString) = errorString
*/
formatSource :: String -> HtmlDisplay
formatSource source = toHtmlDisplay (TextareaTag [ColsAttr "80", RowsAttr "25"] [ Text source ])

tryRender :: GModule GinConfig PrintOption *World -> (String, *World)
tryRender gMod config printOption world
# (st, world) = gToAModule gMod config world
# source = case runParse st of
	GSuccess aMod -> prettyPrintAModule printOption aMod
	GError errors -> "Parse error:\n" +++ ((join "\n" (map (\(path,msg) = msg) errors)))
= (source, world)

showAbout :: EditorState -> Task EditorState
showAbout state = showInformation "Gin workflow editor" [] "version 0.2" >>| return state

accIWorld :: !(*IWorld -> *(!a,!*IWorld)) -> Task a | iTask a
accIWorld fun = mkInstantTask ("Run Iworld function", "Run an IWorld function and get result.") eval
where
	eval taskNr iworld
		# (res,iworld)	= fun iworld
		= (TaskFinished res,iworld)

