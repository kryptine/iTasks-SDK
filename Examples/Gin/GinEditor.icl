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

//from Serialization import qualified serialize, deserialize

ginEditor :: WorkflowContainer Void
ginEditor = Workflow initManagerProperties noMenu /*(staticMenu initMenu)*/ ginEditor`

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
	, changed		= False
	, dirty			= False
	, errors		= []
	, source		= ""
	, compiled		= Nothing
	}

//-----------------------------------------------------------------------------------        
ActionCompile          :== Action "compile"			"Compile"
ActionRun              :== Action "run"				"Run"
ActionViewDeclaration  :== Action "viewdeclaration"	"Declaration"
ActionViewWorkflow     :== Action "viewworkflow"	"Workflow"
ActionViewImports      :== Action "viewimports"		"Imports"
ActionViewTypes        :== Action "viewtypes"		"Types"    
ActionViewSource       :== Action "viewsource"		"Generated source"      
//ActionEnableSC         :== Action "sc_on"  			"Enable syntax checking"
//ActionDisableSC        :== Action "sc_off" 			"Disable syntax checking"

initMenu :: MenuDefinition
initMenu =
    [ Menu "File"    [ MenuItem ActionNew				(Just { key = 'N', ctrl = True, alt = False, shift = False })
                     , MenuItem ActionOpen				(Just { key = 'O', ctrl = True, alt = False, shift = False })
                     , MenuSeparator
                     , MenuItem ActionSave				(Just { key = 'S', ctrl = True, alt = False, shift = False })
                     , MenuItem ActionSaveAs			Nothing
                     , MenuSeparator
                     , MenuItem ActionCompile			(Just { key = 'U', ctrl = True, alt = False, shift = False })
                     , MenuItem ActionRun				(Just { key = 'R', ctrl = True, alt = False, shift = False })
                     , MenuSeparator
                     , MenuItem ActionQuit				Nothing
                     ]
    , Menu "View"    [ MenuItem ActionViewDeclaration	Nothing
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

ginEditor` :: Task Void
ginEditor` = 
	getInitialState >>= \initialState -> 
	ginParallelLayout @>> parallel
		"GiN Editor"
		initialState
		(\_ _ -> Void)
		[ HiddenTask activator
		, InBodyTask \s p -> forever (/*ginInteractionLayout @>>*/ 
				(updateSharedInformation "Workflow diagram" [View (diagramView, diagramUpdate)] s >?* actions s p))
		]

ginParallelLayout :: ParallelLayoutMerger
ginParallelLayout = \{TUIParallel|title,description,items} -> 
	if (length items == 1) 
		(last items) 
		(defaultPanelDescr title "icon-parallel-task" description Nothing (WrapContent 0) (tl items ++ [hd items]))

ginInteractionLayout :: InteractionLayoutMerger
ginInteractionLayout = \interaction = 
	case interaction.editorParts of
		[{TUIDef | content = TUIControl (TUIORYXControl _) _}] =
			{TUIDef | hd interaction.editorParts & width = FillParent 1 (FixedMinSize 400)}
		_ 	= defaultInteractionLayout interaction

diagramView :: EditorState -> ORYXEditor
diagramView { EditorState | gMod = { moduleKind = GGraphicalModule defs }, errors } = 
	{ ORYXEditor 
	| (ginORYXEditor (hd defs).GDefinition.body)
	& errors = errors 
	}

diagramUpdate :: ORYXEditor EditorState -> EditorState
diagramUpdate editor state = { EditorState | state & gMod = setDiagram state.gMod editor, dirty = True}
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

activator :: (SymmetricShared EditorState) (ParallelInfo s) -> Task Void
activator stateShared parallelInfo = forever activator`
where
	activator` :: Task Void
	activator` =	(monitor "Diagram monitor" [] stateShared >? \state -> state.dirty) //Look for the dirty flag to become True
					>>= \state -> generateSource state
					>>= \state -> checkErrors state
					>>= \state -> update (\_ -> { state & dirty = False } ) stateShared //Reset dirty flag
					>>| stop

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
	
actions :: (SymmetricShared EditorState) (ParallelInfo EditorState) -> [(!Action,!TaskContinuation state Void)]
actions stateShared parallelInfo
	=	[ (ActionNew,              Always stop)
		, (ActionOpen,             Always (actionTask "Open" open))
		, (ActionSave,             Always (actionTask "Save" save))
		, (ActionSaveAs,           Always (actionTask "Save as" saveAs))
		, (ActionCompile,          Always (actionTask "Compile" compile))
		, (ActionRun,              Always (actionTask "Run" run))
		, (ActionQuit,             Always (set parallelInfo [StopParallel] >>| stop))
		, (ActionAbout,            Always (actionTask "About" showAbout))
		, (ActionViewDeclaration,  Always (moduleEditor "Declaration" (declarationView, declarationUpdate)))
		, (ActionViewImports,      Always importsEditor)
		, (ActionViewTypes,        Always (moduleEditor "Types" (typesView, typesUpdate)))
		, (ActionViewSource,       Always sourceView)
		]
	where
		actionTask title task = get stateShared >>= task >>= set stateShared >>| stop
	
		addTask title task = set parallelInfo 
			[AppendTask (WindowTask title noMenu (\s _ -> task))] >>| stop
		moduleEditor title v = addTask title (updateSharedInformation title [View (liftModuleView v)] stateShared)
		
		declarationEditor = moduleEditor "declaration" (declarationView, declarationUpdate)
		importsEditor = addTask "imports" 
			(					get stateShared 
				>>= \state	 ->	accWorld (searchPathModules state.EditorState.config)
				>>= \modules ->	moduleEditor "imports" (importsView modules, importsUpdate)
			)
		sourceView = addTask "source" (monitor "source view" [Get (\s -> Note s.EditorState.source)] stateShared)

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
		Just (name, gMod) = return { EditorState | initialState & name = Just name, gMod = gMod }
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

formatSource :: String -> HtmlTag
formatSource source = TextareaTag [ColsAttr "80", RowsAttr "25"] [ Text source ]

tryRender :: GModule GinConfig PrintOption *World -> (String, *World)
tryRender gMod config printOption world
# (st, world) = gToAModule gMod config world
# source = case runParse st of
	GSuccess aMod -> prettyPrintAModule printOption aMod
	GError errors -> "Parse error:\n" +++ ((join "\n" (map (\(path,msg) = msg) errors)))
= (source, world)

showAbout :: EditorState -> Task EditorState
showAbout state = showInformation ("Gin workflow editor", "version 0.1") [] state

accIWorld :: !(*IWorld -> *(!a,!*IWorld)) -> Task a | iTask a
accIWorld fun = mkInstantTask ("Run Iworld function", "Run an IWorld function and get result.") eval
where
	eval taskNr iworld
		# (res,iworld)	= fun iworld
		= (TaskFinished res,iworld)

