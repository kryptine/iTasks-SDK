implementation module iTasks.Framework.Client.LinkerSupport

import StdString, StdList, StdFunc, StdMisc, StdFile
import Data.Maybe, System.File
import graph_to_sapl_string

from StdOverloaded import class <
from StdClass import class Ord, class Eq
from Data.Map import :: Map, newMap, get, put
from Data.Set import :: Set, newSet

from iTasks.API.Core.Client.Interface import :: JSWorld, :: JSEvent
from iTasks.API.Core.Client.Component import :: ComponentEventHandlerFunc, :: ComponentEvent
from iTasks.API.Core.Client.Editlet import :: EditletEventHandlerFunc, :: EditletEvent, :: ComponentId

import iTasks.API.Core.SystemTypes, iTasks.Framework.IWorld
import Sapl.Target.JS.CodeGeneratorJS, Sapl.Linker.LazyLinker, Sapl.SaplParser

//---------------------------------------------------------------------------------------

printlnI :: !String !*IWorld -> *IWorld
printlnI msg iw=:{world} 
	# (console,world)	= stdio world
	# console			= fwrites msg console
	# console			= fwrites "\n" console
	# (_,world)			= fclose console world
	= {iw & world = world} 

println :: !String !*World -> *World
println msg world 
	# (console,world)	= stdio world
	# console			= fwrites msg console
	# console			= fwrites "\n" console
	# (_,world)			= fclose console world
	= world

//---------------------------------------------------------------------------------------

handlerr (Error str) = abort ("LinkerSupport.icl: " +++ str)
handlerr (Ok a) = a

// TODO: we should link for the expression not just compile it

taskletUpdateLinker :: !val !*IWorld -> *(!String, !*IWorld)
taskletUpdateLinker updateVal iworld=:{world,jsCompilerState}
	= (valjs, {iworld & world=world})
where
	(_,_,flavour,mbparserstate,_) = jsCompilerState
	sapl = graph_to_sapl_string updateVal
	valjs = toString (handlerr (exprGenerateJS flavour False sapl mbparserstate))

mbLinkByExpr linkerstate lib mbExpr world
	= case mbExpr of
		Just expr # saplEXPR = graph_to_sapl_string expr
 				  # (linkerstate, lib, _, world) = linkByExpr linkerstate lib saplEXPR world
				  = (linkerstate, lib, Just saplEXPR, world)
				  = (linkerstate, lib, Nothing, world)				

taskletLinker :: !st 							// state
	![(!String, !iarg -> Void)] 				// interface functions
	![(!String, !String, *JSWorld -> Void)]		// event handlers
	!rs											// result function
	!(Maybe cf)									// controller function
	!(Maybe uf)									// update function
	!(Maybe uv)									// update value
	!*IWorld
	->
	*(!String									// JS code of the state
	 ,!String 									// JS code of the support code for all the expressions
	 ,![(!String,!String,!String)]				// JS code of the eventhandlers
	 ,![(!String,!String)]						// JS code of the interface functions
	 ,!String									// JS code of the result function
	 ,!Maybe String								// JS code of the controller function
	 ,!Maybe String								// JS code of the update function
	 ,!*IWorld)

taskletLinker state interfaceFuns eventHandlers resultFunc mbControllerFunc mbUpdateFunc mbUpdateVal 
						//iworld=:{world,currentSession=Just currentInstance,jsCompilerState}
						iworld=:{world,currentInstance,jsCompilerState}
						
	// unpack "compiler state"
	# (loaderstate, ftmap, flavour, mbparserstate, skipmap) = jsCompilerState
	// create per sesssion "linker state"
	# linkerstate = (loaderstate, ftmap, maybe newSet id (get currentInstance skipmap))
	
	/* 1. First, we collect all the necessary function definitions to generate ParserState */

	// link functions indicated by the state structure
	# (linkerstate, lib, sapl_ST, world) = linkByExpr linkerstate newAppender (graph_to_sapl_string state) world
	// link functions indicated by result func
	# (linkerstate, lib, sapl_RF, world) = linkByExpr linkerstate lib (graph_to_sapl_string resultFunc) world
	// link functions indicated by update func (if given)
	# (linkerstate, lib, mb_sapl_UF, world) = mbLinkByExpr linkerstate lib mbUpdateFunc world
	// only link (the source code of the expression is not needed) by update value (if given)
	// this is only a temporary solution. for some types it won't work e.g. when the argument
	// of the data constructor can be arbitrary function
	# (linkerstate, lib, _, world) = mbLinkByExpr linkerstate lib mbUpdateVal world
	// link functions indicated by controller func (if given)
	# (linkerstate, lib, mb_sapl_CF, world) = mbLinkByExpr linkerstate lib mbControllerFunc world
	
	// link functions indicated by event handlers
	# (linkerstate, lib, sapl_eventHandlers, world) 
			= foldl (\(linkerstate, lib, os, world) (id, event, f) = 
				let (linkerstate`, lib`, f`, world`) = linkByExpr linkerstate lib (graph_to_sapl_string f) world
				 in (linkerstate`, lib`, [(id,event,f`):os], world`)) 
			(linkerstate, lib, [], world) eventHandlers

	// link functions indicated by interface functions
	# (linkerstate, lib, sapl_interfaceFuns, world) 
			= foldl (\(linkerstate, lib, os, world) (fn, f) = 
				let (linkerstate`, lib`, f`, world`) = linkByExpr linkerstate lib (graph_to_sapl_string f) world
				 in (linkerstate`, lib`, [(fn,f`):os], world`)) 
			(linkerstate, lib, [], world) interfaceFuns

	// unwrap linker state
	# (loaderstate, ftmap, skipset) = linkerstate

	/* 2. Generate function definitions and ParserState */

	# sapl_lib = toString lib
	# (js_lib, mbPst) = case sapl_lib of
		"" = ("", Nothing)
		   = let (script, pst) = handlerr (generateJS flavour False sapl_lib) in (toString script, Just pst)
	
	/* 3. Merge ParserState with the global one */
	
	# mbparserstate = fmap (\pst -> mergeParserStates pst mbPst) mbparserstate
	# mbparserstate = if (isJust mbparserstate) mbparserstate mbPst
	
	/* 4. Generate expressions by ParserState */
									
	# js_ST = toString (handlerr (exprGenerateJS flavour False sapl_ST mbparserstate))
	# js_RF = toString (handlerr (exprGenerateJS flavour False sapl_RF mbparserstate))			

	# js_eventHandlers = map (\(id,event,saplhandler) = (id,event,toString (handlerr 
							(exprGenerateJS flavour False saplhandler mbparserstate)))) sapl_eventHandlers
	
	# js_interfaceFuns = map (\(fn,saplfun) = (fn, toString (handlerr 
							(exprGenerateJS flavour False saplfun mbparserstate)))) sapl_interfaceFuns
	
	# mb_js_UF = fmap (\sapl -> toString (handlerr (exprGenerateJS flavour False sapl mbparserstate))) mb_sapl_UF
	# mb_js_CF = fmap (\sapl -> toString (handlerr (exprGenerateJS flavour False sapl mbparserstate))) mb_sapl_CF

/* For debugging:
	# (_, world) = writeFile "debug_state.sapl" sapl_ST world
	# (_, world) = writeFile "debug_state.js" js_ST world	
	# (_, world) = writeFile "debug.sapl" sapl_lib world
	# (_, world) = writeFile "debug.js" js_lib world
*/
	= (js_ST, js_lib, js_eventHandlers, js_interfaceFuns, js_RF, mb_js_CF, mb_js_UF, 
			{iworld & world=world, jsCompilerState = (loaderstate, ftmap, flavour, mbparserstate, put currentInstance skipset skipmap)})

editletLinker :: 
	![(!String, !String, EditletEventHandlerFunc a)]	// event handlers
	!idf												// initDiff function
	!dvf												// defVal function
	!uui												// updateUI function
	!gdf												// gendiff function
	!adf												// adddiff function
	!*IWorld
	->
	*(!String									// JS code of the support code for all the expressions
	 ,![(!String,!String,!String)]				// JS code of the eventhandlers
	 ,!String									// JS code of the initDiff	 
	 ,!String									// JS code of the defVal function
	 ,!String									// JS code of the updateUI function
	 ,!String									// JS code of the gendiff function
	 ,!String									// JS code of the adddiff function
	 ,!*IWorld)

editletLinker eventHandlers initDiff defValFunc updateUIFunc genDiffFunc appDiffFunc
						iworld=:{world,currentSession=Nothing} = ("",[],"","","","","",iworld) //REALLY????
editletLinker eventHandlers initDiff defValFunc updateUIFunc genDiffFunc appDiffFunc
						iworld=:{world,currentSession=Just currentInstance,jsCompilerState}

	// unpack "compiler state"
	# (loaderstate, ftmap, flavour, mbparserstate, skipmap) = jsCompilerState
	// create per sesssion "linker state"
	# linkerstate = (loaderstate, ftmap, maybe newSet id (get currentInstance skipmap))

	/* 1. First, we collect all the necessary function definitions to generate ParserState */
	# (linkerstate, lib, sapl_ID, world) = linkByExpr linkerstate newAppender (graph_to_sapl_string initDiff) world
	# (linkerstate, lib, sapl_DV, world) = linkByExpr linkerstate lib (graph_to_sapl_string defValFunc) world
	# (linkerstate, lib, sapl_UU, world) = linkByExpr linkerstate lib (graph_to_sapl_string updateUIFunc) world
	# (linkerstate, lib, sapl_GD, world) = linkByExpr linkerstate lib (graph_to_sapl_string genDiffFunc) world
	# (linkerstate, lib, sapl_AD, world) = linkByExpr linkerstate lib (graph_to_sapl_string appDiffFunc) world

	// link functions indicated by event handlers
	# (linkerstate, lib, sapl_eventHandlers, world) 
			= foldl (\(linkerstate, lib, os, world) (id, event, f) =
				let (linkerstate`, lib`, f`, world`) = linkByExpr linkerstate lib (graph_to_sapl_string f) world
				 in (linkerstate`, lib`, [(id,event,f`):os], world`))
			(linkerstate, lib, [], world) eventHandlers

	// unwrap linker state
	# (loaderstate, ftmap, skipset) = linkerstate

	/* 2. Generate function definitions and ParserState */

	# sapl_lib = toString lib
	# (js_lib, mbPst) = case sapl_lib of
		"" = ("", Nothing)
		   = let (script, pst) = handlerr (generateJS flavour False sapl_lib) in (toString script, Just pst)

	/* 3. Merge ParserState with the global one */
	
	# mbparserstate = fmap (\pst -> mergeParserStates pst mbPst) mbparserstate
	# mbparserstate = if (isJust mbparserstate) mbparserstate mbPst
	
	/* 4. Generate expressions by ParserState */

	# js_ID = toString (handlerr (exprGenerateJS flavour False sapl_ID mbparserstate))
	# js_DV = toString (handlerr (exprGenerateJS flavour False sapl_DV mbparserstate))	
	# js_UU = toString (handlerr (exprGenerateJS flavour False sapl_UU mbparserstate))	
	# js_GD = toString (handlerr (exprGenerateJS flavour False sapl_GD mbparserstate))
	# js_AD = toString (handlerr (exprGenerateJS flavour False sapl_AD mbparserstate))

	# js_eventHandlers = map (\(id,event,saplhandler) = (id,event,toString (handlerr
							(exprGenerateJS flavour False saplhandler mbparserstate)))) sapl_eventHandlers

/* For debugging:
	# (_, world) = withFile "debug.sapl" FAppendData (\file -> (Ok Void, fwrites sapl_lib file)) world
	# (_, world) = withFile "debug.js" FAppendData (\file -> (Ok Void, fwrites js_lib file)) world
*/

	= (js_lib, js_eventHandlers, js_ID, js_DV, js_UU, js_GD, js_AD, 
			{iworld & world=world, jsCompilerState = (loaderstate, ftmap, flavour, mbparserstate, put currentInstance skipset skipmap)})


