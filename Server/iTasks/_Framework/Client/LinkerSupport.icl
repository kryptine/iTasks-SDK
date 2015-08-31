implementation module iTasks._Framework.Client.LinkerSupport

from StdFunc import id
import StdString, StdList, StdMisc, StdFile, StdTuple, StdDebug
import Data.Maybe, System.File
import graph_to_sapl_string

from StdOverloaded import class <
from StdClass import class Ord, class Eq
from Data.Map import :: Map, newMap, get, put, toList, toAscList, foldrWithKey
from Data.Set import :: Set, newSet

from iTasks.UI.JS.Interface import :: JSWorld, :: JSEvent, :: JSObj, :: JSObject
from iTasks.UI.Component import :: ComponentEventHandlerFunc, :: ComponentEvent, :: ComponentDiff
from iTasks.API.Core.Client.Editlet import :: EditletEventHandlerFunc, :: EditletEvent, :: ComponentId

from iTasks._Framework.Client.RunOnClient import createClientIWorld, getUIUpdates
from iTasks._Framework.Engine import background

import iTasks.API.Core.Types, iTasks._Framework.IWorld
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

mbLinkByExpr linkerstate lib mbExpr world
	= case mbExpr of
		Just expr # saplEXPR = graph_to_sapl_string expr
 				  # (linkerstate, lib, _, world) = linkByExpr linkerstate lib saplEXPR world
				  = (linkerstate, lib, Just saplEXPR, world)
				  = (linkerstate, lib, Nothing, world)				

mbExprGenerateJS flavour tramp mbparserstate js_lib (Just sapl)
	# (js, js_lib, parserstate) = handlerr (exprGenerateJS flavour tramp sapl mbparserstate js_lib)
	= (Just js, js_lib, (Just parserstate))

mbExprGenerateJS flavour tramp mbparserstate js_lib Nothing
	= (Nothing, js_lib, mbparserstate)

taskletLinker :: !st 							// state
	![(!String, !iarg -> Void)] 				// interface functions
	![(!String, !String, *JSWorld -> Void)]		// event handlers
	!rs											// result function
	!(Maybe cf)									// controller function
	!*IWorld
	->
	*(!String									// JS code of the state
	 ,!String 									// JS code of the support code for all the expressions
	 ,![(!String,!String,!String)]				// JS code of the eventhandlers
	 ,![(!String,!String)]						// JS code of the interface functions
	 ,!String									// JS code of the result function
	 ,!Maybe String								// JS code of the controller function
	 ,!*IWorld)

taskletLinker state interfaceFuns eventHandlers resultFunc mbControllerFunc
						iworld=:{world,current={taskInstance},jsCompilerState}
						
	// unpack "compiler state"
	# (loaderstate, ftmap, flavour, mbparserstate, skipmap) = jsCompilerState
	// create per sesssion "linker state"
	# linkerstate = (loaderstate, ftmap, maybe newSet id (get taskInstance skipmap))
	
	/* 1. First, we collect all the necessary function definitions to generate ParserState */

	// link functions indicated by the state structure
	# (linkerstate, lib, sapl_ST, world) = linkByExpr linkerstate newAppender (graph_to_sapl_string state) world
	// link functions indicated by result func
	# (linkerstate, lib, sapl_RF, world) = linkByExpr linkerstate lib (graph_to_sapl_string resultFunc) world
	// link functions indicated by controller func (if given)
	# (linkerstate, lib, mb_sapl_CF, world) = mbLinkByExpr linkerstate lib mbControllerFunc world
	// if the controller function is given, link for the client side IWorld creation function
	# (linkerstate, lib, world) 
			= case mbControllerFunc of
					Just _  # (linkerstate, lib, _, world) = linkByExpr linkerstate lib (graph_to_sapl_string createClientIWorld) world
							# (linkerstate, lib, _, world) = linkByExpr linkerstate lib (graph_to_sapl_string getUIUpdates) world
							# (linkerstate, lib, _, world) = linkByExpr linkerstate lib (graph_to_sapl_string background) world
							= (linkerstate, lib, world)
							= (linkerstate, lib, world)
	
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
	# (js_lib, mbparserstate) = case sapl_lib of
		"" = (newAppender, mbparserstate)
		   = let (script, pst) = handlerr (generateJS flavour False sapl_lib mbparserstate) in (script, Just pst)
		
	/* 3. Generate expressions by ParserState */
									
	# (js_ST, js_lib, parserstate) = handlerr (exprGenerateJS flavour False sapl_ST mbparserstate js_lib)
	# (js_RF, js_lib, parserstate) = handlerr (exprGenerateJS flavour False sapl_RF (Just parserstate) js_lib)

	# (js_eventHandlers, js_lib, parserstate)
			= foldl (\(os,js_lib,parserstate) (id,event,saplhandler) = 
							let (ejs,js_lib`,parserstate`) = handlerr (exprGenerateJS flavour False saplhandler (Just parserstate) js_lib)
							 in ([(id,event,ejs):os],js_lib`,parserstate`)) ([],js_lib,parserstate) sapl_eventHandlers
	
	# (js_interfaceFuns, js_lib, parserstate)
			= foldl (\(os,js_lib,parserstate) (fn,saplfun) = 
							let (ijs,js_lib`,parserstate`) = handlerr (exprGenerateJS flavour False saplfun (Just parserstate) js_lib)
							 in ([(fn,ijs):os],js_lib`,parserstate`)) ([],js_lib,parserstate) sapl_interfaceFuns
		
	# (mb_js_CF, js_lib, mbparserstate) = mbExprGenerateJS flavour False (Just parserstate) js_lib mb_sapl_CF
	
/* For debugging:*/
	# (_, world) = writeFile "debug_state.sapl" sapl_ST world
	# (_, world) = writeFile "debug_state.js" js_ST world	
	# (_, world) = writeFile "debug.sapl" sapl_lib world
	# (_, world) = writeFile "debug.js" (toString js_lib) world

	= (js_ST, toString js_lib, js_eventHandlers, js_interfaceFuns, js_RF, mb_js_CF, 
			{iworld & world=world, jsCompilerState = (loaderstate, ftmap, flavour, mbparserstate, put taskInstance skipset skipmap)})

editletLinker :: 
	!id													// initDiff
	!icf												// initClient function
	!adf												// appDiff function
	!*IWorld
	->
	*(!String									// JS code of the support code for all the expressions
	 ,!String									// JS code of the initDiff
	 ,!String									// JS code of the defVal function
	 ,!String									// JS code of the adddiff function
	 ,!*IWorld)

editletLinker initDiff initClientFunc appDiffFunc
						iworld=:{world,current={sessionInstance=Nothing}} = ("","","","",iworld)
editletLinker initDiff initClientFunc appDiffFunc
						iworld=:{world,current={sessionInstance=Just currentInstance},jsCompilerState}

	// unpack "compiler state"
	# (loaderstate, ftmap, flavour, mbparserstate, skipmap) = jsCompilerState	
	// create per sesssion "linker state"
	# linkerstate = (loaderstate, ftmap, maybe newSet id (get currentInstance skipmap)) // (Just newSet))

	/* 1. First, we collect all the necessary function definitions to generate ParserState */
	# (linkerstate, lib, sapl_ID, world) = linkByExpr linkerstate newAppender (graph_to_sapl_string initDiff) world
	# (linkerstate, lib, sapl_IC, world) = linkByExpr linkerstate lib (graph_to_sapl_string initClientFunc) world
	# (linkerstate, lib, sapl_AD, world) = linkByExpr linkerstate lib (graph_to_sapl_string appDiffFunc) world

	// unwrap linker state
	# (loaderstate, ftmap, skipset) = linkerstate

	/* 2. Generate function definitions and ParserState */

	# sapl_lib = toString lib
	# (js_lib, mbparserstate) = case sapl_lib of
		"" = (newAppender, mbparserstate)
		   = let (script, pst) = handlerr (generateJS flavour False sapl_lib mbparserstate) in (script, Just pst)
	
	/* 3. Generate expressions by ParserState */

	# (js_ID, js_lib, parserstate) = handlerr (exprGenerateJS flavour False sapl_ID mbparserstate js_lib)
	# (js_IC, js_lib, parserstate) = handlerr (exprGenerateJS flavour False sapl_IC (Just parserstate) js_lib)
	# (js_AD, js_lib, parserstate) = handlerr (exprGenerateJS flavour False sapl_AD (Just parserstate) js_lib)

    // For debugging:
	//# world = debugToFile "debug_id.sapl" sapl_ID world
	//# world = debugToFile "debug_dv.sapl" sapl_DV world
	//# world = debugToFile "debug_ad.sapl" sapl_AD world
	//# world = debugToFile "debug.sapl"    sapl_lib world
	//# world = debugToFile "debug.js"      (toString js_lib) world

	= (toString js_lib, js_ID, js_IC, js_AD, 
			{iworld & world=world, jsCompilerState = (loaderstate, ftmap, flavour, mbparserstate, put currentInstance skipset skipmap)})

debugToFile :: String String *World -> *World
debugToFile fileName debugOutput world
  # (exists, world) = fileExists fileName world
  # world           = if exists world
                        (snd (writeFile fileName "" world))
  # (_, world)      = withFile fileName FAppendData (\file -> (Ok Void, fwrites debugOutput file)) world
  = world

diffLinker :: !cdf !idf !*IWorld -> (!String,!String,!String,!*IWorld)
diffLinker cdf idf iworld=:{world,current={sessionInstance=Nothing}} = ("","","",iworld)
diffLinker cdf idf iworld=:{world,current={sessionInstance=Just currentInstance},jsCompilerState}

    // unpack "compiler state"
	# (loaderstate, ftmap, flavour, mbparserstate, skipmap) = jsCompilerState	
	// create per sesssion "linker state"
	# linkerstate = (loaderstate, ftmap, maybe newSet id (get currentInstance skipmap)) // (Just newSet))  
	/* 1. First, we collect all the necessary function definitions to generate ParserState */
	# (linkerstate, lib, sapl_cdf, world) = linkByExpr linkerstate newAppender (graph_to_sapl_string cdf) world
	# (linkerstate, lib, sapl_idf, world) = linkByExpr linkerstate lib (graph_to_sapl_string idf) world

	// unwrap linker state
	# (loaderstate, ftmap, skipset) = linkerstate

    # sapl_lib = toString lib

    // For debugging:
    //# world = debugToFile "debug_diff_cdf.sapl" sapl_cdf world
    //# world = debugToFile "debug_diff_idf.sapl" sapl_idf world
    //# world = debugToFile "debug_diff.sapl"     sapl_lib world

	# (js_lib, mbparserstate) = case sapl_lib of
		"" = (newAppender, mbparserstate)
		   = let (script, pst) = handlerr (generateJS flavour False sapl_lib mbparserstate) in (script, Just pst)

	# (js_cdf, js_lib, parserstate) = handlerr (exprGenerateJS flavour False sapl_cdf mbparserstate js_lib)
	# (js_idf, js_lib, parserstate) = handlerr (exprGenerateJS flavour False sapl_idf (Just parserstate) js_lib)
    = (toString js_lib,	js_cdf, js_idf, {iworld & world=world, jsCompilerState = (loaderstate, ftmap, flavour, mbparserstate, put currentInstance skipset skipmap)})



