implementation module LazyLinker

import StdEnv, StdMaybe, Map
import SaplTokenizer, SaplLinkerShared, StringAppender, FastString
import IWorld

import FilePath, File, Directory
from Error import :: MaybeError, fromOk
from OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode

// Module name -> file name
:: ModuleMap :== Map String String

// (normal module map, builtin module map, ...)
:: LoaderState    :== (ModuleMap, ModuleMap, Warnings, IdGenerator)
:: LoaderStateExt :== (LoaderState, FuncTypeMap)

generateLoaderState :: !*IWorld -> *(LoaderStateExt, !*IWorld)
generateLoaderState iworld=:{IWorld|world} 
	# module_directory = "sapl"
	# builtin_module_directory = "sapl" </> "std"
	
	# (res,world) = readDirectory builtin_module_directory world
	# bms = filter (\bm = (snd (splitExtension bm)) == sapl_module_name_extension) (fromOk res)
	
	# (res,world) = readDirectory module_directory world
	# ms = filter (\m = (snd (splitExtension m)) == sapl_module_name_extension) (fromOk res)

	# onlybuiltins = removeMembers bms ms
	# bms = removeMembers bms onlybuiltins

	# bms = map (\bm = (fst (splitExtension bm), builtin_module_directory </> bm)) bms	
	# onlybuiltins = map (\m = (fst (splitExtension m), builtin_module_directory </> m)) onlybuiltins	
	# ms = map (\m = (fst (splitExtension m), module_directory </> m)) ms
	# ms = ms ++ onlybuiltins

	# bmmap = fromList bms
	# mmap = fromList ms
	
	// These modules can't be run on the client anyway
	# mmap = del "graph_to_sapl_string" mmap
	# mmap = del "graph_to_string_with_descriptors" mmap
	# mmap = del "dynamic_string" mmap	
	# mmap = del "sapldebug" mmap
	
	# mmap = del "Base64" mmap
	
	# mmap = del "LazyLinker" mmap
	# mmap = del "CodeGeneratorJS" mmap
	// This module has different implementation at client side
	# mmap = del "SaplHtmlClientSupport" mmap
	
	= (((mmap, bmmap, [], 0), newMap), {iworld & world=world})

linkSaplforExpr :: !String *IWorld -> *(!String,!*IWorld)
linkSaplforExpr expr iworld
	# (ls, iworld) = generateLoaderState iworld
	# (_, a, iworld) = linkSaplforExprByLoaderState ls newAppender expr iworld
	= (toString a, iworld)

linkSaplforExprByLoaderState :: LoaderStateExt !StringAppender !String !*IWorld -> *(LoaderStateExt, !StringAppender, !*IWorld)
linkSaplforExprByLoaderState (ls,lmap) a expr iworld=:{IWorld|world} 
	# maindeps = generate_dependencies (tokens expr) []

	# (lmap, (_, ls), world, a) 
				= foldl (\(llmap, loader, world, a) d = generate_source llmap loader d world a) 
					    (lmap, (lazy_loader, ls), world, a) maindeps

	= ((ls,lmap), a, {iworld & world=world})

where
	getModuleName name
		# (ok, pos) = charIndex name 1 '.'
		| ok
			= name % (0,pos-1)
			= ""

	/* Load a given function (LoaderFunction LoaderState, see SaplLinkerShared)
	 *
	 * @param ls loader state (module map, built-in module map, warning messages, id generator)
	 * @param fn function name to be loaded
	 * @param lmap line map
	 */
	lazy_loader :: LoaderState String FuncTypeMap *World -> *(Maybe LineType, FuncTypeMap, LoaderState, *World)
	lazy_loader ls=:(mmap, bmmap, messages, id) fn lmap world 
		# line = get fn lmap
		| isJust line
			= (line, lmap, ls, world)

			// try to load the module		
			# m = getModuleName fn
			| size m == 0 // the function name doesn't contain module name
				= (Nothing, lmap, ls, world)

				// is it already loaded?
				# (mpath, mmap) = delU m mmap 
				| isNothing mpath
					= (Nothing, lmap, ls, world)
					# (lmap, id, messages, world) = read_module (fromJust mpath) lmap messages id world
				
					// read built-in module if avalaible
					# (bmpath, bmmap) = delU m bmmap
					# (lmap, id, messages, world) = 
						if (isJust bmpath)
							(read_module (fromJust bmpath) lmap messages id world)
							(lmap, id, messages, world)
				
					// try to get the line information again
					= (get fn lmap, lmap, (mmap,bmmap,messages,id), world)
