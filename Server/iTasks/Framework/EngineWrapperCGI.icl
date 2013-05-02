implementation module iTasks.Framework.EngineWrapperCGI

import iTasks.Framework.Engine, iTasks.Framework.IWorld, iTasks.Framework.TaskStore
import HTTP, CGI, Tuple, StdMisc

startEngine :: a !*World -> *World | Publishable a
startEngine publishable world
	# (mbSDKPath,world)		= determineSDKPath SEARCH_PATHS world
	| isNothing mbSDKPath	= abort "Could not determine iTasks SDK location"
	= startCGI [] [(p, withIWorld (fromJust mbSDKPath) f)\\(p,f) <- engine publishable] world
where
	withIWorld path f req world
		= appSnd finalizeCGIIWorld (f req (initCGIIWorld path world))
		
	initCGIIWorld path world
		//Load previous session user interfaces & outdated instancesc information from disk
		//(normally these are only kept in-memory)
		# iworld = initIWorld path world
		# iworld = restoreUICache iworld
		# iworld = restoreWorkQueue iworld
		= iworld

	finalizeCGIIWorld iworld
		//Store the session user interfaces
		# iworld = saveUICache iworld
		# iworld = saveWorkQueue iworld
		= finalizeIWorld iworld
