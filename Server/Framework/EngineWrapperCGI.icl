implementation module EngineWrapperCGI

import Engine
import HTTP, CGI, Tuple, StdMisc

startEngine :: a !*World -> *World | Publishable a
startEngine publishable world
	# (mbSDKPath,world)		= determineSDKPath SEARCH_PATHS world
	| isNothing mbSDKPath	= abort "Could not determine iTasks SDK location"
	= startCGI [] [(p, withIWorld (fromJust mbSDKPath) f)\\(p,f) <- engine publishable] world
where
	withIWorld path f req world
		= appSnd finalizeIWorld (f req (initIWorld path world))
		