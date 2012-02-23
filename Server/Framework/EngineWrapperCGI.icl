implementation module EngineWrapperCGI

import Engine
import HTTP, CGI, StdMisc

startEngine :: a !*World -> *World | Publishable a
startEngine publishable world
	# (mbSDKPath,world)		= determineSDKPath SEARCH_PATHS world
	| isNothing mbSDKPath	= abort "Could not determine iTasks SDK location"
	= startCGI [] (engine (fromJust mbSDKPath) publishable) world