implementation module EngineWrapperStandalone

import StdFile
import Engine
import Http, HttpServer
import iDataSettings

startEngine :: ![Workflow] !*World -> *World 
startEngine flows world
	# world		= instructions world
	# options	= if TraceHTTP [HTTPServerOptDebug True] []
	= http_startServer options (engine flows) world
where
	instructions :: *World -> *World
	instructions world
		# (console, world)	= stdio world
		# console			= fwrites "iTasks standalone server started...\n" console
		# console			= fwrites ("Please point your browser to http://localhost/\n") console
		# (_,world)			= fclose console world
		= world
