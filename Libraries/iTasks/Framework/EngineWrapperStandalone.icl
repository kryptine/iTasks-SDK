implementation module EngineWrapperStandalone

import StdFile, StdInt
import Engine
import Http, HttpServer
import Config

startEngine :: ![Workflow] !*World -> *World 
startEngine flows world
	# (config,world)	= config world
	# world				= instructions config.serverPort world
	# options			= [HTTPServerOptPort config.serverPort, HTTPServerOptDebug config.debug]
	= http_startServer options (engine config flows) world
where
	instructions :: !Int *World -> *World
	instructions port world
		# (console, world)	= stdio world
		# console			= fwrites "iTasks standalone server started...\n" console
		# console			= fwrites ("Please point your browser to " +++ host +++ "\n") console
		# (_,world)			= fclose console world
		= world
		where
			host	= if (port == 80) "http://localhost/" ("http://localhost:" +++ toString port +++ "/")
