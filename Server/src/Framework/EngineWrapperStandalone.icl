implementation module EngineWrapperStandalone

import StdFile, StdInt
import Engine
import Http, HttpServer
import Config

startEngine :: ![Workflow] !*World -> *World 
startEngine flows world
	# (mbConfig,world)	= config world
	# world				= instructions mbConfig world
	# options			= case mbConfig of
							Just config = [HTTPServerOptPort config.serverPort, HTTPServerOptDebug config.debug]
							Nothing		= []
	= http_startServer options (engine mbConfig flows) world
where
	instructions :: !(Maybe Config) *World -> *World
	instructions (Just config=:{serverPort,serverPath,staticPath,clientPath}) world
		# (console,world)	= stdio world
		# console			= fwrites "*** iTasks HTTP server started ***\n\n" console
		# console			= fwrites ("Serving client from directory: " +++ clientPath +++ "\n") console
		# console			= fwrites ("Serving static content from directory: " +++ staticPath +++ "\n\n") console
		# console			= fwrites ("You can access the client at: " +++ host +++ "/\n") console
		# console			= fwrites ("You can access the services directly at: " +++ host +++ serverPath +++ "\n") console 
		# (_,world)			= fclose console world
		= world
		where
			host	= if (serverPort == 80) "http://localhost" ("http://localhost:" +++ toString serverPort)
	instructions Nothing world
		# (console,world)	= stdio world
		# console			= fwrites "*** iTasks HTTP server started in setup mode***\n\n" console
		# console			= fwrites ("Please open http://localhost/ and follow instructions\n") console
		# (_,world)			= fclose console world
		= world