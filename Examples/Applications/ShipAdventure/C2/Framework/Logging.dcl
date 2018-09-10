definition module C2.Framework.Logging

import iTasks

:: Log			=	{ fromWho 		:: !String
					, intendedFor	:: !String
					, when			:: !DateTime
					, about			:: !String
 					}

derive class iTask Log

// shared store for logging events

myLog 	:: SDSLens () [Log] [Log]

// tasks for logging:

showLog :: Task [Log]

addLog 	:: !a !b !c -> Task () | toString a & toString b & toString c



