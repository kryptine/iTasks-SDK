implementation module iTasks.Internal.Test.Stubs

import iTasks.Internal.IWorld
import ABC.Interpreter
import System.Time
import StdEnv
import iTasks
from Data.Map import newMap

//TEST STUBS
toStubIWorld :: !*World -> *IWorld
toStubIWorld world
	# (opts, world) = defaultEngineOptions world
	= 
		{ IWorld
		| options = opts
		, clock = zero
		, nextTick = []
		, current =
			{ taskTime = 0
			, taskInstance = 0
			, sessionInstance = Nothing
			, attachmentChain = []
			, nextTaskNo = 0
			}
		, random = []
		, sdsNotifyRequests = newMap
		, sdsNotifyReqsByTask = newMap
		, memoryShares = newMap
		, readCache = newMap
		, writeCache = newMap
		, abcInterpreterEnv =
			{ pie_code_start     = -1
			, pie_symbols        = {}
			, pie_sorted_symbols = {}
			, pie_host_symbols   = {}
			}
		, ioTasks = {done=[], todo=[]}
		, ioStates = newMap
		, signalHandlers = []
		, world = world
		, resources = []
		, onClient = False
		, shutdown = Nothing
		}
	
fromStubIWorld :: !*IWorld -> *World
fromStubIWorld iworld=:{IWorld|world} = world

toStubVSt :: *VSt
toStubVSt =
	{ VSt
	| taskId            = "STUB"
	, optional          = False
	, selectedConsIndex = -1
	, pathInEditMode    = []
	, abcInterpreterEnv =
			{ pie_code_start     = -1
			, pie_symbols        = {}
			, pie_sorted_symbols = {}
			, pie_host_symbols   = {}
			}
	}
