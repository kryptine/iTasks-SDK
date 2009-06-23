implementation module PropertyHandler

import TSt, Types
import StdInt, StdMaybe
import Http, JSON
import ProcessDB, UserDB

/**
* This handler is used to update properties of main tasks.
*/

:: PropertyResponse =
	{ success	:: Bool
	, error		:: Maybe String
	}

derive JSONEncode PropertyResponse

handlePropertyRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handlePropertyRequest req tst
	= case http_getValue "process" req.arg_get 0 of
		0		= (errorResponse "Invalid process id", tst)
		proc	= case http_getValue "property" req.arg_get "" of		
			"priority"	= updatePriority proc (http_getValue "value" req.arg_get "") tst
			"user"		= updateUser proc (http_getValue "value" req.arg_get -1) tst
			"progress"	= updateProgress proc (http_getValue "value" req.arg_get "") tst
			_			= (errorResponse "Invalid property", tst)
where
	updatePriority proc prio tst
		= case parsePrio prio of
			(Just prio)
				# (_,tst) = updateProcessProperties proc (\p -> {p & priority = prio}) tst
				= (successResponse, tst)
			Nothing
				= (errorResponse "Unknown priority", tst)

	parsePrio "LowPriority"		= Just LowPriority
	parsePrio "NormalPriority"	= Just NormalPriority
	parsePrio "HighPriority"	= Just HighPriority
	parsePrio _					= Nothing
	
	
	updateUser proc userId tst
		| userId < 0			
			= (errorResponse "Invalid user id", tst)	//Only positive user ids are possible
		| otherwise
			# (delegatorId,tst)	= getCurrentUser tst
			# (user,tst)		= getUser userId tst
			# (delegator,tst)	= getUser delegatorId tst
		 	# (_,tst)			= updateProcessProperties proc (\p -> {TaskProperties| p & user = user, delegator = delegator, progress = TPActive}) tst
		 	= (successResponse,tst)
		 	
	updateProgress proc val tst
		= case parseProgress val of
			(Just val)
				# (_,tst) = updateProcessProperties proc (\p -> {p & progress = val}) tst
				= (successResponse, tst)
			Nothing
				= (errorResponse "Unknown progress value", tst)

	parseProgress "TPActive"	= Just TPActive
	parseProgress "TPStuck"		= Just TPStuck
	parseProgress "TPWaiting"	= Just TPWaiting
	parseProgress "TPReject"	= Just TPReject
	parseProgress _				= Nothing
	
errorResponse :: String -> HTTPResponse
errorResponse msg = 
	{ rsp_headers = []
	, rsp_data	= toJSON {PropertyResponse|success = False, error = Just msg}
	}
successResponse :: HTTPResponse
successResponse =
	{ rsp_headers	= []
	, rsp_data		= toJSON {PropertyResponse|success = True, error = Nothing}
	}

instance fromString Int
where
	fromString x = toInt x
