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
	= case http_getValue "process" req.arg_get "" of
		""		= (errorResponse "Invalid process id", tst)
		proc	= case http_getValue "property" req.arg_get "" of		
			"priority"	= updatePriority proc (http_getValue "value" req.arg_get "") tst
			"user"		= updateUser proc (http_getValue "value" req.arg_get "") tst
			"progress"	= updateProgress proc (http_getValue "value" req.arg_get "") tst
			_			= (errorResponse "Invalid property", tst)
where
	updatePriority proc prio tst
		= case parsePrio prio of
			(Just prio)
				# (_,tst) = updateProcessProperties proc (\p -> {p & managerProps = {p.managerProps & priority = prio}}) tst
				= (successResponse, tst)
			Nothing
				= (errorResponse "Unknown priority", tst)

	parsePrio "LowPriority"		= Just LowPriority
	parsePrio "NormalPriority"	= Just NormalPriority
	parsePrio "HighPriority"	= Just HighPriority
	parsePrio _					= Nothing
	
	
	updateUser proc userName tst=:{staticInfo}
		# (user,tst)		= getUser userName tst
		# delegator			= staticInfo.currentSession.user
		# (_,tst)			= updateProcessProperties proc (\p -> {TaskProperties| p & systemProps = {p.systemProps & manager = (delegator.User.userName,delegator.User.displayName)},managerProps = {p.managerProps & worker = (user.User.userName,user.User.displayName)}, workerProps = {p.workerProps & progress = TPActive}}) tst
		= (successResponse,tst)
		 	
	updateProgress proc val tst
		= case parseProgress val of
			(Just val)
				# (_,tst) = updateProcessProperties proc (\p -> {p & workerProps = {p.workerProps & progress = val}}) tst
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
