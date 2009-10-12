implementation module AmbulanceDispatch

import iTasks
import GenEq
import CommonDomain

ambulanceDispatchExamples :: [Workflow]
ambulanceDispatchExamples = flows
where
	flows =	[ workflow "Examples/Crisis response/Report incident" reportIncident
			, workflow "Examples/Crisis response/Dispatch ambulances" dispatchAmbulances
			]
			
//Crisis management data domain

:: Incident =
	{ location    :: Location
	, type        :: IncidentType
	, time        :: Time
	, nrInjured   :: Int
	, description :: String
	}

:: IncidentType = Accident | Fire | Fight | Other String
	
:: Location =
	{ street		:: String
	, place			:: String
	, coordinates	:: Maybe MapCoordinates
	}	
	
:: MapCoordinates =
	{ lat			:: Real
	, lon			:: Real
	}
	
::Provider =
	{ name			:: String
	, id			:: UserId
	, location		:: Location
	, capacity		:: Int
	}

::Opinion = Opinion (UserId,String) Note

//Static population

allproviders  = [{name="Ambulance Post 0",id=30,location={street="Teststreet",place="Testville",coordinates=Just{lat=1.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 1",id=31,location={street="Teststreet",place="Testville",coordinates=Just{lat=2.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 2",id=32,location={street="Teststreet",place="Testville",coordinates=Just{lat=3.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 3",id=33,location={street="Teststreet",place="Testville",coordinates=Just{lat=4.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 4",id=34,location={street="Teststreet",place="Testville",coordinates=Just{lat=5.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 5",id=35,location={street="Teststreet",place="Testville",coordinates=Just{lat=6.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 6",id=36,location={street="Teststreet",place="Testville",coordinates=Just{lat=7.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 7",id=37,location={street="Teststreet",place="Testville",coordinates=Just{lat=8.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 8",id=38,location={street="Teststreet",place="Testville",coordinates=Just{lat=9.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 9",id=39,location={street="Teststreet",place="Testville",coordinates=Just{lat=9.0,lon=3.0}},capacity=2}
				]

derive gPrint		Incident, IncidentType, Location, MapCoordinates, Provider, Opinion			
derive gParse		Incident, IncidentType, Location, MapCoordinates, Provider, Opinion
derive gVisualize 	Incident, IncidentType, Location, MapCoordinates, Provider, Opinion
derive gUpdate		Incident, IncidentType, Location, MapCoordinates, Provider, Opinion



derive gEq IncidentType



// Crisis management procedure examples		
reportIncident :: Task [Void]
reportIncident
	=				enterInformation "Please provide as many details about the incident as possible"
	>>= \inc ->		enterMultipleChoice "Which actions must be taken?"
						(map fst (filter snd 
							[(requestAmbulances inc.Incident.nrInjured inc.Incident.location <<@ "Send ambulances", inc.Incident.nrInjured > 0)
							,(requestFireBrigade <<@ "Request fire brigade", inc.Incident.type === Fire)
							]))
	>>= \tasks ->	allTasks tasks

dispatchAmbulances :: Task Void
dispatchAmbulances
	=					enterInformation "How many ambulances do you need at what location?"
	>>= \(nr,loc) ->	requestAmbulances nr loc

requestFireBrigade :: Task Void
requestFireBrigade = return Void

// Request for amount ambulances from list of candidate providers
// First, from the list enough providers are selected that can in principle provide the needed amount
// They are asked in parallel
// But in case they do not provide enough, more providers are asked
// This is repeated until the requested amount can be fulfilled
// Nore: we assume there are enough providers to supply all ambulances
requestAmbulances :: Int Location -> Task Void
requestAmbulances amount location	
	| isJust location.coordinates	= requestAmbulances` amount (sortProviders location allproviders) >>= showAmbulances
	| otherwise						= requestAmbulances` amount allproviders >>= showAmbulances

requestAmbulances` amount providers
	# (pvs,remainder) = 	determineNeededAmounts amount providers [] 
	= 						displayRequest pvs
							||- ambulanceRequest amount pvs oneMinute ambulanceTask   		
		>>= \(left,list) ->	if (left == 0) (return list) 						// enough ambulances
		                                   (requestAmbulances` left remainder)	// ask for more

// Sort ambulance posts on distance from place of accident
sortProviders :: Location [Provider] -> [Provider]
sortProviders location providers
		= sortBy (\p1 p2 -> distProv location p1.Provider.location < distProv location p2.Provider.location) providers 
where
	distProv l1 l2 = sqrt ((latDist l1 l2)^2.0 + (lonDist l1 l2)^2.0)
	latDist l1 l2 = (fromJust l1.Location.coordinates).lat - (fromJust l2.Location.coordinates).lat
	lonDist l1 l2 = (fromJust l1.Location.coordinates).lon - (fromJust l2.Location.coordinates).lon

displayRequest :: [Provider] -> Task Void
displayRequest providers = showStickyMessage (flatten [[Text (p.Provider.name +++ " is asked for " <+ p.capacity),BrTag []]\\p <- providers])

// Calculates for a needed amount (left,providers,remainder)
// left: is the amount that could not fulfilled (0 in case all can be supplied)
// providers: list of providers and the amount that they should deliver
// remainder: providers that remain (with there amounts)
determineNeededAmounts :: Int [Provider] [Provider] -> ([Provider],[Provider])
determineNeededAmounts tot [] used = (used,[])
determineNeededAmounts tot [sup=:{name,id,location,capacity}:providers] used
	| capacity <  tot = determineNeededAmounts (tot - capacity) providers  [sup : used]
	| capacity >= tot = ([{sup & capacity = tot}:used],providers)

			
// Make a request for amount ambulances to providers
// return (left,[(provider,actualamount)])
// left: the amount that could not be provided
// (provider,actualAmount): the actual amount that provider provides
ambulanceRequest :: Int [Provider] Time (Int -> Task Int) -> Task (Int,[(Provider,Maybe Int)]) 
ambulanceRequest needed providers time_out task 
	= resourceRequestTimeOut 
				    [(prov, prov.Provider.id, prov.capacity) \\ prov <- providers] 
                    time_out 
                    (enough needed) 
                    preCombine
                    allCombine
                    task             
where
	numAmbulances (_,Nothing)	= 0
	numAmbulances (_,Just x)	= x
	
	enough needed as			= sum (map numAmbulances as) >= needed
	
	preCombine as				= (0,as)
	allCombine as				= (needed - sum (map numAmbulances as),as)
	
resourceRequestTimeOut :: [(b,UserId,a)] Time ([(b,Maybe a)] -> Bool) ([(b,Maybe a)] -> (a,[(b,Maybe a)])) ([(b,Maybe a)] -> (a,[(b,Maybe a)])) (a -> Task a) -> 
                             Task (a,[(b,Maybe a)]) | iTask a & iTask b
resourceRequestTimeOut resources time_out check predf allf task
	= parallel "Resource_requests" check predf allf 
             [(delegateTaskTimeOut uid "Resource Request" amount task time_out >>= \mba -> return (resource, mba)) 
             \\ (resource,uid,amount) <- resources]

delegateTaskTimeOut :: UserId String a (a -> Task a) Time -> Task (Maybe a) | iTask a
delegateTaskTimeOut who description value task time_out 
	= timeOutTask (who @: (description, task value)) time_out 
   			  
timeOutTask :: (Task a) Time -> Task (Maybe a) | iTask a
timeOutTask task time
	= (task >>= \a -> return (Just a)) -||- (waitForTimer time >>| return Nothing)

ambulanceTask :: Int -> Task Int
ambulanceTask amount
	= updateInformation ("I need " <+ amount <+ " ambulances, how much can you provide?") amount

showAmbulances :: [(Provider, Maybe Int)] -> Task Void
showAmbulances providers = showMessage "Ambulances are on their way"
/*
	= showMessage (tableView [[p.Provider.name +++ ": ", case i of Nothing = "Timed out" ; (Just n) = ("Can send " <+ n <+ " ambulances")] \\ (p,i) <- providers])
where
	tableView rows = [TableTag [] [TrTag [] [TdTag [] [Text cell] \\cell <- row] \\row <- rows]]	
*/

//Utilities
(<+) infixl :: !String !a -> String | toString a
(<+) str x = str +++ toString x
	

oneMinute	:== {Time | hour = 0, min = 1, sec = 0}
end			:== return Void