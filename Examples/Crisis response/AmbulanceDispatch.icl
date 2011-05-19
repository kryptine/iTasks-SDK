implementation module AmbulanceDispatch

import iTasks

ambulanceDispatchExamples :: [Workflow]
ambulanceDispatchExamples = flows
where
	flows =	[ workflow "Examples/Crisis response/Report incident" "Report an incident" reportIncident
			, workflow "Examples/Crisis response/Dispatch ambulances" "Dispatch ambulances to an incident" dispatchAmbulances
			]
			
//Crisis management data domain

:: Incident =
	{ type        :: IncidentType
	, time        :: Time
	, nrInjured   :: Int
	, description :: String
	, location    :: Address
	}

:: IncidentType = Accident | Fire | Fight | Other String
	
:: Location =
	{ street		:: String
	, place			:: String
	, coordinates	:: Maybe MapCoordinates
	}	

:: Address =
	{ street		:: String
	, place			:: String
	}

:: MapCoordinates =
	{ lat			:: Real
	, lon			:: Real
	}
	
::Provider =
	{ name			:: String
	, id			:: User
	, location		:: Location
	, capacity		:: Int
	}

::Opinion = Opinion User Note

//Static population

allproviders  = [{name="Ambulance Post 0",id=NamedUser "ambupost0",location={street="Teststreet",place="Testville",coordinates=Just{lat=1.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 1",id=NamedUser "ambupost1",location={street="Teststreet",place="Testville",coordinates=Just{lat=2.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 2",id=NamedUser "ambupost2",location={street="Teststreet",place="Testville",coordinates=Just{lat=3.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 3",id=NamedUser "ambupost3",location={street="Teststreet",place="Testville",coordinates=Just{lat=4.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 4",id=NamedUser "ambupost4",location={street="Teststreet",place="Testville",coordinates=Just{lat=5.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 5",id=NamedUser "ambupost5",location={street="Teststreet",place="Testville",coordinates=Just{lat=6.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 6",id=NamedUser "ambupost6",location={street="Teststreet",place="Testville",coordinates=Just{lat=7.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 7",id=NamedUser "ambupost7",location={street="Teststreet",place="Testville",coordinates=Just{lat=8.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 8",id=NamedUser "ambupost8",location={street="Teststreet",place="Testville",coordinates=Just{lat=9.0,lon=2.0}},capacity=2}
				,{name="Ambulance Post 9",id=NamedUser "ambupost9",location={street="Teststreet",place="Testville",coordinates=Just{lat=9.0,lon=3.0}},capacity=2}
				]

derive class iTask	Incident, IncidentType, Location, Address, MapCoordinates, Provider, Opinion
derive bimap (,), Maybe

reportIncident :: Task [Void]
reportIncident
  = enterIncident >>= chooseResponse >>= allTasks
where
  enterIncident :: Task Incident
  enterIncident = enterInformation ("Incident report","Describe the incident")

  chooseResponse :: Incident -> Task [Task Void]
  chooseResponse incident
    = updateMultipleChoice ("Response","Choose response") options (suggestion incident.Incident.type)

  where
    //Generate the list of possible tasks to choose from
    options = [f incident \\ f <- [sendPolice,sendMedics,sendFireBrigade]]

    //Compute the indexes in the options list that are initially selected
    suggestion Accident = [0,1]
    suggestion Fire     = [0,2]
    suggestion Fight    = [0]
    suggestion _        = []

sendPolice :: Incident -> Task Void
sendPolice incident = showMessage ("Send police","Please send police") Void

sendMedics :: Incident -> Task Void
sendMedics incident = Title "Send ambulances" @>> requestAmbulances incident.Incident.nrInjured incident.Incident.location

sendFireBrigade :: Incident -> Task Void
sendFireBrigade incident = showMessage ("Send fire brigade","Please send fire brigade") Void

dispatchAmbulances :: Task Void
dispatchAmbulances
	=					enterInformation ("Dispatch ambulances","How many ambulances do you need at what location?")
	>>= \(nr,loc) ->	requestAmbulances nr loc
 
// Request for amount ambulances from list of candidate providers
// First, from the list enough providers are selected that can in principle provide the needed amount
// They are asked in parallel
// But in case they do not provide enough, more providers are asked
// This is repeated until the requested amount can be fulfilled
// Nore: we assume there are enough providers to supply all ambulances
requestAmbulances :: Int Address -> Task Void
requestAmbulances amount address	
	//| isJust location.coordinates	= requestAmbulances` amount (sortProviders location allproviders) >>= showAmbulances
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
displayRequest providers = showMessageA ("Request",flatten [[Text (p.Provider.name +++ " is asked for " <+ p.capacity),BrTag []]\\p <- providers]) noActionsMsg >>| stop

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

import StdMisc
resourceRequestTimeOut :: [(b,User,a)] Time ([(b,Maybe a)] -> Bool) ([(b,Maybe a)] -> (a,[(b,Maybe a)])) ([(b,Maybe a)] -> (a,[(b,Maybe a)])) (a -> Task a) -> 
                             Task (a,[(b,Maybe a)]) | iTask a & iTask b
resourceRequestTimeOut resources time_out check predf allf task
	= abort "TODO: remove use of oldParallel" //TODO
	//= oldParallel ("Resource requests","Waiting for resources...") [] finalfun (map (\(idx,t) -> DetachedTask initManagerProperties noMenu t (procfun idx)) (zip (indexList tasks,tasks)))
where		
	tasks	=	[delegateTaskTimeOut uid "Resource Request" amount task time_out >>= \mba -> return (resource, mba)
				\\ (resource,uid,amount) <- resources]

  	procfun index (Just result) st
  		# st = sortBy (\x y. snd x < snd y) [(result,index):st]
  		| check (map fst st)	= (st,[StopParallel])
  								= (st,[])
  
  	finalfun _ st
  		# results = map fst st
        | length st < length tasks
        	= predf results
        | otherwise
        	= allf results
                   
delegateTaskTimeOut :: User String a (a -> Task a) Time -> Task (Maybe a) | iTask a
delegateTaskTimeOut who description value task time_out 
	= timeOutTask (who @: (Title description @>> task value)) time_out 
   			  
timeOutTask :: (Task a) Time -> Task (Maybe a) | iTask a
timeOutTask task time
	= (task >>= \a -> return (Just a)) -||- (waitForTimer time >>| return Nothing)

ambulanceTask :: Int -> Task Int
ambulanceTask amount
	= updateInformation ("Amount","I need " <+ amount <+ " ambulances, how much can you provide?") amount

showAmbulances :: [(Provider, Maybe Int)] -> Task Void
showAmbulances providers = showMessage ("Summary","Ambulances are on their way") Void

//Utilities
(<+) infixl :: !String !a -> String | toString a
(<+) str x = str +++ toString x
	

oneMinute	:== {Time | hour = 0, min = 1, sec = 0}
end			:== return Void