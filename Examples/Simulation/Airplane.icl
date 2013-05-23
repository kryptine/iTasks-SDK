module Airplane
/**
* This module provides a simple simulation of an airplane flying a route over the Netherlands
*/

import iTasks, StdInt, StdOverloaded

:: LatLng :== (!Real,!Real)

INITIAL_ROUTE = [(52.187704,4.776612)
				,(52.709971,6.062013)
				,(53.462181,5.567628)
				,(53.324603,4.468995)
				,(51.543222,3.34839)
				,(51.556885,5.095216)
				]

SIMULATE_INTERVAL :== 1 //In seconds

simulateInteractive :: Task [LatLng]
simulateInteractive
	= withShared INITIAL_ROUTE
		\route -> simulateAirplanePosition route
	>&>
		\mbPos -> interactWithSimulation (route >+| (mapRead fromJust mbPos))

//Simulate the movement of the airplane
//First very simple simulation. Just round robin version of clock
simulateAirplanePosition :: (Shared [LatLng]) -> Task LatLng //Just stay in one position
simulateAirplanePosition route
	= withShared 0
		(\pos -> 
			//Return the n-th waypoint
			watch (mapRead (\(route,pos) -> route !! ( pos rem length route)) (route |+| pos))
			-||
			//Step the position
			forever (wait SIMULATE_INTERVAL >>- update inc pos) <<@ SetLayout (partLayout 0)
		)

//Interact with the running simulation
interactWithSimulation :: (ReadWriteShared ([LatLng],LatLng) [LatLng]) -> Task [LatLng]
interactWithSimulation sim
	= updateSharedInformation "Waypoints and position" [UpdateWith toPrj fromPrj] sim
where
	toPrj (route,pos) = {GoogleMap|defaultValue & perspective = perspective, markers = markers}
	where
		markers			= waypointMarkers ++ [planeMarker]
		planeMarker		= {GoogleMapMarker|defaultValue & title = Just "Airforce one", position = {GoogleMapPosition|lat=fst pos,lng=snd pos}, icon = Just ( GoogleMapSimpleIcon "jsf.png")}
		waypointMarkers = [{GoogleMapMarker|defaultValue & title = Just ("Waypoint " <+++ i), position = {GoogleMapPosition|lat=lat,lng=lng}} \\ (lat,lng) <- route & i <- [1..]]
		perspective		= {GoogleMapPerspective|defaultValue.perspective & center = {lat = 52.948300, lng = 4.776007}, zoom = 7}
		

	fromPrj (route,_) map = route

//UTIL
(>>-) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>-) taska taskb = step taska [WhenStable (const taskb)]

//Wait for (at least) n seconds
wait :: Int -> Task Void
wait n = get currentTime >>= \start -> watch currentTime >>* [OnValue (\(Value now _) -> if (now > addSeconds n start) (Just (return Void)) Nothing)]
where
	//ONLY CORRECT FOR n < 60
	addSeconds n t = t + {Time|hour=0,min=0,sec=n}

Start :: *World -> *World
Start world = startEngine simulateInteractive world
