implementation module SharedValues

import iTasks, CommonDomain, GeoDomain, Text

derive bimap Maybe, (,)

quitButton = ButtonAction (ActionQuit, Always)

//Text-Lines Examples
noteEditor = editor {editorFrom = (\txt -> Note txt),		editorTo = (\(Note txt) _ -> txt)}
listEditor = editor {editorFrom = (\txt -> split "\n" txt),	editorTo = (\l _ -> join "\n" l)}

TrimAction :== ActionLabel "Trim"

linesPar :: Task Void
linesPar =
				createShared ""
	>>= \sid.	ignoreResult (noteE sid -||- ignoreResult (updateShared "Lines" [quitButton] sid [listEditor]))
where
	noteE sid = 
							updateShared "Text" [ButtonAction (TrimAction, Always), quitButton] sid [noteEditor]
		>>= \(action,txt).	case action of
								TrimAction	=			setShared sid (trim txt)
												>>|		noteE sid
								_			= 			stop

linesSingle :: Task Void
linesSingle = ignoreResult (updateSharedLocal "Text & Lines" [quitButton] "" [noteEditor,listEditor])

//Calculate Sum Example
calculateSum :: Task Void
calculateSum = ignoreResult (updateSharedLocal "Sum" [quitButton] (0,0) [idEditor, listener {listenerFrom = (\(x,y) -> x + y)}])

//Tree Example
:: Tree a = Leaf | Node (Node a)
:: Node a = { rightChildren	:: Tree a
			, value			:: a
			, leftChildren	:: Tree a
			}
derive gPrint Tree, Node
derive gParse Tree, Node
derive gVisualize Tree, Node
derive gUpdate Tree, Node

toTree :: [a] -> (Tree a) | Ord a
toTree list = makeTree (sort list)
where
	makeTree :: [a] -> (Tree a)
	makeTree []			= Leaf
	makeTree [el:[]]	= Node {rightChildren = Leaf, value = el, leftChildren = Leaf}
	makeTree list		= Node {rightChildren = makeTree end, value = middle, leftChildren = makeTree begin}
	where
		middlePos	= (length list) / 2
		begin		= take (middlePos) list
		middle		= list !! (middlePos) 
		end			= drop (middlePos + 1) list

tree :: Task Void
tree = ignoreResult (updateSharedLocal "List & Balanced Binary Tree" [quitButton] emptyL [idEditor, listener {listenerFrom = toTree}])
where
	emptyL :: [Int]
	emptyL = []

//Merge Test
mergeTest :: Task Void
mergeTest =
				getCurrentUser
	>>= \user.	createShared emptyL			
	>>= \sid.	ignoreResult ((user @: ("1st View", view sid)) -||- (user @: ("2nd View", view sid)))
where
	view sid = updateShared "List" [quitButton] sid [idEditor]
	
	emptyL :: [String]
	emptyL = []

//Google Map Example
:: MarkerInfo =	{ position	:: Coordinate
				, map		:: GoogleMap
				}
:: MapSize = Normal | Large
:: MapOptions =	{ type					:: GoogleMapType
				, showMapTypeControl	:: Bool
				, showNavigationControl	:: Bool
				, showScaleControl		:: Bool
				, size					:: MapSize
				}
derive gPrint		MarkerInfo, MapSize, MapOptions
derive gParse		MarkerInfo, MapSize, MapOptions
derive gVisualize	MarkerInfo, MapSize, MapOptions
derive gUpdate		MarkerInfo, MapSize, MapOptions

RemoveMarkersAction :== ActionLabel "Remove Markers"

googleMaps :: Task Void
googleMaps = googleMaps` mkMap
where
	googleMaps` map =
							updateSharedLocal "Google Map, Overview & Markers" [ButtonAction (RemoveMarkersAction, Always), quitButton] map [optionsEditor, idEditor, overviewEditor, markersListener]
		>>= \(action,map).	case action of
								RemoveMarkersAction	= googleMaps` {GoogleMap| map & markers = []}
								_					= stop

	optionsEditor	= editor	{ editorFrom	= (\map			-> {type = map.mapType, showMapTypeControl = map.mapTypeControl, showNavigationControl = map.navigationControl, showScaleControl = map.scaleControl, size = if (map.GoogleMap.width == 400) Normal Large})
								, editorTo		= (\opts map	-> {map & mapType = opts.MapOptions.type, mapTypeControl = opts.showMapTypeControl, navigationControl = opts.showNavigationControl, scaleControl = opts.showScaleControl, width = case opts.MapOptions.size of Large = 800; Normal = 400})
								}
	overviewEditor	= editor	{ editorFrom	= (\map			-> {GoogleMap| map & mapTypeControl = False, navigationControl = False, scaleControl = False, scrollwheel = False, zoom = 7})
								, editorTo		= (\nmap map	-> {GoogleMap| map & center = nmap.GoogleMap.center})
								}
	markersListener	= listener	{ listenerFrom	= (\map			-> [{position = position, map = {GoogleMap| mkMap & center = position, width = 150, height = 150, zoom = 15, markers = [marker]}} \\ marker=:{GoogleMapMarker| position} <-map.markers])
								}
								
sharedValueExamples :: [Workflow]
sharedValueExamples =	[ workflow "Examples/Shared Values/Text-Lines (parallel tasks)" linesPar
						, workflow "Examples/Shared Values/Text-Lines (single editor)" linesSingle
						, workflow "Examples/Shared Values/Calculate Sum" calculateSum
						, workflow "Examples/Shared Values/Balanced Binary Tree" tree
						, workflow "Examples/Shared Values/Merge Test" mergeTest
						, workflow "Examples/Shared Values/Google Maps Example" googleMaps
						]