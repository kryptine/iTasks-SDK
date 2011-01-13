implementation module SharedVariables

import iTasks, GoogleMaps, Text, ExperimentalDomain

derive bimap Maybe, (,)

quitButton = (ActionQuit, always)

//Text-Lines Examples
noteEditor = (\txt -> Note txt,	\(Note txt) _ -> txt)
listEditor = (split "\n" ,		\l _ -> join "\n" l)

TrimAction :== Action "trim" "Trim"

linesPar :: Task Void
linesPar =
				createDB ""
	>>= \sid.	noteE sid -|| updateSharedInformationA ("Lines","Edit lines") listEditor [quitButton] sid
	>>|			deleteDB sid
	>>|			return Void
where
	noteE sid = 
					updateSharedInformationA ("Text","Edit text") noteEditor [(TrimAction, always), quitButton] sid
		>>= \res.	case app2 (fst,id) res of
						(TrimAction,Just txt) =
								writeDB sid (trim txt)
							>>|	noteE sid
						_ =
							stop

//Calculate Sum Example
calculateSum = updateInformationA ("Sum","Auto compute sum") (\t=:(x,y) -> (t,Display (x+y)),\(t,_) _ -> t) [quitButton] (0,0)

//Tree Example
:: Tree a = Leaf | Node (Node a)
:: Node a = { rightChildren	:: Tree a
			, value			:: a
			, leftChildren	:: Tree a
			}

derive class iTask Tree, Node

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

tree = updateInformationA ("List & Balanced Binary Tree","Type something in the list and the tree will update as well.")
			(\l -> (l,Display (toTree l)), \(l,_) _ -> l) [quitButton] emptyL
where
	emptyL :: [Int]
	emptyL = []

//Merge Tests
mergeTestList :: Task Void
mergeTestList =	
				createDB []
	>>= \sid.	spawnProcess True True (Title "1st View" @>> view sid)
	>>|			spawnProcess True True (Title "2nd View" @>> view sid)
	>>|			stop
where
	view :: (DBId [String]) -> Task (ActionEvent,Maybe [String])
	view sid = updateSharedInformationA ("List","Merging the lists") idBimap [quitButton] sid
	
mergeTestDocuments :: Task Void
mergeTestDocuments =
				createDB emptyL
	>>= \sid.	spawnProcess True True (Title "1st View" @>> view sid)
	>>|			spawnProcess True True (Title "2nd View" @>> view sid)
	>>|			spawnProcess True True (Title "3rd View" @>> showMessageShared "Documents" id [quitButton] sid)
	>>|			stop
where
	view sid = updateSharedInformationA ("List","Merging the documents") idBimap [quitButton] sid
	
	emptyL :: [Document]
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

derive class iTask MarkerInfo, MapSize, MapOptions

RemoveMarkersAction :== Action "remove-markers" "Remove Markers"

googleMaps :: Task GoogleMap
googleMaps = 
				createDB mkMap
	>>= \dbid.	updateSharedInformationA "Options" optionsEditor [] dbid
				||-
				updateSharedInformationA "Google Map" idBimap [] dbid
				||-
				updateSharedInformationA "Overview Map" overviewEditor [] dbid
				||-
				markersDisplay dbid
	>>= \map.	deleteDB dbid
	>>|			return map
where							
	markersDisplay dbid =
							showMessageShared "Markers" markersListener [(RemoveMarkersAction,always),quitButton] dbid
		>>= \(action,map).	case fst action of
								RemoveMarkersAction	= modifyDB dbid (\map -> {GoogleMap| map & markers = []})
								_					= return map

	optionsEditor	=	( \map ->		{ type = map.mapType
										, showMapTypeControl = map.mapTypeControl
										, showNavigationControl = map.navigationControl
										, showScaleControl = map.scaleControl
										, size = if (map.GoogleMap.width == 400) Normal Large
										}
						, \opts map	->	{ map 
										& mapType = opts.MapOptions.type
										, mapTypeControl = opts.showMapTypeControl
										, navigationControl = opts.showNavigationControl
										, scaleControl = opts.showScaleControl
										, width = case opts.MapOptions.size of Large = 650; Normal = 400
										}
						)
	overviewEditor	= 	( \map ->		{ GoogleMap | map
										& mapTypeControl = False
										, navigationControl = False
										, scaleControl = False
										, scrollwheel = False
										, zoom = 7
										}
						, \nmap map	->	{ GoogleMap | map
										& center = nmap.GoogleMap.center
										}
						)
	markersListener	map = [{position = position, map = {GoogleMap| mkMap & center = position, width = 150, height = 150, zoom = 15, markers = [marker]}} \\ marker=:{GoogleMapMarker| position} <-map.markers]

//Auto sorted list
autoSortedList = updateInformationA ("Automatically Sorted List","You can edit the list, it will sort automatically.") (sort, const) [quitButton] emptyL
where
	emptyL :: [String]
	emptyL = []

//Different Views on Formatted Text
formattedText :: Task Void
formattedText =
				[Menu "Example" [MenuItem ActionQuit Nothing]]
	@>>			createDB (mkEmptyFormattedText {allControls & sourceEditControl = False})
	>>= \sid.	dynamicGroupAOnly [t <<@ ExcludeGroupActions <<@ Floating \\ t <- tasks sid] actions actionsGenFunc
	>>|			deleteDB sid
	>>|			return Void
where
	tasks sid =
		[ updateSharedInformationA "WYSIWYG Editor" idBimap [] sid >>| return Void
		, updateSharedInformationA "HTML-Source Editor" (\ft -> Note (getFormattedTextSrc ft), \(Note src) ft -> setFormattedTextSrc src ft) [] sid >>| return Void
		, showMessageShared "Formatted Preview" id [] sid >>| return Void
		, showMessageShared "Unformatted Preview" (\ft -> Note (toUnformattedString ft False)) [] sid >>| return Void
		]
		
	actions = [(ActionQuit, Always)]
	actionsGenFunc (ActionQuit,_) = GOStop
								
sharedValueExamples :: [Workflow]
sharedValueExamples =	[ workflow "Examples/Shared Variables/Text-Lines (grouped tasks)"	"" (Title "Text-Lines"				@>> linesPar)
						, workflow "Examples/Shared Variables/Calculate Sum"				"" (Title "Calculate Sum"			@>> calculateSum)
						, workflow "Examples/Shared Variables/Balanced Binary Tree"			"" (Title "Balanced Binary Tree"	@>> tree)
						, workflow "Examples/Shared Variables/Merge Test (List)"			"" (Title "Merge Test (List)"		@>> mergeTestList)
						, workflow "Examples/Shared Variables/Merge Test (Documents)"		"" (Title "Merge Test (Documents)"	@>> mergeTestDocuments)
						, workflow "Examples/Shared Variables/Google Maps Example"			"" (Title "Google Maps Example"		@>> googleMaps)
						, workflow "Examples/Shared Variables/Sorted List"					"" (Title "Sorted List"				@>> autoSortedList)
						, workflow "Examples/Shared Variables/Formatted Text"				"" (Title "Formatted Text"			@>> formattedText)
						]
