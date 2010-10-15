implementation module SharedVariables

import iTasks, GoogleMaps, Text, ExperimentalDomain

derive bimap Maybe, (,)

quitButton = (ActionQuit, always)

//Text-Lines Examples
noteEditor = editor {editorFrom = \txt -> Note txt,	editorTo = \(Note txt) _ -> txt}
listEditor = editor {editorFrom = split "\n" ,		editorTo = \l _ -> join "\n" l}

TrimAction :== Action "trim" "Trim"

linesPar :: Task Void
linesPar =
				createDB ""
	>>= \sid.	noteE sid -|| updateShared "Lines" "Edit lines" [quitButton] sid [listEditor]
	>>|			deleteDB sid
	>>|			return Void
where
	noteE sid = 
							updateShared "Text" "Edit text" [(TrimAction, always), quitButton] sid [noteEditor]
		>>= \(action,txt).	case fst action of
								TrimAction	=			writeDB sid (trim txt)
												>>|		noteE sid
								_			= 			stop

linesSingle = updateSharedLocal "Text & Lines" "Edit text and lines" [quitButton] "" [noteEditor,listEditor]

//Calculate Sum Example)
calculateSum = updateSharedLocal "Sum" "Auto compute sum" [quitButton] (0,0) [idEditor, listener {listenerFrom = \(x,y) -> x + y}]

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

tree = updateSharedLocal "List & Balanced Binary Tree" "Type something in the list and the tree will update as well."
			[quitButton] emptyL [idEditor, listener {listenerFrom = toTree}]
where
	emptyL :: [Int]
	emptyL = []

//Merge Tests
mergeTestList :: Task Void
mergeTestList =	
				createDB emptyL
	>>= \sid.	spawnProcess True True (Subject "1st View" @>> view sid)
	>>|			spawnProcess True True (Subject "2nd View" @>> view sid)
	>>|			stop
where
	view :: (DBId [String]) -> Task (ActionEvent,[String])
	view sid = updateShared "List" "Merging the lists" [quitButton] sid [idEditor]
	
	emptyL :: [String]
	emptyL = []
	
mergeTestDocuments :: Task Void
mergeTestDocuments =
				createDB emptyL
	>>= \sid.	spawnProcess True True (Subject "1st View" @>> view sid idEditor)
	>>|			spawnProcess True True (Subject "2nd View" @>> view sid idEditor)
	>>|			spawnProcess True True (Subject "3rd View" @>> view sid idListener)
	>>|			stop
where
	view :: (DBId [Document]) (View [Document]) -> Task (ActionEvent,[Document])
	view sid v = updateShared "List" "Merging the documents" [quitButton] sid [v]
	
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

googleMaps :: Task Void
googleMaps = googleMaps` mkMap
where
	googleMaps` map =
							updateSharedLocal "Google Map, Overview & Markers" "Edit in one map. The others are updated automatically."
								[(RemoveMarkersAction, always), quitButton] map [optionsEditor, idEditor, overviewEditor, markersListener]
		>>= \(action,map).	case fst action of
								RemoveMarkersAction	= googleMaps` {GoogleMap| map & markers = []}
								_					= stop

	optionsEditor	= editor	{ editorFrom	= \map			-> {type = map.mapType, showMapTypeControl = map.mapTypeControl, showNavigationControl = map.navigationControl, showScaleControl = map.scaleControl, size = if (map.GoogleMap.width == 400) Normal Large}
								, editorTo		= \opts map	-> {map & mapType = opts.MapOptions.type, mapTypeControl = opts.showMapTypeControl, navigationControl = opts.showNavigationControl, scaleControl = opts.showScaleControl, width = case opts.MapOptions.size of Large = 650; Normal = 400}
								}
	overviewEditor	= editor	{ editorFrom	= \map			-> {GoogleMap| map & mapTypeControl = False, navigationControl = False, scaleControl = False, scrollwheel = False, zoom = 7}
								, editorTo		= \nmap map	-> {GoogleMap| map & center = nmap.GoogleMap.center}
								}
	markersListener	= listener	{ listenerFrom	= \map			-> [{position = position, map = {GoogleMap| mkMap & center = position, width = 150, height = 150, zoom = 15, markers = [marker]}} \\ marker=:{GoogleMapMarker| position} <-map.markers]
								}

//Auto sorted list
autoSortedList = updateSharedLocal "Automatically Sorted List" "You can edit the list, it will sort automatically."
					[quitButton] emptyL [editor {editorFrom = sort, editorTo = \list _ -> list}]
where
	emptyL :: [String]
	emptyL = []

//Different Views on Formatted Text
formattedText :: Task Void
formattedText =
				[Menu "Example" [MenuItem "Quit" ActionQuit Nothing]]
	@>>			createDB (mkEmptyFormattedText {allControls & sourceEditControl = False})
	>>= \sid.	dynamicGroupAOnly [(t <<@ ExcludeGroupActions) <<@ GBFloating >>| return Void \\ t <- tasks sid] actions
	>>|			deleteDB sid
	>>|			return Void
where
	tasks sid =
		[ updateShared "WYSIWYG Editor"	""		[] sid [idEditor]
		, updateShared "HTML-Source Editor" ""		[] sid [editor		{ editorFrom	= \ft -> Note (getFormattedTextSrc ft)
																	, editorTo		= \(Note src) ft -> setFormattedTextSrc src ft
																	}]
		, updateShared "Formatted Preview" ""		[] sid [idListener]
		, updateShared "Unformatted Preview" ""	[] sid [listener	{listenerFrom	= \ft -> Note (toUnformattedString ft False)}]
		]
		
	actions :: [GroupAction GOnlyAction Void Void]
	actions = [GroupAction ActionQuit GOStop GroupAlways]
								
sharedValueExamples :: [Workflow]
sharedValueExamples =	[ workflow "Examples/Shared Variables/Text-Lines (grouped tasks)"	"" (Subject "Text-Lines (grouped tasks)"	@>> linesPar)
						, workflow "Examples/Shared Variables/Text-Lines (single editor)"	"" (Subject "Text-Lines (single editor)"	@>> linesSingle)
						, workflow "Examples/Shared Variables/Calculate Sum"				"" (Subject "Calculate Sum"				@>> calculateSum)
						, workflow "Examples/Shared Variables/Balanced Binary Tree"			"" (Subject "Balanced Binary Tree"			@>> tree)
						, workflow "Examples/Shared Variables/Merge Test (List)"			"" (Subject "Merge Test (List)"			@>> mergeTestList)
						, workflow "Examples/Shared Variables/Merge Test (Documents)"		"" (Subject "Merge Test (Documents)"		@>> mergeTestDocuments)
						, workflow "Examples/Shared Variables/Google Maps Example"			"" (Subject "Google Maps Example"			@>> googleMaps)
						, workflow "Examples/Shared Variables/Sorted List"					"" (Subject "Sorted List"					@>> autoSortedList)
						, workflow "Examples/Shared Variables/Formatted Text"				"" (Subject "Formatted Text"				@>> formattedText)
						]