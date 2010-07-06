implementation module SharedVariables

import iTasks, CommonDomain, GeoDomain, Text, ExperimentalDomain

derive bimap Maybe, (,)

quitButton = ButtonAction (ActionQuit, Always)

//Text-Lines Examples
noteEditor = editor {editorFrom = \txt -> Note txt,			editorTo = \(Note txt) _ -> txt}
listEditor = editor {editorFrom = \txt -> split "\n" txt,	editorTo = \l _ -> join "\n" l}

TrimAction :== ActionLabel "Trim"

linesPar :: Task Void
linesPar =
				createDB ""
	>>= \sid.	noteE sid -||- ignoreResult (updateShared "Lines" [quitButton] sid [listEditor])
	>>|			deleteDB sid
where
	noteE sid = 
							updateShared "Text" [ButtonAction (TrimAction, Always), quitButton] sid [noteEditor]
		>>= \(action,txt).	case action of
								TrimAction	=			writeDB sid (trim txt)
												>>|		noteE sid
								_			= 			stop

linesSingle :: Task Void
linesSingle = ignoreResult (updateSharedLocal "Text & Lines" [quitButton] "" [noteEditor,listEditor])

//Calculate Sum Example
calculateSum :: Task Void
calculateSum = ignoreResult (updateSharedLocal "Sum" [quitButton] (0,0) [idEditor, listener {listenerFrom = \(x,y) -> x + y}])

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
derive gError Tree, Node
derive gHint Tree, Node

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

//Merge Tests
mergeTestList :: Task Void
mergeTestList =
				getCurrentUser
	>>= \user.  createDB emptyL
	>>= \sid.	spawnProcess user True True (Subject "1st View" @>> view sid)
	>>|			spawnProcess user True True (Subject "2nd View" @>> view sid)
	>>|			stop
where
	view :: (DBid [String]) -> Task (Action,[String])
	view sid = updateShared "List" [quitButton] sid [idEditor]
	
	emptyL :: [String]
	emptyL = []
	
mergeTestDocuments :: Task Void
mergeTestDocuments =
				getCurrentUser
	>>= \user.	createDB emptyL
	>>= \sid.	spawnProcess user True True (Subject "1st View" @>> view sid idEditor)
	>>|			spawnProcess user True True (Subject "2nd View" @>> view sid idEditor)
	>>|			spawnProcess user True True (Subject "3rd View" @>> view sid idListener)
	>>|			stop
where
	view :: (DBid [Document]) (View [Document]) -> Task (Action,[Document])
	view sid v = updateShared "List" [quitButton] sid [v]
	
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
derive gPrint		MarkerInfo, MapSize, MapOptions
derive gParse		MarkerInfo, MapSize, MapOptions
derive gVisualize	MarkerInfo, MapSize, MapOptions
derive gUpdate		MarkerInfo, MapSize, MapOptions
derive gError		MarkerInfo, MapSize, MapOptions
derive gHint		MarkerInfo, MapSize, MapOptions

RemoveMarkersAction :== ActionLabel "Remove Markers"

googleMaps :: Task Void
googleMaps = googleMaps` mkMap
where
	googleMaps` map =
							updateSharedLocal "Google Map, Overview & Markers" [ButtonAction (RemoveMarkersAction, Always), quitButton] map [optionsEditor, idEditor, overviewEditor, markersListener]
		>>= \(action,map).	case action of
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
autoSortedList :: Task Void
autoSortedList = ignoreResult (updateSharedLocal "Automatically Sorted List" [quitButton] emptyL [editor {editorFrom = \list -> sort list, editorTo = \list _ -> list}])
where
	emptyL :: [String]
	emptyL = []

//Different Views on Formatted Text
formattedText :: Task Void
formattedText =
				setMenus [Menu "Example" [MenuItem "Quit" ActionQuit]]
	>>|			createDB (mkEmptyFormattedText {allControls & sourceEditControl = False})
	>>= \sid.	dynamicGroupAOnly [(ignoreResult (t <<@ ExcludeGroupActions) <<@ GBFloating) \\ t <- tasks sid] actions
	>>|			deleteDB sid
where
	tasks sid =
		[ updateShared "WYSIWYG Editor"			[] sid [idEditor]
		, updateShared "HTML-Source Editor"		[] sid [editor		{ editorFrom	= \ft -> Note (getFormattedTextSrc ft)
																	, editorTo		= \(Note src) ft -> setFormattedTextSrc src ft
																	}]
		, updateShared "Formatted Preview"		[] sid [idListener]
		, updateShared "Unformatted Preview"	[] sid [listener	{listenerFrom	= \ft -> Note (toUnformattedString ft False)}]
		]
		
	actions :: [GroupAction GOnlyAction Void Void]
	actions = [GroupAction ActionQuit GOStop GroupAlways]
								
sharedValueExamples :: [Workflow]
sharedValueExamples =	[ workflow "Examples/Shared Variables/Text-Lines (grouped tasks)"	(Subject "Text-Lines (grouped tasks)"	@>> linesPar)
						, workflow "Examples/Shared Variables/Text-Lines (single editor)"	(Subject "Text-Lines (single editor)"	@>> linesSingle)
						, workflow "Examples/Shared Variables/Calculate Sum"				(Subject "Calculate Sum"				@>> calculateSum)
						, workflow "Examples/Shared Variables/Balanced Binary Tree"			(Subject "Balanced Binary Tree"			@>> tree)
						, workflow "Examples/Shared Variables/Merge Test (List)"			(Subject "Merge Test (List)"			@>> mergeTestList)
						, workflow "Examples/Shared Variables/Merge Test (Documents)"		(Subject "Merge Test (Documents)"		@>> mergeTestDocuments)
						, workflow "Examples/Shared Variables/Google Maps Example"			(Subject "Google Maps Example"			@>> googleMaps)
						, workflow "Examples/Shared Variables/Sorted List"					(Subject "Sorted List"					@>> autoSortedList)
						, workflow "Examples/Shared Variables/Formatted Text"				(Subject "Formatted Text"				@>> formattedText)
						]