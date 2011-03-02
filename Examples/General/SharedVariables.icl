implementation module SharedVariables

import iTasks, GoogleMaps, Text, ExperimentalDomain
from StdFunc import o

derive bimap Maybe, (,)

quitButton = (ActionQuit, always)

//Text-Lines Examples
noteEditor = (\txt -> Note txt,	\(Note txt) _ -> txt)
listEditor = (split "\n" ,		\l _ -> join "\n" l)

TrimAction :== Action "trim" "Trim"

linesPar :: Task Void
linesPar =
				createSharedStore ""
	>>= \sid.	noteE sid -|| updateSharedInformationA ("Lines","Edit lines") listEditor [quitButton] sid
	>>|			return Void
where
	noteE sid = 
					updateSharedInformationA ("Text","Edit text") noteEditor [(TrimAction, always), quitButton] sid
		>>= \res.	case appFst fst res of
						(TrimAction,Just txt) =
								writeShared sid (trim txt)
							>>|	noteE sid
						_ =
							stop

//Calculate Sum Example
calculateSum = updateInformationA ("Sum","Auto compute sum") (\t=:(x,y) -> (t,Display (x+y)),\(t,_) _ -> t) [quitButton] (0,0)

//Tree Example
:: Tree` a = Leaf` | Node` (Node` a)
:: Node` a = { rightChildren	:: Tree` a
			, value				:: a
			, leftChildren		:: Tree` a
			}

derive class iTask Tree`, Node`

toTree :: [a] -> (Tree` a) | Ord a
toTree list = makeTree (sort list)
where
	makeTree :: [a] -> (Tree` a)
	makeTree []			= Leaf`
	makeTree [el:[]]	= Node` {rightChildren = Leaf`, value = el, leftChildren = Leaf`}
	makeTree list		= Node` {rightChildren = makeTree end, value = middle, leftChildren = makeTree begin}
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
				return (sharedStoreDefault "mergeTestLists")
	>>= \sid.	spawnProcess True True (Title "1st View" @>> view sid)
	>>|			spawnProcess True True (Title "2nd View" @>> view sid)
	>>|			stop
where
	view :: (SymmetricShared [String]) -> Task (ActionEvent,Maybe [String])
	view sid = updateSharedInformationA ("List","Merging the lists") idView [quitButton] sid
	
mergeTestDocuments :: Task Void
mergeTestDocuments =
				return store
	>>= \sid.	spawnProcess True True (Title "1st View" @>> view sid)
	>>|			spawnProcess True True (Title "2nd View" @>> view sid)
	>>|			spawnProcess True True (Title "3rd View" @>> showMessageSharedA "Documents" id [quitButton] sid)
	>>|			stop
where
	view sid = updateSharedInformationA ("List","Merging the documents") idView [quitButton] sid
	
	store :: SymmetricShared [Document]
	store = sharedStoreDefault "mergeTestDocs"

//Google Map Example
:: MarkerInfo =	{ position	:: GoogleMapPosition
				, map		:: GoogleMap
				}
				
:: MapOptions =	{ type					:: GoogleMapType
				, showMapTypeControl	:: Bool
				, showPanControl		:: Bool
				, showStreetViewControl	:: Bool
				, showZoomControl		:: Bool
				, showScaleControl		:: Bool
				}

derive class iTask MarkerInfo, MapOptions

RemoveMarkersAction :== Action "remove-markers" "Remove Markers"

googleMaps :: Task GoogleMap
googleMaps = 
				createSharedStore mkMap
	>>= \dbid.	updateSharedInformationA "Options" optionsEditor [] dbid
				||-
				updateSharedInformationA "Google Map" idView [] dbid
				||-
				updateSharedInformationA "Overview Map" overviewEditor [] dbid
				||-
				markersDisplay dbid
where							
	markersDisplay dbid =
							showMessageSharedA "Markers" markersListener [(RemoveMarkersAction,always),quitButton] dbid
		>>= \(action,map).	case fst action of
								RemoveMarkersAction	= updateShared (\map -> {GoogleMap| map & markers = []}) dbid >>| markersDisplay dbid
								_					= return map

	optionsEditor	=	( \map ->		{ type = map.mapType
										, showMapTypeControl = map.mapTypeControl
										, showPanControl = map.panControl
										, showStreetViewControl = map.streetViewControl
										, showZoomControl = map.zoomControl
										, showScaleControl = map.scaleControl
										}
						, \opts map	->	{ map 
										& mapType = opts.MapOptions.type
										, mapTypeControl = opts.showMapTypeControl
										, panControl = opts.showPanControl
										, streetViewControl = opts.showStreetViewControl
										, zoomControl = opts.showZoomControl
										, scaleControl = opts.showScaleControl
										}
						)
	overviewEditor	= 	( \map ->		{ GoogleMap | map
										& mapTypeControl = False
										, panControl = False
										, streetViewControl = False
										, zoomControl = False
										, scaleControl = False
										, scrollwheel = False
										, zoom = 7
										, markers = [{GoogleMapMarker|m & draggable = False} \\ m <- map.markers]
										}
						, \nmap map	->	{ GoogleMap | map
										& center = nmap.GoogleMap.center
										}
						)
	markersListener	map = [{position = position, map = {GoogleMap| mkMap & center = position, zoom = 15, markers = [marker]}} \\ marker=:{GoogleMapMarker| position} <-map.markers]

//Auto sorted list
autoSortedList = updateInformationA ("Automatically Sorted List","You can edit the list, it will sort automatically.") (sort, const) [quitButton] emptyL
where
	emptyL :: [String]
	emptyL = []

//Different Views on Formatted Text
formattedText :: Task Void
formattedText =
				[Menu "Example" [MenuItem ActionQuit Nothing]]
	@>>			createSharedStore (mkEmptyFormattedText {allControls & sourceEditControl = False})
	>>= \sid.	dynamicGroupAOnly [t <<@ ExcludeGroupActions <<@ Floating \\ t <- tasks sid] actions actionsGenFunc
	>>|			stop
where
	tasks sid =
		[ updateSharedInformationA "WYSIWYG Editor" idView [] sid >>| return Void
		, updateSharedInformationA "HTML-Source Editor" (\ft -> Note (getFormattedTextSrc ft), \(Note src) ft -> setFormattedTextSrc src ft) [] sid >>| return Void
		, showMessageSharedA "Formatted Preview" id [] sid >>| return Void
		, showMessageSharedA "Unformatted Preview" (\ft -> Note (toUnformattedString ft False)) [] sid >>| return Void
		]
		
	actions = [(ActionQuit, Always)]
	actionsGenFunc (ActionQuit,_) = GOStop

//Use a share to simplify data entry by allowing a choice from known values instead of entry
:: Order =
	{ customer	:: !Either CustomerId NewCustomer
	, product	:: !ProductId
	, amount	:: !Int
	}

:: CustomerId :== Int
:: Customer =
	{ customerId	:: !CustomerId
	, name			:: !String
	}
:: NewCustomer =
	{ name			:: String
	}

:: ProductId :== Int
:: Product =
	{ productId		:: !ProductId
	, description	:: !String
	}

:: OrderForm =
	{ customer	:: !(Choice (Int,String), VisualizationHint NewCustomer)
	, product	:: !Choice (Int,String)
	, amount	:: !Int
	}

derive class iTask Order, Customer, NewCustomer, Product, OrderForm

chooseOrAdd :: Task Order
chooseOrAdd = enterOrder >>= showMessageAbout "You created the order:"
where
	getProductDatabase :: Task (ReadOnlyShared [Product])
	getProductDatabase = createSharedStore
						[{productId = 1, description = "Apples"}
						,{productId = 2, description = "Oranges"}
						,{productId = 3, description = "Pears"}
						] >>= transform toReadOnlyShared
	
	getCustomerDatabase :: Task (ReadOnlyShared [Customer])
	getCustomerDatabase = createSharedStore
						[{customerId = 1, name = "Homer"}
						,{customerId = 2, name = "Marge"}
						,{customerId = 3, name = "Bart"}
						,{customerId = 4, name = "Lisa"}
						,{customerId = 5, name = "Maggie"}
						] >>= transform toReadOnlyShared
	
	enterOrder :: Task Order
	enterOrder
		=	getProductDatabase 
		>>= \products ->
			getCustomerDatabase
		>>= \customers ->
			(getDefaultValue >>= createSharedStore)
		>>= \share ->
			updateSharedInformationA "Enter order" view [(ActionOk,ifvalid)] (share >+| (products >+< customers))
		>>= transform (fst o fromJust o snd)
	where
		view = (vfrom,vto)
		vfrom (order,(products,customers))
			= { OrderForm
			  | customer = (Choice (customerOptions customers) (customerSel order customers), newCustomer order )
			  , product = Choice (productOptions products) (productSel order products)
			  , amount = order.Order.amount
			  }
							
		customerOptions db		= [(c.customerId,c.Customer.name) \\ c <- db] ++ [(0, "Other...")]
		
		customerSel order db	= case order.Order.customer of
			(Left customerId)	= case [i \\ i <- [0..] & c <- db | c.customerId == customerId] of [x] = x; _ = 0
			(Right _)			= length db
		
		newCustomer order		= case order.Order.customer of
			(Left customerId)	= VHHidden {NewCustomer|name = ""}
			(Right nc)			= VHEditable nc
										   		
		productOptions db		= [(p.productId,p.Product.description) \\ p <- db]
		
		productSel order db		= case [i \\ i <- [0..] & p <- db | p.productId == order.Order.product] of [x] = x; _ = 0
		
		vto form (order,(products,customers))
			= { Order
			  | customer = customerChoice form.OrderForm.customer
			  , product = fst (getChoice form.OrderForm.product)
			  , amount = form.OrderForm.amount
			  }
			  
		customerChoice (Choice opts i, nc)
			| i == (length opts) - 1	= Right (fromVisualizationHint nc)
										= Left (fst (getChoice (Choice opts i)))	
sharedValueExamples :: [Workflow]
sharedValueExamples =	[ workflow "Examples/Shared Variables/Text-Lines (grouped tasks)"	"" (Title "Text-Lines"				@>> linesPar)
						, workflow "Examples/Shared Variables/Calculate Sum"				"" (Title "Calculate Sum"			@>> calculateSum)
						, workflow "Examples/Shared Variables/Balanced Binary Tree"			"" (Title "Balanced Binary Tree"	@>> tree)
						, workflow "Examples/Shared Variables/Merge Test (List)"			"" (Title "Merge Test (List)"		@>> mergeTestList)
						, workflow "Examples/Shared Variables/Merge Test (Documents)"		"" (Title "Merge Test (Documents)"	@>> mergeTestDocuments)
						, workflow "Examples/Shared Variables/Google Maps Example"			"" (Title "Google Maps Example"		@>> googleMaps)
						, workflow "Examples/Shared Variables/Sorted List"					"" (Title "Sorted List"				@>> autoSortedList)
						, workflow "Examples/Shared Variables/Formatted Text"				"" (Title "Formatted Text"			@>> formattedText)
						, workflow "Examples/Shared Variables/Choose or add"				"" (Title "Choose or add"			@>> chooseOrAdd)
						]
