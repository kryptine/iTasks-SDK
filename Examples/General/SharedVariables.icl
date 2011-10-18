implementation module SharedVariables

import iTasks, GoogleMaps, Text, ExperimentalDomain
from StdFunc import o

derive bimap Maybe, (,)

quitButton _ = UserActions [(ActionQuit, Just Stop)]

//Text-Lines Examples
noteEditor = (GetShared \txt -> Note txt,	PutbackShared \(Note txt) _ _ -> txt)
listEditor = (GetShared (split "\n"),		PutbackShared \l _ _ -> join "\n" l)

TrimAction :== Action "Trim"

linesPar :: Task Void
linesPar = parallel "Lines Example" "" (\_ _ -> Void) [(BodyTask, noteE), (BodyTask, \sid -> updateSharedInformation ("Lines","Edit lines") [UpdateView listEditor] (taskListState sid) Void >>+ quitButton)]
where
	noteE sid = 
			updateSharedInformation ("Text","Edit text") [UpdateView noteEditor] (taskListState sid) Void
		>?*	[ (TrimAction,	IfValid	(\(txt,_) -> update trim (taskListState sid) >>| noteE sid))
			, (ActionQuit,	Always	(return Stop))
			]

//Calculate Sum Example
calculateSum = updateInformation ("Sum","Auto compute sum") [UpdateView (GetLocal \t=:(x,y) -> (t,Display (x+y)), PutbackLocal \(t,_) _ _ -> t)] (0,0) >>+ quitButton

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

tree = updateInformation ("List & Balanced Binary Tree","Type something in the list and the tree will update as well.")
			[UpdateView (GetLocal \l -> (l,Display (toTree l)), PutbackLocal \(l,_) _ _ -> l)] emptyL >>+ quitButton
where
	emptyL :: [Int]
	emptyL = []

//Merge Tests
mergeTestList :: Task Void
mergeTestList =	
				appendTopLevelTask initManagerProperties (Description "1st UpdateView" @>> view sid)
	>>|			appendTopLevelTask initManagerProperties (Description "2nd UpdateView" @>> view sid)
	>>|			return Void
where
	sid = sharedStore "mergeTestLists" []

	view :: (Shared [String]) -> Task ParallelControl
	view sid = updateSharedInformation ("List","Merging the lists") [] sid Void >>+ quitButton
	
mergeTestDocuments :: Task Void
mergeTestDocuments =
		appendTopLevelTask initManagerProperties (Description "1st UpdateView" @>> view store)
	>>|	appendTopLevelTask initManagerProperties (Description "2nd UpdateView" @>> view store)
	>>|	appendTopLevelTask initManagerProperties (Description "3rd UpdateView" @>> showSharedInformation "Documents" [] store Void >>+ quitButton)
	>>|	return Void
where
	view sid = updateSharedInformation ("List","Merging the documents") [] sid Void >>+ quitButton
	store :: Shared [Document]
	store = sharedStore "mergeTestDocs" []

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

RemoveMarkersAction :== Action "Remove Markers"

googleMaps :: Task GoogleMap
googleMaps = parallel "Map Example" defaultMap (\_ m -> m)
	[ (BodyTask, \s -> updateSharedInformation "Options" [UpdateView optionsEditor] (taskListState s) Void >>+ noActions)
	, (BodyTask, \s -> updateSharedInformation "Google Map" [] (taskListState s) Void >>+ noActions)
	, (BodyTask, \s -> updateSharedInformation "Overview Map" [UpdateView overviewEditor] (taskListState s) Void >>+ noActions)
	, (BodyTask, \s -> markersDisplay (taskListState s))
	]
where						
	markersDisplay dbid =
								showSharedInformation "Markers" [ShowView (GetShared markersListener)] dbid Void
		>>*	\{modelValue=map}.	UserActions	[ (RemoveMarkersAction,	Just (update (\map -> {GoogleMap| map & markers = []}) dbid >>| markersDisplay dbid))
											, (ActionQuit,			Just (return Stop))
											]

	optionsEditor	=	( GetShared \map ->				{ type = map.mapType
														, showMapTypeControl = map.mapTypeControl
														, showPanControl = map.panControl
														, showStreetViewControl = map.streetViewControl
														, showZoomControl = map.zoomControl
														, showScaleControl = map.scaleControl
														}
						, PutbackShared \opts _ map	->	{ map 
														& mapType = opts.MapOptions.type
														, mapTypeControl = opts.showMapTypeControl
														, panControl = opts.showPanControl
														, streetViewControl = opts.showStreetViewControl
														, zoomControl = opts.showZoomControl
														, scaleControl = opts.showScaleControl
														}
						)
	overviewEditor	= 	( GetShared \map ->				{ GoogleMap | map
														& mapTypeControl = False
														, panControl = False
														, streetViewControl = False
														, zoomControl = False
														, scaleControl = False
														, scrollwheel = False
														, zoom = 7
														, markers = [{GoogleMapMarker|m & draggable = False} \\ m <- map.markers]
														}
						, PutbackShared \nmap _ map ->	{ GoogleMap | map
														& center = nmap.GoogleMap.center
														}
						)
	markersListener	map = [{position = position, map = {GoogleMap| defaultMap & center = position, zoom = 15, markers = [marker]}} \\ marker=:{GoogleMapMarker| position} <-map.markers]

//Auto sorted list
autoSortedList = updateInformation ("Automatically Sorted List","You can edit the list, it will sort automatically.") [UpdateView (GetLocal sort, PutbackLocal \l _ _ -> l)] emptyL >>+ quitButton
where
	emptyL :: [String]
	emptyL = []

//Different UpdateViews on Formatted Text
/*formattedText :: Task Void
formattedText =
				[Menu "Example" [MenuItem ActionQuit Nothing]]
	@>>			createSharedStore (mkEmptyFormattedText {allControls & sourceEditControl = False})
	>>= \sid.	dynamicGroupAOnly [t \\ t <- tasks sid] actions actionsGenFunc
	>>|			stop
where
	tasks sid =
		[ updateSharedInformationA "WYSIWYG Editor" idUpdateView [] sid >>| return Void
		, updateSharedInformationA "HTML-Source Editor" (\ft -> Note (getFormattedTextSrc ft), \(Note src) ft -> setFormattedTextSrc src ft) [] sid >>| return Void
		, showInformationSharedA "Formatted Preview" id [] sid >>| return Void
		, showInformationSharedA "Unformatted Preview" (\ft -> Note (toUnformattedString ft False)) [] sid >>| return Void
		]
		
	actions = [(ActionQuit, Always)]
	actionsGenFunc actionQuit = GOStop*/

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
	{ customer	:: !(ComboChoice String Int, VisualizationHint NewCustomer)
	, product	:: !ComboChoice String Int
	, amount	:: !Int
	}

derive class iTask Order, Customer, NewCustomer, Product, OrderForm

chooseOrAdd :: Task Order
chooseOrAdd = enterOrder >>= showInformation "You created the order:" []
where
	productDatabase :: ReadOnlyShared [Product]
	productDatabase = toReadOnlyShared (sharedStore "chooseOrAddProductDB"
						[{productId = 1, description = "Apples"}
						,{productId = 2, description = "Oranges"}
						,{productId = 3, description = "Pears"}
						])
	
	customerDatabase :: ReadOnlyShared [Customer]
	customerDatabase = toReadOnlyShared (sharedStore "chooseOrAddCustomerDB"
						[{customerId = 1, name = "Homer"}
						,{customerId = 2, name = "Marge"}
						,{customerId = 3, name = "Bart"}
						,{customerId = 4, name = "Lisa"}
						,{customerId = 5, name = "Maggie"}
						])
	form = sharedStore "chooseOrAddForm" defaultValue
	enterOrder :: Task Order
	enterOrder
		= updateSharedInformation "Enter order" [UpdateView view] (form >+| (productDatabase >+< customerDatabase)) Void >?* [(ActionOk, IfValid (\((order,(_,_)),_) -> return order))]
	where
		view = (GetShared vfrom,PutbackShared vto)
		vfrom (order,(products,customers))
			= { OrderForm
			  | customer = (mkComboChoice (customerOptions customers) (Just (customerSel order)), newCustomer order)
			  , product = mkComboChoice (productOptions products) (Just order.Order.product)
			  , amount = order.Order.amount
			  }
							
		customerOptions db		= [(c.Customer.name, c.customerId) \\ c <- db] ++ [("Other...", 0)]
		
		customerSel order = case order.Order.customer of
			(Left customerId)	= customerId
			(Right _)			= 0
		
		newCustomer order		= case order.Order.customer of
			(Left customerId)	= VHHidden {NewCustomer|name = ""}
			(Right nc)			= VHEditable nc
										   		
		productOptions db		= [(p.Product.description,p.productId) \\ p <- db]
		
		vto form _ (order,(products,customers))
			= { Order
			  | customer = customerChoice form.OrderForm.customer
			  , product = getSelection form.OrderForm.product
			  , amount = form.OrderForm.amount
			  }
			  
		customerChoice (choice, nc)
			| getMbSelection choice == Just 0	= Right (fromVisualizationHint nc)
												= Left (getSelection choice)

phoneBookSearch :: Task (Name,PhoneNumber)
phoneBookSearch
	=	activeQuery Nothing queryPhoneBook
	>>= showInformation ("Result","You chose:") []
	
//Abstract search task with a search that is repeated each time the query is altered
activeQuery :: (Maybe String) (String -> Task [a]) -> Task a | iTask a
activeQuery mbQuery queryTask
	=	parallel "Active Query" (initQuery,initDirty,[],Nothing) (\_ (_,_,_,Just res) -> res)
			[(BodyTask, searchBox), (HiddenTask, activator queryTask), (BodyTask, searchResults)]
where
	initQuery = case mbQuery of
		Nothing = ""
		Just q	= q
	initDirty = isJust mbQuery
	
	searchBox tlist
		=	updateSharedInformation "Enter query:" [UpdateView (GetShared toUpdateView, PutbackShared fromUpdateView)] (taskListState tlist) Void >>+ noActions
	where
		toUpdateView (q,d,r,_) = q
		fromUpdateView q _ (_,d,r,res) = (q,True,r,res)
	
	activator queryTask tlist
		=	showSharedInformation "Query showSharedInformation" [] (taskListState tlist) Void >? (\((_,d,_,_),_) -> d)	//Look for the dirty flag to become True
		>>= \((query,_,_,_),_) ->
			queryTask query
		>>= \results ->
			update (\(q,_,_,res) -> (q,False,results,res)) (taskListState tlist)	//Reset dirty flag
		>>| return Continue

	searchResults tlist
		=	enterSharedChoice ("Search results","The following results were found:") [] (mapSharedRead (\(_,_,r,_) -> r) (taskListState tlist)) >?* [(ActionNext,IfValid return)]
		>>= \x. update (\(q,d,r,_) -> (q,d,r,Just x)) (taskListState tlist)
		>>|	return Stop

//Very simple CSV phonebook implementation
:: Name :== String
:: PhoneNumber :== String

queryPhoneBook :: String -> Task [(Name,PhoneNumber)]
queryPhoneBook query
	=	importCSVFile "phonebook.txt"
	>>= transform (matchQuery query)
where
	matchQuery query entries
		= [(name,phoneNo) \\ [name,phoneNo] <- entries | match query name || match query phoneNo]

	match query subject
		= indexOf (toUpperCase (trim query)) (toUpperCase subject) <> -1


timeShareUpdateView :: Task DateTime
timeShareUpdateView
	= showSharedInformation "A view on the current time" [] currentDateTime Void >>+ \{modelValue=(dateTime,_)} -> (UserActions [(ActionClose,Just dateTime)])

sharedValueExamples :: [Workflow]
sharedValueExamples =	[ workflow "Examples/Shared Variables/Text-Lines"					"" linesPar
						, workflow "Examples/Shared Variables/Calculate Sum"				"" calculateSum
						, workflow "Examples/Shared Variables/Balanced Binary Tree"			"" tree
						, workflow "Examples/Shared Variables/Merge Test (List)"			"" mergeTestList
						, workflow "Examples/Shared Variables/Merge Test (Documents)"		"" mergeTestDocuments
						, workflow "Examples/Shared Variables/Google Maps Example"			"" googleMaps
						, workflow "Examples/Shared Variables/Sorted List"					"" autoSortedList
						//, workflow "Examples/Shared Variables/Formatted Text"				"" formattedText
						, workflow "Examples/Shared Variables/Choose or add"				"" chooseOrAdd
						, workflow "Examples/Shared Variables/Phonebook Search"				"" phoneBookSearch
						, workflow "Examples/Shared Variables/Time share view"				"" timeShareUpdateView
						]
