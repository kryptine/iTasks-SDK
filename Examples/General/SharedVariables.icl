implementation module SharedVariables

import iTasks, GoogleMaps, Text, ExperimentalDomain
from StdFunc import o

derive bimap Maybe, (,)

quitButtonShared _	= [(ActionQuit, Just Void)]
quitButton _		= [(ActionQuit, Just Void)]

//Text-Lines Examples
noteEditor = (\txt -> Note txt,	\(Note txt) _ -> txt)
listEditor = (split "\n" ,		\l _ -> join "\n" l)

TrimAction :== Action "trim" "Trim"

linesPar :: Task Void
linesPar = parallel "Lines Example" "" (\_ _ -> Void) [InBodyTask noteE, InBodyTask (\sid _ -> updateSharedInformationA ("Lines","Edit lines") listEditor sid (const [(ActionQuit,Just Void)]))]
where
	noteE sid os = 
			updateSharedInformationA ("Text","Edit text") noteEditor sid
		>?*	[ (TrimAction,	IfValid	(\txt -> update trim sid >>| noteE sid os))
			, (ActionQuit,	Always	stop)
			]

//Calculate Sum Example
calculateSum = updateInformationA ("Sum","Auto compute sum") (\t=:(x,y) -> (t,Display (x+y)),\(t,_) _ -> t) (0,0) quitButton

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
			(\l -> (l,Display (toTree l)), \(l,_) _ -> l) emptyL quitButton
where
	emptyL :: [Int]
	emptyL = []

//Merge Tests
mergeTestList :: Task Void
mergeTestList =	
				spawnProcess True initManagerProperties noMenu (Title "1st View" @>> view sid)
	>>|			spawnProcess True initManagerProperties noMenu (Title "2nd View" @>> view sid)
	>>|			stop
where
	sid = sharedStore "mergeTestLists" []

	view :: (SymmetricShared [String]) -> Task Void
	view sid = updateSharedInformationA ("List","Merging the lists") idView sid (const [(ActionQuit,Just Void)])
	
mergeTestDocuments :: Task Void
mergeTestDocuments =
		spawnProcess True initManagerProperties noMenu (Title "1st View" @>> view store)
	>>|	spawnProcess True initManagerProperties noMenu (Title "2nd View" @>> view store)
	>>|	spawnProcess True initManagerProperties noMenu (Title "3rd View" @>> monitorA "Documents" id store (const (UserActions [(ActionQuit,Just Void)])))
	>>|	stop
where
	view sid = updateSharedInformationA ("List","Merging the documents") idView sid (const [(ActionQuit,Just Void)])
	
	store :: SymmetricShared [Document]
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

RemoveMarkersAction :== Action "remove-markers" "Remove Markers"

googleMaps :: Task GoogleMap
googleMaps = parallel "Map Example" mkMap (\_ m -> m)
	[ InBodyTask (\s _ -> updateSharedInformationA "Options" optionsEditor s noActions)
	, InBodyTask (\s _ -> updateSharedInformationA "Google Map" idView s noActions)
	, InBodyTask (\s _ -> updateSharedInformationA "Overview Map" overviewEditor s noActions)
	, InBodyTask (\s _ -> markersDisplay s)
	]
where						
	markersDisplay dbid =
								monitorA "Markers" markersListener dbid
		>>*	\map.				UserActions	[ (RemoveMarkersAction,	Just (update (\map -> {GoogleMap| map & markers = []}) dbid >>| markersDisplay dbid))
											, (ActionQuit,			Just (return map))
											]

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
autoSortedList = updateInformationA ("Automatically Sorted List","You can edit the list, it will sort automatically.") (sort, const) emptyL quitButton
where
	emptyL :: [String]
	emptyL = []

//Different Views on Formatted Text
/*formattedText :: Task Void
formattedText =
				[Menu "Example" [MenuItem ActionQuit Nothing]]
	@>>			createSharedStore (mkEmptyFormattedText {allControls & sourceEditControl = False})
	>>= \sid.	dynamicGroupAOnly [t \\ t <- tasks sid] actions actionsGenFunc
	>>|			stop
where
	tasks sid =
		[ updateSharedInformationA "WYSIWYG Editor" idView [] sid >>| return Void
		, updateSharedInformationA "HTML-Source Editor" (\ft -> Note (getFormattedTextSrc ft), \(Note src) ft -> setFormattedTextSrc src ft) [] sid >>| return Void
		, showMessageSharedA "Formatted Preview" id [] sid >>| return Void
		, showMessageSharedA "Unformatted Preview" (\ft -> Note (toUnformattedString ft False)) [] sid >>| return Void
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
	{ customer	:: !(Choice (Int,String), VisualizationHint NewCustomer)
	, product	:: !Choice (Int,String)
	, amount	:: !Int
	}

derive class iTask Order, Customer, NewCustomer, Product, OrderForm

chooseOrAdd :: Task Order
chooseOrAdd = enterOrder >>= showMessageAbout "You created the order:"
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
		= updateSharedInformationA "Enter order" view (form >+| (productDatabase >+< customerDatabase)) >?* [(ActionOk, IfValid (\(order,(_,_)) -> return order))]
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

phoneBookSearch :: Task (Name,PhoneNumber)
phoneBookSearch
	=	activeQuery Nothing queryPhoneBook
	>>= showMessageAbout ("Result","You chose:")
	
//Abstract search task with a search that is repeated each time the query is altered
activeQuery :: (Maybe String) (String -> Task [a]) -> Task a | iTask a
activeQuery mbQuery queryTask
	=	parallel "Active Query" (initQuery,initDirty,[],Nothing) (\_ (_,_,_,Just res) -> res)
			[InBodyTask searchBox, HiddenTask (activator queryTask), InBodyTask searchResults]
where
	initQuery = case mbQuery of
		Nothing = ""
		Just q	= q
	initDirty = isJust mbQuery
	
	searchBox pstate pinfo
		= updateSharedInformationA "Enter query:" (toView,fromView) pstate noActions
	where
		toView (q,d,r,_) = q
		fromView q (_,d,r,res) = (q,True,r,res)
	
	activator queryTask pstate pinfo
		=	monitor "Query monitor" id (\(_,d,_,_) -> d) True pstate	//Look for the dirty flag to become True
		>>= \(query,_,_,_) ->
			queryTask query
		>>= \results ->
			update (\(q,_,_,res) -> (q,False,results,res)) pstate	//Reset dirty flag
		

	searchResults pstate pinfo
		=	enterSharedChoiceA ("Search results","The following results were found:") id (mapSharedRead (\(_,_,r,_) -> r) pstate) (\mbR -> [(ActionNext,mbR)])
		>>= \x. update (\(q,d,r,_) -> (q,d,r,Just x)) pstate
	
from Shared import mapSharedRead

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


timeShareView :: Task DateTime
timeShareView
	= monitorA "A view on the current time" id currentDateTime (\dateTime -> (UserActions [(ActionClose,Just dateTime)]))

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
						, workflow "Examples/Shared Variables/Time share view"				"" timeShareView
						]
