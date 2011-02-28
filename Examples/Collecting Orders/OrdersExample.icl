module OrdersExample

import iTasks

derive class iTask Item, Order, OrderState, SupOrder, SupOrderState
derive bimap (,), Maybe

:: Order a			= 	{ item  	  :: [a]		
						, requester	  :: Requester
						, buyer		  :: Buyer	
						, orderNumber :: OrderNumber a
						, orderStatus :: [TimeStamp OrderState]					
						, supOrders	  :: [SupOrder a]	
	  					}
:: Requester 		:== User
:: Buyer	 		:== User
:: OrderNumber a 	:== Int
:: OrderState		=	SentToBuyer
					|	SuppliersChosen 		
					|	ShippedToRequester
					|	OrderCancelled	
:: TimeStamp a		:== (a,DateTime)
:: SupOrder a		=	{ item			:: [a]
						, supplier		:: Supplier
						, supOrderNumber:: SupOrderNumber a
						, supOrderStatus:: [TimeStamp SupOrderState]
						}
:: Supplier			:== User
:: SupOrderNumber a	:== Int
:: SupOrderState	=	ToBeSendToSupplier 	
					|	SentToSupplier 		
					|	ShippedToBuyer		
					|	RejectedBySupplier 			
:: OrderId a		:== (OrderNumber a, OrderStore a)
:: SupOrderId a		:== (SupOrderNumber a, OrderNumber a, OrderStore a)

:: OrderStore a 	:== Shared [Order a] [Order a]

// start 

Start world = startEngine [ workflow "Order Example" "Order Example" (Title "Request items..." @>> request itemStore)
						  , workflow "Start Buyers" "Start batch process buyers" (buyersBatch itemStore)
						  ] world	  	
	  	
// specific instantiations

:: Item
	=	{ description 	:: String
//		, productCode	:: String
//		, color			:: String
		}

:: ItemStore 		:== OrderStore Item

itemStore :: ItemStore
itemStore = sharedStore "sharedItemOrderStore"

// storage

addOrder ::  Requester Buyer [a] OrderState (OrderStore a) -> Task (OrderNumber a) | iTask a
addOrder requester buyer items orderstate store 
	= 						getCurrentDateTime
		>>= \timestamp ->	readShared store
		>>= \as ->			let orderNumber = length as in
							 (writeShared store (as ++ [ { item 		= items
										 				 , orderNumber 	= orderNumber
										  				 , requester 	= requester
										  				 , buyer 		= buyer
										  				 , orderStatus 	= [(orderstate,timestamp)]
										  				 , supOrders 	= [] 
										  				}
										  			    ]) 
							  >>| return orderNumber)
		
fetchOrder :: (OrderId a) -> Task (Order a) | iTask a 
fetchOrder (nr, store)
	=						readShared store
		>>= \as -> 			return (as!!nr)

updateOrder :: (Order a) (OrderId a) -> Task (Order a) | iTask a 
updateOrder order (nr, store)
	=						readShared store
		>>= \orders -> 		writeShared store (updateAt nr order orders) 
		>>|					return order

setOrderState :: OrderState (OrderId a) -> Task (Order a) | iTask a
setOrderState orderState orderId
	=						getCurrentDateTime
		>>= \timestamp ->	fetchOrder orderId
		>>= \order -> 		updateOrder {order & orderStatus = [(orderState,timestamp):order.orderStatus]} orderId

addSupplier :: [a] Supplier SupOrderState (OrderId a) -> Task (SupOrderNumber a) | iTask a
addSupplier items supplier supstate orderId
	=						getCurrentDateTime
		>>= \timestamp ->	fetchOrder orderId
		>>= \order ->		let subOrderNumber 	= length order.supOrders
								nsup			= { item 			= items
												  , supplier 		= supplier
												  , supOrderNumber 	= subOrderNumber
												  , supOrderStatus 	= [(supstate,timestamp)]
												  }
							in 	(updateOrder {order & supOrders = order.supOrders ++ [nsup]} orderId
							     >>| return subOrderNumber) 

// utility functions

instance == Item
where
	(==) i1 i2 = i1 === i2

selectUserWithRole :: String -> Task User
selectUserWithRole role 
	= 						getUsersWithRole role 
		>>= \users ->		enterChoice ("Choose a " +++ role) (map userName users)
		>>= \name ->		return (NamedUser name)

// for every buyer and supplier start up a workflow which controls the oustanding orders 
// probably can be replaced by a monitor tasks ....

buyersBatch :: (OrderStore a) -> Task Void | iTask a
buyersBatch store
	=						writeShared store []
		>>|					getUsersWithRole "buyer"
		>>= \buyers ->		getUsersWithRole "supplier"
		>>= \suppliers ->	startBatch [(b,s) \\ b <- buyers, s <- suppliers]
where
	startBatch :: [(User,User)] -> Task Void 
	startBatch [] 
		= return Void
	startBatch [(b,s):bsids]	
		=					spawnProcess True True (b @: shipOrder b s store)
			>>|				startBatch bsids

// --------------- workflow

// requester

request :: (OrderStore a) -> Task Void | iTask a & Eq a
request store
	=						getCurrentUser
		>>= \requester ->	enterInformation "What do you want to order ?"
		>>= \items ->		selectUserWithRole "buyer"
		>>= \buyer ->		addOrder requester buyer items SentToBuyer store 
		>>= \ordernr ->		buyer @: buy items (ordernr, store)
		>>|					showMessageSharedA "Status of your order:" (showStatus ordernr) [(ActionOk,always)] store
		>>|					return Void
where
	showStatus ordernr orders = Display [o \\ o <- orders | o.orderNumber == ordernr]  

// buyer

buy :: [a] (OrderId a) -> Task Void | iTask a & Eq a
buy items orderId
	=						spawnProcess True True (Title "Choose suppliers..." @>> chooseSuppliers items orderId)
		>>|					return Void 

chooseSuppliers :: [a]  (OrderId a) -> Task Void | iTask a & Eq a
chooseSuppliers [] orderId  
	=						setOrderState SuppliersChosen orderId
		>>|					return Void
chooseSuppliers items orderId
	=						(enterMultipleChoice "Select items from list intended for the same supplier " items
							-&&-
							selectUserWithRole "supplier")
		>>= \(chosen,supplier) ->
							addSupplier chosen supplier ToBeSendToSupplier orderId
		>>= \norder ->		chooseSuppliers [item \\ item <- items | not (isMember item chosen)] orderId
	
shipOrder :: Buyer Supplier (OrderStore a) -> Task (Order a) | iTask a
shipOrder buyer supplier store
	=					updateSharedInformationA "Ready to order:" (toView,fromView) [(ActionOk,always)] store
	 >>= \(_,mba) ->	shipOrder buyer supplier store //handleOrder store
where
	toView orders	= Display orders // [o \\ o <- orders | o.buyer == buyer ]
	fromView _ order = order
	



// suppliers



/* old stuf

sid :: Buyer Supplier -> Shared a a | iTask a
sid b s = sharedStore (userName b +++ userName s)

// line item types
:: LineItem 
	= 	{ lineItemId  	 :: Int
	  	, requesterId 	 :: User 	// original: Int
	  	, lineItemStatus :: LineItemStatus
	  	, item           :: Item
	  	}
:: LineItemStatus
	=	ItemCreated
	|	ItemSentToBuyer
	|	ItemSentToSupplier
	|	ItemManufactured
	|	ItemShippedToBuyer
	|	ItemRejected
	|	ItemCanceled
:: Item
	=	{ itemDescription :: String
//		, productCode	  :: String
//		, color			  :: String
		}

// requisition order
:: RequisitionOrder
	=	{ requisitionOrderId 	   :: Int
		, reqesterId	     	   :: User // org: Int
		, buyerId			 	   :: User // org: Int
		, lineItems		     	   :: [LineItem]
		, procurementOrders  	   :: [ProcecurementOrderId]
		, procurementOrdersCounter :: Int // ?? is this needed or wanted
		, requisitionOrderStatus   :: RequisitionOrderStatus
		}
:: ProcecurementOrderId :== Int
:: RequisitionOrderStatus
	=	RequisitionCreated
	|	RequisitionSentToBuyer
	|	RequisitionSentToSupplier
	|	RequisitionShippedToBuyer
	|	RequisitionSippedToRequester
	|	RequisitionCanceled
	
// procurement order
:: Procurement
	=	{ procurementOrderId	:: Int
		, buyerId				:: User // org: Int
		, supplierId			:: User // org: Int
		, lineItems				:: [LineItem]
		, procurementStatus		:: ProcurementStatus
		}
:: ProcurementStatus
	=	ProcurementCreated
	|	ProcurementSentToSupplier
	|	ProcurementOrderAssembled
	|	ProcurementShippedToBuyer
	|	ProcurementRejected
	|	ProcurementCanceled

*/
