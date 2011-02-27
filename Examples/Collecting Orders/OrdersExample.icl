module OrdersExample

import iTasks

derive class iTask Item, Order, OrderState, SupplierState
derive bimap (,), Maybe

:: Item
	=	{ description 	:: String
//		, productCode	:: String
//		, color			:: String
		}

// Item Storage
:: Order a 
	= 	{ item  	  :: [([a],[TimeStamp SupplierState])]
		, orderNumber :: String
		, requester	  :: Requester
		, buyer		  :: Buyer	
		, orderStatus :: [TimeStamp OrderState]		// History
	  	}
:: Requester 	:== User
:: Buyer	 	:== User
:: Supplier		:== User
:: TimeStamp a	:== (DateTime,a)
:: OrderState
	=	SentToBuyer
	|	ReadyToSendToSuppliers 		
	|	ShippedToRequester	
:: SupplierState
	=	ToBeSendToSupplier Supplier
	|	SentToSupplier Supplier
	|	RejectedBy Supplier

:: OrderStore a		:== Shared [Order a] [Order a]

orderStore :: (OrderStore a) | iTask a
orderStore = sharedStore "sharedOrderStore"

// start 

Start world = startEngine [ workflow "Order Example" "Order Example" myRequest
						  , workflow "Start Buyers" "Start batch process buyers" buyersBatch
						  ] world	  	
	  	
myRequest :: (Task (Order Item))
myRequest = request

// utility functions

instance == Item
where
	(==) i1 i2 = i1 === i2

selectUserWithRole :: String -> Task User
selectUserWithRole role 
	= 						getUsersWithRole role 
		>>= \users ->		enterChoice ("Choose a " +++ role) (map userName users)
		>>= \name ->		return (NamedUser name)

setOrderState :: (Order a) OrderState -> Task (Order a) | iTask a
setOrderState order status 
	=						getCurrentDateTime
		>>= \timestamp ->	return {order & orderStatus = [(timestamp,status):order.orderStatus]}

addOrderSupplierState :: (Order a) [a] SupplierState -> Task (Order a) | iTask a
addOrderSupplierState order items state 
	=						getCurrentDateTime
		>>= \timestamp ->	return {order & item = [(items,[(timestamp,state)]):order.item]}


initOrder :: Requester Buyer [a] -> Task (Order a) | iTask a
initOrder requester buyer items
	=						getCurrentDateTime
		>>= \timestamp ->	return 	{ item = [(items,[])]
									, orderNumber = toString timestamp
									, requester = requester
									, buyer = buyer
									, orderStatus = []}
		
// for every buyer and supplier start up a workflow which controls the oustanding orders 
// probably can be replaced by a monitor tasks ....

buyersBatch :: Task Void
buyersBatch
	=						getUsersWithRole "buyer"
		>>= \buyers ->		getUsersWithRole "supplier"
		>>= \suppliers ->	startBatch [(b,s) \\ b <- buyers, s <- suppliers]
where
	startBatch :: [(User,User)] -> Task Void 
	startBatch [] 
		= return Void
	startBatch [(b,s):bsids]	
		=					let 
								store :: OrderStore Item 
								store = orderStore in // one store for the time being seems to be fine ....
							writeShared store []
			>>|				spawnProcess True True (b @: shipOrder b s store)
			>>|				startBatch bsids

// --------------- workflow

// requester

request :: (Task (Order a)) | iTask a & Eq a
request 
	=						getCurrentUser
		>>= \requester ->	enterInformation "What do you want to order ?"
		>>= \items ->		selectUserWithRole "buyer"
		>>= \buyer ->		initOrder requester buyer []  
		>>= \order ->		buyer @: buy buyer order items 
		>>|					showMessageSharedA "Status of your order:" (showStatus order) [(ActionOk,always)] orderStore
		>>|					return order
where
	showStatus :: (Order a) [(Order a)] -> Display [(Order a)] | iTask a 
	showStatus order orders = Display [o \\ o <- orders | o.orderNumber == order.orderNumber]  

// buyer

buy :: Buyer (Order a) [a] -> Task Void | iTask a & Eq a
buy buyer order items
	=						setOrderState order SentToBuyer
		>>= \order ->		Description "Choose suppliers..." @>> spawnProcess True True (chooseSuppliers order items)
		>>|					return Void 
	
chooseSuppliers :: (Order a) [a]   -> Task Void | iTask a & Eq a
chooseSuppliers order [] 
	=						setOrderState order ReadyToSendToSuppliers
		>>= \norder ->		readShared orderStore
		>>= \orders -> 		writeShared orderStore (orders ++ [norder])
		>>|					return Void
chooseSuppliers order items 
	=						(enterMultipleChoice "Select items from list intended for the same supplier " items
							-&&-
							selectUserWithRole "supplier")
		>>= \(chosen,supplier) ->
							addOrderSupplierState order chosen (ToBeSendToSupplier supplier)
		>>= \norder ->		chooseSuppliers norder [item \\ item <- items | not (isMember item chosen)] 

	
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
