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
	= 	{ item  	  :: [([a],[TimeStamp SupplierState])]		// Sub-orders history
		, orderNumber :: String
		, requester	  :: Requester
		, buyer		  :: Buyer	
		, orderStatus :: [TimeStamp OrderState]					// Order history
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

// start 

Start world = startEngine [ workflow "Order Example" "Order Example" (Title "Request items..." @>> request itemStore)
						  , workflow "Start Buyers" "Start batch process buyers" (buyersBatch itemStore)
						  ] world	  	
	  	
// storage

:: ItemStore :== OrderStore Item

itemStore :: ItemStore
itemStore = sharedStore "sharedItemOrderStore"


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


initOrder :: Requester Buyer -> Task (Order a) | iTask a
initOrder requester buyer 	
	=						getCurrentDateTime
		>>= \timestamp ->	return 	{ item = []
									, orderNumber = toString timestamp
									, requester = requester
									, buyer = buyer
									, orderStatus = []}
		
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

request :: (OrderStore a) -> (Task Void) | iTask a & Eq a
request store
	=						getCurrentUser
		>>= \requester ->	enterInformation "What do you want to order ?"
		>>= \items ->		selectUserWithRole "buyer"
		>>= \buyer ->		initOrder requester buyer   
		>>= \order ->		buyer @: buy buyer order items store
		>>|					showMessageSharedA "Status of your order:" (showStatus order) [(ActionOk,always)] store
		>>|					return Void
where
	showStatus :: (Order a) [(Order a)] -> Display [(Order a)] | iTask a 
	showStatus order orders = Display [o \\ o <- orders | o.orderNumber == order.orderNumber]  

// buyer

buy :: Buyer (Order a) [a] (OrderStore a) -> Task Void | iTask a & Eq a
buy buyer order items store
	=						setOrderState order SentToBuyer
		>>= \order ->		spawnProcess True True (Title "Choose suppliers..." @>> chooseSuppliers order items store)
		>>|					return Void 

chooseSuppliers :: (Order a) [a] (OrderStore a) -> Task Void | iTask a & Eq a
chooseSuppliers order [] store
	=						setOrderState order ReadyToSendToSuppliers
		>>= \norder ->		readShared store
		>>= \orders -> 		writeShared store (orders ++ [norder])
		>>|					return Void
chooseSuppliers order items store
	=						(enterMultipleChoice "Select items from list intended for the same supplier " items
							-&&-
							selectUserWithRole "supplier")
		>>= \(chosen,supplier) ->
							addOrderSupplierState order chosen (ToBeSendToSupplier supplier)
		>>= \norder ->		chooseSuppliers norder [item \\ item <- items | not (isMember item chosen)] store

	
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
