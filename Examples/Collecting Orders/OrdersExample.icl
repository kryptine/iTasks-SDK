module OrdersExample

import iTasks

derive class iTask Item, Order, Status
derive bimap (,), Maybe

:: Item
	=	{ description 	:: String
//		, productCode	:: String
//		, color			:: String
		}

// Item Storage
:: Order a 
	= 	{ item  	  :: a
		, orderStatus :: [OrderState]		// History
	  	}
:: Requester 	:== User
:: Buyer	 	:== User
:: Supplier		:== User
:: OrderState	:== (DateTime,Status)
:: Status
	=	CreatedBy			Requester			
	|	SentToBuyer 		Buyer
	|	SelectedForSupplier	Supplier
	|	SentToSupplier		Supplier
	|	ShippedToBuyer		Buyer
	|	ShippedToRequester	Requester
	|	RejectedBy			User
	|	CanceledBy			User

:: OrderStore		:== Shared [Order[Item]] [Order[Item]]
// start 

Start world = startEngine [ workflow "Order Example" "Order Example" myRequest
						  , workflow "Start Buyers" "Start batch process buyers" buyersBatch
						  ] world	  	
	  	
myRequest :: (Task (Order [Item]))
myRequest = request

// utility

instance == Item
where
	(==) i1 i2 = i1 === i2

selectUserWithRole :: String -> Task User
selectUserWithRole role 
	= 						getUsersWithRole role 
		>>= \users ->		enterChoice ("Choose a " +++ role) (map userName users)
		>>= \name ->		return (NamedUser name)

sid :: Buyer Supplier -> Shared a a | iTask a
sid b s = sharedStore (userName b +++ userName s)

setStatus :: (Order a) Status -> Task (Order a) | iTask a
setStatus order status 
	=						getCurrentDateTime
		>>= \timestamp ->	return {order & orderStatus = [(timestamp,status):order.orderStatus]}

// requester

request :: (Task (Order[a])) | iTask a & Eq a
request 
	=						getCurrentUser
		>>= \requester ->	enterInformation "What do you want to order ?"
		>>= \items ->		setStatus {item = items, orderStatus = []} (CreatedBy requester) 
		>>= \order ->		selectUserWithRole "buyer"
		>>= \buyer ->		buyer @: buy buyer order 
		>>|					return order

// buyer

buy :: Buyer (Order [a]) -> Task Void | iTask a & Eq a
buy buyer order
	=						setStatus order (SentToBuyer buyer)
		>>= \order ->		spawnProcess True True (chooseSuppliers buyer order)
		>>|					return Void 
	
chooseSuppliers :: Buyer (Order [a]) -> Task Void | iTask a & Eq a
chooseSuppliers buyer order=:{item = []}
	=						return Void
chooseSuppliers buyer order
	=						(enterMultipleChoice "Select items from list" order.item
							-&&-
							selectUserWithRole "supplier")
		>>= \(chosen,supplier) ->
							setStatus {order & item = chosen} (SelectedForSupplier supplier)
		>>= \norder ->		addToOrderList buyer supplier norder
		>>|					chooseSuppliers buyer {order & item = [item \\ item <- order.item | not (isMember item chosen)]}

addToOrderList :: Buyer Supplier (Order [a]) -> Task Void | iTask a
addToOrderList buyer supplier order
	=						let store = sid buyer supplier in
							readShared store
		>>= \orders ->		writeShared store (orders ++ [order])
		>>|					return Void


// for every buyer and supplier start up a workflow which controls the oustanding orders 

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
		=					let store = sid b s in
							writeShared store []
			>>|				spawnProcess True True (b @: shipOrder s store)
			>>|				startBatch bsids
	
shipOrder :: Supplier OrderStore -> Task ([Order [Item]])
shipOrder supplier store
	=					showMessageSharedA "Order collected so far:" (\store -> Display store)  [(ActionOk,always)] store
	 >>= \(_,mba) ->	shipOrder supplier store //handleOrder store
//where
//	readyToOrder (Valid a) = i
//		| 													

// suppliers



/* old stuf

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
