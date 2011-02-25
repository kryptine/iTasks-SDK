module OrdersExample

import iTasks

derive class iTask LineItem, LineItemStatus, Item
derive bimap (,), Maybe

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
		, productCode	  :: String
		, color			  :: String
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
	
// start 

Start world = startEngine [workflow "Order Example" "Order Example" request] world	  	
	  	
// requester

request :: (Task [(Int,Item)])
request 
	=						enterInformation "Which items do you want to order ?"
		>>= \items ->		getUsersWithRole "buyer" 				
		>>= \buyers ->  	enterChoice "Choose a buyer ?" (map userName buyers)
		>>= \buyer ->		(NamedUser buyer) @: buy (NamedUser buyer) [(i,item) \\ item <- items & i <- [0..]]

// buyer

buy :: User [(Int,Item)] -> Task [(Int,Item)]
buy buyer []
	=						return []
buy buyer items
	=						getUsersWithRole "supplier" 
		>>= \suppliers -> 	(enterMultipleChoice "Select items from list" items
							-&&-
							enterChoice "Select supplier" (map userName suppliers))
		>>= \(chosen,supplier) ->
							(addToOrderList supplier chosen 
							-&&-
							buy buyer [(i,item) \\ (i,item) <- items | not (isMember i (map fst chosen))])
		>>= \(list,_) ->	return list

addToOrderList :: String [(Int,Item)] -> Task [(Int,Item)]
addToOrderList supplier chosen
	=						let sid = (sharedStore supplier) in
							readShared sid
		>>= \list ->		writeShared sid (list++chosen)


// suppliers


