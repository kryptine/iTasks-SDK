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
	
// start 

Start world = startEngine [workflow "Order Example" "Order Example" request] world	  	
	  	
// utility


instance == Item
where
	(==) i1 i2 = i1 === i2

selectUserWithRole :: String -> Task String
selectUserWithRole role 
	= 						getUsersWithRole role 
		>>= \users ->		enterChoice ("Choose a " +++ role) (map userName users)

// requester

request :: (Task [(Int,Item)])
request 
	=						enterInformation "Which items do you want to order ?"
		>>= \items ->		selectUserWithRole "buyer"
		>>= \buyer ->		(NamedUser buyer) @: buy (NamedUser buyer) [(i,item) \\ item <- items & i <- [0..]]

// buyer

buy :: User [(Int,Item)] -> Task [(Int,Item)]
buy buyer []
	=						return []
buy buyer items
	=						(enterMultipleChoice "Select items from list" items
							-&&-
							selectUserWithRole "supplier")
		>>= \(chosen,supplier) ->
							(// addToOrderList supplier chosen 			
//							-&&-									// incorrect behaviour if && is called here !
							buy buyer [item \\ item <- items | not (isMember item chosen)])
//		>>= \(list,_) ->	return list

addToOrderList :: String [(Int,Item)] -> Task [(Int,Item)]
addToOrderList supplier chosen
	=						let sid = (sharedStore supplier) in
//							readShared sid
//		>>= \list ->		writeShared sid (list++chosen)
							return chosen

// suppliers


