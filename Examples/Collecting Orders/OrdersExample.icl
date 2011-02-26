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

Start world = startEngine [ workflow "Order Example" "Order Example" request
						  , workflow "Start Buyers" "Start batch process buyers" buyersBatch
						  ] world	  	
	  	
// utility

instance == Item
where
	(==) i1 i2 = i1 === i2

selectUserWithRole :: String -> Task User
selectUserWithRole role 
	= 						getUsersWithRole role 
		>>= \users ->		enterChoice ("Choose a " +++ role) (map userName users)
		>>= \name ->		return (NamedUser name)

sid :: User User -> Shared [(Int,Item)] [(Int,Item)]
sid b s = sharedStore (userName b +++ userName s)

// requester

request :: (Task [(Int,Item)])
request 
	=						enterInformation "Which items do you want to order ?"
		>>= \items ->		selectUserWithRole "buyer"
		>>= \buyer ->		buyer @: buy buyer [(i,item) \\ item <- items & i <- [0..]]

// buyer

buy :: User [(Int,Item)] -> Task [(Int,Item)]
buy buyer []
	=						return []
buy buyer items
	=						(enterMultipleChoice "Select items from list" items
							-&&-
							selectUserWithRole "supplier")
		>>= \(chosen,supplier) ->
							addToOrderList chosen (sid buyer supplier)		
		>>|					buy buyer [item \\ item <- items | not (isMember item chosen)]

addToOrderList :: [a] (Shared [a] [a]) -> Task [a] | iTask a
addToOrderList chosen sid
	=						readShared sid
		>>= \list ->		writeShared sid (list++chosen)

buyersBatch :: Task Void
buyersBatch
	=						getUsersWithRole "buyer"
		>>= \buyers ->		getUsersWithRole "supplier"
		>>= \suppliers ->	startBatch [(b,s,sid b s) \\ b <- buyers, s <- suppliers]
where
	startBatch :: [(User,User,Shared [a] [a])] -> Task Void | iTask a
	startBatch [] 
		= return Void
	startBatch [(b,s,sid):bsids]	
		=					spawnProcess True True (b @: batch s sid)
			>>|				startBatch bsids
	
	batch :: User (Shared [a] [a]) -> Task Void | iTask a
	batch supplier sid
		=					writeShared sid []
			>>| 			order supplier sid
													
order :: User (Shared [a] [a]) -> Task Void | iTask a
order supplier sid  
	=						updateSharedInformationA "Order collected so far:" idView [(ActionOk,always)] sid
		>>= \(_,_) ->		order supplier sid

// suppliers


