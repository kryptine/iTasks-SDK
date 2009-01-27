module bid
/**
* This module defines a simple workflow to let a number of suppliers
* bid on a potential purchase.
* 
* Informal description of the workflow
* - A description of the item that has to be purchased is made
* - A set of potential suppliers is selected
* - Each supplier is asked to make a bid for the purchase
* - The cheapest supplier is suggested
* - This suggestion is either accepted or a different supplier is selected manually
* - A confirmation is sent to the selected supplier
*/

import StdEnv, StdiTasks, iData

derive gForm []
derive gUpd []

UID_CUSTOMER = 0
UID_SUPPLIER1 = 1
UID_SUPPLIER2 = 2
UID_SUPPLIER3 = 3

Start :: *World -> *World
Start world = startEngine [bidFlow] world

bidFlow :: Workflow
bidFlow = { name		= "bid"
		  , label		= "Purchase product"
		  , roles		= []
		  , mainTask	= purchaseTask
		  }

purchaseTask :: Task Void
purchaseTask =
	definePurchase					=>>	\purchase	->
	selectSuppliers 				=>>	\suppliers	->
	collectBids	purchase suppliers	=>> \bids		->
	selectBid bids	 				=>> \bid		->
	confirmBid purchase bid
	
definePurchase :: Task String
definePurchase = 
	[Text "Describe the product you want to purchase"]
	?>> editTask "Ok" "" 

selectSuppliers :: Task [(Int,String)]
selectSuppliers
	= getUsersWithRoleTask "supplier" =>> \suppliers ->
	  ( mchoiceAndTasks
	  		[Text "Select the suppliers from which you want to receive a bid", HrTag []]
	  		[(label, return_V supplier) \\ supplier =: (uid, label) <- suppliers]
	  )
	
collectBids :: String [(Int,String)] -> Task [((Int,String),Real)]
collectBids purchase suppliers
	= andTasks
		[("Bid for " +++ purchase +++ " from " +++ name, uid @: ("Bid request regarding " +++ purchase, collectBid purchase supplier)) \\ supplier =: (uid,name) <- suppliers]
where
	collectBid :: String (Int,String) -> Task ((Int,String),Real)
	collectBid purchase bid
		= [Text "Please make a bid to supply ",ITag [] [Text purchase], HrTag []]
		  ?>>
		  editTask "Ok" createDefault =>> \price ->
		  return_V (bid, price)
	
selectBid :: [((Int,String),Real)] -> Task ((Int,String),Real)
selectBid bids
	= determineCheapest bids	=>> \cheapestBid =: ((uid,name),price) ->	
	[ Text "The cheapest bid is ",Text (toString price), Text " by ", Text name, BrTag [],
	  Text "Do you want to accept this bid?", BrTag []]
	?>>
	yesOrNo =>> \acceptCheapest ->
	if acceptCheapest
		( return_V cheapestBid)
		( chooseTask
			[Text "Please select a bid"]
			[(name +++ " " +++ toString price, return_V bid) \\ bid =: ((uid,name),price) <- bids] 
		)
where
	determineCheapest bids = return_V (hd (sortBy (\(_,x) (_,y) -> x < y) bids))
	yesOrNo = (editTask "Yes" Void #>> return_V True) -||- (editTask "No" Void #>> return_V False)
	
confirmBid :: String ((Int,String),Real) -> Task Void
confirmBid purchase bid =: ((uid,label),price)
	= uid @: ("Bid confirmation",(
		[Text "Your bid of ", Text (toString price),Text " for the product ",ITag [] [Text purchase], Text " has been accepted."]
		?>> editTask "Ok" Void
	))
	