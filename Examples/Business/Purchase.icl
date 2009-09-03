implementation module Purchase
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

import iTasks
import CommonDomain

//Additional imports for custom combinator creation
from TSt 		import applyTask, setCombination, mkSequenceTask, mkParallelTask
from TSt 		import :: TSt{..}, :: TaskInfo{..}, :: StaticInfo{..}, :: Options{..}, :: Store
from SessionDB	import :: Session
from Types		import :: ProcessId, :: TaskNr
from TaskTree	import :: TaskTree

//Main types
:: Purchase	=	{	name	:: !String
				,	amount	:: !Int
				,	express	:: !Bool
				,	note	:: !Note
				}
				
//Generic derives
derive gPrint		Purchase
derive gParse		Purchase
derive gVisualize	Purchase
derive gUpdate		Purchase

purchaseExample :: [Workflow]
purchaseExample
= [	{ name		= "Examples/Business/Purchase"
	, label		= "Purchase product"
	, roles		= []
	, mainTask	= purchaseTask
	}
  ]

purchaseTask :: Task Void
purchaseTask =
	definePurchase					>>=	\purchase	->
	selectSuppliers 				>>=	\suppliers	->
	collectBids	purchase suppliers	>>= \bids		->
	selectBid bids	 				>>= \bid		->
	confirmBid purchase bid
	
definePurchase :: Task Purchase
definePurchase = enterInformation "Please describe the product you would like to purchase"
	
selectSuppliers :: Task [(Int,String)]
selectSuppliers
	= getUsersWithRole "supplier" >>= \suppliers ->
	  ( enterMultipleChoice
	  		[Text "Select the suppliers from which you want to receive a bid"]
	  		suppliers
	  )
	
collectBids :: Purchase [(Int,String)] -> Task [((Int,String),Currency)]
collectBids purchase suppliers
	= andTasksEnough
		[("Bid for " +++ purchase.Purchase.name +++ " from " +++ name, uid @: ("Bid request regarding " +++ purchase.Purchase.name, collectBid purchase supplier)) \\ supplier =: (uid,name) <- suppliers]
where
	collectBid :: Purchase (Int,String) -> Task ((Int,String),Currency)
	collectBid purchase bid
		=	enterInformationAbout
				"Please make a bid to supply the following product"
				purchase
				>>= \price -> return (bid,price)  	
	
selectBid :: [((Int,String),Currency)] -> Task ((Int,String),Currency)
selectBid bids
	=	determineCheapest bids	>>= \cheapestBid=:((uid,name),price) ->	
		requestConfirmation
			[ Text "The cheapest bid is ", Text (toString price), Text " by ", Text name, BrTag [],
	  		  Text "Do you want to accept this bid?"
	  		] >>= \acceptCheapest ->
		if acceptCheapest
			( return cheapestBid )
			( enterChoice "Please select one of the following bids" bids )
where
	determineCheapest bids = return (hd (sortBy (\(_,x) (_,y) -> x < y) bids))
	
confirmBid :: Purchase ((Int,String),Currency) -> Task Void
confirmBid purchase bid =: ((uid,label), price)
	= uid @: ("Bid confirmation", showMessage [Text "Your bid of ", Text (toString price),Text " for the product ",ITag [] [Text purchase.Purchase.name], Text " has been accepted."])
			
//Custom utility combinators 
andTasksEnough:: ![LabeledTask a] -> (Task [a]) | iTask a
andTasksEnough taskCollection = mkParallelTask "andTasksEnough" andTasksEnough`
where
	andTasksEnough` tst
		# tst						= setCombination TTVertical  tst	//Show parallel sub tasks in reversed order
		# (_,tst =:{activated})		= applyTask (mkSequenceTask "enough" (applyTask (showMessage "Stop if enough results are returned..."))) tst
		= applyTask (mkSequenceTask "tasks" (applyTask ((parallel "andTask" (\list -> length list >= 1 && activated) id id [t <<@ l \\(l,t) <- taskCollection])))) {tst & activated = True}

	