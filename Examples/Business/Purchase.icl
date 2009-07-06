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

//Additional imports for custom combinator creation
from TSt 		import applyTask, setCombination, mkSequenceTask, mkParallelTask
from TSt 		import :: TSt{..}, :: TaskInfo{..}, :: StaticInfo{..}, :: Options{..}
from SessionDB	import :: Session
from Types		import :: ProcessId, :: TaskNr
from TaskTree	import :: TaskTree

//Main types
:: Purchase	=	{	name	:: !String
				,	amount	:: !Int
				,	express	:: !Bool
				,	note	:: !HtmlTextarea
				}
				
//Generic derives
derive gForm	Purchase
derive gUpd		Purchase
derive gPrint	Purchase
derive gParse	Purchase

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
definePurchase = 
	[Text "Please describe the product you would like to purchase"]
	?>> edit ok snd createDefault

selectSuppliers :: Task [(Int,String)]
selectSuppliers
	= getUsersWithRole "supplier" >>= \suppliers ->
//	= getUsers >>= \suppliers ->
	  ( mchoiceAndTasks
	  		[Text "Select the suppliers from which you want to receive a bid"]
	  		[(label, return supplier) \\ supplier =: (uid, label) <- suppliers]
	  )
	
collectBids :: Purchase [(Int,String)] -> Task [((Int,String),HtmlCurrency)]
collectBids purchase suppliers
	= andTasksEnough
		[("Bid for " +++ purchase.Purchase.name +++ " from " +++ name, uid @: ("Bid request regarding " +++ purchase.Purchase.name, collectBid purchase supplier)) \\ supplier =: (uid,name) <- suppliers]
where
	collectBid :: Purchase (Int,String) -> Task ((Int,String),HtmlCurrency)
	collectBid purchase bid
		= [Text "Please make a bid to supply the following product"]
		  ?>> 
		  (displayValue purchase -||- (editTask "Ok" createDefault >>= \price -> return (bid, price)) <<@ TTVertical)
		  	
	
selectBid :: [((Int,String),HtmlCurrency)] -> Task ((Int,String),HtmlCurrency)
selectBid bids
	= determineCheapest bids	>>= \cheapestBid =: ((uid,name),price) ->	
	[ Text "The cheapest bid is ", Text (toString price), Text " by ", Text name, BrTag [],
	  Text "Do you want to accept this bid?", BrTag []]
	?>>
	yesOrNo >>= \acceptCheapest ->
	if acceptCheapest
		( return cheapestBid)
		( chooseTask
			[Text "Please select one of the following bids"]
			[(name +++ " " +++ toString price, return bid) \\ bid =: ((uid,name),price) <- bids] 
		)
where
	determineCheapest bids = return (hd (sortBy (\(_,x) (_,y) -> x < y) bids))
	yesOrNo = (editTask "Yes" Void >>| return True) -||- (editTask "No" Void >>| return False)
	
confirmBid :: Purchase ((Int,String),HtmlCurrency) -> Task Void
confirmBid purchase bid =: ((uid,label),price)
	= uid @: ("Bid confirmation",(
		[Text "Your bid of ", Text (toString price),Text " for the product ",ITag [] [Text purchase.Purchase.name], Text " has been accepted."]
		?>> editTask "Ok" Void
	))
	
//Custom utility combinators 
andTasksEnough:: ![LabeledTask a] -> (Task [a]) | iData a
andTasksEnough taskCollection = mkParallelTask "andTasksEnough" andTasksEnough`
where
	andTasksEnough` tst
		# tst						= setCombination TTVertical  tst	//Show parallel sub tasks in reversed order
		# (_,tst =:{activated})		= applyTask (mkSequenceTask "enough" (applyTask ([Text "Stop if enough results are returned..."] ?>> editTask "Enough" Void))) tst
		= applyTask (mkSequenceTask "tasks" (applyTask ((parallel "andTask" (\list -> length list >= 1 && activated) id id [t <<@ l \\(l,t) <- taskCollection]) <<@ (TTSplit msg)))) {tst & activated = True}

	msg = [Text "This task is waiting for the completion of the following tasks:"]

	