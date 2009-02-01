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

import StdEnv, iTasks, iData



//Main types
:: Purchase	=	{	name	:: !String
				,	amount	:: !Int
				,	express	:: !Bool
				,	note	:: !HtmlTextarea
				}
				
//Generic derives
derive gForm	Purchase, []
derive gUpd		Purchase, []
derive gPrint	Purchase
derive gParse	Purchase

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
	
definePurchase :: Task Purchase
definePurchase = 
	[Text "Please describe the product you would like to purchase"]
	?>> editTask "Done" createDefault

selectSuppliers :: Task [(Int,String)]
selectSuppliers
	= getUsersWithRoleTask "supplier" =>> \suppliers ->
	  ( mchoiceAndTasks
	  		[Text "Select the suppliers from which you want to receive a bid"]
	  		[(label, return_V supplier) \\ supplier =: (uid, label) <- suppliers]
	  )
	
collectBids :: Purchase [(Int,String)] -> Task [((Int,String),Real)]
collectBids purchase suppliers
	= andTasksEnough
		[("Bid for " +++ purchase.Purchase.name +++ " from " +++ name, uid @: ("Bid request regarding " +++ purchase.Purchase.name, collectBid purchase supplier)) \\ supplier =: (uid,name) <- suppliers]
where
	collectBid :: Purchase (Int,String) -> Task ((Int,String),Real)
	collectBid purchase bid
		= [Text "Please make a bid to supply the following product"]
		  ?>> (
		  	displayValue purchase
		  	-|||-
		  	(editTask "Ok" createDefault =>> \price -> return_V (bid, price))
		  )
	
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
			[Text "Please select one of the following bids"]
			[(name +++ " " +++ toString price, return_V bid) \\ bid =: ((uid,name),price) <- bids] 
		)
where
	determineCheapest bids = return_V (hd (sortBy (\(_,x) (_,y) -> x < y) bids))
	yesOrNo = (editTask "Yes" Void #>> return_V True) -||- (editTask "No" Void #>> return_V False)
	
confirmBid :: Purchase ((Int,String),Real) -> Task Void
confirmBid purchase bid =: ((uid,label),price)
	= uid @: ("Bid confirmation",(
		[Text "Your bid of ", Text (toString price),Text " for the product ",ITag [] [Text purchase.Purchase.name], Text " has been accepted."]
		?>> editTask "Ok" Void
	))
	
//Custom utility combinators 
andTasksEnough:: ![LabeledTask a] -> (Task [a]) | iData a
andTasksEnough taskCollection = mkParallelTask "andTasksEnough" (Task andTasksEnough`)
where
	andTasksEnough` tst
		# tst					= setCombination (TTCustom (\list -> flatten (reverse list))) tst	//Show parallel sub tasks in reversed order
		# (_,tst=:{activated})	= accTaskTSt (mkParallelSubTask "enough" 0 ([Text "Stop if enough results are returned..."] ?>> editTask "Enough" Void )) tst
		= accTaskTSt (mkParallelSubTask "tasks" 1 (allTasksCond "andTask" (TTSplit msg) (\list -> length list >= 1 && activated) taskCollection)) {tst & activated = True}

	msg = [Text "This task is waiting for the completion of the following tasks:"]


(-|||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iData a
(-|||-) top bottom
	= allTasksCond "-|||-" TTVertical (\l -> length l == 1) [("top",top),("bottom",bottom)] =>> \firstDone -> return_V (hd firstDone)
	