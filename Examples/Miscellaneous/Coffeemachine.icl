implementation module Coffeemachine

import iTasks

coffeemachineExample :: [Workflow]
coffeemachineExample = [ workflow "Examples/Miscellaneous/Coffeemachine" "A coffee machine demo" coffeemachine ]

coffeemachine :: Task (String,Currency)
coffeemachine  =				enterChoice ("Product","Choose your product")
									[("Coffee", EUR 100)
									,("Cappucino", EUR 150)
									,("Tea", EUR 50)
									,("Chocolate", EUR 100)
									] 
	>>= \(product,toPay) ->		getCoins product (toPay,EUR 0)

getCoins :: String (Currency,Currency) -> Task (String,Currency)
getCoins product (cost,paid) = getCoins`
where
	getCoins`		
		=			enterChoiceA  ("Insert coins",[ Text ("Chosen product: " <+++ product), BrTag[]
					              , Text ("To pay: " <+++ cost), BrTag []
					              , Text "Please insert a coin..."
					              ]) id actions coins 
		>>= 	handleMoney

	actions = [(ActionCancel, always), (ActionOk, always)]
	coins	= [EUR 5,EUR 10,EUR 20,EUR 50,EUR 100,EUR 200]

	handleMoney ((ActionCancel,_), coin)
					= show "Cancelled" paid
	handleMoney (_, coin) 
	| cost > coin	= getCoins product (cost-coin, paid+coin)
	| otherwise		= show product (coin-cost)
	
	show product money = showMessage ("Coffemaker+",[ Text ("product = " <+++ product) , BrTag[]
									 , Text ("money returned = " <+++ money)
									 ]) (product,money)
