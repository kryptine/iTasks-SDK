implementation module BasicAPIExamples.SequentialExamples.CoffeeMachine

// A coffee machine simulator

import iTasks
import iTasks.Extensions.Currency, Text.HTML

wf :: String -> Workflow
wf a = workflow a "Calculator" coffeemachine

main :: Task ()
main = coffeemachine @! ()

coffeemachine :: Task (String,EUR)
coffeemachine
	=
	forever
	( 	Title "Product" @>> Hint "Choose your product:" @>> enterChoice []
					[("Coffee", EUR 100)
					,("Cappucino", EUR 150)
					,("Tea", EUR 50)
					,("Chocolate", EUR 100)
					]
	>>!  getCoins (EUR 0)
	>>!	 return
	)

getCoins :: EUR (String,EUR) -> Task (String,EUR)
getCoins paid (product,toPay)
	= 				(Title "Coffee Machine" @>> viewInformation [ViewAs view1] toPay)
					||-
					(Title "Insert coins" @>> Hint "Please insert a coin..." @>> enterChoice  [ChooseFromCheckGroup id] coins)
			>>*		[ OnAction ActionCancel 	 (always (stop ("Product Cancelled",paid)))
					, OnAction (Action "Insert") (hasValue handleMoney)
					]
where
	coins	= [EUR 5,EUR 10,EUR 20,EUR 50,EUR 100,EUR 200]

	handleMoney coin
	| toPay > coin	= getCoins (paid+coin) (product, toPay-coin)
	| otherwise		= stop (product,coin-toPay)

	stop (product, money) = Title "Coffee Machine" @>> viewInformation [ViewAs view2] (product,money)

	view1 toPay 		   = [(DivTag [] [Text ("Chosen product: " <+++ product), BrTag [], Text ("To pay: " <+++ toPay)])]
	view2 (product,money)  = [(DivTag [] [Text ("Enjoy your: " <+++ product), BrTag [], Text ("Money returned: " <+++ money)])]
