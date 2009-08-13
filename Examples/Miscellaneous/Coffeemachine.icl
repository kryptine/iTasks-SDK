implementation module Coffeemachine

// (c) MJP 2007
//
// This is a demo of a coffeemachine programmed with iTasks combinators.
// The persistent variant remembers the state in which the coffee machine was left.
// Garbage collection of unused tasks is done automatically.

// Some alternative coffee machine definitions have been added as example for the ICFP07 paper.

import iTasks
import CommonDomain

coffeemachineExample :: [Workflow]
coffeemachineExample = [{ name = "Examples/Miscellaneous/Coffeemachine"
						, label = "Coffeemachine"
						, roles =[]
						, mainTask =(forever coffeemachine) >>| return Void
						}]
coffeemachine :: Task (String,Currency)
coffeemachine  =				enterChoice "Choose product"
									[("Coffee", EUR 100)
									,("Cappucino", EUR 150)
									,("Tea", EUR 50)
									,("Chocolate", EUR 100)
									] 
	>>= \(product,toPay) ->		getCoins product (toPay,EUR 0)
	>>= \(cancel,returnMoney) ->let nproduct = if cancel "Cancelled" product in
								showMessage ("product = " <+++ nproduct <+++ ", returned money = " <+++ returnMoney)
									>>| return (nproduct,returnMoney) 

getCoins :: String (Currency,Currency) -> Task (Bool,Currency)
getCoins product (cost,paid) = getCoins`
where
	getCoins`		
		=  					(enterChoice [ Text ("Chosen product: " <+++ product), BrTag[]
							              , BrTag []
							              , Text ("To pay: " <+++ cost), BrTag []
							              , Text "Please insert a coin..."
							              ] coins >>= \c -> return (False,c))
						  	-||-
							((requestConfirmation "...or do you want to stop and get your money back?" <! id )>>| return (True, EUR 0))
		>>= handleMoney

	handleMoney (cancel,coin)
		| cancel		= return (cancel,   paid)
		| cost > coin	= getCoins product (cost-coin,paid+coin)
		| otherwise		= return (cancel,   coin-cost)
	
	coins			= [EUR 5,EUR 10,EUR 20,EUR 50,EUR 100,EUR 200]

//	getCoins2 is alternative definition of getCoins, but uses repeatTask instead of direct recursion

getCoins2 :: ((Bool,Int,Int) -> Task (Bool,Int,Int))
getCoins2 			= repeatTask get (\(cancel,cost,paid) -> cancel || cost <= 0)
where
	get (cancel,cost,paid)
	= 						chooseTask[Text ("To pay: " <+++ cost),Br,Br]
					 		[(c +++> " cents", return (False,c)) \\ c <- coins]
					  		-||-
					  		buttonTask "Cancel" (return (True,0))
		>>= \(cancel,c) ->	return (cancel,cost-c,paid+c)

	coins			= [5,10,20,50,100,200]

// for the ICFP07 paper: a single step coffee machine

singleStepCoffeeMachine :: Task (String,Int)
singleStepCoffeeMachine
=						chooseTask [Text "Choose product:",Br,Br] 
						[(p<+++": "<+++c, return prod) \\ prod=:(p,c)<-products]
	>>= \prod=:(p,c) -> [Text ("Chosen product: "<+++p),Br,Br] 
						?>>	pay prod (buttonTask "Thanks" (return prod))
where
	products	= [("Coffee",100),("Tea",50)]
	
//	version using labeled action:
//	pay (p,c) t	= buttonTask ("Pay "<+++c<+++ " cents") t
//	version using getCoins:
/*	pay (p,c) t	= getCoins (c,0) >>= \(cancel,returnMoney) ->
				  [Text ("Product = "<+++if cancel "cancelled" p
				                    <+++". Returned money = "<+++returnMoney),Br,Br] 
				  ?>> t
*/
//	version using getCoins2:
	pay (p,c) t	= getCoins2 (False,c,0) >>= \(cancel,_,paid) ->
				  if cancel [Text ("Cancelled. Your money = "<+++paid),Br,Br]
				            [Text ("Product = "<+++p<+++". Returned money ="<+++(paid-c)),Br,Br]
				  ?>> t


// A very simple coffee machine

SimpleCoffee :: Task Void
SimpleCoffee
= 								chooseTask [Text "Choose product:",Br,Br] 
									[("Coffee",	return ("Coffee"))
									,("Tea",	return ("Tea"))
									]	
	>>=  \(product) ->			[Text ("Enjoy your " <+++ product)]
								?>> buttonTask "OK" (return Void)

// and another one

SimpleCoffee2 :: Task Void
SimpleCoffee2
= 								chooseTask [Text "Choose product:",Br,Br] 
									[("Coffee: 20", return (20,"Coffee"))
									,("Tea: 10", 	return (10,"Tea"))
									]	
	>>=  \(toPay,product) ->	payDimes toPay 
	>>|							[Text ("Enjoy your " <+++ product)]
								?>> buttonTask "OK" (return Void)
where
	payDimes 0 = 							return Void
	payDimes n = 							buttonTask "10 cts" (return Void) 
					>>| payDimes (n - 10)


Br = BrTag []