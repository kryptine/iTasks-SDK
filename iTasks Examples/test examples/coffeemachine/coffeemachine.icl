module coffeemachine

// (c) MJP 2007
//
// This is a demo of a coffeemachine programmed with iTasks combinators.
// The persistent variant remembers the state in which the coffee machine was left.
// Garbage collection of unused tasks is done automatically.

// Some alternative coffee machine definitions have been added as example for the ICFP07 paper.

import StdEnv, iTasks, iDataTrivial

Start world =  startEngine [{name = "coffeemachine",label="Coffeemachine",roles=[],mainTask=(foreverTask CoffeeMachine) #>> return_V Void}] world

CoffeeMachine :: Task (String,Int)
CoffeeMachine  
=								chooseTask [Text "Choose product:",Br,Br] 
									[("Coffee: 100",    return_V (100,"Coffee"))
									,("Cappucino: 150", return_V (150,"Cappucino"))
									,("Tea: 50",        return_V (50, "Tea"))
									,("Chocolate: 100", return_V (100,"Chocolate"))
									] 
	=>> \(toPay,product) ->		[Text ("Chosen product: " <+++ product),Br,Br] 
								?>>	getCoins (toPay,0)
	=>> \(cancel,returnMoney) ->let nproduct = if cancel "Cancelled" product in
								[Text ("product = " <+++ nproduct <+++ ", returned money = " <+++ returnMoney),Br,Br] 
								?>>	buttonTask "Thanks" (return_V (nproduct,returnMoney))

getCoins :: (Int,Int) -> Task (Bool,Int)
getCoins (cost,paid) = getCoins`
where
	getCoins`		
	= 						chooseTask [Text ("To pay: " <+++ cost),Br,Br] 
					  		[(c +++> " cents", return_V (False,c)) \\ c <- coins]
						  	-||-
						  	buttonTask "Cancel" (return_V (True,0))
		=>> handleMoney

	handleMoney (cancel,coin)
	| cancel		= return_V (cancel,   paid)
	| cost > coin	= getCoins (cost-coin,paid+coin)
	| otherwise		= return_V (cancel,   coin-cost)

	coins			= [5,10,20,50,100,200]

//	getCoins2 is alternative definition of getCoins, but uses repeatTask instead of direct recursion

getCoins2 :: ((Bool,Int,Int) -> Task (Bool,Int,Int))
getCoins2 			= repeatTask get (\(cancel,cost,paid) -> cancel || cost <= 0)
where
	get (cancel,cost,paid)
	= 						chooseTask[Text ("To pay: " <+++ cost),Br,Br]
					 		[(c +++> " cents", return_V (False,c)) \\ c <- coins]
					  		-||-
					  		buttonTask "Cancel" (return_V (True,0))
		=>> \(cancel,c) ->	return_V (cancel,cost-c,paid+c)

	coins			= [5,10,20,50,100,200]

// for the ICFP07 paper: a single step coffee machine

singleStepCoffeeMachine :: Task (String,Int)
singleStepCoffeeMachine
=						chooseTask [Text "Choose product:",Br,Br] 
						[(p<+++": "<+++c, return_V prod) \\ prod=:(p,c)<-products]
	=>> \prod=:(p,c) -> [Text ("Chosen product: "<+++p),Br,Br] 
						?>>	pay prod (buttonTask "Thanks" (return_V prod))
where
	products	= [("Coffee",100),("Tea",50)]
	
//	version using labeled action:
//	pay (p,c) t	= buttonTask ("Pay "<+++c<+++ " cents") t
//	version using getCoins:
/*	pay (p,c) t	= getCoins (c,0) =>> \(cancel,returnMoney) ->
				  [Text ("Product = "<+++if cancel "cancelled" p
				                    <+++". Returned money = "<+++returnMoney),Br,Br] 
				  ?>> t
*/
//	version using getCoins2:
	pay (p,c) t	= getCoins2 (False,c,0) =>> \(cancel,_,paid) ->
				  if cancel [Text ("Cancelled. Your money = "<+++paid),Br,Br]
				            [Text ("Product = "<+++p<+++". Returned money ="<+++(paid-c)),Br,Br]
				  ?>> t


// A very simple coffee machine

SimpleCoffee :: Task Void
SimpleCoffee
= 								chooseTask [Text "Choose product:",Br,Br] 
									[("Coffee",	return_V ("Coffee"))
									,("Tea",	return_V ("Tea"))
									]	
	=>>  \(product) ->			[Text ("Enjoy your " <+++ product)]
								?>> buttonTask "OK" (return_V Void)

// and another one

SimpleCoffee2 :: Task Void
SimpleCoffee2
= 								chooseTask [Text "Choose product:",Br,Br] 
									[("Coffee: 20", return_V (20,"Coffee"))
									,("Tea: 10", 	return_V (10,"Tea"))
									]	
	=>>  \(toPay,product) ->	payDimes toPay 
	#>>							[Text ("Enjoy your " <+++ product)]
								?>> buttonTask "OK" (return_V Void)
where
	payDimes 0 = 							return_V Void
	payDimes n = 							buttonTask "10 cts" (return_V Void) 
					#>> payDimes (n - 10)


Br = BrTag []