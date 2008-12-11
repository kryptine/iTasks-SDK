module internetShop

import StdEnv, StdiTasks, iDataTrivial

/*
The scenario contains a number of different events and activities:
The customer places an order and fills out her credit card number. 
Your web system checks that the credit card is valid and sends a request to the bank for the money. 
After a while a confirmation arrives from the bank telling the system that there are sufficient funds in the account. 
The system sends a notification to storage with the customers address along with information on what the customer ordered. 
The storage personnel package the order and send it off with the delivery service. They also log into the system and confirm that the order has been sent. 
The system sends an email with the confirmation to the customer. 
*/

// high level

Start :: *World -> *World
Start world = startTaskEngine (orderPlacement user) world

orderPlacement user
	= 								(user 		@:> ("Place order",(selectFromShop -&&- fillInAndCheckCreditCard createDefault)))
		=>> \(basket,cardInfo) ->	user 		@:> ("Order Confirmation", confirmOrder basket cardInfo)
		=>> \_ ->					bank 		@:> ("Cash Request", cashRequest bank cardInfo (amountFrom basket))
		=>> \granted ->				if granted
									 (storage	@:> ("Order Delivery Request", deliverOrder user basket (deliveryAddress cardInfo)) #>>
									  user		@:> ("Delivery Notice",  deliverOKNotice 	 user basket (deliveryAddress cardInfo)))
									 (user		@:> ("Delivery Failure", deliverFailureNotice user basket (amountFrom basket))) 
									  
fillInAndCheckCreditCard :: CardInfo -> Task CardInfo
fillInAndCheckCreditCard cardInfo 
	=							fillInCreditCard cardInfo
		=>> \cardInfo ->		webSystem 	@: ("Validate CreditCard",validateCreditCard cardInfo)
		=>> \valid ->			if valid
									(return_V cardInfo)
									(invalidCreditcard cardInfo #>> fillInAndCheckCreditCard cardInfo)
									
// low level

:: CardInfo :== String

user 		:== 10
bank 		:== 11
storage 	:== 12
webSystem	:== 13

items = [(DisplayMode "Apples", 1.0,DisplayMode 3.50),(DisplayMode "Pears", 1.0,DisplayMode 2.50)]

instance toString (DisplayMode a) | toString a
where
	toString (DisplayMode a) = toString a

instance toString (a,b,c)  | toString a & toString b & toString c
where
	toString (a,b,c) = "(" <+++ a <+++ "," <+++ b <+++ "," <+++ c <+++ ")" 

amountFrom (item,amount,DisplayMode price) = amount * price

deliveryAddress cardInfo	= "delivery address " +++ cardInfo


selectFromShop 
	=					[Text "Please select an item from our shop"]
						?>> editTask "Ok" (DisplayMode "Appels", 1.0,DisplayMode 3.50)

fillInCreditCard cardInfo
	=					[Text "Please fill in your credit card number"]
						?>> editTask "Ok" cardInfo

validateCreditCard cardInfo
	=					return_V (size cardInfo == 5)	//Arbitrary condition to decide if a card is valid
	
invalidCreditcard cardInfo 
	=					[Text "Your credit card was invalid!"]
						?>> ok

confirmOrder basket cardInfo
	=					[Text "Your order will be processed!"]
						?>> ok

cashRequest bank cardInfo amount
	=					[Text ("Can we get " <+++ amount <+++ " from " <+++ cardInfo)]
						?>> yesOrNo

deliverOrder user basket address
	=					[Text ("Please deliver " <+++ basket <+++ " to " <+++ address)]
						?>> ok

deliverOKNotice user basket address
	=					[Text ("Your order " <+++ basket <+++ " will be delivered to " <+++ address)]
						?>> ok

deliverFailureNotice  user basket address
	=					[Text ("Your order " <+++ basket <+++ " cannot be delivered to " <+++ address)]
						?>> ok

ok
	= editTask "Ok" Void
yesOrNo
	= (editTask "Yes" Void #>> return_V True) -||- (editTask "No" Void #>> return_V False)

