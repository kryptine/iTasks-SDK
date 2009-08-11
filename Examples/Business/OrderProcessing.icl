implementation module OrderProcessing

import iTasks

/*
 * The scenario contains a number of different events and activities:
 * The customer places an order and fills out her credit card number. 
 * Your web system checks that the credit card is valid and sends a 
 * request to the bank for the money. After a while a confirmation 
 * arrives from the bank telling the system that there are sufficient 
 * funds in the account. The system sends a notification to storage 
 * with the customers address along with information on what the 
 * customer ordered. The storage personnel package the order and send 
 * it off with the delivery service. They also log into the system and 
 * confirm that the order has been sent. The system sends an email with 
 * the confirmation to the customer. 
 */

orderProcessingExample :: [Workflow]
orderProcessingExample
= [	{	name		= "Examples/Business/Order processing"
	,	label		= "Order processing"
	,	roles		= []
	,	mainTask	= orderPlacement customer >>| return Void
	}
  ]
	
orderPlacement :: UserId -> Task Void
orderPlacement user =
  customer @:  ( "Order items from shop"
               , orderItemsFromShop -&&- fillInAndCheckCreditCard createDefault
               ) >>= \(basket, cardInfo) ->
  customer @:  ( "Order confirmation"
               , confirmOrder
               ) >>| 
  bank     @:  ("Cash request"
               , cashRequest (amountFrom basket) cardInfo
               ) >>= \granted ->
  if granted
    (storage  @:  ( "Deliver order"
                  , deliverOrder basket (deliveryAddress cardInfo)
                  ) >>|
     customer @:  ( "Delivery confirmation"
                  , confirmDelivery basket (deliveryAddress cardInfo)
                  )
    )
    (customer @:  ( "Delivery failure"
                  , failedDelivery basket (deliveryAddress cardInfo)
                  )
    ) 
									  
fillInAndCheckCreditCard :: CardInfo -> Task CardInfo
fillInAndCheckCreditCard cardInfo =
  fillInCreditCard cardInfo >>= \cardInfo ->
  creditcard @: ( "Validate credit card"
                , validateCreditCard cardInfo
                ) >>= \valid ->
  if valid
    (return cardInfo)
    (invalidCreditCard cardInfo >>| 
     fillInAndCheckCreditCard cardInfo
    )
												
// Low level definitions
customer   :== 10
bank       :== 11
storage    :== 12
creditcard :== 13 // not used at the moment (see validateCreditCard)

:: CardInfo :== String
:: Address  :== CardInfo

deliveryAddress :: CardInfo -> String
deliveryAddress cardInfo = "delivery address: " +++ cardInfo

:: ItemInfo :== (Int, String, Real)
:: Basket   :== ItemInfo

amountFrom :: ItemInfo -> Real
amountFrom (amount, _,price) = toReal amount * price

items :: [ItemInfo]
items = map (\(descr, price) -> (1, descr, price)) descrs
  where 
    descrs :: [(String, Real)]
    descrs = [ ("The Shawshank Redemption", 4.99)
             , ("O Brother Where Art Thou", 6.99)
             , ("Bambi", 9.99)
             ]    

orderItemsFromShop :: Task ItemInfo
orderItemsFromShop
	= updateInformation
		"Please select how many items you would like to order from our shop:"
		(hd items)

fillInCreditCard :: CardInfo -> Task CardInfo
fillInCreditCard cardInfo
	= updateInformation
		"Please fill in your credit card number (at least 5 digits):"
		cardInfo
		
validateCreditCard :: CardInfo -> Task Bool
validateCreditCard cardInfo = return (size cardInfo == 5)

invalidCreditCard :: CardInfo -> Task Void	
invalidCreditCard cardInfo
	= showMessage "Your credit card was invalid!"

confirmOrder :: Task Void
confirmOrder
	= showMessage "Your order will be processed!"

cashRequest :: Real CardInfo -> Task Bool
cashRequest amount cardInfo
	= requestConfirmation ("Can we subtract " <+++ amount <+++ " from card " <+++ cardInfo <+++ "?")
    
deliverOrder :: Basket Address -> Task Void 
deliverOrder basket address
	= showMessage ("Please deliver " <+++ basket <+++ " to " <+++ address <+++ ".")

confirmDelivery :: Basket Address -> Task Void
confirmDelivery basket address
	= showMessage ("Your order " <+++ basket <+++ " will be delivered to " <+++ address <+++ ".")

failedDelivery :: Basket Address -> Task Void
failedDelivery basket address
	= showMessage ("Your order " <+++ basket <+++ " cannot be delivered to " <+++ address <+++ ".")
