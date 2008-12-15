module internetShop

import iDataTrivial, StdEnv, StdiTasks

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

// High level definitions
Start :: *World -> *World
Start world = startTaskEngine (orderPlacement user) world

orderPlacement :: UserId -> Task Void
orderPlacement user =
  user @:> ( "Order items from shop"
           , orderItemsFromShop -&&- fillInAndCheckCreditCard createDefault
           ) =>> \(basket, cardInfo) ->
  user @:> ( "Order confirmation"
           , confirmOrder
           ) #>> 
  bank @:> ("Cash request"
           , cashRequest (amountFrom basket) cardInfo
           ) =>> \granted ->
  if granted
    (storage @:> ( "Deliver order"
                 , deliverOrder basket (deliveryAddress cardInfo)
                 ) #>>
     user    @:> ( "Delivery confirmation"
                 , confirmDelivery basket (deliveryAddress cardInfo)
                 )
    )
    (user @:> ( "Delivery failure"
              , failedDelivery basket (deliveryAddress cardInfo)
              )
    ) 
									  
fillInAndCheckCreditCard :: CardInfo -> Task CardInfo
fillInAndCheckCreditCard cardInfo =
  fillInCreditCard cardInfo =>> \cardInfo ->
  webSystem @: ( "Validate credit card"
               , validateCreditCard cardInfo
               ) =>> \valid ->
  if valid
    (return_V cardInfo)
    (invalidCreditCard cardInfo #>> 
     fillInAndCheckCreditCard cardInfo
    )
			
									
// Low level definitions
user 		:== 10 // customer
bank 		:== 11 // bank
storage 	:== 12 // webshop
webSystem	:== 13 // mastercard

:: CardInfo :== String
:: Address  :== CardInfo

deliveryAddress :: CardInfo -> String
deliveryAddress cardInfo = "delivery address: " +++ cardInfo

:: ItemInfo :== (Int, DisplayMode String, DisplayMode Real)
:: Basket   :== ItemInfo

instance toString (DisplayMode a) | toString a where
  toString (DisplayMode a) = toString a

instance toString (a, b, c) | toString a & toString b & toString c where
  toString (a, b, c) = "(" <+++ a <+++ ", " <+++ b <+++ ", " <+++ c <+++ ")" 
  
amountFrom :: ItemInfo -> Real
amountFrom (amount, _, DisplayMode price) = toReal amount * price

items :: [ItemInfo]
items = map (\(descr, price) -> (1, DisplayMode descr, DisplayMode price)) descrs
  where 
    descrs :: [(String, Real)]
    descrs = [ ("The Shawshank Redemption", 4.99)
             , ("O Brother Where Art Thou", 6.99)
             , ("Bambi", 9.99)
             ]    

orderItemsFromShop :: Task ItemInfo
orderItemsFromShop =
  [Text "Please select how many items you would like to order from our shop:"]
    ?>> editTask "Ok" (hd items)

fillInCreditCard :: CardInfo -> Task CardInfo
fillInCreditCard cardInfo =
  [Text "Please fill in your credit card number (at least 5 digits):"]
    ?>> editTask "Ok" cardInfo

validateCreditCard :: CardInfo -> Task Bool
validateCreditCard cardInfo = return_V (size cardInfo == 5)

invalidCreditCard :: CardInfo -> Task Void	
invalidCreditCard cardInfo = 
  [Text "Your credit card was invalid!"] 
    ?>> ok

confirmOrder :: Task Void
confirmOrder = 
  [Text "Your order will be processed!"]
    ?>> ok

cashRequest :: Real CardInfo -> Task Bool
cashRequest amount cardInfo =
  [Text ("Can we subtract " <+++ amount <+++ " from card " <+++ cardInfo <+++ "?")]
    ?>> editTask "Ok" False
    
deliverOrder :: Basket Address -> Task Void 
deliverOrder basket address =
  [Text ("Please deliver " <+++ basket <+++ " to " <+++ address <+++ ".")]
    ?>> ok

confirmDelivery :: Basket Address -> Task Void
confirmDelivery basket address =
  [Text ("Your order " <+++ basket <+++ " will be delivered to " <+++ address <+++ ".")]
    ?>> ok

failedDelivery :: Basket Address -> Task Void
failedDelivery basket address =
  [Text ("Your order " <+++ basket <+++ " cannot be delivered to " <+++ address <+++ ".")]
    ?>> ok

ok :: Task Void
ok = editTask "Ok" Void