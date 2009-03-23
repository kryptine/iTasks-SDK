definition module ShopDSL

import iTasks, ShopDSLboilerplate

//	The Domain Specific Language for the shop workflow case.
class Product a | nameOf, priceOf, id_Of, inStockOf a
class InCart  a | nameOf, priceOf, amountOrderedOf  a

:: Book				=	{ id_				:: !DBRef Book
						, title				:: !String
						, author            :: !String
						, price				:: !HtmlCurrency
						, inStock			:: !Int
						}
:: Cart     item	:== [CartItem item]
:: CartItem	item	=	{ itemNr			:: !DBRef item
						, name				:: !String
						, inStock			:: !Int
						, amountOrdered		:: !Int
						, price				:: !HtmlCurrency
						}
:: CartAmount		=	{ orderAmount		:: !Int
						}
:: Order item		=	{ id_				:: !DBRef (Order item)
						, name				:: !String
						, itemsOrdered		:: !Cart item
						, billingAddress	:: !Address
						, shippingAddress	:: !Address
						}
:: Address			=	{ street			:: !String
						, number			:: !String
						, postalCode		:: !String
						, city				:: !String
						}
:: InCart			=	{ name				:: !String
						, amountOrdered		:: !Int
						, price				:: !HtmlCurrency
						}
:: ShopAction		=	LeaveShop | ToCart | ToPay | ToShop

defaultProduct		:: Book
defaultCart			:: Cart Book

//	Conversions between DSL data types:
toCartItem			:: a -> CartItem a | Product a
toInCart			:: a -> InCart     | InCart  a

//	Database operations on DSL data types:
instance DB Book
instance DB (Order a)

eqItemNr			:: !(CartItem item) !(CartItem item) -> Bool
totalCost			:: [a] -> HtmlCurrency | priceOf, amountOrderedOf a
shopOwner			:: UserId
