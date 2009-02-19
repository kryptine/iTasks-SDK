definition module ShopDSL

import iTasks, database

//	The Domain Specific Language for the shop workflow case.
:: Product			=	{ id_				:: !ProductId
						, name				:: !String
						, price				:: !HtmlCurrency
						, amount			:: !Int
						}
:: ProductId 		:== DBRef Product

:: Cart items		:== [(CartItem items, CartAmount)]

:: CartItem	items	=	{ id_				:: !CartId items
						, itemNr			:: !DBRef items
						, description		:: !String
						, amountInStock		:: !Int
						, amountOrdered		:: !Int
						, pricePerUnit		:: !HtmlCurrency
						}
:: CartAmount		=	{ orderAmount		:: !Int }
:: CartId items		:== DBRef (Cart items)

:: Order items		=	{ id_				:: !OrderId items
						, name				:: !String
						, itemsOrdered		:: !Cart items
						, billingAddress	:: !Address
						, shippingAddress	:: !Address
						}
:: OrderId items	:== DBRef (Order items)

:: Address			=	{ street			:: !String
						, number			:: !String
						, postalCode		:: !String
						, city				:: !String
						}

:: ShopAction		= LeaveShop | ToCart | ToPay | ToShop

derive gForm	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction
derive gUpd		DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction
derive gPrint	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction
derive gParse	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction

//	Boilerplate code for the above data types:
billingAddressOf	:: !(Order a) -> Address
shippingAddressOf	:: !(Order a) -> Address
billingAddressUpd	:: !(Order a) !Address -> Order a
shippingAddressUpd	:: !(Order a) !Address -> Order a

class    nameOf a :: a -> String
instance nameOf (Order items)
instance nameOf Product

class    nameUpd a :: a String -> a
instance nameUpd Product
instance nameUpd (Order items)

class    toCart a :: Int a -> CartItem a
instance toCart Product

instance DB Product
instance DB (Order items)
