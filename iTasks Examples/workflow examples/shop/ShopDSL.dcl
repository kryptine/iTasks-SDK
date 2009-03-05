definition module ShopDSL

import iTasks, database

//	The Domain Specific Language for the shop workflow case.
:: Product			=	{ id_				:: !DBRef Product
						, name				:: !String
						, author            :: !String
						, price				:: !HtmlCurrency
						, amount			:: !Int
						}
:: Cart     item	:== [CartItem item]
:: CartItem	item	=	{ itemNr			:: !DBRef item
						, description		:: !String
						, amountInStock		:: !Int
						, amountOrdered		:: !Int
						, pricePerUnit		:: !HtmlCurrency
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
:: ShopAction		=	LeaveShop | ToCart | ToPay | ToShop
:: InCart			=	{ item				:: !String
						, nrOrdered			:: !Int
						, pricePerItem		:: !HtmlCurrency
						}

derive gForm	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart
derive gUpd		DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart
derive gPrint	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart
derive gParse	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart

class toInCart a	:: a -> InCart
instance toInCart (CartItem a)

//	Boilerplate code for the above data types:
billingAddressOf	:: !(Order a) -> Address
shippingAddressOf	:: !(Order a) -> Address
billingAddressUpd	:: !(Order a) !Address -> Order a
shippingAddressUpd	:: !(Order a) !Address -> Order a
itemNrOf			:: !(CartItem a) -> DBRef a
itemNrUpd			:: !(CartItem a) (DBRef a) -> CartItem a
amountOrderedOf		:: !(CartItem a) -> Int
amountOrderedUpd	:: !(CartItem a) !Int -> CartItem a

eqItemNr			:: !(CartItem a) !(CartItem a) -> Bool

class    nameOf  a :: a -> String
class    nameUpd a :: a String -> a
class    id_Of   a :: a -> DBRef a
class    id_Upd  a :: a (DBRef a) -> a

instance nameOf     Product
instance nameUpd    Product
instance id_Of      Product
instance id_Upd     Product

instance nameOf     (Order item)
instance nameUpd    (Order item)
instance id_Of      (Order item)
instance id_Upd     (Order item)

class    toCartItem a :: a -> CartItem a
instance toCartItem Product

instance DB Product
instance DB (Order item)
