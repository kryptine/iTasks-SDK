definition module ShopDSL

import iTasks, database, ShopDSLboilerplate

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

//	Conversions between DSL data types:
class    toInCart   a :: a -> InCart
class    toCartItem a :: a -> CartItem a

instance toInCart   (CartItem a)
instance toCartItem Product

//	Database operations on DSL data types:
instance DB Product
instance DB (Order a)

eqItemNr			:: !(CartItem a) !(CartItem a) -> Bool
