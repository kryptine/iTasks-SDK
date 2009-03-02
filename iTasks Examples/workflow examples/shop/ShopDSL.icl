implementation module ShopDSL

import StdClass, StdInt
import GenBimap
import iTasks, database

derive gForm	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction
derive gUpd		DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction
derive gPrint	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction
derive gParse	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction

billingAddressOf :: !(Order a) -> Address
billingAddressOf   r		= r.billingAddress

shippingAddressOf :: !(Order a) -> Address
shippingAddressOf  r		= r.shippingAddress

billingAddressUpd :: !(Order a) !Address -> Order a
billingAddressUpd  r new	= {r & billingAddress  = new}

shippingAddressUpd :: !(Order a) !Address -> Order a
shippingAddressUpd r new	= {r & shippingAddress = new}

class nameOf a :: a -> String
instance nameOf (Order items)  where nameOf r		= r.Order.name
instance nameOf Product        where nameOf r		= r.Product.name

class nameUpd a :: a String -> a
instance nameUpd Product       where nameUpd r new	= {r & Product.name = new}
instance nameUpd (Order items) where nameUpd r new	= {r & Order.name   = new}


class toCart a :: Int a -> CartItem a

instance toCart Product where
	toCart id product		= { id_				= DBRef id
							  , itemNr 			= getItemId product
							  , description		= product.Product.name
							  , amountInStock	= product.amount
							  , amountOrdered	= if (product.amount >= 0) 1 0
							  , pricePerUnit	= product.price
							  }

instance DB Product where
	databaseId				= mkDBid "products" LSTxtFile
	getItemId item			= item.Product.id_
	setItemId id item 		= {Product|item & id_ = id}

instance DB (Order items) where
	databaseId				= mkDBid "orders" LSTxtFile
	getItemId item			= item.Order.id_
	setItemId id item		= {Order|item & id_ = id}
