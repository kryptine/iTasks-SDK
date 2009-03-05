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

itemNrOf :: !(CartItem a) -> DBRef a
itemNrOf r					= r.itemNr

itemNrUpd :: !(CartItem a) (DBRef a) -> CartItem a
itemNrUpd r new				= {r & itemNr = new}

amountOrderedOf :: !(CartItem a) -> Int
amountOrderedOf r			= r.amountOrdered

amountOrderedUpd :: !(CartItem a) !Int -> CartItem a
amountOrderedUpd r new		= {r & amountOrdered = new}

eqItemNr :: !(CartItem a) !(CartItem a) -> Bool
eqItemNr x y				= itemNrOf x == itemNrOf y

class nameOf a :: a -> String
instance nameOf (Order item)  where nameOf r		= r.Order.name
instance nameOf Product       where nameOf r		= r.Product.name

instance id_Of Product        where id_Of r			= r.Product.id_
instance id_Of (Order item)   where id_Of r			= r.Order.id_

class nameUpd a :: a String -> a
instance nameUpd Product      where nameUpd r new	= {r & Product.name = new}
instance nameUpd (Order item) where nameUpd r new	= {r & Order.name   = new}

instance id_Upd Product       where id_Upd r new	= {r & Product.id_  = new}
instance id_Upd (Order item)  where id_Upd r new	= {r & Order.id_    = new}

class toCartItem a :: a -> CartItem a
instance toCartItem Product where
	toCartItem product	= { itemNr 			= id_Of product
						  , description		= nameOf product
						  , amountInStock	= product.amount
						  , amountOrdered	= if (product.amount >= 0) 1 0
						  , pricePerUnit	= product.price
						  }

instance DB Product where
	databaseId				= mkDBid "products" LSTxtFile
	getItemId item			= id_Of  item
	setItemId id item 		= id_Upd item id
instance DB (Order item) where
	databaseId				= mkDBid "orders" LSTxtFile
	getItemId item			= id_Of  item
	setItemId id item		= id_Upd item id
