implementation module ShopDSLboilerplate

import GenBimap
import ShopDSL

//	Generic boilerplate code:
derive gForm	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart
derive gUpd		DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart
derive gPrint	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart
derive gParse	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart

//	Manual boilerplate code:
instance billingAddressOf   (Order    a) where billingAddressOf   r		= r.Order.billingAddress
instance shippingAddressOf  (Order    a) where shippingAddressOf  r		= r.Order.shippingAddress
instance amountOrderedOf    (CartItem a) where amountOrderedOf    r		= r.CartItem.amountOrdered
instance nameOf             (Order    a) where nameOf             r		= r.Order.name
instance nameOf             Product      where nameOf             r		= r.Product.name
instance id_Of              Product      where id_Of              r		= r.Product.id_
instance id_Of              (Order    a) where id_Of              r		= r.Order.id_

instance billingAddressUpd  (Order    a) where billingAddressUpd  r new	= {r & Order.billingAddress   = new}
instance shippingAddressUpd (Order    a) where shippingAddressUpd r new	= {r & Order.shippingAddress  = new}
instance amountOrderedUpd   (CartItem a) where amountOrderedUpd   r new	= {r & CartItem.amountOrdered = new}
instance nameUpd            Product      where nameUpd            r new	= {r & Product.name           = new}
instance nameUpd            (Order    a) where nameUpd            r new	= {r & Order.name             = new}
instance id_Upd             Product      where id_Upd             r new	= {r & Product.id_            = new}
instance id_Upd             (Order    a) where id_Upd             r new	= {r & Order.id_              = new}
