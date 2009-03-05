definition module ShopDSLboilerplate

import ShopDSL

//	Generic boilerplate code:
derive gForm	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart
derive gUpd		DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart
derive gPrint	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart
derive gParse	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction, InCart

//	Manual boilerplate code:
class billingAddressOf   a :: a -> Address
class shippingAddressOf  a :: a -> Address
class amountOrderedOf    a :: a -> Int
class nameOf             a :: a -> String
class id_Of              a :: a -> DBRef a

class billingAddressUpd  a :: a Address   -> a
class shippingAddressUpd a :: a Address   -> a
class amountOrderedUpd   a :: a Int       -> a
class nameUpd            a :: a String    -> a
class id_Upd             a :: a (DBRef a) -> a

instance nameOf             Product
instance nameUpd            Product
instance id_Of              Product
instance id_Upd             Product

instance billingAddressOf   (Order a)
instance shippingAddressOf  (Order a)
instance billingAddressUpd  (Order a)
instance shippingAddressUpd (Order a)
instance nameOf             (Order a)
instance nameUpd            (Order a)
instance id_Of              (Order a)
instance id_Upd             (Order a)

instance amountOrderedOf    (CartItem a)
instance amountOrderedUpd   (CartItem a)
