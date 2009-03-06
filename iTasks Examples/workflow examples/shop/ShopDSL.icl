implementation module ShopDSL

import StdClass, StdInt, StdList
import GenBimap
import iTasks, database
import ShopDSLboilerplate

toCartItem :: a -> CartItem a | Product a
toCartItem p			= { itemNr        = id_Of     p
						  , name          = nameOf    p
						  , inStock       = inStockOf p
						  , amountOrdered = if (inStockOf p >= 0) 1 0
						  , price         = priceOf p
						  }
toInCart :: a -> InCart | InCart a
toInCart p				= { InCart
						  | name          = nameOf          p
						  , amountOrdered = amountOrderedOf p
						  , price         = priceOf         p
						  }
instance DB Book where
	databaseId			= mkDBid "books" LSTxtFile
	getItemId item		= id_Of  item
	setItemId id item	= id_Upd item id
instance DB (Order item) where
	databaseId			= mkDBid "orders" LSTxtFile
	getItemId item		= id_Of  item
	setItemId id item	= id_Upd item id


defaultProduct :: Book
defaultProduct			= createDefault

defaultCart :: Cart Book
defaultCart				= createDefault

eqItemNr :: !(CartItem a) !(CartItem a) -> Bool
eqItemNr x y			= x.itemNr == y.itemNr

totalCost :: [a] -> HtmlCurrency | priceOf, amountOrderedOf a
totalCost set			= HtmlCurrency EUR (sum [amountOrderedOf item * toInt (priceOf item) \\ item <- set])

shopOwner :: UserId
shopOwner				= 0
