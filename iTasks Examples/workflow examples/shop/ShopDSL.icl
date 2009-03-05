implementation module ShopDSL

import StdClass, StdInt, StdList
import GenBimap
import iTasks, database
import ShopDSLboilerplate

instance toInCart (CartItem a)	where toInCart item		= { item         = item.description
														  , nrOrdered    = item.amountOrdered
														  , pricePerItem = item.pricePerUnit
														  }
instance toCartItem Product		where toCartItem p		= { itemNr        = id_Of p
														  , description   = nameOf p
														  , amountInStock = p.amount
														  , amountOrdered = if (p.amount >= 0) 1 0
														  , pricePerUnit  = p.price
														  }
instance DB Product				where databaseId		= mkDBid "products" LSTxtFile
									  getItemId item	= id_Of  item
									  setItemId id item = id_Upd item id
instance DB (Order item)		where databaseId		= mkDBid "orders" LSTxtFile
									  getItemId item	= id_Of  item
									  setItemId id item	= id_Upd item id

eqItemNr :: !(CartItem a) !(CartItem a) -> Bool
eqItemNr x y	= x.itemNr == y.itemNr

totalCost :: !(Cart a) -> HtmlCurrency
totalCost cart	= HtmlCurrency EUR (sum [ n * toInt p \\ {pricePerUnit=p,amountOrdered=n} <- cart])

shopOwner :: UserId
shopOwner		= 0
