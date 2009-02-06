module shop

import StdEnv, iTasks

:: Product		=	{ id	:: !ProductId
					, name	:: !String
					, price	:: !Int
					}
:: ProductId	:== Int

:: Cart			:== [(Product, Int)]

:: Order		=	{ id				:: !OrderId
					, name				:: !String
					, products			:: !Cart
					, billingAddress	:: !Address
					, shippingAddress	:: !Address
					}
:: OrderId		:== Int

:: Address		=	{ street		:: !String
					, number		:: !String
					, postalCode	:: !String
					, city			:: !String
					}

:: Action		=	AAdd | AEdit | ADelete

derive gForm	Product, Order, Address, Action
derive gUpd		Product, Order, Address, Action
derive gPrint	Product, Order, Address, Action
derive gParse	Product, Order, Address, Action

Start :: *World -> *World
Start world = startEngine flows world
where
	flows = [ { name		= "manageCatalog"
			  , label		= "Manage catalog"
			  , roles		= []
			  , mainTask	= manageCatalog
			  }
			, { name		= "browseShop"
			  , label		= "Browse shop"
			  , roles		= []
			  , mainTask	= browseShop
			  }
			]

//Backend
manageCatalog :: Task Void
manageCatalog =
	loadCatalog										=>> \catalog ->	
	( browseCatalog catalog
	  -||-
	  buttonTask "Add product" (return_V (AAdd,0))
	)												=>> \(action, pid) ->
	
	case action of
		AAdd	= addProduct
		AEdit	= editProduct pid
		ADelete	= deleteProduct pid
		
browseCatalog :: [Product] -> Task (Action, ProductId)
browseCatalog products
	= orTasks [(p.Product.name, browseItem p) \\ p <- products]
where
	browseItem :: Product -> Task (Action, ProductId)
	browseItem product
		= chooseTask_btn (display product) [("Edit",return_V (AEdit, product.Product.id)),("Delete",return_V (ADelete, product.Product.id))]
	where
		display product = [H1Tag [][Text product.Product.name],BrTag [],BTag [] [Text "Price: "],Text (toString product.Product.price)]

addProduct :: Task Void
addProduct = return_V Void

editProduct :: ProductId -> Task Void
editProduct pid = return_V Void

deleteProduct :: ProductId -> Task Void
deleteProduct pid = return_V Void

//Frontend
		
browseShop :: Task Void
browseShop = return_V Void

//Product database
productsDB :: (DBid [Product])
productsDB = mkDBid "products" LSTxtFile	 

loadCatalog :: Task [Product]
loadCatalog = return_V [{Product| id = 1, name = "Clean T-Shirt", price = 10},{Product| id = 2, name = "Clean Mug", price = 5}]
//loadCatalog = readDB productsDB

//Order database
orderDB :: (DBid [Order])
orderDB = mkDBid "orders" LSTxtFile
