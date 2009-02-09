module shop

import StdEnv, iTasks

:: DBRef a		= DBRef Int

:: Product		=	{ id_	:: !ProductId
					, name	:: !String
					, price	:: !Int
					}
:: ProductId 	:== DBRef Product

:: Cart			:== [(Product, Int)]

:: Order		=	{ id_				:: !OrderId
					, name				:: !String
					, products			:: !Cart
					, billingAddress	:: !Address
					, shippingAddress	:: !Address
					}
:: OrderId		:== DBRef Order

:: Address		=	{ street		:: !String
					, number		:: !String
					, postalCode	:: !String
					, city			:: !String
					}

:: Action		=	AAdd | AEdit | ADelete

derive gForm	DBRef, Product, Order, Address, Action
derive gUpd		DBRef, Product, Order, Address, Action
derive gPrint	DBRef, Product, Order, Address, Action
derive gParse	DBRef, Product, Order, Address, Action

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
	dbReadAll										=>> \catalog ->
	checkEmpty catalog								=>> \catalog ->
	( browseCatalog catalog
	  -||-
	  buttonTask "Add product" (return_V (AAdd,DBRef 0))
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
		= chooseTask_btn (display product) [("Edit",return_V (AEdit, product.Product.id_)),("Delete",return_V (ADelete, product.Product.id_))]
	where
		display product = [H1Tag [][Text product.Product.name],BrTag [],BTag [] [Text "Price: "],Text (toString product.Product.price)]

idProduct:: Product -> Product
idProduct x = x

addProduct :: Task Void
addProduct
	= editTask "Add" createDefault =>> \product ->
	  dbCreate (idProduct product)

editProduct :: ProductId -> Task Void
editProduct pid = return_V Void

deleteProduct :: ProductId -> Task Void
deleteProduct pid = return_V Void

//Frontend		
browseShop :: Task Void
browseShop = return_V Void

//Utility function which creates a single default item when a database is empty
checkEmpty :: [a] -> Task [a] | iData a
checkEmpty [] = return_V [createDefault]
checkEmpty l = return_V l

//Polymorhpic database operations
class DB a
where
	databaseId		:: (DBid [a])
	getItemId		:: a -> DBRef a
	setItemId		:: (DBRef a) a -> a
	
//CRUD
dbCreate :: a -> Task Void | iData a & DB a 
dbCreate item
	= readDB databaseId														=>> \items ->
	  writeDB databaseId (items ++ [setItemId (DBRef (length items)) item])	#>>
	  return_V Void
	
dbRead :: (DBRef a) -> Task a | iData a & DB a
dbRead ref
	= readDB databaseId 		=>> \items -> 
	  return_V (hd [item \\ item <- items | getItemId item == ref])
	
dbReadAll :: Task [a] | iData a & DB a
dbReadAll = readDB databaseId

dbUpdate :: (DBRef a) a -> Task Void | DB a
dbUpdate id item = return_V Void

dbDelete :: (DBRef a) -> Task Void | iData a & DB a 
dbDelete ref
	= readDB databaseId												=>> \items ->
	  writeDB databaseId [item \\ item <- items | getItemId item <> ref]	#>>
	  return_V Void

instance == (DBRef a)
where
	(==) (DBRef x) (DBRef y) = (x == y)

instance DB Product where
	databaseId			= mkDBid "products" LSTxtFile
	getItemId item		= item.Product.id_
	setItemId id item	= {Product|item & id_ = id}

instance DB Order where
	databaseId			= mkDBid "orders" LSTxtFile
	getItemId item		= item.Order.id_
	setItemId id item	= {Order|item & id_ = id}
