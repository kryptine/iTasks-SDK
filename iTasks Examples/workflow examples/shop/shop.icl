module shop

import StdEnv, iTasks

:: DBRef a		= DBRef Int

:: Product		=	{ id_	:: !ProductId
					, name	:: !String
					, price	:: !Int // will become currency
					, amount:: !Int
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
manageCatalog = stopTask manageCatalog`
where
	manageCatalog`
		=	dbReadAll					=>> \catalog ->
			browseCatalog catalog		=>> \_ -> 
			manageCatalog`

continue :: String (Task Void) -> (Task Void) 
continue prompt task
	=	chooseTask [Text prompt, BrTag []]
		[ ("Yes", task)
		, ("No",  return_V Void)
		]

stopTask :: (Task a) -> (Task a) | iData a 
stopTask task
	= 	orTasksV [("task",task),("stop", stopIt)]
where
	stopIt = [BrTag []] ?>> editTask "Stop Task" Void  #>> return_V createDefault


browseCatalog :: [Product] -> Task Void
browseCatalog products
	= orTasksV (showEditProducts products ++ addNewProduct)
where
	addNewProduct :: [LabeledTask Void]
	addNewProduct = [("add",[HrTag []] ?>> buttonTask "Add product" addProduct)]

	browseItem :: Product -> Task Void
	browseItem product
		= chooseTask_btn (display product) 
			[("Edit",editProduct product.Product.id_)
			,("Delete",deleteProduct product.Product.id_)
			]
	where
		display product = [toHtml product]

	showEditProducts :: [Product] -> [LabeledTask Void]
	showEditProducts [] = 		[]
	showEditProducts products = [(p.Product.name, browseItem p) \\ p <- products]


addProduct :: Task Void
addProduct
	= editTask "Add" createDefault =>> \product ->
	  dbCreate (idProduct product)
where
	idProduct:: Product -> Product
	idProduct x = x


editProduct :: ProductId -> Task Void
editProduct pid 
	= dbRead pid =>> \product ->
	  editTask "Store" product =>> \nproduct ->
	  dbUpdate pid nproduct #>>
	  return_V Void

deleteProduct :: ProductId -> Task Void
deleteProduct pid 
	= dbDelete pid #>>
	  return_V Void

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

dbUpdate :: (DBRef a) a -> Task Void |  iData a & DB a
dbUpdate ref nitem 
	= readDB databaseId									=>> \items ->
	  writeDB databaseId (update ref nitem items)		#>>
	  return_V Void
where
	update ref nitem items 
		= [if (getItemId item == ref) nitem item \\ item <- items]


dbDelete :: (DBRef a) -> Task Void | iData a & DB a 
dbDelete ref
	= readDB databaseId							=>> \items ->
	  writeDB databaseId (delete ref items)		#>>
	  return_V Void
where
	delete ref  items 
		= [item \\ item <- items | not (ref == getItemId item)]

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
