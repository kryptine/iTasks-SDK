module shop

import StdEnv, iTasks

:: DBRef a		= DBRef Int

:: Product		=	{ id_	:: !ProductId
					, name	:: !String
					, price	:: !Int 					// will become currency
					, amount:: !Int
					}
:: ProductId 	:== DBRef Product

:: Cart			:== [(CartItem, CartAmount)]

:: CartAmount	=	{ orderAmount			:: !Int}
:: CartItem		=	{ id_				:: !CartId
					, productNr			:: !ProductId
					, description		:: !String
					, amountInStore		:: !Int
					, pricePerUnit		:: !Int
					}

:: Order		=	{ id_				:: !OrderId
					, name				:: !String
					, products			:: !Cart
					, billingAddress	:: !Address
					, shippingAddress	:: !Address
					}
:: CartId		:== DBRef Cart
:: OrderId		:== DBRef Order

:: Address		=	{ street		:: !String
					, number		:: !String
					, postalCode	:: !String
					, city			:: !String
					}

:: Action		=	AAdd | AEdit | ADelete

derive gForm	DBRef, Product, Order, Address, Action, CartItem, CartAmount
derive gUpd		DBRef, Product, Order, Address, Action, CartItem, CartAmount
derive gPrint	DBRef, Product, Order, Address, Action, CartItem, CartAmount
derive gParse	DBRef, Product, Order, Address, Action, CartItem, CartAmount

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

//Frontend		
browseShop :: Task Void
browseShop = browseShop` [] 
where
	browseShop` :: Cart -> Task Void
	browseShop` cart
		=	dbReadAll				=>> \products ->
			shopCatalog products	=>> \product -> 
			addToChart product cart	=>> \ncart ->
			browseShop` ncart

	shopCatalog products
		= orTasksV (editItems itemActions products)
	where
		itemActions :: Product -> Task Product
		itemActions product
			= chooseTask_btn [toHtml product] 
				[("Add to Cart",return_V product)
				]

	addToChart :: Product Cart -> Task Cart
	addToChart product cart
		# ncart = cart ++ [(productToChart (length cart) product, {orderAmount = 1})]
		=  orTasksV (editItems (itemActions ncart) ncart)
	where	
		itemActions :: Cart (CartItem, CartAmount) -> Task Cart
		itemActions cart (cartItem,amount)
			= [toHtml cartItem] 
				?>> editTask "Change" amount =>> \namount ->
					return_V (adjust (cartItem, namount) cart)

	adjust (cartItem,amount) [] = []
	adjust (cartItem,amount) [(c,a):carts]
		| cartItem.CartItem.id_ == c.CartItem.id_ = [(c,amount):carts]		 
		= [(c,a): adjust (cartItem,amount) carts]

/*	addToChart product cart
		=  let ncart = [(addProductToChart product,1):cart] in
		   ([toHtml item \\ item <- ncart] ?>> editTask "OK" Void) #>> return_V ncart
*/
//Backend
manageCatalog :: Task Void
manageCatalog = stopTask manageCatalog`
where
	manageCatalog`
		=	dbReadAll					=>> \catalog ->
			browseCatalog catalog		=>> \_ -> 
			manageCatalog`

	browseCatalog :: [Product] -> Task Void
	browseCatalog []
		= addProduct   =>> \product ->
		  browseCatalog [product]
	where
		addProduct :: Task Product
		addProduct
			= editTask "Store" createDefault =>> \product ->
			  dbCreate product
	
	browseCatalog products
		= orTasksV (editItems itemActions products)
	where
		itemActions :: Product -> Task Void
		itemActions product
			= chooseTask_btn [toHtml product] 
				[("Edit",	editProduct   product) //product.Product.id
				,("Insert",	insertProduct product)
				,("Append",	appendProduct product)
				,("Delete",	deleteProduct product)
				]
		
		insertProduct :: Product -> Task Void
		insertProduct product 
			=	editTask "Store" createDefault <<? [toHtml product] =>> \nproduct ->
			   	dbReadAll =>> \products -> 
			   	let (before,after) = span (\p -> getItemId p <>  getItemId product) products in
			  		dbWriteAll (before ++ [setItemId (newDBRef products) nproduct] ++ after) #>>
			  		return_V Void
	
		appendProduct :: Product -> Task Void
		appendProduct product 
			=  [toHtml product] ?>> editTask "Store" createDefault =>> \nproduct ->
			   dbReadAll =>> \products -> 
			   let (before,after) = span (\p -> getItemId p <>  getItemId product) products in
			  	dbWriteAll (before ++ [product,setItemId (newDBRef products) nproduct] ++ (tl after)) #>>
			  	return_V Void
		
		editProduct :: Product -> Task Void
		editProduct product 
			= editTask "Store" product =>> \nproduct ->
			  dbUpdate (getItemId product) nproduct #>>
			  return_V Void
		
		deleteProduct :: Product -> Task Void
		deleteProduct product 
			= dbDelete (getItemId product) #>>
			  return_V Void
	


// general stuf

editItems :: (b -> Task a) [b] -> [LabeledTask a] | iData a & gUpd {|*|} b
editItems action [] = 		[] //editItems action [createDefault]
editItems action products = [(toString i, action p) \\ p <- products & i <- [0..]]

stopTask :: (Task a) -> (Task a) | iData a 
stopTask task
	= 	orTasksV [("task",task),("stop", stopIt)]
where
	stopIt = [BrTag []] ?>> editTask "Stop Task" Void  #>> return_V createDefault


productToChart itemnr product
	= { id_				= DBRef itemnr
	  , productNr 		= getItemId product
	  , description		= product.Product.name
	  , amountInStore	= product.amount
	  , pricePerUnit	= product.price
	   }

continue :: String (Task Void) -> (Task Void) 
continue prompt task
	=	chooseTask [Text prompt, BrTag []]
		[ ("Yes", task)
		, ("No",  return_V Void)
		]

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
dbCreate :: a -> Task a | iData a & DB a 
dbCreate item
	= writeDB databaseId  [setItemId (DBRef 0) item]	#>>
	  return_V item

newDBRef items =  newDBRef` [getItemId item \\ item <- items] (DBRef 0)
where
	newDBRef` [] (DBRef i) = DBRef (inc i)
	newDBRef` [j:js] i 
	| j > i = newDBRef` js j
	= newDBRef` js i
	
dbRead :: (DBRef a) -> Task a | iData a & DB a
dbRead ref
	= readDB databaseId 		=>> \items -> 
	  return_V (hd [item \\ item <- items | getItemId item == ref])
	
dbReadAll :: Task [a] | iData a & DB a
dbReadAll = readDB databaseId

dbWriteAll :: [a] -> Task [a] | iData a & DB a
dbWriteAll all = writeDB databaseId all

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

instance < (DBRef a)
where
	(<) (DBRef x) (DBRef y) = (x < y)

instance DB Product where
	databaseId			= mkDBid "products" LSTxtFile
	getItemId item		= item.Product.id_
	setItemId id item	= {Product|item & id_ = id}

instance DB Order where
	databaseId			= mkDBid "orders" LSTxtFile
	getItemId item		= item.Order.id_
	setItemId id item	= {Order|item & id_ = id}
