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
	flows
		=	[ { name		= "Manage catalog"
 			  , label		= "Manage catalog"
			  , roles		= []
			  , mainTask	= manageCatalog defaultProduct
			  }
			, { name		= "browseShop"
			  , label		= "Browse shop"
			  , roles		= []
			  , mainTask	= browseShop
			  }
			]

defaultProduct :: Product
defaultProduct = createDefault

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
		= orTasksVert (editItems itemActions products)
	where
		itemActions :: Product -> Task Product
		itemActions product
			= chooseTask_btn [toHtml product] 
				[("Add to Cart",return_V product)
				]

	addToChart :: Product Cart -> Task Cart
	addToChart product cart
		# ncart = cart ++ [(productToChart (length cart) product, {orderAmount = 1})]
		=  orTasksVert (editItems (itemActions ncart) ncart)
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
manageCatalog :: a -> Task Void | iData a &  DB a
manageCatalog defaultValue = stopTask manageCatalog`
where
	manageCatalog`
		=	dbReadAll							=>> \catalog ->
			browseCatalog defaultValue catalog	=>> \_ -> 
			manageCatalog`

	browseCatalog :: a [a] -> Task Void | iData a &  DB a
	browseCatalog defaultValue []
		= addItem defaultValue  =>> \item ->
		  browseCatalog defaultValue [item]
	where
		addItem :: a -> Task a | iData a &  DB a
		addItem defaultValue
			= editTask "Store" defaultValue =>> \item ->
			  dbCreate item #>>
			  return_V item
	
	browseCatalog defaultValue items
		= orTasksVert (editItems itemActions items)
	where
		itemActions :: a -> Task Void | iData a & DB a
		itemActions item
			= chooseTask_btn [toHtml item] 
				[("Edit",	editItem   item) 
				,("Insert",	insertItem item)
				,("Append",	appendItem item)
				,("Delete",	deleteItem item)
				]
		
		insertItem :: a -> Task Void | iData a & DB a
		insertItem item 
			=	editTask "Store" createDefault <<? [toHtml item] =>> \nitem ->
			   	dbReadAll =>> \items -> 
			   	let (before,after) = span (\p -> getItemId p <>  getItemId item) items in
			  		dbWriteAll (before ++ [setItemId (newDBRef items) nitem] ++ after) 
	
		appendItem :: a -> Task Void | iData a & DB a
		appendItem item 
			=  [toHtml item] ?>> editTask "Store" createDefault =>> \nitem ->
			   dbReadAll =>> \items -> 
			   let (before,after) = span (\p -> getItemId p <>  getItemId item) items in
			  	dbWriteAll (before ++ [item,setItemId (newDBRef items) nitem] ++ (tl after)) #>>
			  	return_V Void
		
		editItem :: a -> Task Void | iData a & DB a
		editItem item 
			= editTask "Store" item =>> \nitem ->
			  dbUpdate (getItemId item) nitem 
		
		deleteItem :: a -> Task Void | iData a & DB a
		deleteItem item 
			= dbDelete (getItemId item) 
	


// general stuf

editItems :: (b -> Task a) [b] -> [Task a] | iData a
editItems action [] 	= [] 
editItems action items 	= [action p \\ p <- items]

stopTask :: (Task a) -> (Task a) | iData a 
stopTask task
	= 	orTasksVert [task,stopIt]
where
	stopIt = [BrTag []] ?>> editTask "Stop Task" Void  #>> return_V createDefault

orTasksVert :: [Task a] -> Task a | iData a
orTasksVert items = orTasksV [(toString i,item) \\ item <- items & i <- [0..]]


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
dbCreate :: a -> Task Void | iData a & DB a 
dbCreate item
	= writeDB databaseId  [setItemId (DBRef 0) item]	#>>
	  return_V Void

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

dbWriteAll :: [a] -> Task Void | iData a & DB a
dbWriteAll all = writeDB databaseId all #>> return_V Void

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
