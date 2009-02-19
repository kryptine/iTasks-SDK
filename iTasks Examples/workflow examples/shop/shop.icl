module shop

import StdEnv, iTasks, iDataTrivial

:: DBRef a		= DBRef Int

:: Product		=	{ id_	:: !ProductId
					, name	:: !String
					, price	:: !HtmlCurrency
					, amount:: !Int
					}
:: ProductId 	:== DBRef Product

:: Cart items		:== [(CartItem items, CartAmount)]
:: CartItem	items	=	
					{ id_				:: !CartId items
					, itemNr			:: !DBRef items
					, description		:: !String
					, amountInStock		:: !Int
					, amountOrdered		:: !Int
					, pricePerUnit		:: !HtmlCurrency
					}
:: CartAmount	=	{ orderAmount			:: !Int}
:: CartId items		:== DBRef (Cart items)

:: Order items	=	{ id_				:: !OrderId items
					, name				:: !String
					, itemsOrdered		:: !Cart items
					, billingAddress	:: !Address
					, shippingAddress	:: !Address
					}
:: OrderId items	:== DBRef (Order items)

:: Address		=	{ street		:: !String
					, number		:: !String
					, postalCode	:: !String
					, city			:: !String
					}

:: ShopAction		= LeaveShop | ToCart | ToPay | ToShop


derive gForm	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction
derive gUpd		DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction
derive gPrint	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction
derive gParse	DBRef, Product, Order, Address, CartItem, CartAmount, ShopAction

Start :: *World -> *World
Start world = startEngine flows world
where
	flows
		=	[ { name		= "Manage catalog"
 			  , label		= "Manage catalog"
			  , roles		= []
			  , mainTask	= manageCatalog catalogPrompt defaultProduct
			  }
			, { name		= "browseShop"
			  , label		= "Browse shop"
			  , roles		= []
			  , mainTask	= browseShop shopPrompt cartPrompt defaultCart
			  }
			]

defaultProduct :: Product
defaultProduct = createDefault

defaultCart :: (Cart Product)
defaultCart = []

shopPrompt 		= [HrTag [], boldText "Welcome to My Shop", HrTag []]
cartPrompt 		= [HrTag [], boldText "My Shop Shopping Cart", HrTag []]
catalogPrompt 	= [HrTag [], boldText "My Shop Product Catalogue Browser", HrTag []]

//Frontend		

browseShop :: [HtmlTag] [HtmlTag] (Cart a) -> Task Void | toCart a & iData a & DB a
browseShop shopPrompt cartPrompt initCart 
	= 	dbReadAll =>> \items ->
		doShopping initCart items =>> 
		doAction initCart items
where
	doShopping cart []
		=	(shopPrompt ++ [normalText "Currently no items in catalogue, sorry."])
			?>> OK #>> return_V (LeaveShop,cart)
	doShopping cart items
		= 	orTasksVert
				[ chooseAction shopPrompt cart
				: map (itemActions cart) items					
				] 
	where
		itemActions :: (Cart a) a -> Task (ShopAction,Cart a) | toCart a & iData a
		itemActions cart item
			= chooseTask_btn [toHtml item] 
				[("Add to Cart", addToCart cart item)
				]

		addToCart :: (Cart a) a -> Task (ShopAction,Cart a) | toCart a & iData a 
		addToCart cart item 
			# ncart = cart ++ [(toCart (length cart) item, {orderAmount = 1})]
			= return_V (ToCart,ncart)

	chooseAction :: [HtmlTag] (Cart a) -> Task (ShopAction, Cart a) | toCart a & iData a
	chooseAction promptOfShop cart
		=	chooseTask 	[] 
				[ ("Do Shopping",   	return_V (ToShop,cart))
				, ("Check Out and Pay", return_V (ToPay,cart))
				, ("Show Cart",     	return_V (ToCart,cart))
				, ("Leave Shop",    	return_V (LeaveShop,cart))
				] <<? [BrTag [], boldText ("Total cost of ordered items = "), toHtml(totalCost cart), DivTag [] promptOfShop]

	doAction :: (Cart a) [a] (ShopAction, Cart a) -> Task Void | toCart a & iData a & DB a
	doAction initCart items (LeaveShop,cart) = return_V Void
	doAction initCart items (ToCart,cart) 	= showCart cart =>> doAction initCart items 
	doAction initCart items (ToShop,cart) 	= doShopping cart items =>> doAction initCart items
	doAction initCart items (ToPay,cart) 	= checkOutAndPay {createDefault & itemsOrdered = cart} #>>
												browseShop shopPrompt cartPrompt initCart 

	showCart ::(Cart a) -> Task (ShopAction,Cart a) | toCart a & iData a 
	showCart []
		=	chooseAction cartPrompt [] <<? [normalText "No items in Cart yet !"]
	showCart cart
		= 	orTasksVert 
				[ chooseAction cartPrompt cart
				: map (itemActions cart) cart
				]
	where
		itemActions :: (Cart a) (CartItem a, CartAmount) -> Task (ShopAction,Cart a) | toCart a & iData a 
		itemActions cart (cartItem,amount)
			= [toHtml cartItem] 
				?>> editTask "Change" {orderAmount = cartItem.amountOrdered} =>> \namount ->
					return_V (ToCart, adjustAmount (cartItem, namount) cart)
		where
			adjustAmount :: (CartItem a, CartAmount) (Cart a) -> (Cart a) 
			adjustAmount (cartItem,amount) [] = []
			adjustAmount (cartItem, amount) [(c,a):carts]
				| cartItem.CartItem.id_ == c.CartItem.id_ 
					= [({c & amountOrdered = max 0 amount.orderAmount},amount):carts]		 
				= [(c,a): adjustAmount (cartItem,amount) carts]


// finishing ordering process
	
checkOutAndPay :: (Order a) -> Task Void | iData a
checkOutAndPay order 
	= 	fillInClientInfo order =>> \order ->
		confirmOrder order =>> \continue ->
		if continue (doOrder order) (return_V Void)
where
	doOrder order 
		=	dbReadAll =>> \orders ->
			dbWriteAll (orders ++ [order]) #>>
			getMyName =>> \(myid,myname) ->			
			spawnProcess myid True ("Order Confirmed",[toHtml order, costOrder order] ?>> OK) #>>	
			spawnProcess shopOwner True ("New Order from " <+++ myname,[toHtml order, costOrder order] ?>> OK) #>>	
			return_V Void

	costOrder order = boldText ("Amount to pay is: " <+++ totalCost order.itemsOrdered <+++ ".")

	confirmOrder order
		=	chooseTask 	[ costOrder order
					 	, normalText "Confirm Order"
						]
			[("Yes",return_V True),("No",return_V False)]

	fillInClientInfo order
		=	fillInYourName order		=>> \order ->
			fillInBillingAddress order	=>> \order ->
			fillInShippingAddress order	=>> \order ->
			isCorrect order fillInClientInfo
	where
		fillInYourName :: (Order a) -> Task (Order a) | iData a
		fillInYourName order
			= 	[normalText "Please fill in your name:"] 
				?>> editTask "Commit" order.Order.name =>> \name ->
				return_V {order & Order.name = name}
	
		fillInBillingAddress :: (Order a) -> Task (Order a) | iData a
		fillInBillingAddress order
			= 	[normalText "Please fill in the billing address:"]
				?>> editTask "Commit" order.billingAddress =>> \billingAddress ->
				return_V {order & billingAddress = billingAddress}
	
		fillInShippingAddress :: (Order a)  -> Task (Order a) | iData a
		fillInShippingAddress order
			= 	chooseTask 	[ normalText "Billing address:"
							, toHtml order.billingAddress
							, normalText "Is the shipping addres same as the billing address above?"
							]
					[ ("Yes", return_V {order & shippingAddress = order.billingAddress})
					, ("No", [normalText "Please fill in the shipping address:"]
							 ?>> editTask "Commit" order.shippingAddress =>> \shippingAddress ->
							 return_V {order & shippingAddress = shippingAddress})
					]
		isCorrect :: (Order a) ((Order a) -> Task (Order a)) -> Task (Order a) | iData a
		isCorrect data tryagain
			= 	chooseTask 	[toHtml data, costOrder order, normalText "Is the data above correct ?"]
					[ ("Yes", return_V data)
					, ("No", tryagain data)
					]


//Backend
manageCatalog :: [HtmlTag] a -> Task Void | iData a & DB a
manageCatalog catalogPrompt defaultValue 
	= stopTask 
		(catalogPrompt ?>>foreverTask
			(	dbReadAll							=>> \catalog ->
				browseCatalog defaultValue catalog
			))
where
	browseCatalog :: a [a] -> Task Void | iData a &  DB a
	browseCatalog defaultValue []
		=	editTask "Store" defaultValue =>>
			dbCreate 
	
	browseCatalog defaultValue items
		=	 orTasksVert (map itemActions items)
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
			   let (before,[item`:after]) = span (\p -> getItemId p <>  getItemId item) items in
			  	dbWriteAll (before ++ [item`,setItemId (newDBRef items) nitem] ++ after) 
		
		editItem :: a -> Task Void | iData a & DB a
		editItem item 
			= editTask "Store" item =>>
			  dbUpdate 
		
		deleteItem :: a -> Task Void | iData a & DB a
		deleteItem item 
			= dbDelete item 

// general stuf


stopTask :: (Task a) -> (Task a) | iData a 
stopTask task
	= 	orTasksVert [task,stopIt]
where
	stopIt = [BrTag []] ?>> editTask "Finish" Void  #>> return_V createDefault

orTasksVert :: [Task a] -> Task a | iData a
orTasksVert items = orTasksV [(toString i,item) \\ item <- items & i <- [0..]]

totalCost cart = HtmlCurrency EUR (sum [price * amount.orderAmount \\ ({pricePerUnit=HtmlCurrency _ price},amount) <- cart])

boldText text 	= BTag [] [Text text, BrTag [], BrTag []]
normalText text = BodyTag [] [Text text, BrTag [], BrTag []]

shopOwner = 0

getMyName
=					getCurrentUserId
	=>> \userid ->	getDisplayNamesTask [userid]
	=>> \names -> 	return_V (userid, hd names)     			

OK = editTask "OK" Void

class toCart a 
where
	toCart :: Int a -> CartItem a


instance toCart Product
where
	toCart itemnr product
	= { id_				= DBRef itemnr
	  , itemNr 			= getItemId product
	  , description		= product.Product.name
	  , amountInStock	= product.amount
	  , amountOrdered	= if (product.amount >= 0) 1 0
	  , pricePerUnit	= product.price
	   }

continue :: String (Task Void) -> (Task Void) 
continue prompt task
	=	chooseTask [normalText prompt]
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
/*	
dbRead :: (DBRef a) -> Task a | iData a & DB a
dbRead ref
	= readDB databaseId 		=>> \items -> 
	  return_V (hd [item \\ item <- items | getItemId item == ref])
*/	
dbReadAll :: Task [a] | iData a & DB a
dbReadAll = readDB databaseId

dbWriteAll :: [a] -> Task Void | iData a & DB a
dbWriteAll all = writeDB databaseId all #>> return_V Void

dbUpdate ::  a -> Task Void |  iData a & DB a
dbUpdate nitem 
	= readDB databaseId									=>> \items ->
	  writeDB databaseId (update (getItemId nitem) nitem items)		#>>
	  return_V Void
where
	update ref nitem items 
		= [if (getItemId item == ref) nitem item \\ item <- items]


dbDelete :: a -> Task Void | iData a & DB a 
dbDelete item
	= readDB databaseId							=>> \items ->
	  writeDB databaseId (delete (getItemId item) items)		#>>
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
	setItemId id item 	= {Product|item & id_ = id}

instance DB (Order items) where
	databaseId			= mkDBid "orders" LSTxtFile
	getItemId item		= item.Order.id_
	setItemId id item	= {Order|item & id_ = id}
