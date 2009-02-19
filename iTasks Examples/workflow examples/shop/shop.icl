module shop

import StdEnv, iTasks, iDataTrivial

:: DBRef a			= DBRef Int

:: Product			=	{ id_	:: !ProductId
						, name	:: !String
						, price	:: !HtmlCurrency
						, amount:: !Int
						}
:: ProductId 		:== DBRef Product

:: Cart items		:== [(CartItem items, CartAmount)]

:: CartItem	items	=	{ id_				:: !CartId items
						, itemNr			:: !DBRef items
						, description		:: !String
						, amountInStock		:: !Int
						, amountOrdered		:: !Int
						, pricePerUnit		:: !HtmlCurrency
						}
:: CartAmount		=	{ orderAmount		:: !Int}
:: CartId items		:== DBRef (Cart items)

:: Order items		=	{ id_				:: !OrderId items
						, name				:: !String
						, itemsOrdered		:: !Cart items
						, billingAddress	:: !Address
						, shippingAddress	:: !Address
						}
:: OrderId items	:== DBRef (Order items)

:: Address			=	{ street		:: !String
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
	flows	= [ { name		= "Manage catalog"
 			    , label		= "Manage catalog"
			    , roles		= []
			    , mainTask	= manageCatalog catalogPrompt defaultProduct
			    }
			  , { name		= "Browse Shop"
			    , label		= "Browse Shop"
			    , roles		= []
			    , mainTask	= browseShop shopPrompt cartPrompt defaultCart
			    }
			  ]

defaultProduct :: Product
defaultProduct	= createDefault

defaultCart :: Cart Product
defaultCart		= []

shopPrompt 		= [HrTag [], boldText "Welcome to My Shop",                HrTag []]
cartPrompt 		= [HrTag [], boldText "My Shop Shopping Cart",             HrTag []]
catalogPrompt 	= [HrTag [], boldText "My Shop Product Catalogue Browser", HrTag []]

// Frontend		

browseShop :: [HtmlTag] [HtmlTag] (Cart a) -> Task Void | toCart, iData, DB a
browseShop shopPrompt cartPrompt initCart 
	= dbReadAll      =>> \items ->
	  doShopping initCart items =>> 
	  doAction   initCart items
where
	doShopping cart []
		= (shopPrompt ++ [normalText "Currently no items in catalogue, sorry."])
		  ?>> OK #>> return_V (LeaveShop,cart)
	doShopping cart items
		= orTasksVert
			[ chooseAction shopPrompt cart
			: map (itemActions cart) items					
			] 
	where
		itemActions :: (Cart a) a -> Task (ShopAction,Cart a) | toCart, iData a
		itemActions cart item
			= chooseTask_btn [toHtml item] [("Add to Cart", addToCart cart item)]

		addToCart :: (Cart a) a -> Task (ShopAction,Cart a) | toCart, iData a
		addToCart cart item 
			= return_V 
			    (ToCart,append (toCart (length cart) item, {orderAmount = 1}) cart)

	chooseAction :: [HtmlTag] (Cart a) -> Task (ShopAction, Cart a) | toCart, iData a
	chooseAction promptOfShop cart
		= chooseTask [] 
			[ ("Do Shopping",   	return_V (ToShop,   cart))
			, ("Check Out and Pay", return_V (ToPay,    cart))
			, ("Show Cart",     	return_V (ToCart,   cart))
			, ("Leave Shop",    	return_V (LeaveShop,cart))
			] <<? [ BrTag []
			      , boldText "Total cost of ordered items = "
			      , toHtml  (totalCost cart)
			      , DivTag [] promptOfShop
			      ]

	doAction :: (Cart a) [a] (ShopAction, Cart a) -> Task Void | toCart, iData, DB a
	doAction initCart items (LeaveShop,cart)	= return_V Void
	doAction initCart items (ToCart,   cart) 	= showCart   cart       =>> doAction initCart items 
	doAction initCart items (ToShop,   cart) 	= doShopping cart items =>> doAction initCart items
	doAction initCart items (ToPay,    cart) 	= checkOutAndPay {createDefault & itemsOrdered = cart} #>>
												  browseShop shopPrompt cartPrompt initCart 

	showCart ::(Cart a) -> Task (ShopAction,Cart a) | toCart, iData, DB a
	showCart []
		= chooseAction cartPrompt [] <<? [normalText "No items in cart yet!"]
	showCart cart
		= orTasksVert 
			[ chooseAction cartPrompt cart
			: map (itemActions cart) cart
			]
	where
		itemActions :: (Cart a) (CartItem a, CartAmount) -> Task (ShopAction,Cart a) | toCart, iData, DB a
		itemActions cart (cartItem,amount)
			= [toHtml cartItem] 
				?>> editTask "Change" {orderAmount = cartItem.amountOrdered} =>> \namount ->
					return_V (ToCart, adjustAmount (cartItem, namount) cart)
		where
			adjustAmount :: (CartItem a, CartAmount) (Cart a) -> Cart a | DB a
			adjustAmount (cartItem,amount) cart
				= map (\(c,a) -> if (cartItem.CartItem.id_ == c.CartItem.id_) 
				                    ({c & amountOrdered = max 0 amount.orderAmount},amount)
				                    (c,a)
				      ) cart

// finishing ordering process
	
checkOutAndPay :: (Order a) -> Task Void | iData a
checkOutAndPay order 
	= fillInClientInfo order =>> \order ->
	  yesOrNo [DivTag [] (costOrder order),normalText "Confirm Order"]
		(dbModify (append order) #>>
		 getMyName =>> \(myid,me) ->			
		 spawnProcess myid      True ("Order Confirmed",        [toHtml order:costOrder order] ?>> OK) #>>	
		 spawnProcess shopOwner True ("New Order from " <+++ me,[toHtml order:costOrder order] ?>> OK) #>>	
		 return_V Void)
		(return_V Void)
where
	costOrder order	= [boldText "Amount to pay is: ", toHtml (totalCost order.itemsOrdered)]

	fillInClientInfo order
		=	fillInYourName        order	=>> \order ->
			fillInBillingAddress  order	=>> \order ->
			fillInShippingAddress order	=>> \order ->
			yesOrNo [toHtml order, DivTag [] (costOrder order), normalText "Is the data above correct?"]
			        (return_V         order) 
			        (fillInClientInfo order)
	where
		fillInData prompt valueOf updateOf record
			= [normalText prompt] ?>> editTask "Commit" (valueOf record) =>> \value -> 
			  return_V (updateOf record value)
		
		fillInYourName :: ((Order a) -> Task (Order a)) | iData a
		fillInYourName 
			= fillInData "Please fill in your name:" nameOf nameUpd
	
		fillInBillingAddress :: ((Order a) -> Task (Order a)) | iData a
		fillInBillingAddress
			= fillInData "Please fill in the billing address:" billingAddressOf billingAddressUpd
	
		fillInShippingAddress :: (Order a)  -> Task (Order a) | iData a
		fillInShippingAddress order
			= yesOrNo [ normalText "Billing address:"
					  , toHtml order.billingAddress
					  , normalText "Is the shipping addres same as the billing address above?"
					  ]
					  (return_V {order & shippingAddress = order.billingAddress})
					  (fillInData "Please fill in the shipping address:" 
					              shippingAddressOf shippingAddressUpd order)

// Backend
manageCatalog :: [HtmlTag] a -> Task Void | iData, DB a
manageCatalog catalogPrompt defaultValue 
	= stopTask 
		(catalogPrompt ?>>foreverTask
			(	dbReadAll             =>> \catalog ->
				browseCatalog defaultValue catalog
			))
where
	browseCatalog :: a [a] -> Task Void | iData, DB a
	browseCatalog defaultValue []
		= editTask "Store" defaultValue =>>
		  dbCreate 
	
	browseCatalog defaultValue items
		= orTasksVert (map itemActions items)
	where
		itemActions :: a -> Task Void | iData, DB a
		itemActions item
			= chooseTask_btn [toHtml item] 
				[("Edit",	editTask "Store" item =>> \nitem -> dbModify (insert eqItemId nitem)) 
				,("Insert",	addItem (<<?) span item)
				,("Append",	addItem (flip (?>>)) span_inc item)
				,("Delete",	dbModify (filter (not o eqItemId item)))
				]
		
		addItem show spanF item
			= show (editTask "Store" createDefault) [toHtml item] =>> \nitem -> dbModify (addItem` nitem)
		where
			addItem` nitem items
				= let (before,after) = spanF (not o (eqItemId item)) items
				   in before ++ [setItemId (newDBRef items) nitem] ++ after

// general stuff
// little task functions:
stopTask :: (Task a) -> Task a | iData a 
stopTask task				= orTasksVert [task,stopIt]
where stopIt				= [BrTag []] ?>> editTask "Finish" Void #>> return_V createDefault

yesOrNo question yes no		= chooseTask question [("Yes",yes),("No",no)]

orTasksVert :: [Task a] -> Task a | iData a
orTasksVert items			= orTasksV [(toString i,item) \\ item <- items & i <- [0..]]

OK							= editTask "OK" Void

getMyName					= getCurrentUserId             =>> \userid ->
							  getDisplayNamesTask [userid] =>> \names  ->
							  return_V (userid, hd names)     			

// little markup functions:
boldText   text				= BTag    [] [Text text, BrTag [], BrTag []]
normalText text				= BodyTag [] [Text text, BrTag [], BrTag []]

// little domain specific functions:
totalCost cart				= HtmlCurrency EUR (sum [(toInt price.pricePerUnit) * amount.orderAmount \\ (price,amount) <- cart])
shopOwner					= 0

class toCart a where
	toCart :: Int a -> CartItem a

instance toCart Product where
	toCart id product		= { id_				= DBRef id
							  , itemNr 			= getItemId product
							  , description		= product.Product.name
							  , amountInStock	= product.amount
							  , amountOrdered	= if (product.amount >= 0) 1 0
							  , pricePerUnit	= product.price
							  }

// Polymorhpic database operations
class DB a where
	databaseId				:: (DBid [a])
	getItemId				:: a -> DBRef a
	setItemId				:: (DBRef a) a -> a
	
// CRUD
dbCreate :: a -> Task Void | iData, DB a
dbCreate item				= writeDB databaseId [setItemId (DBRef 0) item]	#>> return_V Void

newDBRef items				= let (DBRef i) = maxList (map getItemId items) in DBRef (i+1)
	
dbReadAll :: Task [a] | iData, DB a
dbReadAll					= readDB databaseId

dbWriteAll :: [a] -> Task Void | iData, DB a
dbWriteAll all				= writeDB databaseId all #>> return_V Void

dbModify :: ([a] -> [a]) -> Task Void | iData, DB a
dbModify f					= dbReadAll =>> \items -> dbWriteAll (f items)

instance == (DBRef a) where (==) (DBRef x) (DBRef y) = x == y
instance <  (DBRef a) where	(<)  (DBRef x) (DBRef y) = x <  y

eqItemId :: a a -> Bool | DB a
eqItemId a b				= getItemId a == getItemId b

instance DB Product where
	databaseId				= mkDBid "products" LSTxtFile
	getItemId item			= item.Product.id_
	setItemId id item 		= {Product|item & id_ = id}

instance DB (Order items) where
	databaseId				= mkDBid "orders" LSTxtFile
	getItemId item			= item.Order.id_
	setItemId id item		= {Order|item & id_ = id}

// Functions that should be in StdList
span_inc :: (a -> Bool) [a] -> ([a],[a])
span_inc pred xs
	= case span pred xs of
		(bef,[incl:aft])	= (bef ++ [incl],aft)
		not_incl			= not_incl

append x xs					= xs ++ [x]

// Automatic derived operations
billingAddressOf   r		= r.billingAddress
shippingAddressOf  r		= r.shippingAddress
billingAddressUpd  r new	= {r & billingAddress  = new}
shippingAddressUpd r new	= {r & shippingAddress = new}

class nameOf a :: a -> String
instance nameOf (Order items)  where nameOf r		= r.Order.name
instance nameOf Product        where nameOf r		= r.Product.name

class nameUpd a :: a String -> a
instance nameUpd Product       where nameUpd r new	= {r & Product.name = new}
instance nameUpd (Order items) where nameUpd r new	= {r & Order.name   = new}
