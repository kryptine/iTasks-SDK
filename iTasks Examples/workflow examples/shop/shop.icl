module shop

import StdClass, StdList
from   StdFunc import o, flip
import iTasks, iDataTrivial
import StdListExt, database, iTaskCombinatorsExt, ShopDSL
	
Start :: *World -> *World
Start world = startEngine flows world
where
	flows	= [ { name		= "Manage catalog"
 			    , label		= "Manage catalog"
			    , roles		= []
			    , mainTask	= manageCatalog catalogPrompt defaultProduct
			    }
			  , { name		= "Browse shop"
			    , label		= "Browse shop"
			    , roles		= []
			    , mainTask	= browseShop shopPrompt cartPrompt defaultCart
			    }
			  ]

shopPrompt 		= ruleText "Welcome to My Shop"
cartPrompt 		= ruleText "My Shop Shopping Cart"
catalogPrompt 	= ruleText "My Shop Product Catalogue Browser"

// The customer workflow:		

browseShop :: [HtmlTag] [HtmlTag] (Cart a) -> Task Void | iData, DB, Product a
browseShop shopPrompt cartPrompt initCart 
	= dbReadAll      =>> \items ->
	  doShopping initCart items =>> 
	  doAction   initCart items
where
	doShopping :: (Cart a) [a] -> Task (ShopAction,Cart a) | iData, Product a
	doShopping cart []
		= (shopPrompt ++ [normalText "Currently no items in catalogue, sorry."])
		  ?>> OK #>> return (LeaveShop,cart)
	doShopping cart items
		= orTasksVert
			[ navigateShop shopPrompt cart
			: map (itemActions cart) items					
			] 
	where
		itemActions :: (Cart a) a -> Task (ShopAction,Cart a) | iData, Product a
		itemActions cart item
			= chooseTask_btn [toHtml item] 
			    [("Add to Cart", return (ToCart, add (toCartItem item) cart))]
		where
			add :: (CartItem a) [CartItem a] -> [CartItem a]
			add new []				= [new]
			add new [item:items]	= if (eqItemNr new item) 
									     [amountOrderedUpd item (amountOrderedOf item+1):items]
									     [item:add new items]

	navigateShop :: [HtmlTag] (Cart a) -> Task (ShopAction, Cart a) | iData a
	navigateShop prompt cart
		= chooseTask [] 
			[ ("Do Shopping",   	return (ToShop,   cart))
			, ("Check Out And Pay", return (ToPay,    cart))
			, ("Show Cart",     	return (ToCart,   cart))
			, ("Leave Shop",    	return (LeaveShop,cart))
			] <<? [ BrTag []
			      , boldText "Total cost of ordered items = "
			      , toHtml  (totalCost cart)
			      , DivTag [] prompt
			      ]

	doAction :: (Cart a) [a] (ShopAction, Cart a) -> Task Void | iData, DB, Product a
	doAction initCart items (action,cart)
		= case action of
			LeaveShop	= return Void
			ToCart		= showCart   cart       =>> doAction initCart items
			ToShop		= doShopping cart items =>> doAction initCart items
			ToPay		= checkOutAndPay cart   #>> browseShop shopPrompt cartPrompt initCart
	
	showCart ::(Cart a) -> Task (ShopAction,Cart a) | iData a
	showCart cart=:[]
		= navigateShop cartPrompt cart <<? [normalText "No items in cart yet!"]
	showCart cart
		= orTasksVert 
			[ navigateShop cartPrompt cart : map (itemActions cart) cart ]
	where
		itemActions :: (Cart a) (CartItem a) -> Task (ShopAction,Cart a) | iData a
		itemActions cart item
			= [toHtml item]
				?>> editTask "Change" {orderAmount = amountOrderedOf item} =>> \{orderAmount=n} -> 
				    return (ToCart,if (n <= 0) (filter (not o eqItemNr item) cart)
				                               (replace eqItemNr (amountOrderedUpd item n) cart)
				           )
	
	checkOutAndPay :: (Cart a) -> Task Void | iData a
	checkOutAndPay cart 
		= fillInClientInfo {createDefault & itemsOrdered = cart} =>> \order ->
		  yesOrNo [DivTag [] (costOrder order),normalText "Confirm Order"]
			(dbModify (append order) #>>
			 getMyName =>> \(myid,me) ->			
			 spawnProcess myid      True ("Order Confirmed",        [toHtml order:costOrder order] ?>> OK) #>>	
			 spawnProcess shopOwner True ("New Order from " <+++ me,[toHtml order:costOrder order] ?>> OK) #>>	
			 return Void)
			(return Void)
	where
		costOrder order	= section "Amount to pay is: " [toHtml (totalCost order.itemsOrdered)]
	
		fillInClientInfo order
			= fillInData name_prompt nameOf              nameUpd           order =>> \order ->
			  fillInData billing_prompt billingAddressOf billingAddressUpd order =>> \order ->
			  yesOrNo [ normalText "Billing address:", toHtml (billingAddressOf order)
			          , normalText "Is the shipping addres same as the billing address above?"
			          ]
			          (return {order & shippingAddress = billingAddressOf order})
			          (fillInData shipping_prompt 
						          shippingAddressOf shippingAddressUpd order) =>> \order ->
			  yesOrNo (showOrder        order)
				      (return           order) 
				      (fillInClientInfo order)
		where
			name_prompt     = "Please fill in your name:"
			billing_prompt  = "Please fill in the billing address:"
			shipping_prompt = "Please fill in the shipping address:"
			
			fillInData prompt valueOf updateOf record
				= [normalText prompt] ?>> editTask "Commit" (valueOf record) =>> \value -> 
				  return (updateOf record value)
			
			showOrder order
				= section "name:"             [toHtml (nameOf            order)] ++
				  section "billing address:"  [toHtml (billingAddressOf  order)] ++
				  section "shipping address:" [toHtml (shippingAddressOf order)] ++
				  section "ordered items:"    (map (toHtml o toInCart) order.itemsOrdered ++ [DivTag [] (costOrder order)]) ++
				  section "Confirm:"          [normalText "Is the data above correct?"]


// The shop management workflow:

manageCatalog :: [HtmlTag] a -> Task Void | iData, DB a
manageCatalog prompt defaultValue 
	= stopTask 
		(prompt ?>> foreverTask (dbReadAll =>> browseCatalog defaultValue))
where
	browseCatalog :: a [a] -> Task Void | iData, DB a
	browseCatalog defaultValue []	= editTask "Store" defaultValue =>> dbCreate 
	browseCatalog _ items			= orTasksVert (map itemActions items)
	where
		itemActions :: a -> Task Void | iData, DB a
		itemActions item
			= chooseTask_btn [toHtml item] 
				[("Edit",	editTask "Store" item =>> \nitem -> dbModify (replace eqItemId nitem)) 
				,("Insert",	addItem  (flip (<<?)) span     item)
				,("Append",	addItem        (?>>)  span_inc item)
				,("Delete",	dbModify (filter (not o eqItemId item)))
				]
		
		addItem show spanF item
			= show [toHtml item] (editTask "Store" createDefault) =>> \nitem -> dbModify (addItem` nitem)
		where
			addItem` nitem items
				= let (before,after) = spanF (not o (eqItemId item)) items
				   in before ++ [setItemId (newDBRef items) nitem] ++ after

// little markup functions:
boldText   text				= BTag    [] [Text text, BrTag [], BrTag []]
normalText text				= BodyTag [] [Text text, BrTag [], BrTag []]
ruleText   text				= section text [HrTag []]
section    label content	= [HrTag  [], boldText label : content]
