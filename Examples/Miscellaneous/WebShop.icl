implementation module WebShop

import StdClass, StdList
from   StdFunc import o, flip
import iTasks
import StdListExt, iTaskCombinatorsExt, ShopDSL

webShopExample :: [Workflow]
webShopExample 
= [ { name		= "Examples/Miscellaneous/Webshop/Manage catalog"
    , label		= "Manage catalog"
    , roles		= []
    , mainTask	= manageCatalog defaultProduct catalogPrompt >>| return Void
    }
  , { name		= "Examples/Miscellaneous/Webshop/Browse shop"
	, label		= "Browse shop"
	, roles		= []
	, mainTask	= browseShop defaultCart shopPrompt cartPrompt
    }
  ]

shopPrompt 		= ruleText "Welcome to My Shop"
cartPrompt 		= ruleText "My Shop Shopping Cart"
catalogPrompt 	= ruleText "My Shop Product Catalogue Browser"

// The shop management workflow:

manageCatalog :: a [HtmlTag] -> Task a | iTask, DB a
manageCatalog _ prompt
	= stopTask (prompt ?>> forever (dbReadAll >>= browseCatalog))
where
	browseCatalog :: [a] -> Task a | iTask, DB a
	browseCatalog items = orTasksVert 
							(map itemActions items ++ [chooseTask_btn [] [("Append",new)]])
	where
		new	= dbCreateItem >>= \first -> editTask "Store" first >>= dbUpdateItem
	
		itemActions :: a -> Task a | iTask, DB a
		itemActions item
			= chooseTask_btn (visualizeAsHtmlDisplay item) 
				[("Edit",	editTask "Store" item >>= dbUpdateItem)
				,("Delete",	dbDeleteItem (getItemId item) >>| return item)
				]

// The customer workflow:		

browseShop :: (Cart a) [HtmlTag] [HtmlTag] -> Task Void | iTask, DB, Product a
browseShop initCart shopPrompt cartPrompt
	= dbReadAll      >>= \items ->
	  doShopping initCart items >>= 
	  doAction   initCart items
where
	doShopping :: (Cart a) [a] -> Task (ShopAction,Cart a) | iTask, Product a
	doShopping cart []
		= (shopPrompt ++ [normalText "Currently no items in catalogue, sorry."])
		  ?>> OK >>| return (LeaveShop,cart)
	doShopping cart items
		= orTasksVert
			[ navigateShop shopPrompt cart
			: map (itemActions cart) items					
			] 
	where
		itemActions :: (Cart a) a -> Task (ShopAction,Cart a) | iTask, Product a
		itemActions cart item
			= chooseTask_btn (visualizeAsHtmlDisplay item)
			    [("Add to Cart", return (ToCart, add (toCartItem item) cart))]
		where
			add :: (CartItem a) [CartItem a] -> [CartItem a]
			add new []				= [new]
			add new [item:items]	= if (eqItemNr new item) 
									     [amountOrderedUpd item (amountOrderedOf item+1):items]
									     [item:add new items]

	navigateShop :: [HtmlTag] (Cart a) -> Task (ShopAction, Cart a) | iTask a
	navigateShop prompt cart
		= chooseTask [] 
			[ ("Do Shopping",   	return (ToShop,   cart))
			, ("Check Out And Pay", return (ToPay,    cart))
			, ("Show Cart",     	return (ToCart,   cart))
			, ("Leave Shop",    	return (LeaveShop,cart))
			] <<? ([ BrTag []
			      , boldText "Total cost of ordered items = "
			      : visualizeAsHtmlDisplay (totalCost cart)] ++
			      [ DivTag [] prompt])

	doAction :: (Cart a) [a] (ShopAction, Cart a) -> Task Void | iTask, DB, Product a
	doAction initCart items (action,cart)
		= case action of
			LeaveShop	= return Void
			ToCart		= showCart   cart       >>= doAction initCart items
			ToShop		= doShopping cart items >>= doAction initCart items
			ToPay		= checkOutAndPay cart   >>| browseShop initCart shopPrompt cartPrompt
	
	showCart :: (Cart a) -> Task (ShopAction,Cart a) | iTask a
	showCart cart=:[]
		= navigateShop cartPrompt cart <<? [normalText "No items in cart yet!"]
	showCart cart
		= orTasksVert 
			[ navigateShop cartPrompt cart : map (itemActions cart) cart ]
	where
		itemActions :: (Cart a) (CartItem a) -> Task (ShopAction,Cart a) | iTask a
		itemActions cart item
			= (visualizeAsHtmlDisplay item)
				?>> editTask "Change" {orderAmount = amountOrderedOf item} >>= \{orderAmount=n} -> 
				    return (ToCart,if (n <= 0) (filter (not o eqItemNr item) cart)
				                               (lreplace eqItemNr (amountOrderedUpd item n) cart)
				           )
	
	checkOutAndPay :: (Cart a) -> Task Void | iTask a
	checkOutAndPay cart 
		= dbCreateItem >>= \order -> 
		  fillInClientInfo {order & itemsOrdered = cart} >>= \order ->
		  yesOrNo [DivTag [] (costOrder order),normalText "Confirm Order"]
		    (dbUpdateItem order >>|
		     getCurrentUser >>= \(myid,me) -> 
			 spawnProcess myid True (((visualizeAsHtmlDisplay order ++ costOrder order) ?>> OK) <<@ "Order Confirmed" >>|	
			 	(spawnProcess shopOwner True (((visualizeAsHtmlDisplay order ++ costOrder order) ?>> OK) <<@ "New Order from " <+++ me) )
			) >>| return Void)
			(return Void)
	where
		costOrder order	= section "Amount to pay is: " (visualizeAsHtmlDisplay (totalCost order.itemsOrdered))
	
		fillInClientInfo order
			= fillInData name_prompt nameOf              nameUpd           order >>= \order ->
			  fillInData billing_prompt billingAddressOf billingAddressUpd order >>= \order ->
			  yesOrNo ([ normalText "Billing address:" :visualizeAsHtmlDisplay (billingAddressOf order)
			          ] ++ [ normalText "Is the shipping addres same as the billing address above?"
			          ])
			          (return {order & shippingAddress = billingAddressOf order})
			          (fillInData shipping_prompt 
						          shippingAddressOf shippingAddressUpd order) >>= \order ->
			  yesOrNo (showOrder        order)
				      (return           order) 
				      (fillInClientInfo order)
		where
			name_prompt     = "Please fill in your name:"
			billing_prompt  = "Please fill in the billing address:"
			shipping_prompt = "Please fill in the shipping address:"
			
			fillInData prompt valueOf updateOf record
				= [normalText prompt] ?>> editTask "Commit" (valueOf record) >>= \value -> 
				  return (updateOf record value)
			
			showOrder order
				= section "name:"             (visualizeAsHtmlDisplay (nameOf            order)) ++
				  section "billing address:"  (visualizeAsHtmlDisplay (billingAddressOf  order)) ++
				  section "shipping address:" (visualizeAsHtmlDisplay (shippingAddressOf order)) ++
				  section "ordered items:"    (flatten (map (visualizeAsHtmlDisplay o toInCart) order.itemsOrdered) ++ [DivTag [] (costOrder order)]) ++
				  section "Confirm:"          [normalText "Is the data above correct?"]

// little markup functions:

boldText   text				= BTag    [] [Text text, BrTag [], BrTag []]
normalText text				= BodyTag [] [Text text, BrTag [], BrTag []]
ruleText   text				= section text [HrTag []]
section    label content	= [HrTag  [], boldText label : content]
