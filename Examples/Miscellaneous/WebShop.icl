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
	= stopTask (showStickyMessage prompt ||- forever (dbReadAll >>= browseCatalog))
where
	browseCatalog :: [a] -> Task a | iTask, DB a
	browseCatalog items = orTasksVert 
							(map itemActions items ++ [showMessage "Append?" >>| new])
	where
		new	= dbCreateItem >>= \first -> updateInformation "Enter details of new item:" first >>= dbUpdateItem
	
		itemActions :: a -> Task a | iTask, DB a
		itemActions item
			= enterChoiceAbout "" item
				[(updateInformation "Please update the following item:" item >>= dbUpdateItem) <<@ "Edit"
				,(dbDeleteItem (getItemId item) >>| return item) <<@ "Delete"
				]
			>>= \task -> task

// The customer workflow:		

browseShop :: (Cart a) [HtmlTag] [HtmlTag] -> Task Void | iTask, DB, Product a
browseShop initCart shopPrompt cartPrompt
	= dbReadAll      >>= \items ->
	  doShopping initCart items >>= 
	  doAction   initCart items
where
	doShopping :: (Cart a) [a] -> Task (ShopAction,Cart a) | iTask, Product a
	doShopping cart []
		= showMessage (shopPrompt ++ [normalText "Currently no items in catalogue, sorry."])
		  >>| return (LeaveShop,cart)
	doShopping cart items
		= orTasksVert
			[ navigateShop shopPrompt cart
			: map (itemActions cart) items					
			] 
	where
		itemActions :: (Cart a) a -> Task (ShopAction,Cart a) | iTask, Product a
		itemActions cart item
			=	showMessageAbout "Add to cart" item
			>>|	return (ToCart, add (toCartItem item) cart)
		where
			add :: (CartItem a) [CartItem a] -> [CartItem a]
			add new []				= [new]
			add new [item:items]	= if (eqItemNr new item) 
									     [amountOrderedUpd item (amountOrderedOf item+1):items]
									     [item:add new items]

	navigateShop :: [HtmlTag] (Cart a) -> Task (ShopAction, Cart a) | iTask a
	navigateShop prompt cart
		= (enterChoice "" 
			[ return (ToShop,   cart) <<@ "Do Shopping"
			, return (ToPay,    cart) <<@ "Check Out And Pay"
			, return (ToCart,   cart) <<@ "Show Cart"
			, return (LeaveShop,cart) <<@ "Leave Shop"
			] >>= \task -> task) -||- (( showStickyMessageAbout [BrTag [], boldText "Total cost of ordered items = "] (totalCost cart)
			      	  -&&-
			      	  showStickyMessage prompt
			      	 ) >>| return defaultValue)
			     
	doAction :: (Cart a) [a] (ShopAction, Cart a) -> Task Void | iTask, DB, Product a
	doAction initCart items (action,cart)
		= case action of
			LeaveShop	= return Void
			ToCart		= showCart   cart       >>= doAction initCart items
			ToShop		= doShopping cart items >>= doAction initCart items
			ToPay		= checkOutAndPay cart   >>| browseShop initCart shopPrompt cartPrompt
	
	showCart :: (Cart a) -> Task (ShopAction,Cart a) | iTask a
	showCart cart=:[]
		= navigateShop cartPrompt cart -||- (showStickyMessage [normalText "No items in cart yet!"] >>| return defaultValue)
	showCart cart
		= orTasksVert 
			[ navigateShop cartPrompt cart : map (itemActions cart) cart ]
	where
		itemActions :: (Cart a) (CartItem a) -> Task (ShopAction,Cart a) | iTask a
		itemActions cart item
			= 	showStickyMessageAbout "Item: " item
				||-
			 	updateInformation "Change amount: " {orderAmount = amountOrderedOf item}
				>>= \{orderAmount=n} -> 
					return (ToCart,if (n <= 0) (filter (not o eqItemNr item) cart)
				                               (lreplace eqItemNr (amountOrderedUpd item n) cart)
				           )
	
	checkOutAndPay :: (Cart a) -> Task Void | iTask a
	checkOutAndPay cart 
		= dbCreateItem >>= \order -> 
		  fillInClientInfo {order & itemsOrdered = cart} >>= \order ->
		  yesOrNo [normalText "Confirm Order"]
		    (dbUpdateItem order >>|
		     getCurrentUser >>= \me -> 
			 spawnProcess me.User.userId True (showMessageAbout "Order:" (order,costOrder order) <<@ "Order Confirmed")
			 >>|	
			 spawnProcess shopOwner True (showMessageAbout "Order" (order,costOrder order) <<@ "New Order from " <+++ me.User.displayName) 
			 >>| return Void
			)
			(return Void)
	where
		costOrder order	= totalCost order.itemsOrdered
	
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
				= updateInformation [normalText prompt] (valueOf record) >>= \value -> 
				  return (updateOf record value)
			
			showOrder order
				= section "name:"             (visualizeAsHtmlDisplay (nameOf            order)) ++
				  section "billing address:"  (visualizeAsHtmlDisplay (billingAddressOf  order)) ++
				  section "shipping address:" (visualizeAsHtmlDisplay (shippingAddressOf order)) ++
				  section "ordered items:"    (flatten (map (visualizeAsHtmlDisplay o toInCart) order.itemsOrdered) ++ [DivTag [] (visualizeAsHtmlDisplay (costOrder order))]) ++
				  section "Confirm:"          [normalText "Is the data above correct?"]

// little markup functions:

boldText   text				= BTag    [] [Text text, BrTag [], BrTag []]
normalText text				= BodyTag [] [Text text, BrTag [], BrTag []]
ruleText   text				= section text [HrTag []]
section    label content	= [HrTag  [], boldText label : content]
