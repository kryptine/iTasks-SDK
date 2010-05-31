implementation module CoffeeTime

import iTasks

coffeeTimeExample :: [Workflow]
coffeeTimeExample = [workflow "Examples/Miscellaneous/Coffee time" coffeeTime]

/*
* Main workflow:
* - list all users
* - collect orders from all users
* - determine who goes to get coffee
* - let someone get coffee
*/
coffeeTime :: Task Void
coffeeTime
	=	getUsers
	>>= \users ->
		collectOrders users 
	>>= \orders ->
		determineWhoGoes (orderlist users orders)
	>>= \victim ->
		goGetCoffee victim (orderlist users orders)
where
	orderlist users orders = [(user, fromJust order) \\ user <- users & order <- orders | isJust order]
/*
* Collect the drinks orders from all users
*/
collectOrders :: [User] -> Task [Maybe String] 
collectOrders users = allTasks [u @: ("Coffee time!" @>> getOrder) \\ u <- users]
/*
* Ask someone if he/she wants something to drink
*/
getOrder :: Task (Maybe String)
getOrder
	=	requestConfirmation "It is coffee time, do you want something?"
	>>= \yes -> if yes
		(enterChoice "What do you want" ["Coffee","Tea","Chocolate"] >>= \choice -> return (Just choice))
		(return Nothing)
/*
* Determine who has to go get coffee
* A random choice is made between the people who want something
*/
determineWhoGoes :: [(User,String)] -> Task User
determineWhoGoes orders = randomChoice [user \\ (user,_) <- orders]
/*
* Give someone directions to go get coffee for everyone
*/
goGetCoffee :: User [(User,String)] -> Task Void
goGetCoffee user orders = user @: ("Get coffee" @>> showInstructionAbout "Coffee orders" "You have been chosen to get the following drinks" orders)
