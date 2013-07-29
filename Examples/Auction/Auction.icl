module Auction

Start = 0

//import iTasks

//:: Bid =
  //{  user   :: User
  //,  ware   :: String
  //,  price  :: Int
  //}

//derive class iTask Bid

//auction :: (Bid -> Task Bid) User [User] Bid -> Task Bid
//auction bidf auctioneer bidders current =
  //let g bidf auctioneer bidders current =
         //anyTask [b @: bidf current \\ b <- [auctioneer : bidders]] >>= \newBid ->
           //case newBid.price > current.price of
             //True  -> (\current -> g bidf auctioneer bidders current) newBid
             //False -> case newBid.user == auctioneer of
                        //True  -> return current
                        //False -> g bidf auctioneer bidders current
  //in g bidf auctioneer bidders current

:: Task a = Task a

auction :: Task Int
auction = Task 0

