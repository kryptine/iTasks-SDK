module Auction

//:%s/<I\d*, E\d*>\|<\d*>//g 

Start = 0

import iTasks

:: Bid =
  {  user   :: User
  ,  ware   :: String
  ,  price  :: Int
  }

derive class iTask Bid

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

auction :: (Bid -> Task Bid) User [User] Bid -> Task Bid
auction bidf auctioneer bidders current =
  anyTask [b @: bidf current \\ b <- [auctioneer : bidders]] >>= \newBid ->
    case newBid.price > current.price of
      True  -> (\current -> auction bidf auctioneer bidders current) newBid
      False -> case newBid.user == auctioneer of
                 True  -> return current
                 False -> auction bidf auctioneer bidders current

//auction :: (Bid -> Task Bid) User [User] Bid -> Task Bid
//auction bidf auctioneer bidders current =
  //anyTask  [  return { Bid | user = auctioneer, ware = "food", price = 42 }
           //,  return { Bid | user = auctioneer, ware = "food", price = 42 }] >>= \newBid ->
  ////anyTask [auctioneer @: bidf current, auctioneer @: bidf current] >>= \newBid ->
    //case newBid.price > current.price of
      //True  -> (\current -> auction bidf auctioneer bidders current) newBid
      //False -> case newBid.user == auctioneer of
                 //True  -> return current
                 //False -> auction bidf auctioneer bidders current

//auction :: (Bid -> Task Bid) User [User] Bid -> Task Bid
//auction bidf auctioneer bidders current =
  //return { Bid | user = auctioneer, ware = "food", price = 42 } >>= \newBid ->
    //case newBid.price > current.price of
      //True  -> (\current -> auction bidf auctioneer bidders current) newBid
      //False -> case newBid.user == auctioneer of
                 //True  -> return current
                 //False -> auction bidf auctioneer bidders current

//auction :: (Bid -> Task Bid) User [User] Bid -> Task Bid
//auction bidf auctioneer bidders current =
  //return { Bid | user = auctioneer, ware = "food", price = 42 } >>= \newBid ->
    //case newBid.price > current.price of
      //True  -> auction bidf auctioneer bidders newBid
      //False -> case newBid.user == auctioneer of
                 //True  -> return current
                 //False -> auction bidf auctioneer bidders current

//auction :: (Bid -> Task Bid) User [User] Bid -> Task Bid
//auction bidf auctioneer bidders current =
  //return { Bid | user = auctioneer, ware = "food", price = 42 } >>= \newBid ->
    //if (newBid.price > current.price)
      //(auction bidf auctioneer bidders newBid)
      //(if (newBid.user == auctioneer)
         //(return current)
         //(auction bidf auctioneer bidders current))

//auction :: Task Int
//auction = let x = 1
          //in let y = 2
             //in return (x + y)
//auction = case True of
            //True  -> return 1
            //False -> return 0

