implementation module TravelBooking

import iTasks

// (c) 2007 MJP

// Test for multiple choice
// One can choose to book a flight, hotel and / or a car
// One by one the chosen bookings will be handled
// The bill is made up in the end

derive class iTask	PlaceToGo, FlightHotel

derive bimap	(,), Maybe

BookTrip :: Task FlightHotel
BookTrip
	=						enterInformation ("Specify trip","Please fill in trip information to make booking") []
		>>= \info ->		info.delegateTo @: enterInformation ("Book trip","Please book the following trip") [About info]
		>>= \booking ->		showInformation ("Trip booked","The following trip has been booked") [] booking
	

:: PlaceToGo
	=	{ destination 	:: String
		, leaving		:: Date
		, returning		:: Date
		, delegateTo	:: User
		}
:: FlightHotel
	=	{ carrier		:: String
		, priceOfFlight	:: Currency
		, nameOfHotel	:: String
		, priceAllIn	:: Currency
		}


travelBookingExample :: [Workflow]
travelBookingExample = [workflow "Examples/Business/Delegate book a trip" "Book a trip" BookTrip]

:: Booking :== (String,String,String,Currency)

travel :: Task Void
travel 
	=	sequence "travel" [ makeBookings 
						  , confirmBookings 				   
						  ]
		-||- 
		(showInformation ("Cancel","Cancel task?") [] [])

	>>= \booking -> handleBookings booking
where
	makeBookings :: Task [Booking]
	makeBookings
		=	Title "Step 1: Make Bookings:"
		@>> enterMultipleChoice ("Booking options","Choose Booking options:") [] [BookFlight,BookHotel,BookCar]
		>>= sequence "Bookings"

	confirmBookings :: Task [Booking]
 	confirmBookings 
 		=	Title "Step 2: Confirm Bookings:"
 		@>> showInformation ("Confirmation","Confirm") [] []
 	
	handleBookings :: [[Booking]] -> Task Void
	handleBookings booking
		| isEmpty	booking	= showInformation "Summary" [] "Cancelled" >>| stop
		| otherwise			= (updateInformation ("Payment","Pay") [] (calcCosts booking) >>| showInformation "Summary" [] "Paid" >>| stop)
	where
		calcCosts booked = sum [cost \\ (_,_,_,cost) <- hd booked]


	BookFlight  = updateInformation ("Book Flight","BookFlight") [] ("Flight Number "	,"", "Costs ",DefCosts)
	BookHotel  	= updateInformation ("Book Hotel","BookHotel") [] ("Hotel Name "		,"", "Costs ",DefCosts)
	BookCar  	= updateInformation ("Book Car","BookCar") [] ("Car Brand "		,"", "Costs ",DefCosts)

	DefCosts = EUR 0
