implementation module TravelBooking

import iTasks
import CommonDomain

// (c) 2007 MJP

// Test for multiple choice
// One can choose to book a flight, hotel and / or a car
// One by one the chosen bookings will be handled
// The bill is made up in the end

travelBookingExample :: [Workflow]
travelBookingExample
= [	{ name		= "Examples/Business/Book a trip"
	, label		= "Book your journey"
	, roles		= []
	, mainTask	= travel
	}
  ]

:: Booking :== (String,String,String,Currency)

travel :: Task Void
travel 
	=	sequence "travel" [ makeBookings <<@ "Step 1: Make Bookings:"
						  , confirmBookings <<@ "Step 2: Confirm Bookings:"					   
						  ]
		-||- 
		(showMessage "Cancel task?" >>| return [])

	>>= \booking -> handleBookings booking
where
	makeBookings :: Task [Booking]
	makeBookings = enterMultipleChoice [Text "Choose Booking options:"]
						[ (BookFlight <<@ "Book Flight")
						, (BookHotel <<@ "Book Hotel")
						, (BookCar <<@ "Book Car")
						]
					>>= \tasks -> sequence "bookings" tasks

	confirmBookings :: Task [Booking]
 	confirmBookings = showMessage "Confirm" >>| return []
 	
	handleBookings :: [[Booking]] -> Task Void
	handleBookings booking
		| isEmpty	booking	= showMessage "Cancelled"
		| otherwise			= (updateInformation "Pay" (calcCosts booking) >>| showMessage "Paid")
	where
		calcCosts booked = sum [cost \\ (_,_,_,cost) <- hd booked]


	BookFlight  = updateInformation "BookFlight" ("Flight Number "	,"", "Costs ",DefCosts)
	BookHotel  	= updateInformation "BookHotel" 	("Hotel Name "		,"", "Costs ",DefCosts)
	BookCar  	= updateInformation "BookCar" 	("Car Brand "		,"", "Costs ",DefCosts)

	DefCosts = EUR 0
