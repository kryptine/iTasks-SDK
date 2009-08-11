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
= [	{ name		= "Examples/Business/Travel booking/Book a trip"
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
		buttonTask "Cancel" (return [])

	>>= \booking -> handleBookings booking
where
	makeBookings :: Task [Booking]
	makeBookings = requestMultipleChoice [Text "Choose Booking options:"]
						[ (BookFlight <<@ "Book Flight")
						, (BookHotel <<@ "Book Hotel")
						, (BookCar <<@ "Book Car")
						]
					>>= \tasks -> sequence "bookings" tasks

	confirmBookings :: Task [Booking]
 	confirmBookings = buttonTask "Confirm" (return [])
 	
	handleBookings :: [[Booking]] -> Task Void
	handleBookings booking
		| isEmpty	booking	= showMessage "Cancelled"
		| otherwise			= (requestInformationWD "Pay" (calcCosts booking) >>| showMessage "Paid")
	where
		calcCosts booked = sum [cost \\ (_,_,_,cost) <- hd booked]


	BookFlight  = requestInformationWD "BookFlight" ("Flight Number "	,"", "Costs ",DefCosts)
	BookHotel  	= requestInformationWD "BookHotel" 	("Hotel Name "		,"", "Costs ",DefCosts)
	BookCar  	= requestInformationWD "BookCar" 	("Car Brand "		,"", "Costs ",DefCosts)

	DefCosts = EUR 0
