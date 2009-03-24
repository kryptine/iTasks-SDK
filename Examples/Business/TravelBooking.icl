implementation module TravelBooking

import iTasks, iDataTrivial

// (c) 2007 MJP

// Test for multiple choice
// One can choose to book a flight, hotel and / or a car
// One by one the chosen bookings will be handled
// The bill is made up in the end

travelBookingExample :: [Workflow]
travelBookingExample
= [	{ name		= "Examples/Business/Travel booking/Book a trip"
	, label		= "Book a trip"
	, roles		= []
	, mainTask	= travel
	}
  ]

travel :: Task Void
travel 
= 			[Text "Book your journey:",BrTag [],BrTag []]
			?>>	seqTasks	[ ( "Step 1: Make Bookings:"
							  , mchoiceTasks [Text "Choose Booking options:"]
							  	[ ("Book Flight",BookFlight)
								, ("Book Hotel", BookHotel)
								, ("Book Car",   BookCar)
								]
							  )
							, ( "Step 2: Confirm Bookings:"
							  , buttonTask "Confirm" (return_V [])
							  )
							]
				-||- 
				buttonTask "Cancel" (return_V [])
	=>> \booking -> [Text "Handling bookings:",BrTag [],BrTag []]
					?>> handleBookings booking
where
	handleBookings booking
	| isNil	booking	= 		editTask "Cancelled" Void
	| otherwise		= 		editTask "Pay" (Dsp (calcCosts booking))
					  #>>	editTask "Paid" Void
	where
		calcCosts booked = sum [cost \\ (_,_,_,cost) <- hd booked]

		isNil [] = True
		isNil _ = False

	BookFlight  = editTask "BookFlight" (Dsp "Flight Number "	,"",Dsp "Costs ",DefCosts) 	<<@ Submit
	BookHotel  	= editTask "BookHotel" 	(Dsp "Hotel Name "		,"",Dsp "Costs ",DefCosts)	<<@ Submit
	BookCar  	= editTask "BookCar" 	(Dsp "Car Brand "		,"",Dsp "Costs ",DefCosts)	<<@ Submit


Dsp = DisplayMode 

DefCosts = HtmlCurrency EUR 0
