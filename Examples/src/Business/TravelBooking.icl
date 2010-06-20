implementation module TravelBooking

import iTasks
import CommonDomain

// (c) 2007 MJP

// Test for multiple choice
// One can choose to book a flight, hotel and / or a car
// One by one the chosen bookings will be handled
// The bill is made up in the end

derive gParse		PlaceToGo, FlightHotel
derive gPrint		PlaceToGo, FlightHotel
derive gUpdate		PlaceToGo, FlightHotel
derive gError		PlaceToGo, FlightHotel
derive gHint		PlaceToGo, FlightHotel
derive gVisualize	PlaceToGo, FlightHotel
derive bimap	(,), Maybe

BookTrip :: Task FlightHotel
BookTrip
	=						enterInformation "Please fill in trip information to make booking"
		>>= \info ->		assign info.delegateTo (enterInformationAbout "Please book the following trip" info)
		>>= \booking ->		showMessageAbout "The following trip has been booked" booking
		>>|					return booking
	

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
travelBookingExample
= [	// workflow "Examples/Business/Book a trip" travel  , 
  	workflow "Examples/Business/Delegate book a trip" BookTrip
  ]

:: Booking :== (String,String,String,Currency)

travel :: Task Void
travel 
	=	sequence "travel" [ makeBookings <<@ Subject "Step 1: Make Bookings:"
						  , confirmBookings <<@ Subject "Step 2: Confirm Bookings:"					   
						  ]
		-||- 
		(showMessage "Cancel task?" >>| return [])

	>>= \booking -> handleBookings booking
where
	makeBookings :: Task [Booking]
	makeBookings = enterMultipleChoice [Text "Choose Booking options:"]
						[ (BookFlight <<@ Subject "Book Flight")
						, (BookHotel <<@ Subject "Book Hotel")
						, (BookCar <<@ Subject "Book Car")
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
