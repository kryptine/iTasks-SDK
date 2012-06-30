module FlightCheckIn

import iTasks,Tasklet,FlightSupport

ifStable (Value v Stable) = True
ifStable _ = False

always = const True

returnV :: (TaskValue a) -> Task a | iTask a
returnV (Value v _) = return v

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

:: BookingInfo = BookingReference String | PassangerLastName String
:: Booking = {bookingRef :: String, firstName :: String, lastName :: String, flightNumber :: String, id :: Hidden String, seat :: Maybe Seat}
:: Flight = {flightNumber :: String, rows :: Int, layout :: [Int], freeSeats :: [Seat]}

derive class iTask BookingInfo, Booking, MaybeError, Flight

derive gVisualizeEditor Seat
derive gHeaders Seat
derive gGridRows Seat
derive gUpdate Seat
derive gDefaultMask Seat
derive gVerify Seat
derive JSONEncode Seat
derive JSONDecode Seat
derive gEq Seat

gVisualizeText{|Seat|} _ seat = [toString seat]

commitCheckIn b seat (bs, fs)
	= (updateBooking b.id seat bs, removeSeat b.Booking.flightNumber seat fs)

removeSeat flightNumber seat [f:fs]
	| f.Flight.flightNumber == flightNumber
		= [{f & freeSeats = removeMember seat f.freeSeats}:fs]
		= [f:removeSeat flightNumber seat fs] 

updateBooking pid seat [] = []
updateBooking pid seat [p:ps]
	| fromHidden p.id == fromHidden pid 
		= [{p & seat = Just seat}:ps]
		= [p:updateBooking pid seat ps] 

findFlight fno = 
	get flightStore >>= return  o find (\f -> f.Flight.flightNumber == fno)

findBooking ref = 
	get bookingStore >>= return o find (\{bookingRef} -> bookingRef == ref)

getBookings f =
	get bookingStore >>= return o filter f

bookingStore :: Shared [Booking]
bookingStore = sharedStore "Bookings" 
	[ {firstName = "Laszlo", lastName = "Domoszlai", bookingRef = "BHJZ345", flightNumber = "BA1234", id = Hidden "ID1L", seat = Nothing}
	, {firstName = "Igor",   lastName = "Domoszlai", bookingRef = "BHJZ346", flightNumber = "BA1234", id = Hidden "ID8I", seat = Nothing}
	, {firstName = "Teodor", lastName = "Domoszlai", bookingRef = "BHJZ347", flightNumber = "BA1234", id = Hidden "ID3T", seat = Just (Seat 1 1)}]

flightStore :: Shared [Flight]
flightStore = sharedStore "Flights" 
	[{flightNumber = "BA1234", rows = 20, layout = [3,3], freeSeats = [Seat 13 1,Seat 13 5, Seat 11 1,Seat 10 2]}]

task_checkin :: Task Void
task_checkin
	= catchAll (	
	    enterInformation "Please enter booking information:" []
	>>=	\ref  -> lookUpBooking ref
	>>= \mbP  -> verifyBooking mbP
	>>= \p    -> findFlight p.Booking.flightNumber 
	>>= \f    -> chooseSeat f
	>>= \seat -> update (commitCheckIn p seat) (bookingStore >+< flightStore)
	>>| findBooking p.bookingRef // refresh booking record
	>>= viewInformation "Check-in succeeded:" []
	>>*	[OnAction ActionNext always (const task_checkin)]
	) errorHandler
where
	errorHandler msg = viewInformation "Error:" [] msg >>| task_checkin

	lookUpBooking (BookingReference ref) = findBooking ref
	lookUpBooking (PassangerLastName lname) 
		=   getBookings (\p -> p.lastName == lname && isNothing p.seat)
		>>= \bs -> case bs of 
			[] = return Nothing
			fs = enterChoice "Please choose seat:" [ChooseWith ChooseFromGrid id] fs >>= return o Just	

	verifyBooking Nothing = throw "Passanger cannot be found."
	verifyBooking (Just {seat = Just _}) = throw "Passanger already checked-in."
	verifyBooking (Just p) =  viewInformation "Passanger:" [] p 
								||-
								enterInformation "Please enter you id number:" []
		>>= \id -> if (fromHidden p.id == id) (return p) (throw "Identification falied.")

/*
	chooseSeat (Just f)
	    =   enterChoice "..." [] (map toString (sort f.freeSeats))
		>>= \s -> return (fromString s)
*/

chooseSeat (Just f)
	= mkTask seatTasklet

where
	seatTasklet :: Tasklet (Maybe Seat) Seat
	seatTasklet = 
		{ defSt				= Nothing
		, generatorFunc		= generateGUI
		, resultFunc		= \mbS = case mbS of 
										Nothing = NoValue 
										Just s  = Value s Stable
		, tweakUI  			= \t = (paneled (Just "Seat chooser Tasklet") (Just "Choose seat:") Nothing [t])	
		}

	occupiedStyle = StyleAttr "float: left; border-style:solid; background-color:blue; border-color:black; width: 15px; height: 15px; margin: 1px;"
	freeStyle = StyleAttr "float: left; border-style:solid; background-color:white; border-color:black; width: 15px; height: 15px; margin: 1px;"
	corridorStyle = StyleAttr "float: left; background-color:white; width: 20px; height: 15px;"

	mm i [] = []
	mm i [x:xs] = [y : mm (i+length y) xs] where y = take x [i..]

	rowlayout = intercalate [-1] (mm 1 f.layout)
	genSeatId row seat = "_seat_" +++ toString (Seat row seat)

	genRowLayout row -1 = DivTag [corridorStyle] []
	genRowLayout row seat | isMember (Seat row seat) f.freeSeats
			= DivTag [TitleAttr (toString (Seat row seat)), IdAttr (genSeatId row seat), freeStyle] []
			= DivTag [TitleAttr (toString (Seat row seat)), occupiedStyle] []

	attachHandlers s=:(Seat row seat) = 
 		[HtmlEvent (genSeatId row seat) "click" (onClick s),
 		 HtmlEvent (genSeatId row seat) "mouseover" (setColor "pink"),
 		 HtmlEvent (genSeatId row seat) "mouseout" (setColor "white")]
 		
 	onClick seat _ _ _ d = (d, Just seat)
 	setColor color state _ e d
		# (d, e, target) = getObjectAttr d e "target" 	
		# (d, _, _) = setObjectAttr d target "style.backgroundColor" color
 		= (d, state)

	generateGUI _ state iworld  

		# html = DivTag []
					(intercalate [DivTag [StyleAttr "clear: both;"] []]
								 [map (genRowLayout i) rowlayout \\ i <- [1 .. f.rows]])
			
		# gui = { TaskletHTML |
				  width  		= Fixed 300
				, height 		= Fixed 300
				, html   		= toString html
				, eventHandlers = flatten (map attachHandlers f.freeSeats)
				}
			
		= (TaskletHTML gui, state, iworld)
								 
taskletExamples :: [Workflow]
taskletExamples =
	[workflow "Check-in" "Flight check-in" task_checkin]
    
Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world



