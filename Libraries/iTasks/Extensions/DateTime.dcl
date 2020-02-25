definition module iTasks.Extensions.DateTime
/**
* This module provides types for working with dates and times
*/
import iTasks.WF.Definition
import iTasks.SDS.Definition

from Data.Error import :: MaybeError, :: MaybeErrorString
from Text.GenPrint import generic gPrint, :: PrintState, class PrintOutput
from StdString import class toString, class fromString
from StdClass import class <
from StdOverloaded import class ==

//* local date and time
:: Date	=
	{ year	:: !Int
	, mon	:: !Int
	, day	:: !Int
	}

:: Time =
	{ hour	:: !Int
	, min	:: !Int
	, sec	:: !Int
	}

derive gPrint Time

:: DateTime =
	{ year	:: !Int
	, mon	:: !Int 
	, day	:: !Int
	, hour	:: !Int
	, min	:: !Int
	, sec	:: !Int
	}

//Conversion
toTime :: DateTime -> Time
toDate :: DateTime -> Date
toDateTime :: Date Time -> DateTime

//Printing and parsing
instance toString Date, Time, DateTime

parseDate :: String -> MaybeErrorString Date         //Expected format: "yyyy-mm-dd"
parseTime :: String -> MaybeErrorString Time         //Expected format: "hh:mm:ss"
parseDateTime :: String -> MaybeErrorString DateTime //Expected format: "yyyy-mm-dd hh:mm:ss"

instance fromString	Date, Time, DateTime //Assumes parse* succeeds

//Comparison
instance ==	Date, Time, DateTime
instance <	Date, Time, DateTime

derive JSONEncode		Date, Time, DateTime
derive JSONDecode		Date, Time, DateTime
derive gEq				Date, Time, DateTime
derive gText	        Date, Time, DateTime
derive gEditor 			Date, Time, DateTime

/*** Time & Date Conversion ***/
/**
* Converts a timestamp to UTC DateTime.
*
* @param Timestamp: The timestamp to convert.
*
* @return The resulting UTC DateTime
*/
timestampToGmDateTime	 :: !Timestamp -> DateTime
/**
* Converts a timestamp to local DateTime.
* This is a task, as the local time zone has to be detected.
*
* @param Timestamp: The timestamp to convert.
*
* @return The resulting local DateTime
*/
timestampToLocalDateTime :: !Timestamp -> Task DateTime
/**
* Converts a local Date to a timestamp.
* This is a task, as the local time zone has to be detected.
*
* @param Date: The date to convert
*
* @return The resulting timestamp
*/
localDateToTimestamp     :: !Date      -> Task Timestamp
/**
* Converts a local DateTime to a timestamp.
* This is a task, as the local time zone has to be detected.
*
* @param Date: The date & time to convert
*
* @return The resulting timestamp
*/
localDateTimeToTimestamp :: !DateTime  -> Task Timestamp
/**
* Converts a UTC Date to a timestamp.
*
* @param Date: The date to convert
*
* @return The resulting timestamp
*/
utcDateToTimestamp     :: !Date      -> Timestamp
/**
* Converts a UTC DateTime to a timestamp.
*
* @param Date: The date & time to convert
*
* @return The resulting timestamp
*/
utcDateTimeToTimestamp :: !DateTime  -> Timestamp


/*** Special wait tasks ***/
/**
* Creates a task which blocks a workflow until a specified time.
*
* @param Whether to show a UI with the current time
* @param Time: The specified time at which the task should complete
*
* @return The time to wait for
*/
waitForTime :: !Bool !Time -> Task Time

/**
* Creates a task which blocks a workflow until a specified date.
*
* @param Whether to show a UI with the current date
* @param Date: The specified date at which the task should complete
*
* @return The date to wait for
*/
waitForDate :: !Bool !Date -> Task Date

/**
* Creates a task which blocks a workflow until a specified date and time.
*
* @param Whether to show a UI with the current date and time
* @param DateTime: The specified date and time at which the task should complete
*
* @return The date and time to wait for
*/
waitForDateTime :: !Bool !DateTime -> Task DateTime

/**
* Task completes after specified amount of time has passed
* since the creation of the task.
*
* @param Whether to show a UI with the current date and time
* @param The time to wait (in seconds before the task should complete
*
* @return The time the timer went off
*/
waitForTimer :: !Bool !Int -> Task DateTime


/**
 * Automatically stamp the share data with the datetime of writing
 *
 * @param the sds to automatically stamp the data for
 * @return the resulting sds
 */
dateTimeStampedShare :: !(sds p b (DateTime,c)) -> SDSLens p b c | gText{|*|}, TC p & TC b & TC c & RWShared sds
