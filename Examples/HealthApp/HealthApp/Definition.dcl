definition module HealthApp.Definition

import iTasks
import iTasks.Extensions.DateTime

derive class iTask Address, SSN, AppointmentLength, Appointment, Medication, Client, ClientPhoto

:: Address =
	{ street :: String
	, number :: String
	, zipCode :: String
	, city :: String
	, state :: String
	, country :: String }

:: SSN = SSN String
:: AppointmentLength =
	{ hours :: Int
	, minutes :: Int }

:: Appointment =
	{ description :: String
	, time :: DateTime
	, length :: AppointmentLength
	, location :: String }

:: Medication =
	{ name :: String
	, description :: String
	, amount :: String }

:: ClientPhoto = ClientPhoto

:: Client =
	{ name :: String
	, address :: Address
	, ssn :: SSN
	, appointments :: [Appointment]
	, medication :: [Medication]
	, photo :: ClientPhoto }
