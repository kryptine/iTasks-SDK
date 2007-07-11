definition module iDataSettings

// iData & iTask Library
// Concept & Programming (c) 2005 - 2007 Rinus Plasmeijer

import iDataHandler
import Gerda							// OPTION: GEneRic Database Access to a standard relational database, made by Arjen van Weelden
import DataFile							// OPTION: A fast generic database stored in a file, made by Arjen van Weelden


// Global settings of iData applications

class iData a							// The collection of generic functions needed to make iData:	
		| gForm {|*|}					// Creates an Html Form
		, iCreateAndPrint				
		, iParse
		, iSpecialStore a

class iCreateAndPrint a					// Used for tracing iTasks
		| iCreate
		, iPrint a	

class iCreate a
		| gUpd  {|*|} a					// Makes it possible to update and create any value, given a change somewhere in the data structure

class iPrint a
		| gPrint{|*|} a					// To serialize a value to a String

class iParse a
		| gParse{|*|} a					// To de-serialize a string back to a value
		
class iSpecialStore a
		| TC 							// To be able to store values in a dynamic
		
										// OPTION: Comment out the next line if you do not have access to an ODCB database on your machine !!!!
										// -or- if you do not want to make use of the Database option
		, gerda {|*|} 	 				// To store and retrieve any Clean value in a standard relational database (slow but standard)

										// OPTION: Comment out the next line if you do not want to use the DataFile option
		, read  {|*|}, write {|*|}		// To store and retrieve any Clean value in a special database for which a file is used (fast but non standard)

		a

// Set here the kind of server 

:: ServerType
	=	Internal						// The application is linked with a Clean http 1.0 server
	|	External						// The application runs as a subserver connected to a http 1.1 server
	|	TestMode						// The application is tested with Gast (in collaboration with Gast)

instance == ServerType

ServerKind				:==	Internal	// Enable this one for developing an iData or iTask application
//ServerKind				:==	External	// or: Enable this one for the final version using an http 1.1 server
SocketNr				:== 80			// Socket you wnat to work on

// Set here the storage options you would like to have

// Database OPTION : comment out *one* of the following macro definitions
// Use the first line if you *do* want to use the Database option *and* you have an ODCB database interface installed on your machine !!!!
// otherwise use the second line

IF_Database db no_db 	:== db			// If Database option is used
//IF_Database db no_db 	:== no_db		// otherwise, BUT also manually flag of ", gerda{|*|}" in the class definition above

// DataFile OPTION : comment out *one* of the following macro definitions
// Use the first line if you *do* want to use the storage in a file
// otherwise use the second line

IF_DataFile df no_df 	:== df			// If DataFile option is used
//IF_DataFile df no_df 	:== no_df		// otherwise, BUT also manually flag of ", read  {|*|}, write {|*|}" in the class definition above


// Global Settings

ThisExe				:: String			// name of this executable (without .exe)
MyAbsDir			:: String			// absolute path name of directory in which this execuatble is located in

iDataStorageDir 	:== MyAbsDir +++ ThisExe +++ "-iStore"		// directory name where iData and iTask files are stored

ODCBDataBaseName	:== ThisExe +++ "-ODCB"	// name of ODCB Database being used by iData applications
DataFileName		:== ThisExe +++ "-CLDB"	// name of 		DataFile being used by iData applications

iDataIdSeparator 	 :== "."			// used as separator when combining iData form id's
radioButtonSeparator :== '.'			// used as extension for family of radiobuttons


// Debug switches								

TraceInput			:== True			// show what kind of information is received from Client
TraceOutput			:== True			// show what kind of information is stored

TraceHttp10			:== True			// show what kind of information is received by the Clean http 1.0 HtmlServer
TraceHttp11			:== False			// show what kind of information is received by the Clean http 1.1 SubServer, stored in TraceFile
TraceFile		 	:== MyAbsDir +++ ThisExe +++ "-traceSubServer.txt"
