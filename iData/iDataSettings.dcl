definition module iDataSettings

// iData & iTask Library
// Concept & Programming (c) 2005 - 2007 Rinus Plasmeijer

import iDataHandler
import iDataCompileOptions
import Gerda							// OPTION: GEneRic Database Access to a standard relational database, made by Arjen van Weelden
import DataFile							// OPTION: A fast generic database stored in a file, made by Arjen van Weelden
import HttpServer


// Global settings of iData applications

// The following options are switched on or off by a Clean macro definition.
// Switching off options you don't use increases the efficiency of the application.

// Database OPTION : comment out *one* of the following macro definitions
// Use the first line if you *do* want to use the Database option *and* you have an ODCB database interface installed on your machine !!!!
// otherwise use the second line

//IF_Database db no_db 	:== db			// If Database option is used
IF_Database db no_db 	:== no_db		// otherwise, BUT also manually flag of ", gerda{|*|}" in the iSpecialStore class definition

//IF_DataFile df no_df 	:== df			// If DataFile option is used
IF_DataFile df no_df 	:== no_df		// otherwise, BUT also manually flag of ", read  {|*|}, write {|*|}" in the iSpecialStore class definition

//IF_Ajax aj no_aj		:== aj			// If you want to enable sub-page (thread) handling using "Ajax" technology
IF_Ajax aj no_aj		:== no_aj		// Otherwise

//IF_ClientServer cs no_cs	:== cs		// If you want to have a client server architecture (with Sapl running on the client)
IF_ClientServer cs no_cs	:== no_cs		// Otherwise


IF_ClientTasks :: .a .a -> .a			// Follows IF_Client setting


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
//		,  gerda {|*|} 	 				// To store and retrieve any Clean value in a standard relational database (slow but standard)

										// OPTION: Comment out the next line if you do not want to use the DataFile option
//		,  read  {|*|}, write {|*|}		// To store and retrieve any Clean value in a special database for which a file is used (fast but non standard)

		a

// There are several kind of server options:

:: ServerType
	=	Internal						// Default: The application is linked with a Clean http 1.0 server
	|	CGI								// The application is used through an external server with the Common Gateway Interface
	|	External						// The application runs as a subserver connected to a http 1.1 server
	|	TestMode						// The application is tested with Gast (in collaboration with Gast)

instance == ServerType

// Set here the kind of server you want to use and the port number to use for the communication

ServerKind				:==	Internal	// Enable this one for developing an iData or iTask application
//ServerKind				:== CGI			// or: Enable this one for running the final version through CGI
//ServerKind				:==	External	// or: Enable this one for the final version using an http 1.1 server


ServerOptions			:== [HTTPServerOptPort 80]		//Options for the internal HTTP server.
//ServerOptions			:== []							//Options for the CGI wrapper
//ServerOptions			:== [HTTPSubServerOptPort 80]	//Options for the external subserver.

// Global Settings determining where files are stored

ThisExe					:: String									// name of this executable (without .exe)
MyAbsDir				:: String									// absolute path name of directory in which this execuatble is located in


// absolute path name of directory where static resources (css,js,img etc.) are stored 
//ResourceDir				:== MyAbsDir +++ ThisExe 					
//ResourceDir				:== "C:\\Documents and Settings\\rinus\\Desktop\\Clean 2.2\\Libraries\\iData\\Resources\\"
ResourceDir				:== "C:\\Documents and Settings\\rinus\\Bureaublad\\Current Work\\Clean 2.2\\Libraries\\iData\\Resources\\"
iDataStorageDir 		:== MyAbsDir +++ ThisExe +++ "-iStore"		// directory name where iData and iTask files are stored

ODCBDataBaseName		:== ThisExe +++ "-ODCB"						// name of ODCB Database being used by iData applications
DataFileName			:== ThisExe +++ "-CLDB"						// name of 		DataFile being used by iData applications

TraceFile		 		:== MyAbsDir +++ ThisExe +++ "-traceSubServer.txt" // name of file in which trace information from subserver is stored

// Debug switches								

TraceInput				:== False									// show what kind of information is received from Client
TraceOutput				:== False									// show what kind of information is stored when application is finished
TraceThreads			:== False									// show the threadtable

// separators

iDataIdSeparator 		:== "."										// used for combining iData form id's
radioButtonSeparator 	:== '.'										// used as extension for family of radiobuttons

// Ajax separators

State_FormList_Separator	:== "##;"								// separator between state info and list of form info
FormElem_Separator			:== "#;;"								// separator between form elements in form list
FormName_Content_Separator	:== "###"								// separator between name of form and the contents of a form element


