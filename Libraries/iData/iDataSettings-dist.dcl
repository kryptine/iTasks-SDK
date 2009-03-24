definition module iDataSettings

// iData & iTask Library
// Concept & Programming (c) 2005 - 2007 Rinus Plasmeijer

import iDataForms
import iDataCompileOptions
import DataFile							// OPTION: A fast generic database stored in a file, made by Arjen van Weelden

// Installation options

// Absolute path to the directory where static resources (css,js,img etc.) are stored 
ResourceDir				:== "C:\\PATH\\TO\\MY CLEAN IDE\\iTasks2\\Resources"


// Debug switches								
TraceInput				:== False									// show what kind of information is received from Client
TraceOutput				:== False									// show what kind of information is stored when application is finished
TraceThreads			:== False									// show the threadtable
TraceHTTP				:== False									// show http traffic in the console


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
IF_ClientServer cs no_cs	:== no_cs	// Otherwise

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
		
										// OPTION: Comment out the next line if you do not want to use the DataFile option
//		,  read  {|*|}, write {|*|}		// To store and retrieve any Clean value in a special database for which a file is used (fast but non standard)

		a
			
// Global Settings determining where files are stored

ThisExe					:: String		// name of this executable (without .exe)
MyAbsDir				:: String		// absolute path name of directory in which this execuatble is located in

iDataStorageDir 		:== MyAbsDir +++ ThisExe +++ "-iStore"		// directory name where iData and iTask files are stored

DataFileName			:== ThisExe +++ "-CLDB"						// name of 		DataFile being used by iData applications

TraceFile		 		:== MyAbsDir +++ ThisExe +++ "-traceSubServer.txt" // name of file in which trace information from subserver is stored

// separators
iDataIdSeparator 			:== "."									// used for combining iData form id's

// Ajax separators

State_FormList_Separator	:== "##;"								// separator between state info and list of form info
FormElem_Separator			:== "#;;"								// separator between form elements in form list
FormName_Content_Separator	:== "###"								// separator between name of form and the contents of a form element


