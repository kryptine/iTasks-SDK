definition module ImportTasks
/**
* This module provides tasks for importing external data into a workflow.
*/
import Types
/**
* Import a file on the server's filesystem as a Document
*
* @param The path of the file to import
*
* @return The imported document
*/
importDocument		:: !String -> Task Document
/**
* Import the content of  a text file on the server's filesystem.
*
* @param The path of the file to import
*
* @return The imported content
*/
importTextFile		:: !String -> Task String
/**
* Import a comma separated vector (CSV) file on the server's filesystem.
*
* @param The path of the file to import
*
* @return The imported content
*/
importCSVFile		:: !String -> Task [[String]]
/**
* Import a comma separated vector (CSV) file on the server's filesystem using
* custom separator characters.
*
* @param The field separator
* @param The string quote character
* @param The escape character
* @param The path of the file to import
*
* @return The imported content
*/
importCSVFileWith	:: !Char !Char !Char !String -> Task [[String]]
/**
* Import and parse a JSON datafile on the server's filesystem.
*
* @param The path of the file to import
*
* @return The imported content 
*/
importJSONFile		:: !String -> Task a | JSONDecode{|*|} a
/**
* Import and parse a JSON datafile on the server's filesystem using
* a custom parse function.
*
* @param The JSON decoder function
* @param The path of the file to import
*
* @return The imported content 
*/
importJSONFileWith	:: !(JSONNode -> Maybe a) !String -> Task a