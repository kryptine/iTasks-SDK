definition module ImportTasks
/**
* This module provides tasks for importing external data into a workflow.
*/
import FilePath, Types, Task
/**
* Import a file on the server's filesystem as a Document
*
* @param The path of the file to import
*
* @return The imported document
* @throws FileException
*/
importDocument		:: !FilePath -> Task Document
/**
* Import the content of  a text file on the server's filesystem.
*
* @param The path of the file to import
*
* @return The imported content
* @throws FileException
*/
importTextFile		:: !FilePath -> Task String
/**
* Import a comma separated vector (CSV) file on the server's filesystem.
*
* @param The path of the file to import
*
* @return The imported content
* @throws FileException
*/
importCSVFile		:: !FilePath -> Task [[String]]
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
* @throws FileException
*/
importCSVFileWith	:: !Char !Char !Char !FilePath -> Task [[String]]
/**
* Import and parse a JSON datafile on the server's filesystem.
*
* @param The path of the file to import
*
* @return The imported content
* @throws FileException 
*/
importJSONFile		:: !FilePath -> Task a | JSONDecode{|*|} a
/**
* Import and parse a JSON datafile on the server's filesystem using
* a custom parse function.
*
* @param The JSON decoder function
* @param The path of the file to import
*
* @return The imported content
* @throws FileException 
*/
importJSONFileWith	:: !(JSONNode -> Maybe a) !FilePath -> Task a
