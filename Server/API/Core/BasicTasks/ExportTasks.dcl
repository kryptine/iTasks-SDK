definition module ExportTasks
/**
* This module provides tasks for exporting data from a workflow to an external source
*/
import Types, Task

/**
* Export a document to the server's filesystem.
*
* @param The path of the exported file
* @param The document to export
*
* @return The exported document
*/
exportDocument		:: !String !Document -> Task Document

/**
* Export a string as text file to the server's filesystem.
*
* @param The path of the exported file
* @param The content to export
*
* @return The exported content
*/
exportTextFile		:: !String !String -> Task String

/**
* Export a list of rows of fields to a comma separated vector (CSV) file on the server's filesystem.
*
* @param The path of the exported file
* @param The content to export as a list of rows of lists of fields
*
* @return The exported content
*/
exportCSVFile		:: !String ![[String]] -> Task [[String]]

/**
* Export a list of rows of fields to a comma separated vector (CSV) file on the server's filesystem
* using custom separator characters.
*
* @param The field separator
* @param The string quote character
* @param The escape character
* @param The path of the exported file
* @param The content to export as a list of rows of lists of fields
*
* @return The exported content
*/
exportCSVFileWith	:: !Char !Char !Char !String ![[String]] -> Task [[String]]

/**
* Encode and export a JSON datafile to the server's filesystem.
*
* @param The path of the exported file
* @param The content to encode as JSON using the generic JSON encoder
*
* @param The exported content
*/
exportJSONFile		:: !String a -> Task a | JSONEncode{|*|} a
/**
* Encode and export a JSON datafile to the server's filesystem using a custom encode function.
* 
* @param The JSON encoder function
* @param The path of the exported file
* 
* @return The exported content
*/
exportJSONFileWith	:: !(a -> JSONNode) !String a -> Task a
