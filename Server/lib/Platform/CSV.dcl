definition module CSV
/**
* This module provides functions for reading and writing comma separated vector (CSV) files.
*/
import StdFile, StdMaybe

/**
* Read a single record from a CSV file
* A comma ',' is used as field separator, double quotes '"' may be used to enclose fields
* and the escape character is backslash '\'.
*
* @param The file handle to read from
*
* @return The record as a list of strings, or Nothing if there are no more records in the file. 
* @return The file handle
*/
readCSVRecord		:: !*File -> (!Maybe [String],!*File)
/**
* Read a single record from a CSV file with custom separator characters.
*
* @param The field separator character
* @param The field enclosure character
* @param The escape character
* @param The file handle to read from
* 
* @return The record as a list of strings, or Nothing if there are no more records in the file. 
* @return The file handle
*/
readCSVRecordWith	:: !Char !Char !Char !*File -> (!Maybe [String], !*File)
/**
* Read an entire CSV file.
*
* A comma ',' is used as field separator, double quotes '"' may be used to enclose fields
* and the escape character is backslash '\'.
*
* @param The file handle to read from
*
* @return The list of records which are lists of strings
* @return The file handle
*/
readCSVFile			:: !*File -> (![[String]],!*File)
/**
* Read an entire CSV file with custom separator characters.
*
* @param The field separator character
* @param The field enclosure character
* @param The escape character
* @param The file handle to read from
*
* @return The list of records which are lists of strings
* @return The file handle
*/
readCSVFileWith		:: !Char !Char !Char !*File -> (![[String]],!*File)