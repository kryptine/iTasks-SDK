definition module ImportTasks
/**
* This module provides tasks for importing external data into a workflow.
* 
*/
import iTasks


importTextFile		:: !String -> Task String

importCSVFile		:: !String -> Task [[String]]

importCSVFileWith	:: !Char !Char !Char !String -> Task [[String]]