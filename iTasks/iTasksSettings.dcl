definition module iTasksSettings

// *********************************************************************************************************************************
// Costumize some global constants
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdOverloaded
import iDataTrivial, iDataSettings
import Html

defaultUser						:== 0												// the system starts with this user id... 

iTaskVersion 					:== "2.0 - December 2008 - "
defaultWorkflowName 			:== "start"			
traceId							:== "User_Trace" 
refreshId						:== "User_refresh"
applicationVersionNr			:== ThisExe <+++ "_Version" 

userVersionNr thisUser			:== "User" <+++ thisUser <+++ "_VersionPNr"
usersessionVersionNr thisUser	:== "User" <+++ thisUser <+++ "_VersionSNr" 

showText   		text :== Text text
showLabel  		text :== Text text
showMainLabel	text :== Text text
showHighLight	text :== Text text
showLowLight	text :== Text text
showTrace  		text :== Text text



