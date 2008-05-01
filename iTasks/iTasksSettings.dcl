definition module iTasksSettings

// *********************************************************************************************************************************
// Costumize some global constants
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdOverloaded
import iDataHtmlDef, iDataStylelib, iDataTrivial

iTaskVersion 					:== "0.991 - May 2008 - "
defaultWorkflowName 			:== "start"			
traceId							:== "User_Trace" 
refreshId						:== "User_refresh"
applicationVersionNr			:== ThisExe <+++ "_Version" 

userVersionNr thisUser			:== "User" <+++ thisUser <+++ "_VersionPNr"
usersessionVersionNr thisUser	:== "User" <+++ thisUser <+++ "_VersionSNr" 

showText   		text :== Txt text
showLabel  		text :== TxtStyle  LabelStyle 		text
showMainLabel	text :== TxtStyle  MainLabelStyle   text
showHighLight	text :== TxtStyle  HighLightStyle   text
showLowLight	text :== TxtStyle  LowLightStyle	text
showTrace  		text :== TxtStyle  TraceStyle 		text

TxtStyle style message :== Font [`Fnt_Std [style]] [Txt (toString message)]
