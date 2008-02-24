definition module iDataStylelib

import iDataStyleDef, StdInt, iDataSettings

// This module controlles the styles being used by iData & iTask applications

StyleSheetIntern	:== True						// True, if the internlly defined CleanStyles are being used, otherwise use styles defined externally

InternalCleanStyles	:: [Style]						// Internal styles defined in this module
ExternalCleanStyles		:== ThisExe +++ "/clean.css"	// External styles can be found here



//TableHeaderStyle :: Standard_Attr
TableHeaderStyle	:== Std_Class "TableHeader"

//TableRowStyle :: Standard_Attr
TableRowStyle		:== Std_Class "TableRow"

//CleanStyle :: Standard_Attr
CleanStyle			:== Std_Class "CleanStyle"

//EditBoxStyle :: Standard_Attr
EditBoxStyle		:== Std_Class "EditBox"

//DisplayBoxStyle :: Standard_Attr
DisplayBoxStyle		:== Std_Class "DisplayBox"


// Some related default constants used for the length of input boxes

defsize  :== 12 						// size of inputfield
defpixel :== 107						// size in pixels for buttons, pull-down buttons

