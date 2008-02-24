definition module iDataStylelib

import iDataStyleDef, StdInt, iDataSettings

// This module controlles the styles being used by iData & iTask applications

StyleSheetIntern	:== False							// True, if the internlly defined CleanStyles are being used, otherwise use styles defined externally

InternalCleanStyles	:: [Style]							// Internal styles defined in this module
ExternalCleanStyles	:== ThisExe +++ "\/clean.css"		// External styles can be found here



CleanStyle			:== Std_Class "CleanStyle"
EditBoxStyle		:== Std_Class "EditBox"
DisplayBoxStyle		:== Std_Class "DisplayBox"

TableHeaderStyle	:== Std_Class "TableHeader"
TableRowStyle		:== Std_Class "TableRow"

LabelStyle  		:== Std_Class "Label"
MainLabelStyle		:== Std_Class "MainLabel"
HighLightStyle		:== Std_Class "HighLight"
LowLightStyle		:== Std_Class "LowLight"
TraceStyle  		:== Std_Class "Trace"

// Some related default constants used for the length of input boxes

defsize  :== 12 						// size of inputfield
defpixel :== 107						// size in pixels for buttons, pull-down buttons

