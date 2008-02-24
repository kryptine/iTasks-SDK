definition module iTasksSettings

// (c) iTask & iData Concept and Implementation by Rinus Plasmeijer, 2006 - 2008 - MJP

// To costumize the iTasks:

import StdOverloaded
import iDataHtmlDef 		
import iDataStylelib

showText   		text :== Txt text
showLabel  		text :== TxtStyle  LabelStyle 		text
showMainLabel	text :== TxtStyle  MainLabelStyle   text
showHighLight	text :== TxtStyle  HighLightStyle   text
showLowLight	text :== TxtStyle  LowLightStyle	text
showTrace  		text :== TxtStyle  TraceStyle 		text

TxtStyle style message :== Font [`Fnt_Std [style]] [Txt (toString message)]
