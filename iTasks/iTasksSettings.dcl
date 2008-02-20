definition module iTasksSettings

// (c) iTask & iData Concept and Implementation by Rinus Plasmeijer, 2006 - 2008 - MJP

// To costumize the iTasks:

import StdOverloaded
import iDataHtmlDef 		

CTxt  color message :== Font [Fnt_Color (`Colorname color)] [B []   (toString message)]
BCTxt color message :== Font [Fnt_Color (`Colorname color)] [Big [] (toString message)]

showText   		text :== Txt          text
showLabel  		text :== CTxt  Yellow text
showMainLabel	text :== CTxt  Red    text
showHighLight	text :== BCTxt Aqua   text
showLowLight	text :== CTxt  Aqua   text
showTrace  		text :== CTxt  Silver text