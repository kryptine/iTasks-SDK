definition module GinStorage
 
import 	iTasks
import	GinSyntax

derive class iTask ModuleStore				

:: ModuleStore	= 	{ name  :: !String
			  		, value :: !GModule
			  		, dbref :: !DBRef ModuleStore
			  		}

instance DB ModuleStore 

readAllModules 	:: Task [ModuleStore]
newModuleName 	:: !GModule -> Task (!String, !GModule)				// creates new entry in store
storeModule 	:: !(String, !GModule) -> Task (!String, !GModule) 	// item with name assumed to be in store
chooseModule 	:: Task (!String, !GModule)

