implementation module GinStorage
 
import 	iTasks
from 	StdFunc import o
import	GinSyntax

derive class iTask ModuleStore				

:: ModuleStore	= 	{ name  :: !String
			  		, value :: !GModule
			  		, dbref :: !DBRef ModuleStore
			  		}

// *************************************************

instance DB ModuleStore where
	databaseId	:: Shared [ModuleStore]
	databaseId = mkSharedReference "ModuleStore"
	
	getItemId	:: ModuleStore -> DBRef ModuleStore
	getItemId a = a.dbref

	setItemId	:: (DBRef ModuleStore) ModuleStore -> ModuleStore
	setItemId dbref a = {a & dbref = dbref}

readAllModules :: Task [ModuleStore]
readAllModules = dbReadAll

newModuleName :: !GModule -> Task (!String, !GModule)
newModuleName value
	=						enterInformation "Give name of new module:" 
		>>= \name ->		readAllModules
		>>= \allModules ->	case [this \\ this <- allModules | this.ModuleStore.name == name] of
								[] -> 		getDefaultValue 
											>>= \item -> 	dbCreateItem {ModuleStore | name = name, value = value, dbref = item} 
											>>|				return (name,value) 
								found ->	requestConfirmation ("Module " +++ (hd found).ModuleStore.name +++ " already exists, do you want to overwrite?")
								 			>>= \ok -> if ok (return (name,value)) (newModuleName value)

chooseModule ::  Task (!String, !GModule)
chooseModule   
	=						readAllModules
		>>= \all ->			let names = [this.ModuleStore.name \\ this <- all] in
								case names of
								 [] ->					showMessage "Choose module" "No modules stored !" >>| stop 
								 		>>|				return ("", newModule)
								 names ->				enterChoice "Choose module you want to use:" names
										>>= \choice ->	return (hd [(this.ModuleStore.name, this.ModuleStore.value) \\ this <- all | this.ModuleStore.name == choice])

storeModule :: !(String, !GModule) -> Task (!String, !GModule) // item assumed to be in store
storeModule (name, value)
	=					readAllModules
		>>= \all ->		return (hd [this \\ this <- all | this.ModuleStore.name == name])
		>>= \store ->  	dbUpdateItem {ModuleStore | store & name = name, value = value}
		>>|				return (name,value)
