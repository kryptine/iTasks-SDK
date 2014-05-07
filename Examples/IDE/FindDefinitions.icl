implementation module FindDefinitions

// Searches for definitions in Clean modules

import PmParse 
from 	UtilStrictLists import :: List
import	iTasks.API.Extensions.Development.Codebase
import System.FilePath  

derive class iTask IdentifierPositionList

// Clean system syntax dependant parsing

from UtilStrictLists import :: List, StrictListToList, ListToStrictList

searchForIdentifier :: !SearchWhat !Bool !Identifier !(Maybe CleanModuleName)  !CodeBase 
												-> Task (![(!CleanModule,!IdentifierPositionList)],![CleanModuleName])

searchForIdentifier what inImports identifier maybeName codeBase 
# allNames = codeBaseToCleanModuleNames codeBase
= case maybeName of
		Nothing -> if (allNames == [])
					  (return ([],[]))
					  (search allNames [] [] []) 
		(Just name) -> (search [name] allNames [] [])
where
	search ::  ![CleanModuleName] ![CleanModuleName] ![CleanModuleName] [(!CleanModule,!IdentifierPositionList)] 
												-> Task (![(!CleanModule,!IdentifierPositionList)],![CleanModuleName])
	search [] notSearched searched found 			
		= return (reverse found,reverse searched)								
	search [(path,moduleName):rest] notSearched searched found 
		=					searchInFile what inImports identifier (path </> moduleName +++ if inImports ".icl" ".dcl")
		>>= continue 
	where
		continue (imported,pos) 						// modules imported by searched file, found identifiers
		# nfound 			= ifPosNil pos found [(((path,moduleName),if inImports Icl Dcl),pos):found]
		# (toDo,restBase)   = searchModulesInCodeBase imported notSearched
		# toSearch		    = removeDup (rest ++ toDo)
		= search toSearch restBase [(path,moduleName):searched] nfound	
	
	ifPosNil PosNil then else = then
	ifPosNil _      then else = else

// returns (module names imported, positions where identifier has been found)
searchInFile :: !SearchWhat !Bool !Identifier !String -> Task !(![String],!IdentifierPositionList)
searchInFile SearchIdentifier inImports identifier fileName 
	= 					accWorld (accFiles (FindIdentifiersInFile inImports [!!] identifier fileName ))
	>>= \(list,pos) ->  return (StrictListToList list,pos)
searchInFile SearchImplementation inImports identifier fileName 
	= 					accWorld (accFiles (FindDefinitionInFile inImports [!!] identifier fileName ))
	>>= \(list,pos) ->  return (StrictListToList list,pos)
searchInFile SearchDefinition inImports identifier fileName 
	= 					accWorld (accFiles (FindDefinitionInFile inImports [!!] identifier fileName ))
	>>= \(list,pos) ->  return (StrictListToList list,pos)
	
searchModulesInCodeBase :: ![ModuleName] ![CleanModuleName] -> ([CleanModuleName],[CleanModuleName])
searchModulesInCodeBase moduleNames codeBase 
	= span (\(path,name) -> isMember name moduleNames) codeBase




		