implementation module FindDefinitions

// Searches for definitions in Clean modules

import PmParse 
from 	UtilStrictLists import :: List
import	iTasks.API.Extensions.Development.Codebase

derive class iTask IdentifierPositionList

// Clean system syntax dependant parsing

from UtilStrictLists import :: List, StrictListToList, ListToStrictList

searchForIdentifier :: !SearchWhat !Bool !Identifier !CleanModuleName  !CodeBase 
												-> Task (![(!CleanModule,!IdentifierPositionList)],![CleanModuleName])

searchForIdentifier what inImports identifier cleanModuleName codeBase 
	= search [cleanModuleName] (codeBaseToCleanModuleNames codeBase) [] [] 
where
	search ::  ![CleanModuleName] ![CleanModuleName] ![CleanModuleName] [(!CleanModule,!IdentifierPositionList)] 
												-> Task (![(!CleanModule,!IdentifierPositionList)],![CleanModuleName])
	search [] baseNotSearched searched found 
		= return (reverse found,reverse searched)								
	search [(path,moduleName):rest] baseNotSearched searched found 
		=					searchInFile what inImports identifier (path +++ moduleName +++ if inImports ".icl" ".dcl")
		>>= \(new,pos) -> 	let (nsearched,nfound) = calc (new,pos) 
								(toDo,restBase)    = searchModulesInCodeBase new baseNotSearched
								toSearch		   = removeDup (rest ++ toDo)
							in	search toSearch restBase nsearched nfound
	where
		calc (new,pos) 
		# nsearched 	= [(path,moduleName):searched]
		# nfound 		= ifPosNil pos found [(((path,moduleName),if inImports Icl Dcl),pos):found]
		= (nsearched,nfound)
	
	ifPosNil PosNil then else = then
	ifPosNil _      then else = else

// returns (module names imported, positions where identifier has been found)
searchInFile :: !SearchWhat !Bool !Identifier !String -> Task !(![String],!IdentifierPositionList)
searchInFile SearchIdentifier inImports identifier fileName 
	= 					accWorld (accFiles (FindIdentifiersInFile inImports [!fileName!] identifier fileName ))
	>>= \(list,pos) ->  return (StrictListToList list,pos)
searchInFile SearchImplementation inImports identifier fileName 
	= 					accWorld (accFiles (FindDefinitionInFile inImports [!fileName!] identifier fileName ))
	>>= \(list,pos) ->  return (StrictListToList list,pos)
searchInFile SearchDefinition inImports identifier fileName 
	= 					accWorld (accFiles (FindDefinitionInFile inImports [!fileName!] identifier fileName ))
	>>= \(list,pos) ->  return (StrictListToList list,pos)
	
searchModulesInCodeBase :: ![ModuleName] ![CleanModuleName] -> ([CleanModuleName],[CleanModuleName])
searchModulesInCodeBase moduleNames codeBase 
	= span (\(path,name) -> isMember name moduleNames) codeBase




		