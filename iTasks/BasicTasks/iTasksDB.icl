implementation module iTasksDB

// *********************************************************************************************************************************
// iTasks for easy database creation and access - based on iData
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdFunc, StdList
import iDataTrivial, iDataFormlib
import iTasksLiftingCombinators

::DBid a :== (!String,!Lifespan)

// Common db access

readDB	:: !(DBid a) -> Task a | iData a
readDB 	name=:(idn,_) = appHStOnce ("readDB " +++ idn) (DB name id)

writeDB	:: !(DBid a) !a -> Task a | iData a
writeDB name=:(idn,_) value = appHStOnce ("writeDB " +++ idn) (DB name (const value))

readDB2	:: !(DBid a) -> Task a | iData a
readDB2 name=:(idn,_) = appHSt ("readDB2 " +++ idn) (DB name id)

DB :: !(DBid a) !(a -> a) !*HSt -> (!a,!*HSt) | iData a
DB (name,storageKind) fun hst 
# (form,hst)	= mkStoreForm (Init,nFormId (db_prefix +++ name) createDefault <@ storageKind <@ NoForm) fun hst
= (form.Form.value,hst)

mkDBid :: !String !Lifespan -> (DBid a)
mkDBid s LSDatabase 	
| and (map isControl (mkList s)) 	= abort (s <+++ " contains control characters which is illegal!...\n\n")
mkDBid s attr						= (s,attr) 

