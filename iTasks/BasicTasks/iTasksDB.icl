implementation module iTasksDB

// *********************************************************************************************************************************
// iTasks for easy database creation and access - based on iData
// *********************************************************************************************************************************

import StdFunc, StdList
import iDataTrivial, iDataFormlib
import LiftingCombinators

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
mkDBid s attr						= (s,attr) 

