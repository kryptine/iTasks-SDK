implementation module iTaskDB

//	super simple database creation and access based on iData
// (c) mjp 2007 

import iTasks, iDataFormlib, StdEnv, iDataTrivial

::DBid a :== (String,Lifespan)

// Common db access

readDB	:: (DBid a) -> Task a | iData a
readDB 	name=:(idn,_) = appHSt ("readDB " +++ idn) (DB name id)

writeDB	:: (DBid a) a -> Task a | iData a
writeDB name=:(idn,_) value = appHSt ("writeDB " +++ idn) (DB name (const value))

readDB2	:: (DBid a) -> Task a | iData a
readDB2 name=:(idn,_) = appHSt2 ("readDB2 " +++ idn) (DB name id)

DB :: (DBid a) (a -> a) *HSt -> (a,*HSt) | iData a
DB (name,storageKind) fun hst 
# (form,hst)	= mkStoreForm (Init,nFormId (db_prefix +++ name) createDefault <@ storageKind <@ NoForm) fun hst
= (form.value,hst)

mkDBid :: String Lifespan -> (DBid a)
mkDBid s Database 	
| and (map isControl (mkList s)) 	= abort (s <+++ " contains control characters which is illegal!...\n\n")
mkDBid s attr						= (s,attr) 

