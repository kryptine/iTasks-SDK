implementation module DynamicDB

import StdList, StdClass, StdTuple, StdArray, StdMaybe
import Types, HSt, TSt, iDataForms, iDataFormlib
import dynamic_string

from StdFunc import id

instance DynamicDB HSt
where
	createDynamic :: !Dynamic !*HSt -> (!DynamicId,!*HSt)
	createDynamic dyn hst
		# (dyns, hst)	= staticDynamicStore id hst
		# newDid		= inc (maxDynId dyns)
		# (dyns, hst)	= staticDynamicStore (\_ -> dyns ++ [(newDid, dynamic_to_string dyn)]) hst
		= (newDid,hst)
	
	updateDynamic :: !Dynamic !DynamicId !*HSt -> (!Bool, !*HSt)
	updateDynamic dyn dynid hst = (False, hst)
	
	deleteDynamic :: !DynamicId !*HSt -> (!Bool, !*HSt)
	deleteDynamic dynid hst = (False, hst)
	
	deleteDynamics :: ![DynamicId] !*HSt -> (!Bool, !*HSt)
	deleteDynamics dynids hst = (False, hst)
	
	getDynamic :: !DynamicId !*HSt -> (!Maybe Dynamic, !*HSt)
	getDynamic dynid hst 
		# (dyns, hst)	= staticDynamicStore id hst
		= case [dyn \\ (did,dyn) <- dyns | did == dynid] of
			[entry]	= (Just (string_to_dynamic {c \\ c <-: entry}),hst)
			_		= (Nothing, hst)

staticDynamicStore ::  !([(DynamicId,String)] -> [(DynamicId,String)]) !*HSt -> (![(DynamicId,String)],!*HSt) 
staticDynamicStore fn hst		
	# (form,hst) = mkStoreForm (Init, pFormId "DynamicDB" []) fn hst
	= (form.Form.value, hst)

maxDynId :: [(DynamicId,String)] -> DynamicId
maxDynId db = foldr max 0 (map fst db)

instance DynamicDB TSt
where
	createDynamic dyn tst		= accHStTSt (createDynamic dyn) tst
	updateDynamic dyn dynid tst	= accHStTSt (updateDynamic dyn dynid) tst
	deleteDynamic dynid tst		= accHStTSt (deleteDynamic dynid) tst
	deleteDynamics dynids tst	= accHStTSt (deleteDynamics dynids) tst
	getDynamic dynid tst		= accHStTSt (getDynamic dynid) tst
