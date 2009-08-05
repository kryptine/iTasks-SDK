implementation module DynamicDB

import StdList, StdClass, StdTuple, StdArray, StdMaybe
import Store, Types, TSt
import dynamic_string

from StdFunc import id

instance DynamicDB TSt
where
	createDynamic :: !Dynamic !*TSt -> (!DynamicId,!*TSt)
	createDynamic dyn tst
		# (dyns, tst)	= staticDynamicStore id tst
		# newDid		= inc (maxDynId dyns)
		# (dyns, tst)	= staticDynamicStore (\_ -> dyns ++ [(newDid, dynamic_to_string dyn)]) tst
		= (newDid,tst)
	
	updateDynamic :: !Dynamic !DynamicId !*TSt -> (!Bool, !*TSt)
	updateDynamic dyn dynid tst
		# (dyns, tst)	= staticDynamicStore id tst
		# (ndyns,upd)	= unzip (map (update dyn) dyns)
		# (ndyns,tst)	= staticDynamicStore (\_ -> ndyns) tst
		= (or upd,tst)
	where
		update dyn (x,s)
			| x == dynid	= ((x, dynamic_to_string dyn), True)
			| otherwise		= ((x,s),False) 
	
	deleteDynamic :: !DynamicId !*TSt -> (!Bool, !*TSt)
	deleteDynamic dynid tst
		# (dyns,tst)	= staticDynamicStore id tst
		# (ndyns,tst)	= staticDynamicStore (\_ -> [(x,s) \\ (x,s) <- dyns | x <> dynid]) tst
		= (length dyns <> length ndyns, tst)
	
	deleteDynamics :: ![DynamicId] !*TSt -> (!Bool, !*TSt)
	deleteDynamics dynids tst
		# (dyns,tst)	= staticDynamicStore id tst
		# (ndyns,tst)	= staticDynamicStore (\_ -> [(x,s) \\ (x,s) <- dyns | not (isMember x dynids)]) tst
		= (length dyns <> length ndyns, tst)
		
	getDynamic :: !DynamicId !*TSt -> (!Maybe Dynamic, !*TSt)
	getDynamic dynid tst 
		# (dyns, tst)	= staticDynamicStore id tst
		= case [dyn \\ (did,dyn) <- dyns | did == dynid] of
			[entry]	= (Just (string_to_dynamic {c \\ c <-: entry}),tst)
			_		= (Nothing, tst)

staticDynamicStore ::  !([(DynamicId,String)] -> [(DynamicId,String)]) !*TSt -> (![(DynamicId,String)],!*TSt) 
staticDynamicStore fn tst=:{store,world}
	# (mbList,store,world)	= loadValue "DynamicDB" store world
	# list 					= fn (case mbList of Nothing = []; Just list = list)
	# store					= storeValue "DynamicDB" list store 
	= (list, {tst & store = store, world = world})

maxDynId :: [(DynamicId,String)] -> DynamicId
maxDynId db = foldr max 0 (map fst db)
