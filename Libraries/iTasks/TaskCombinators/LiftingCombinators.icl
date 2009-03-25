implementation module LiftingCombinators

import TSt, UITasks, BasicCombinators, CommonCombinators
import Util

(*=>) infix 4 :: !(TSt -> (!a,!TSt)) !(a -> Task b) -> (Task b)
(*=>) ftst b = Task doit
where
	doit tst
	# (a,tst) = ftst tst
	= accTaskTSt (b a) tst

(*#>) infix 4 :: !(TSt -> TSt) !(Task a) -> Task a
(*#>) ftst b = Task doit
where
	doit tst
	# tst = ftst tst
	= accTaskTSt b tst

appIData :: !(IDataFun a) -> (Task a) | iData a 
appIData idatafun = (mkParallelTask "appIData" appIData`)
where
	appIData` tst
		# tst					= setCombination TTVertical tst
		# (a,tst)				= accTaskTSt (mkSequenceTask "iData" (accTaskTSt (mkBasicTask "doIData" (doIData idatafun)))) tst
		# (_,tst)				= accTaskTSt (mkSequenceTask "Done" (accTaskTSt (editTask "Done" Void))) tst
		= (a,tst)
		
	doIData idata tst =:{taskNr,hst}
		# (idata, hst)	= idatafun hst
		# tst			= setOutput idata.form {tst & hst = hst}
		# tst			= setInputs idata.inputs tst
		= (idata.Form.value, tst)

appIData2 :: !(String *HSt -> *(!Form a,!*HSt)) -> (Task a) | iData a 
appIData2 idatafun = (mkParallelTask "appIData" appIData`)
where
	appIData` tst
		# tst					= setCombination TTVertical tst
		# (a,tst)				= accTaskTSt (mkSequenceTask "iData" (accTaskTSt (mkBasicTask "doIData" (doIData idatafun)))) tst
		# (_,tst)				= accTaskTSt (mkSequenceTask "Done" (accTaskTSt (editTask "Done" Void))) tst
		= (a,tst)

	doIData idata tst =:{taskNr,hst}
		# taskId		= iTaskId taskNr "iData"
		# (idata, hst)	= idatafun taskId hst
		# tst			= setOutput idata.form {tst & hst = hst}
		# tst			= setInputs idata.inputs tst
		= (idata.Form.value, tst)


appHSt :: !String !(HSt -> (!a,!HSt)) -> (Task a) | iData a
appHSt label fun = mkBasicTask label (accHStTSt fun)

appWorld :: !String !(*World -> *(!a,!*World)) -> (Task a) | iData a
appWorld label fun = mkBasicTask label (liftWorld fun)

liftWorld :: !(*World -> *(!a,!*World)) !*TSt -> *(!a,!*TSt)
liftWorld f tst=: {hst = hst=:{nworld = nworld=:{world}}}
	# (a,world)	= f world
	= (a,{tst & hst = {hst & nworld = {nworld & world = world}}})	
