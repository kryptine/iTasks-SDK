implementation module LiftingCombinators

import EditTasks, BasicCombinators
import InternaliTasksCommon

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
appIData idatafun = Task ( \tst -> accTaskTSt (mkBasicTask "appIData" (Task (appIData` idatafun))) tst)
where
	appIData` idata tst =:{taskNr,html,hst}
		# (idata,hst) 											= idatafun hst
		# (_, {taskNr,activated,html=ahtml,hst,processdb}) 	= accTaskTSt (editTaskLabel "appIDataDone" "Done" Void) {tst & activated = True, html = BT [] [],hst = hst}	
		= (idata.Form.value, {tst & taskNr = taskNr,activated	= activated, html = html +|+ 
																	(if activated (BT idata.form idata.inputs) (BT idata.form idata.inputs +|+ ahtml)), hst = hst, processdb = processdb})

appIData2 :: !(String *HSt -> *(!Form a,!*HSt)) -> (Task a) | iData a 
appIData2 idatafun = Task (\tst -> accTaskTSt (mkBasicTask "appIData" (Task (appIData` idatafun))) tst)
where
	appIData` idata tst =:{taskNr,html,hst,userId}
	# taskId												= iTaskId userId taskNr "iData"
	# (idata,hst) 											= idatafun taskId hst
	# (_,{taskNr,activated,html=ahtml,hst,processdb}) 		= accTaskTSt (editTaskLabel "appIDataDone" "Done" Void) {tst & activated = True, html = BT [] [],hst = hst}	
	= (idata.Form.value,{tst & taskNr = taskNr,activated	= activated, html = html +|+ 
																(if activated (BT idata.form idata.inputs) (BT idata.form idata.inputs +|+ ahtml)), hst = hst, processdb = processdb})

appHStOnce :: !String !(HSt -> (!a,!HSt)) -> (Task a) | iData a
appHStOnce label fun = once label (Task (liftHst fun))

appHSt :: !String !(HSt -> (!a,!HSt)) -> (Task a) | iData a
appHSt label fun = mkBasicTask label (Task (liftHst fun))

liftHst :: !(*HSt -> *(.a,*HSt)) !*TSt -> *(.a,*TSt)
liftHst fun tst=:{hst}
# (form,hst) = fun hst
= (form,{tst & hst = hst})

appWorldOnce :: !String !(*World -> *(!a,!*World)) -> (Task a) | iData a
appWorldOnce label fun = once label (Task (liftWorld fun))

appWorld :: !String !(*World -> *(!a,!*World)) -> (Task a) | iData a
appWorld label fun = mkBasicTask label (Task (liftWorld fun))

liftWorld :: !(*World -> *(!a,!*World)) !*TSt -> *(!a,!*TSt)
liftWorld f tst=: {hst = hst=:{nworld = nworld=:{world}}}
# (a,world)	= f world
= (a,{tst & hst = {hst & nworld = {nworld & world = world}}})	
