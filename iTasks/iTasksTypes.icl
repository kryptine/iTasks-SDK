implementation module iTasksTypes

derive gForm 	Void, Maybe, []
derive gUpd 	Void, Maybe, []
derive gParse 	Void
derive gPrint	Void, Maybe
derive gerda 	Void
derive read 	Void
derive write 	Void

import StdEnv
import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string
import DrupBasic
import InternaliTasksCommon, InternaliTasksThreadHandling, iTasksBasicCombinators

// ******************************************************************************************************

instance == GarbageCollect
where
	(==) Collect   Collect 		= True
	(==) NoCollect NoCollect 	= True
	(==) _ _ 					= False

// ******************************************************************************************************
// Overloaded Functions on Tasks
// ******************************************************************************************************

class 	(<<@) infixl 3 b ::  !(Task a) !b  -> (Task a)
instance <<@  Lifespan
where   (<<@) task lifespan			= setTaskLifespan
		where
			setTaskLifespan tst=:{options}
			
			= IF_Ajax 
				(IF_ClientServer															// we running both client and server
					(IF_ClientTasks												
						(if (options.tasklife == Client && (lifespan == TxtFile || lifespan == DataFile || lifespan == Database))
							(abort "Cannot make persistent storage on Client\n")
							(\tst -> task {tst & options.tasklife = lifespan}))						// assign option on client
						(\tst -> task {tst & options.tasklife = lifespan})tst							// assign option on server
					)
					(task {tst & options.tasklife = lifespan})								// assign option on server
				)
				(task {tst & options.tasklife = lifespan}) 									// assign option on server

instance <<@  StorageFormat
where   (<<@) task storageformat 	= \tst -> task {tst & options.taskstorage = storageformat}
instance <<@  Mode
where   (<<@) task mode 			= \tst -> task {tst & options.taskmode = mode}
instance <<@  GarbageCollect
where   (<<@) task gc 				= \tst -> task {tst & options.gc = gc}

class 	(@>>) infixl 7 b ::  !b !(Task a)   -> (Task a) | iData a
instance @>>  EvaluationOption
where   (@>>) UseAjax task			= \tst -> IF_Ajax 
												(mkTaskThread UseAjax task tst)
												(newTask "Ajax Thread Disabled" task tst) 
		(@>>) OnClient  task 		= \tst -> IF_Ajax 
												(mkTaskThread OnClient task tst)
												(newTask "Client Thread Disabled" task tst) 
// ******************************************************************************************************
// TCl specialization
// ******************************************************************************************************

write{|TCl|} write_a (TCl task) wst
	= write{|*|} (copy_to_string task) wst

read {|TCl|} read_a  wst 
	# (Read str i file) = read{|*|} wst
	= Read (TCl  (deserialize str)) i file
where
	deserialize :: .String -> .(Task .a)
	deserialize str = fst (copy_from_string {c \\ c <-: str })

gPrint{|TCl|} ga (TCl task) ps = ps <<- copy_to_string task

gParse{|TCl|} ga expr
# mbstring = parseString expr
| isNothing mbstring = Nothing
= Just (TCl (fst(copy_from_string {s` \\ s` <-: fromJust mbstring})))
where
	parseString :: Expr -> Maybe String
	parseString expr = gParse{|*|} expr

gUpd{|TCl|} gc (UpdSearch _ 0)	  	 c		= (UpdDone, c)								
gUpd{|TCl|} gc (UpdSearch val cnt)  c		= (UpdSearch val (cnt - 2),c)						
gUpd{|TCl|} gc (UpdCreate l)        _		
# (mode,default)	= gc (UpdCreate l) undef
= (UpdCreate l, TCl (\tst -> (default,tst)))			
gUpd{|TCl|} gc mode                 b		= (mode, b)										

gForm{|TCl|} gfa (init,formid) hst
= ({value=formid.ival,changed=False,form=[]},hst)