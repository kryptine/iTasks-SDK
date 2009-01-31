module BasicCombinatorsTest
/**
* This module tests the combinators in BasicCombinators.dcl
*/
import StdEnv, iTasks, iData
import BasicCombinators, CommonCombinators, PromptingCombinators

derive gForm []
derive gUpd []

Start :: *World -> *World
Start world = startEngine tests world
where
	tests = [ 	{ name		= "returnbind_test"
		  		, label		= "return_V and =>>"
		  		, roles		= []
		  		, mainTask	= returnbind_test
		  		}
		  	,	{ name		= "assignTaskTo_test"
		  		, label		= "assignTaskTo"
		  		, roles		= []
		  		, mainTask	= assignTaskTo_test
		  		}
		  	,	{ name		= "foreverTask_test"
		  		, label		= "foreverTask"
		  		, roles		= []
		  		, mainTask	= foreverTask_test
		  		}
		  	,	{ name		= "loop_tst"
		  		, label		= "loop (&lt;!)"
		  		, roles		= []
		  		, mainTask	= loop_test
		  		}
		  	,	{ name		= "seqTasks_test"
		  		, label		= "seqTasks"
		  		, roles		= []
		  		, mainTask	= seqTasks_test
		  		}
		  	,	{ name		= "selectTasks_test"
		  		, label		= "selectTasks"
		  		, roles		= []
		  		, mainTask	= selectTasks_test
		  		}
		  	]

returnbind_test :: Task Void
returnbind_test
	= return_V 42 =>> \value -> displayHtml [Text (toString value)]
	
assignTaskTo_test :: Task Void
assignTaskTo_test
	= assignTaskTo 0 ("Message for root", displayHtml [Text "This message is for root only"])

foreverTask_test :: Task Void
foreverTask_test
	= (foreverTask 
		( editTask "Task 1" 0 =>> \a ->
		  editTask "Task 2" 0 =>> \b ->
		  editTask "Sum" (a + b)
		 )
	  ) #>> return_V Void

loop_test :: Task Void
loop_test
	= ( (editTask "Ok" 0) <! (\x -> x > 10) ) #>> return_V Void
	
seqTasks_test :: Task Void
seqTasks_test
	= seqTasks [(toString i, editTask "Ok" i) \\ i <- [1..3]] =>> \list -> displayValue list #>> return_V Void
	
selectTasks_test :: Task Void
selectTasks_test
	= selectTasks doEvenTasks seqTasks [(toString i, editTask "Ok" i) \\ i <- [0..2]] #>> return_V Void  
where
	doEvenTasks tasks = return_V [i \\ task <- tasks & i <- [0..] | isEven i]