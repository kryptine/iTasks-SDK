module BasicCombinatorsTest
/**
* This module tests the combinators in BasicCombinators.dcl
*/
import StdEnv, iTasks, iData
import BasicCombinators, CommonCombinators, PromptingCombinators

Start :: *World -> *World
Start world = startEngine tests world
where
	tests = [ 	{ name		= "returnbind_test"
		  		, label		= "Return and bind"
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
