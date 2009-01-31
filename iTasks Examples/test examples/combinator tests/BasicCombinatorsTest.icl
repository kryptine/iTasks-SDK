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
		  	/*
		  	,	{ name		= "chooseTask_cbox_test"
		  		, label		= "chooseTask_cbox"
		  		, roles		= []
		  		, mainTask	= chooseTask_cbox_test
		  		}
		  		*/
		  	]

returnbind_test :: Task Void
returnbind_test
	= return_V 42 =>> \value -> displayHtml [Text (toString value)]
	
assignTaskTo_test :: Task Void
assignTaskTo_test
	= assignTaskTo 0 ("Message for root", displayHtml [Text "This message is for root only"])