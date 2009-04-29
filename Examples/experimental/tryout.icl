module tryout

import iTasks
import dictionaryUtil
import iDataTrivial

Start world = startEngine tryout world

tryout :: [Workflow]
tryout
=	[{ name		= "tryout"
	 , label	= "tryout"
	 , roles	= []
	 , mainTask	= mytest1 >>| return Void
	 }]

// ********************************

// The following function has to be moved to dictionaryUtil

iDataFun2Dynamic :: (A.a: (Dictionary_iData a) -> (b -> Task a)) -> Dynamic | TC b
iDataFun2Dynamic f = dynamic f :: (A.a: (Dictionary_iData a) -> (b^ -> Task a))

// ********************************

workflow1 :: Task [Int]
workflow1 = applyDynamicTask2 (iDataFun2Dynamic d_iTaskEditor) createDefault

// ********************************

workflow2 :: Task [Int]
workflow2 = applyDynamicTask2 (iDataFun2Dynamic (d_iTaskDelegate OO d_iTaskEditor)) createDefault
// ********************************

// workflow to change:

myChangedWorkflow normalTask alternativeTask whatToApply 
	= pushChangeRequest whenToApply whatToApply myWorkflow
where
	whenToApply = CC (pred 30)
	where
		pred 0 tst =	({newCondition = Nothing, 				 isApplicable = False, applyChange = False},tst)
		pred n tst =	({newCondition = Just (CC (pred (n-1))), isApplicable = True,  applyChange = isEven n},tst)

	myWorkflow 
		= 	parallel "andTasks"  (\_ -> False) id  [(toString i, myTask i) \\ i <- [0..5]] >>|
			parallel "andTasks"  (\_ -> False) id  [(toString i, myTask i) \\ i <- [0..5]]
	where
		myTask val = normalTask val <\/> alternativeTask val
	
// ********************************

mytest 						= myChangedWorkflow normalTask alternativeTask whatToApply
normalTask val 				= editTask ("Normal OK" <+++ val) val 
alternativeTask val dyn 	= applyDynamicTask2 dyn val
whatToApply 				= iDataFun2Dynamic d_iTaskEditor


// ********************************

mytest1 					= myChangedWorkflow normalTask alternativeTask whatToApply1
whatToApply1 				= iDataFun2Dynamic (d_iTaskDelegate OO d_iTaskEditor)

// ********************************

