implementation module VerifiedTest

import iTasks
import StdMisc
import CommonDomain, Text

derive bimap (,),Maybe

// Verified Types

derive gVisualize PositiveNum
derive gUpdate PositiveNum
derive JSONDecode PositiveNum
derive JSONEncode PositiveNum
//derive gVerify PositiveNum

:: PositiveNum = Positive Int

gVerify{|PositiveNum|} Nothing vst=:{VerSt | updateMask,verifyMask,optional}
 # (cm,um) = popMask updateMask
 # cm		= fst (popMask cm) //step past the constructor
 | optional = {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [VMValid (Just "You may add a positive number") Nothing []])} 		
 | otherwise = {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMUntouched Nothing Nothing [VMUntouched (Just "Add a positive number") Nothing []])}
gVerify{|PositiveNum|} (Just (Positive x)) vst=:{VerSt | updateMask,verifyMask,optional}
 # (cm,um) = popMask updateMask
 # cm		= fst (popMask cm) //step past the constructor
 | x < 0
 	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMInvalid (ErrorMessage "") Nothing [VMInvalid (ErrorMessage "Enter a positive number") Nothing []])}
 | optional = {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [VMValid (Just "You may add a positive number") Nothing []])} 		
 | otherwise
 	= case cm of
		(Untouched _ _)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMUntouched Nothing Nothing [VMUntouched (Just "Add a positive number") Nothing []])}
		(Touched _ _)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [VMValid (Just "Add a positive number") Nothing []])}
		(Blanked _ _)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMInvalid IsBlankError Nothing [VMInvalid IsBlankError Nothing []])}
	
verifiedTest :: Task (Maybe PositiveNum)
verifiedTest = enterInformation "Enter number" "Enter a postive number" >>= showMessageAbout "Result" "The result is"

Start :: *World -> *World
Start world = startEngine [workflow "Verification Test" verifiedTest] world