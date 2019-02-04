definition module C2.Apps.ShipAdventure.Scripting

import iTasks

import C2.Apps.ShipAdventure.Types


// script language


:: Target		= 	Section Coord3D
				|	Nearest ObjectType
				|	TargetSection
:: Script		=	MoveTo Target
				|	Take ObjectType
				|	Drop ObjectType
				|	Use ObjectType
				|	ReSetTargetDetector
				|	If Condition [Script] [Script]
:: Condition	=	ObjectInCurrentSection ObjectType
				|	CarriesObject ObjectType
				|	ActorStatus ActorStatus
				|	And Condition Condition
				|	Or Condition Condition

derive class iTask Target, Script, Condition

handleFireScript 	:: SimpleSDSLens [Script]
handleFloodScript 	:: SimpleSDSLens [Script]
handleSmokeScript 	:: SimpleSDSLens [Script]

changeFireScript	:: Task ()
changeFloodScript 	:: Task ()
changeSmokeScript 	:: Task ()

interperScript 		:: !(!Coord3D, !SectionStatus) !User ![Script] -> Task Bool

