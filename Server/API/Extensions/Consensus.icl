implementation module Consensus

import iTasks, GenEq
import Messages, Groups

derive bimap Maybe, (,)

:: Topic = { topic :: String, description :: Maybe Note}
:: Results a :== [(a,[(User,String)])]

derive class iTask Topic

askOpinions	:: Task Void
askOpinions	
 	=	defineTopic
	>>= \topic ->
		defineItemType
	>>= \type -> case type of
		"Date" 		= askOpinionsDate topic		>>| stop
		"Document"	= askOpinionsDocument topic	>>| stop
		"Other"		= askOpinionsOther topic	>>| stop
		
//The type dependent second part of the flow
askOpinionsGeneric :: Topic -> Task (Results a) | iTask a
askOpinionsGeneric topic
	=	defineItems
	>>= \items ->
		defineAnswers
	>>= \answers ->
		defineParticipants
	>>= \participants ->
		collectOpinions topic items answers participants
	>>= showResult
	
askOpinionsDate :: Topic -> Task (Results Date)
askOpinionsDate topic = askOpinionsGeneric topic

askOpinionsDocument :: Topic -> Task (Results Document)
askOpinionsDocument topic = askOpinionsGeneric topic

askOpinionsOther :: Topic -> Task (Results String)
askOpinionsOther topic = askOpinionsGeneric topic

defineTopic :: Task Topic
defineTopic
	= enterInformation ("Define topic","Please define the topic that you would like to get opinions about")

defineItemType :: Task String
defineItemType
	= enterChoice ("Define item type","What type of item(s) would you like to get opinions about") ["Date","Document","Other"]
	
defineItems :: Task [a] | iTask a
defineItems
	= enterInformation ("Define items","Enter the item(s) you would like to get opinions about")
	
defineAnswers :: Task [String]
defineAnswers
	= enterInformation ("Define answers","Please define the available answer choices")
	  -||-
	  enterChoice ("Common answers","Or select one of these common answer sets")
	  	[["Yes","No"],["Yes","No","Maybe"],["I agree","I disagree"],["1","2","3","4","5"]]
	  	
defineParticipants :: Task [User]
defineParticipants
	=	getMyGroups
	>>= \groups -> case groups of
		[]	= enterInformation ("Define people","Enter the people you would like to ask for an opinion")
		_	=	(enterChoice ("Choose a group","Choose a group...") groups >>= \group -> return group.members)
	  			-||-
	  			(enterInformation ("Define people","Or enter individual people to ask for an opinion")) 

collectOpinions :: Topic [a] [String] [User] -> Task (Results a) | iTask a
collectOpinions topic items answers participants
	= 	( Title "Collecting opinions..." @>> 
		  Description "Waiting for everyone to give their opinion" @>> 
		  allProc [(collectOpinion topic user items answers,initManagerProperties,noMenu) \\ user <- participants ]
		)
	>>= transform (orderByItem items)
where
	orderByItem :: [a] [(User,[(a,String)])] -> [(a,[(User,String)])]
	orderByItem items opinions = [(item, [(user, snd (options !! i)) \\ (user,options) <- opinions ]) \\ item <- items & i <- [0..]]
	
collectOpinion :: Topic User [a] [String] -> Task (User,[(a,String)]) | iTask a
collectOpinion topic user items answers
	=	user @:
		(Title ("Your opinion about: " +++ topic.topic) @>>
 		(allTasks [enterChoiceAbout ("Option " <+++ i,"What is your opinion about:") item answers  \\ item <- items & i <- [1..]] >>= transform (merge items)))
where
	merge items opinions = (user,zip (items,opinions))

showResult :: (Results a) -> Task (Results a) | iTask a
showResult result = showMessageAbout ("Opinions","The results of your opinion request:") result


