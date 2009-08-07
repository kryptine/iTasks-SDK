implementation module Vote

//This example lets users give a vote.
//Only the user with role "root" can view the votes
 
import iTasks
import CommonDomain

derive gPrint Vote
derive gParse Vote
derive gVisualize Vote
derive gUpdate Vote

:: Vote = { userId		:: Int
		  , vote		:: Int
		  , comment		:: String
		  }

voteExample :: [Workflow]
voteExample = [{ name		= "Examples/Business/Vote/Show votes"
			   , label		= "Show votes"
			   , roles		= ["root"]
			   , mainTask	= showVotes
			   }
			  ,{ name		= "Examples/Business/Vote/New vote"
			   , label		= "Give vote"
			   , roles		= []
			   , mainTask	= giveVote
			   }
			  ,{ name		= "Examples/Business/Vote/New comment"
			   , label		= "Give comment"
			   , roles		= []
			   , mainTask	= giveComment
			   }
			  ]

showVotes :: Task Void
showVotes
=  						readVotesDB
	>>=	\votes 		-> 	getDisplayNames [vote.Vote.userId \\ vote <- votes]
	>>= \userNames	->	showMessage
							[ Text "The following votes are given:", BrTag [], BrTag []
							, formatVotes	[(toString i) 					\\ i <- [0..10]]
											[(toString (number i votes))	\\ i <- [0..10]]
							, HrTag []
							, formatComments [(u,v) \\ v <- votes & u <- userNames]
							, HrTag []
							]
where	
	number i votes = length [n \\ n <- votes | n.vote == i]
	formatVotes	header data	= TableTag	[StyleAttr "border: 1px solid #ccc"]
										[TrTag [] [ThTag [] [BTag [] [Text th]] \\th <- header]
										,TrTag [] [TdTag [] [Text td] \\ td <- data]
										]
	formatComments rows		= TableTag	[]
										[TrTag [] [TdTag [] [Text u,Text ":"],TdTag [] [Text v.comment]] \\ (u,v) <- rows]
	
giveVote :: Task Void
giveVote 
=							getCurrentUser
	>>= \(currentUser,_)->	readMyVoteDB currentUser
	>>= \(vote,comment) ->	chooseTask 
								[ Text ("Previous vote given:" +++ if (vote == -1) "No vote given" (toString vote)), BrTag [], BrTag []
								, Text "Give your new vote (0 = lowest, 10 = highest)", BrTag [], BrTag []]
								[(toString i,return i) \\ i <- [0..10]]
	>>= \vote -> 			readMyVoteDB currentUser
	>>= \(_,comment) ->		writeVotesDB {userId = currentUser, vote = vote, comment = comment}
	>>|						showMessage [Text ("Your vote " +++ toString vote +++ " has been stored!")]

giveComment :: Task Void
giveComment
=							getCurrentUser
 	>>= \(currentUser,_)->	readMyVoteDB currentUser
	>>= \(vote,comment) ->	requestInformationWD 
								[ Text "Previous comment given:", BrTag [], BrTag []
								, Text (if (comment == "" ) "None" comment), BrTag [], BrTag []
								, Text "Submit a new comment:", BrTag [], BrTag []
								] 
								textBox
	>>= \(Note comment) -> readMyVoteDB currentUser
	>>= \(vote,_) ->		
							writeVotesDB {userId = currentUser, vote = vote, comment = comment}
	>>|						showMessage
								[ Text "Your comment:", BrTag [], BrTag []
								, Text comment, BrTag [], BrTag []
								, Text "has been stored!"
								]
						
where
	textBox :: Note
	textBox = createDefault

//Simple votes database

votesId :: DBid [Vote]
votesId	= mkDBid "votes"

readVotesDB :: Task [Vote]
readVotesDB = readDB votesId

readMyVoteDB :: Int -> Task (Int,String)
readMyVoteDB id
=					readVotesDB
	>>=	\votes ->	return 	(case (filter (\vote -> vote.Vote.userId == id) votes) of 
									[] 			-> (-1,"")
									[vote:_] 	-> (vote.vote, vote.comment)
								)

writeVotesDB :: Vote -> Task [Vote]
writeVotesDB acc
=	readVotesDB
	>>= \accs -> writeDB votesId [acc:[oacc \\ oacc <- accs | oacc.Vote.userId <> acc.Vote.userId]] 

