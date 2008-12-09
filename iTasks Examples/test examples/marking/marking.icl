module marking

// This example show how marks can be given by people logged in
// The marks are intended for user 0 who can show them
// (c) mjp 2007/2008

import StdEnv, StdiTasks, iDataTrivial
//import iTaskUtil

derive gForm  Mark, [] 
derive gUpd   Mark, []
derive gParse Mark
derive gPrint Mark
derive gerda  Mark
derive read   Mark
derive write  Mark

:: Mark = {userName :: String, loginId :: Int, mark :: Int, comment :: String}

Start world = startTaskEngine (marking 0 "manager") world

marking i accountname 	=	[Text ("Welcome user " <+++ accountname),BrTag [],BrTag []] !>> respond i accountname
where

	respond uniqueId name
	=				spawnWorkflow uniqueId True ("Give Mark",    foreverTask (giveMark    uniqueId name))
		#>>			spawnWorkflow uniqueId True ("Give Comment", foreverTask (giveComment uniqueId name))
		#>>			foreverTask show 							


show
=  					readMarksDB
	=>>	\marks -> 	[ Text "Here are the scores given by the users:", BrTag [], BrTag []
					, STable [BorderAttr (toString 1)]	[[Text (toString (number i marks)) \\ i <- [0..10]]
							   				,[BTag [] [Text (toString i)] 			  \\ i <- [0..10]]
							   				]
					, BrTag [], BrTag [] 
					, HrTag []
					, Text (foldl (+++) "" [m.userName +++ " : " +++ m.comment +++ " +++ "  \\ m <- marks  ])
					, HrTag []
					] ?>> Confirm "Refresh"
where	
	number i marks = length [n\\n <- marks | n.mark == i]


giveMark uniqueId name 
= 							readMyMarksDB uniqueId
	=>> \(mark,comment) ->	[ Text ("Previous mark given:" <+++ if (mark == -1) "No mark given" (toString mark)), BrTag [], BrTag []
							, Text "Give your new mark (0 = lowest, 10 = highest)", BrTag [], BrTag []] 
							?>> chooseTask [] [(toString i,return_V i) \\ i <- [0..2]] -||-
								chooseTask [] [(toString i,return_V i) \\ i <- [3..5]] -||-
								chooseTask [] [(toString i,return_V i) \\ i <- [6..8]] -||-
								chooseTask [] [(toString i,return_V i) \\ i <- [9..10]]
	=>> \mark -> 			readMyMarksDB uniqueId
	=>> \(_,comment) ->		writeMarksDB {userName = name, loginId = uniqueId, mark = mark, comment = comment}
	#>>						[Text ("Your mark " <+++ mark <+++ " has been stored!"),BrTag [],BrTag []]
							?>> OK

giveComment uniqueId name 
= 							readMyMarksDB uniqueId
	=>> \(mark,comment) ->	[ Text "Previous comment given:", BrTag [], BrTag []
							, Text (if (comment == "" ) "None" comment), BrTag [], BrTag []
							, Text "Submit a new comment:", BrTag [], BrTag []] 
							?>> editTask "OK" textBox <<@ Submit
	=>> \(HtmlTextarea _  comment) -> readMyMarksDB uniqueId
	=>> \(mark,_) ->		
							writeMarksDB {userName = name, loginId = uniqueId, mark = mark, comment = comment}
	#>>						[ Text "Your comment:", BrTag [], BrTag []
							, Text comment, BrTag [], BrTag []
							, Text "has been stored!",BrTag [],BrTag []]
							?>> OK
where
	textBox :: HtmlTextarea
	textBox = createDefault

Confirm name =  buttonTask name (return_V Void)

OK = Confirm "OK" 

STable atts table		= TableTag atts (mktable table)
where
	mktable table 	= [TrTag [] (mkrow rows)           \\ rows <- table]
	mkrow   rows 	= [TdTag [ValignAttr "top"]  [row] \\ row  <- rows ]


// database specialized

marksId :: DBid [Mark]
marksId	= mkDBid "marks" LSTxtFile

readMarksDB :: Task [Mark]
readMarksDB = readDB marksId

readMyMarksDB :: Int -> Task (Int,String)
readMyMarksDB id
=					readMarksDB
	=>>	\marks ->	return_V 	(case (filter (\mark -> mark.loginId == id) marks) of 
									[] 			-> (-1,"")
									[mark:_] 	-> (mark.mark,mark.comment)
								)

writeMarksDB :: Mark -> Task [Mark]
writeMarksDB acc
=	readMarksDB
	=>> \accs -> writeDB marksId [acc:[oacc \\ oacc <- accs | oacc.loginId <> acc.loginId]] 

