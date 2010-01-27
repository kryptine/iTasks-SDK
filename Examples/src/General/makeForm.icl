module makeForm
 
import 	iTasks, CommonDomain, GeoDomain
from 	StdFunc import o
				
derive gPrint 		Basic, ViewWhat, DynFlow, EditorInfo, AssignInfo
derive gParse 		Basic, ViewWhat, DynFlow, EditorInfo, AssignInfo
derive gUpdate 		Basic, ViewWhat, DynFlow, EditorInfo, AssignInfo
derive gVisualize 	Basic, ViewWhat, DynFlow, EditorInfo, AssignInfo

Start :: *World -> *World 
Start world = startEngine dynFormEditor world

dynFormEditor :: [Workflow]
dynFormEditor
= [	{ Workflow 
	| name		= "Basic/makeForm"
	, label		= "Basic/makeForm"
	, roles		= []
	, mainTask	= test >>| return Void
	}
  ]

:: T a b	= T a & iTask b

// String is used to prompt user
// Maybe type used to ensure that the designer doe not need to fill in the form when designing it

:: MyField a :== (String,Maybe a)			

:: Basic 	= Integer 	
			| Real 		
			| String 	
			| Bool 		
			| Tuple  	Basic Basic
			| List 		Basic
			| Option 	Basic
			| Labeled 	String Basic
			| Notes 	
			| Date 		
			| Time 		
			| Document 	
			| GoogleMap 

:: ViewWhat	= ShowPreview
			| EnterDefaultValues

// ------------

test = makeFlow 

// ------------

:: DynFlow	= Editor EditorInfo
			| DisplayIt EditorInfo
			| Return
			| First
			| Second
			| Or  [DynFlow] [DynFlow]
			| And [DynFlow] [DynFlow]

:: EditorInfo 
			= { prompt  	:: String
			  ,	assignTo 	:: Maybe AssignInfo
			  }
:: AssignInfo
			= { idOfUser	:: Int
			  ,	taskName 	:: String
			  }

makeFlow ::  Task Void
makeFlow 	
	=						makeForm []
		>>= \(form,_) ->	makeFlow form []
where
	makeFlow form flow
		=					updateInformation "Construct a flow:" flow
			>>= \flow ->	checkFlow form flow
			>>= \dyn ->		eval form flow dyn

	eval form flow (T t:: T (Task a) a)	= t >>| makeFlow form flow
	eval form flow (T v:: T a a)		= showMessageAbout "Result is not a task" v >>| makeFlow form flow
	eval form flow _					= showMessage "Type error !" >>| makeFlow form flow

checkFlow :: Dynamic [DynFlow] -> Task Dynamic  
checkFlow (T v:: T a a) [Editor info : flows]
	= checkFlow (dynamic T (assignTask info v) :: T (Task a) a) flows
checkFlow (T t:: T (Task a) a) [Editor info : flows]
	= checkFlow (dynamic T (t >>= \a -> assignTask info a) :: T (Task a) a) flows

checkFlow (T v:: T a a) [DisplayIt info : flows]
	= checkFlow (dynamic T (showMessageTask info v) :: T (Task Void) Void) flows
checkFlow (T t:: T (Task a) a) [DisplayIt info : flows]
	= checkFlow (dynamic T (t >>= \a -> showMessageTask info a) :: T (Task Void) Void) flows

checkFlow dyn [Return : flows]
	= checkFlow dyn flows

checkFlow dyn [Or dfa dfb : flows]
	= checkFlow dyn dfa >>= \da -> checkFlow dyn dfb >>= \db -> orTask da db flows
checkFlow dyn _
	= return dyn

assignTask info=:{assignTo = Just some} v 		
	= assign some.idOfUser NormalPriority Nothing (updateInformation info.prompt v <<@ some.taskName)
assignTask info v 		
	= updateInformation info.prompt v

showMessageTask info=:{assignTo = Just some} v 		
	= assign some.idOfUser NormalPriority Nothing (showMessageAbout info.prompt v <<@ some.taskName)
showMessageTask info v 		
	= showMessageAbout info.prompt v


orTask (T ta :: T (Task a) a) (T tb :: T (Task a) a) flows  
	= checkFlow (dynamic T (ta -||- tb) :: T (Task a) a) flows	
orTask _ _ flows  
	= dynError "Type error in Or tasks"


dynError s = return (dynamic T s :: T String String)

// ------------

makeForm :: [Basic] -> Task (Dynamic,[Basic])
makeForm basics 
	= 					editorB basics
		>>= \basics ->	converter basics 
		>>= \dyn ->		show basics dyn
where
	editorB :: [Basic] -> Task [Basic]
	editorB basics = updateInformation "Construct a form by chosing the types of the form fields:" basics

	show :: [Basic] Dynamic -> Task (Dynamic,[Basic])
	show basics d=:(T v :: T a a)	
		= 	(						updateInformation "Resulting form, if OK, set default values:" (Just v) 
			)
			-||-				
			(						updateInformation "Cancel, redo form construction" Void
				>>|					return Nothing
			)			
			>>= makeChoice basics			
	where
		makeChoice :: [Basic] (Maybe a) -> Task (Dynamic,[Basic]) | iTask a
		makeChoice basics Nothing 	 = makeForm basics
		makeChoice basics (Just nv)  = returnShow basics d nv				

		returnShow ::  [Basic] Dynamic a -> Task (Dynamic,[Basic]) | iTask a
		returnShow basics d=:(T v :: T a^ b) nv = return (dynamic T nv :: T a^ a^,basics)

	converter :: [Basic] -> Task Dynamic
	converter bs = convertBasics bs >>= return o tupling
	where
		tupling [] 		= dynamic T Void :: T Void Void
		tupling [d]		= d
		tupling [d:ds]	= case (d, tupling ds) of 
								(T d1 :: T a a, T d2 :: T b b) -> dynamic T (d1,d2) :: T (a,b) (a,b)	

		convertBasics :: [Basic] -> Task [Dynamic]
		convertBasics [] 		= return []
		convertBasics [b:bs] 	= convert b >>= \d -> convertBasics bs >>= \ds -> return [d:ds] 
		where
			convert :: Basic -> Task Dynamic
			convert	Integer				= getDefaultValue >>= \v -> return (dynamic T v :: T Int Int)	
			convert	Real				= getDefaultValue >>= \v -> return (dynamic T v :: T Real Real)	
			convert	String				= getDefaultValue >>= \v -> return (dynamic T v :: T String String)	
			convert	Bool				= getDefaultValue >>= \v -> return (dynamic T v :: T Bool Bool)	
			convert	(Tuple b1 b2)		= 				convert b1 
											>>= \db1 -> convert b2
											>>= \db2 -> returnTuple db1 db2	
			convert (List b)			=				convert b
											>>= \dl ->	returnList dl
			convert (Option b)			=				convert b
											>>= \db ->	returnOption db
			convert (Labeled s b)		=				convert b
											>>= \db ->	returnLabel s db
			convert	Notes				= getDefaultValue >>= \v -> return (dynamic T v :: T Note Note)	
			convert	Date				= getDefaultValue >>= \v -> return (dynamic T v :: T Date Date)	
			convert	Time				= getDefaultValue >>= \v -> return (dynamic T v :: T Time Time)	
			convert	Document			= getDefaultValue >>= \v -> return (dynamic T v :: T Document Document)	
			convert	GoogleMap			= getDefaultValue >>= \v -> return (dynamic T v :: T GoogleMap GoogleMap)	

			returnTuple (T t1 :: T a a) (T t2 :: T b b) = return (dynamic T (t1,t2) :: T (a,b) (a,b))
			
			returnList (T v :: T a a) = return (dynamic T [] :: T [a] [a])
			
			returnOption (T v :: T a a) = return (dynamic T Nothing :: T (Maybe a) (Maybe a))

			returnLabel s (T v :: T a a) = return (dynamic T (s,v) :: T (String,a) (String,a))

	

