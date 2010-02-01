module makeForm
 
import 	iTasks, CommonDomain, GeoDomain
from 	StdFunc import o
from	EstherBackend import toStringDynamic
				
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
// Maybe type used to ensure that the designer does not need to fill in the form when designing it

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
/*
:: T2 a b = T2 a & TC b

f :: Dynamic -> Dynamic
f (T2 (x,y) :: T2 (a,b) (a,b)) = dynamic x :: T2 a a

*/

test = makeFlow 

showDyn :: Dynamic -> (String,String)
showDyn dyn	
# (v,t) =  toStringDynamic dyn 
=	case dyn of
		(fun :: a -> b) -> ("<function> ",t) 
		_				-> (foldr (+++) "" v,t)

showDynType  = snd o showDyn
showDynVal   = fst o showDyn
showDynValType s d = let (v,t) = showDyn d in s +++ ", " +++ v +++ "::" +++ t

dynError  s d1    = return (dynamic T (dynErrorMess s d1):: T String String)
dynErrorMess s d1 = s +++ ", Type Error: " +++ showDynType d1

dynError2 s d1 d2 = return (dynamic T (dynErrorMess2 s d1 d2):: T String String)
dynErrorMess2 s d1 d2 = s +++ ", Cannot Unify: " +++ showDynType d1 +++ " with "  +++ showDynType d2

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
	eval form flow d=:(T v:: T a a)		= showMessage (showDynValType "Result" (dynamic v :: a)) >>| makeFlow form flow
	eval form flow d					= showMessage (dynErrorMess "Eval" d) >>| makeFlow form flow

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

checkFlow dyn [And dfa dfb : flows]
	= checkFlow dyn dfa >>= \da -> checkFlow dyn dfb >>= \db -> andTask da db flows

checkFlow (T (x,y) :: T (a,b) c) [First : flows]			// (a,b) would make more sense
	= checkFlow (dynamic T x :: T a c) flows
//checkFlow (T t :: T (Task (a,b)) (a,b)) [First : flows]			// (a,b) would make more sense
//	= checkFlow (dynamic T (t >>= \(x,y) -> return x) :: T (Task a) a) flows
checkFlow d1 [First : flows]			
	= dynError2 "First" d1 (dynamic fst :: A.a b : (a,b) -> a)
	
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
orTask d1 d2 flows  
	= dynError2 "Or" d1 d2

andTask (T ta :: T (Task a) a) (T tb :: T (Task b) b) flows  
	= checkFlow (dynamic T (ta -&&- tb) :: T (Task (a,b)) (a,b)) flows	
andTask d1 d2 flows  
	= dynError2 "And" d1 d2



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

	

