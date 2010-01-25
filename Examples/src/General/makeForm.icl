module makeForm
 
import 	iTasks, CommonDomain, GeoDomain
from 	StdFunc import o
				
derive gPrint 		Basic, ViewWhat
derive gParse 		Basic, ViewWhat
derive gUpdate 		Basic, ViewWhat
derive gVisualize 	Basic, ViewWhat

Start :: *World -> *World 
Start world = startEngine dynFormEditor world

dynFormEditor :: [Workflow]
dynFormEditor
= [	{ Workflow 
	| name		= "Basic/makeForm"
	, label		= "Basic/makeForm"
	, roles		= []
	, mainTask	= test [] >>| return Void
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

test :: [Basic] -> Task Dynamic
test basics 
	= 					editorB basics
		>>= \basics ->	converter basics 
		>>= \dyn ->		show basics dyn
where
	editorB :: [Basic] -> Task [Basic]
	editorB basics = updateInformation "Construct a form by chosing the types of the form fields:" basics

	show :: [Basic] Dynamic -> Task Dynamic
	show basics d=:(T v :: T a a)	
		= 	(						updateInformation "Resulting form, fill in what you like" (Just v) 
				>>= \(Just nv) ->	returnShow d nv
			)
			-||-				
			(						updateInformation "Cancel, redo form construction" Void
				>>|					test basics
			)			
								
	where
		returnShow :: Dynamic a -> Task Dynamic | iTask a
		returnShow d=:(T v :: T a^ b) nv = return (dynamic T nv :: T a^ a^)

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

	

