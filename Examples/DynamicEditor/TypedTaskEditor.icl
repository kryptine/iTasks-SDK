module TypedTaskEditor

import StdEnv
import Data.Func
import Text
import iTasks
import iTasks.Extensions.DateTime
import iTasks.Extensions.Editors.DynamicEditor


// Helpers /////////////////////////////////////////////////////////////////////

:: Message :== String
:: Button  :== String

cons x xs :== [x:xs]

(>?>) infixl 1 :: !(Task a) [(Button, a -> Bool, a -> Task b)] -> Task b | iTask a & iTask b
(>?>) task options = task >>* map trans options
where
	trans (a, p, t) = OnAction (Action a) (ifValue p t)

// Main ////////////////////////////////////////////////////////////////////////

Start :: !*World -> *World
Start world = doTasks (editTaskExpr Nothing) world

editTaskExpr :: (Maybe (DynamicEditorValue TaskExpr)) -> Task (Maybe (DynamicEditorValue TaskExpr))
editTaskExpr mv =
	enterOrUpdateExpr "Contruct a task" info1 mv >?>
	[ ( "Run", const True, \v ->
			Title "Evaluate the task" @>> Hint info2 @>> viewInformation [] ()
		||-
			(set [] globalValueShare >-| evalTaskExpr (valueCorrespondingTo taskEditor v))
		>>*
		[ OnAction (Action "Back") (always (editTaskExpr (Just v)))
		, OnAction
			(Action "Finish")
			( ifValue
				(const True)
				\r ->
					Title "Done!" @>> Hint info3 @>> viewInformation [] (toString r) >?>
					[ ("Back", const True, \_ -> editTaskExpr (Just v))
					]
			)
		]
	  )
	]
where
	info1 = "Select the editors and combinators you'd like to use. When you're ready, push the 'Continue' button below to run your program."
	info2 = "Now step through the task you just created to test it."
	info3 = "The program is done, the result is given below."

	enterOrUpdateExpr ::
		!String !String !(Maybe (DynamicEditorValue TaskExpr)) -> Task (DynamicEditorValue TaskExpr)
	enterOrUpdateExpr title descr Nothing =
		Title title @>> Hint descr @>> enterInformation [EnterUsing id $ dynamicEditor taskEditor]
	enterOrUpdateExpr title descr (Just v) =
		Title title @>> Hint descr @>> updateInformation [UpdateUsing id (curry snd) (dynamicEditor taskEditor)] v

// Data ////////////////////////////////////////////////////////////////////////

:: TaskExpr
	= Done      Expr
	| EnterInfo Ty String
	| Then      TaskExpr TaskFunc
	| Both      TaskExpr TaskExpr
	| Any       TaskExpr TaskExpr
	| One       Button TaskExpr Button TaskExpr
	| Watch     String
	| Forever   TaskExpr

:: TaskFunc
	= ThenF   TaskFunc TaskFunc
	| ViewF   String Func
	| UpdateF String Func
	| StoreF
	| WatchF  String

:: Expr
	= Int    Int
	| Bool   Bool
	| String String
	| Date   Date
	| Pair   Expr Expr
	| Apply  Func Expr

:: Func
	= Identity
	| Conj     Expr
	| Disj     Expr
	| Not
	| Gt       Expr
	| Ge       Expr
	| Eq       Expr
	| Le       Expr
	| Lt       Expr
	| Add      Expr
	| Sub      Expr
	| Mul      Expr
	| Div      Expr
	| Fst
	| Snd

:: Value
	= VUnit
	| VInt    Int
	| VBool   Bool
	| VString String
	| VDate   Date
	| VPair   Value Value

:: Ty = E.a: Ty (a -> Value) & iTask a

:: Typed a b =: Typed a

derive class iTask TaskExpr, TaskFunc, Expr, Func, Value, Typed

// These instances cannot be auto derived because of the existential quantifier.
// However, they will be never used, so we make them undefined.
gDefault{|Ty|}       = abort "Typed task editor: internal error with gDefault of Ty"
gEq{|Ty|}        _ _ = abort "Typed task editor: internal error with gEq of Ty"
JSONEncode{|Ty|} _ _ = abort "Typed task editor: internal error with JSONEncode of Ty"
JSONDecode{|Ty|} _ _ = abort "Typed task editor: internal error with JSONDecode of Ty"
gText{|Ty|}      _ _ = abort "Typed task editor: internal error with gText of Ty"
gEditor{|Ty|}        = abort "Typed task editor: internal error with gEditor of Ty"


// Editor //////////////////////////////////////////////////////////////////////

taskEditor :: DynamicEditor TaskExpr
taskEditor = DynamicEditor
	[ // This cons is used to provide untyped `TaskExpr` values.
	  DynamicCons $ functionConsDyn "TaskExpr" "(enter task)"
		(dynamic \(Typed taskExpr) -> taskExpr ::  A.a: (Typed TaskExpr a) -> TaskExpr)
	  <<@ HideIfOnlyChoice
	, DynamicConsGroup "Combinators"
	  [ functionConsDyn "Then" "sequence"
			( dynamic \(Typed task) (Typed taskFunc) -> Typed (Then task taskFunc) ::
				A.a b: (Typed TaskExpr (Task a)) (Typed TaskFunc (a -> Task b)) -> Typed TaskExpr (Task b)
			)
			<<@ applyVerticalBoxedLayout
	  , functionConsDyn "ThenF" "sequence"
			( dynamic \(Typed taskFunc1) (Typed taskFunc2) -> Typed (ThenF taskFunc1 taskFunc2) ::
				A.a b c: (Typed TaskFunc (a -> Task b)) (Typed TaskFunc (b -> Task c)) -> Typed TaskFunc (a -> Task c)
			)
			<<@ applyVerticalBoxedLayout
	  , functionConsDyn "Both" "both"
			( dynamic \(Typed task1) (Typed task2) -> Typed (Both task1 task2) ::
				A.a b: (Typed TaskExpr (Task a)) (Typed TaskExpr (Task b)) -> Typed TaskExpr (Task (a, b))
			)
			<<@ applyVerticalBoxedLayout
	  , functionConsDyn "Any" "any of"
			( dynamic \(Typed task1) (Typed task2) -> Typed (Any task1 task2) ::
				A.a b: (Typed TaskExpr (Task a)) (Typed TaskExpr (Task a)) -> Typed TaskExpr (Task a)
			)
			<<@ applyVerticalBoxedLayout
	  , functionConsDyn "One" "one of"
			( dynamic \button1 (Typed task1) button2 (Typed task2) -> Typed (One button1 task1 button2 task2) ::
				A.a b: String (Typed TaskExpr (Task a)) String (Typed TaskExpr (Task a)) -> Typed TaskExpr (Task a)
			)
			<<@ applyVerticalBoxedLayout
	  , functionConsDyn "Forever" "forever"
			( dynamic \(Typed taskExpr) -> Typed (Forever taskExpr) ::
				A.a: (Typed TaskExpr (Task a)) -> Typed TaskExpr (Task a)
			)
			<<@ applyVerticalBoxedLayout
	  ]
	, DynamicConsGroup "Editors"
		[ functionConsDyn "Enter" "enter"
			(dynamic \(Typed ty) s -> Typed (EnterInfo ty s) :: A.a: (Typed Ty a) String -> Typed TaskExpr (Task a))
			<<@ applyHorizontalBoxedLayout
			<<@ AddLabels [Nothing, Just "message"]
	  , functionConsDyn "ViewF" "view"
			( dynamic \s (Typed func) -> Typed (ViewF s func) ::
				A.a b: String (Typed Func (a -> b)) -> Typed TaskFunc (a -> Task b)
			)
			<<@ applyHorizontalBoxedLayout
			<<@ AddLabels [Just "message"]
	  , functionConsDyn "UpdateF" "update"
			( dynamic \s (Typed func) -> Typed (UpdateF s func) ::
				A.a b: String (Typed Func (a -> b)) -> Typed TaskFunc (a -> Task b)
			)
			<<@ applyHorizontalBoxedLayout
			<<@ AddLabels [ Just "message" ]
	  , functionConsDyn "Done" "done"
			(dynamic \(Typed expr) -> Typed (Done expr) :: A.a: (Typed Expr a) -> Typed TaskExpr (Task a))
			<<@ applyHorizontalBoxedLayout
	  ]
	// Task expressions and functions on shares
	, DynamicConsGroup "Shares"
		[ functionConsDyn "StoreF" "store"
			(dynamic Typed StoreF :: Typed TaskFunc (Int -> Task ()))
			<<@ applyHorizontalBoxedLayout
			<<@ AddLabels [ Just "message" ]
	  , functionConsDyn "Watch" "watch"
			(dynamic \msg -> Typed (Watch msg) :: A.a: String -> Typed TaskExpr (Task ()))
			<<@ applyHorizontalBoxedLayout
			<<@ AddLabels [ Just "message" ]
	  , functionConsDyn "WatchF" "watch"
			(dynamic \msg -> Typed (WatchF msg) :: A.a: String -> Typed TaskFunc (a -> Task ()))
			<<@ applyHorizontalBoxedLayout
			<<@ AddLabels [ Just "message" ]
	  ]
	// Non-task functions:
	, DynamicConsGroup "Basics"
		[ functionConsDyn "Identity" "this value"
			(dynamic Typed Identity :: A.a: Typed Func (a -> a))
			<<@ applyHorizontalLayout
		, functionConsDyn "Apply" "apply"
			( dynamic \(Typed func) (Typed expr) -> Typed (Apply func expr) ::
				A.a b: (Typed Func (a -> b)) (Typed Expr a) -> Typed Expr b
			)
			<<@ applyHorizontalBoxedLayout
			<<@ AddLabels [ Just "the function", Just "to" ]
		, functionConsDyn "Fst" "first element"
		  (dynamic Typed Fst :: A.a b: Typed Func ((a, b) -> a))
		  <<@ applyHorizontalLayout
		, functionConsDyn "Snd" "second element"
			(dynamic Typed Snd :: A.a b: Typed Func ((a, b) -> b))
			<<@ applyHorizontalLayout
		]
	, DynamicConsGroup "Arithmetic"
		[ functionConsDyn "Add" "add"
			(dynamic \(Typed i) -> Typed (Add i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
			<<@ applyHorizontalBoxedLayout
		, functionConsDyn "Sub" "subtract"
			(dynamic \(Typed i) -> Typed (Sub i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
			<<@ applyHorizontalBoxedLayout
		, functionConsDyn "Mul" "multiply with"
			(dynamic \(Typed i) -> Typed (Mul i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
			<<@ applyHorizontalBoxedLayout
		, functionConsDyn "Div" "divide by"
			(dynamic \(Typed i) -> Typed (Div i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
			<<@ applyHorizontalBoxedLayout
		]
	, DynamicConsGroup "Logic"
		[ functionConsDyn "Conj" "and"
			(dynamic \(Typed b) -> Typed (Conj b) :: (Typed Expr Bool) -> Typed Func (Bool -> Bool))
			<<@ applyHorizontalBoxedLayout
		, functionConsDyn "Disj" "or"
			(dynamic \(Typed b) -> Typed (Disj b) :: (Typed Expr Bool) -> Typed Func (Bool -> Bool))
			<<@ applyHorizontalBoxedLayout
		, functionConsDyn "Not" "negate"
			(dynamic Typed Not :: Typed Func (Bool -> Bool))
			<<@ applyHorizontalLayout
		]
	, DynamicConsGroup "Comparison"
		[ functionConsDyn "Gt" "is greater than"
			(dynamic \(Typed i) -> Typed (Gt i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
			<<@ applyHorizontalBoxedLayout
		, functionConsDyn "Ge" "is greater or equal"
			(dynamic \(Typed i) -> Typed (Ge i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
			<<@ applyHorizontalBoxedLayout
		, functionConsDyn "Eq" "is equal to"
			(dynamic \(Typed i) -> Typed (Eq i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
			<<@ applyHorizontalBoxedLayout
		, functionConsDyn "Le" "is lesser than"
			(dynamic \(Typed i) -> Typed (Le i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
			<<@ applyHorizontalBoxedLayout
		, functionConsDyn "Lt" "is lesser than"
			(dynamic \(Typed i) -> Typed (Lt i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
			<<@ applyHorizontalBoxedLayout
		]
	// Non-task expressions:
	, DynamicConsGroup "Values"
		[ functionConsDyn "Int" "the integer"
			(dynamic \i -> Typed (Int i) :: Int -> Typed Expr Int)
			<<@ applyHorizontalLayout
		, functionConsDyn "Bool" "the boolean"
			(dynamic \b -> Typed (Bool b) :: Bool -> Typed Expr Bool)
			<<@ applyHorizontalLayout
		, functionConsDyn "String" "the string"
			(dynamic \s -> Typed (String s) :: String -> Typed Expr String)
			<<@ applyHorizontalLayout
		, functionConsDyn "Date" "the date"
			(dynamic \d -> Typed (Date d) :: Date -> Typed Expr Date)
			<<@ applyHorizontalLayout
		, functionConsDyn "Pair" "the pair"
			( dynamic \(Typed a) (Typed b) -> Typed (Pair a b) ::
				A.a b: (Typed Expr a) (Typed Expr b) -> Typed Expr (a, b)
			)
			<<@ applyHorizontalBoxedLayout
			<<@ AddLabels [ Just "with", Just "and" ]
	]
	// Types
	, DynamicConsGroup "Types"
		[ functionConsDyn "Ty.Int" "Integer"
			(dynamic Typed (Ty VInt) :: Typed Ty Int)
			<<@ applyHorizontalLayout
		, functionConsDyn "Ty.Bool" "Boolean"
			(dynamic Typed (Ty VBool) :: Typed Ty Bool)
			<<@ applyHorizontalLayout
		, functionConsDyn "Ty.String" "String"
			(dynamic Typed (Ty VString) :: Typed Ty String)
			<<@ applyHorizontalLayout
		, functionConsDyn "Ty.Date" "Date"
			(dynamic Typed (Ty VDate) :: Typed Ty Date)
			<<@ applyHorizontalLayout
		, functionConsDyn "Ty.Pair" "Pair"
			( dynamic
				\(Typed (Ty toValue1)) (Typed (Ty toValue2)) -> Typed (Ty \(x, y) -> VPair (toValue1 x) (toValue2 y)) ::
				A.a b: (Typed Ty a) (Typed Ty b) -> Typed Ty (a, b)
			)
			<<@ applyHorizontalBoxedLayout
	  ]
	// Internal helper editors
	, DynamicConsGroup "Helpers"
		[ customEditorCons "int"    "(enter integer)" intEditor    <<@ HideIfOnlyChoice
		, customEditorCons "bool"   "(enter boolean)" boolEditor   <<@ HideIfOnlyChoice
		, customEditorCons "string" "(enter string )" stringEditor <<@ HideIfOnlyChoice
		, customEditorCons "date"   "(enter date )"   dateEditor   <<@ HideIfOnlyChoice
		]
	]
where
	intEditor :: Editor Int (Maybe Int)
	intEditor = gEditor{|*|}

	boolEditor :: Editor Bool (Maybe Bool)
	boolEditor = gEditor{|*|}

	stringEditor :: Editor String (Maybe String)
	stringEditor = gEditor{|*|}

	dateEditor :: Editor Date (Maybe Date)
	dateEditor = gEditor{|*|}

	basicClasses      = [ "typedtasks-base" ]
	horizontalClasses = [ "typedtasks-horizontal" ]
	verticalClasses   = [ "typedtasks-vertical" ]
	boxedClasses      = [ "typedtasks-boxed" ]

	applyHorizontalBoxedLayout = ApplyCssClasses $ basicClasses ++ horizontalClasses ++ boxedClasses
	applyVerticalBoxedLayout   = ApplyCssClasses $ basicClasses ++ verticalClasses ++ boxedClasses
	applyHorizontalLayout      = ApplyCssClasses $ basicClasses ++ horizontalClasses
	applyVerticalLayout        = ApplyCssClasses $ basicClasses ++ verticalClasses

// Evaluation //////////////////////////////////////////////////////////////////

globalValueShare :: SimpleSDSLens [Int]
globalValueShare = sharedStore "global share for typed task editor" []

evalTaskExpr :: TaskExpr -> Task Value
evalTaskExpr (Done expr) = return $ evalExpr expr
evalTaskExpr (EnterInfo (Ty toValue) msg) = Hint msg @>> enterInformation [] @ toValue
evalTaskExpr (Then task taskFunc) = evalTaskExpr task >>? evalTaskFunc taskFunc
evalTaskExpr (Both task1 task2) =
	(evalTaskExpr task1 -&&- evalTaskExpr task2) <<@ ApplyLayout arrangeHorizontal @ \(a, b) -> VPair a b
evalTaskExpr (Any task1 task2) = (evalTaskExpr task1 -||- evalTaskExpr task2) <<@ ApplyLayout arrangeHorizontal
evalTaskExpr (One button1 task1 button2 task2) =
	Hint "Make a choice" @>> viewInformation [] () >?>
	[ ( button1, const True, \_ -> evalTaskExpr task1 )
	, ( button2, const True, \_ -> evalTaskExpr task2 )
	]
evalTaskExpr (Watch msg) = Hint msg @>> viewSharedInformation [] globalValueShare @ (const VUnit)
evalTaskExpr (Forever task) = forever (evalTaskExpr task)

evalTaskFunc :: TaskFunc Value -> Task Value
evalTaskFunc (ThenF this next) val =
	evalTaskFunc this val >>? evalTaskFunc next
evalTaskFunc (ViewF msg func) val = case evalFunc val func of
	(VInt i) -> (Hint msg @>> viewInformation [] i @ VInt) <<@ ApplyLayout arrangeHorizontal
	(VBool b) -> (Hint msg @>> viewInformation [] b @ VBool) <<@ ApplyLayout arrangeHorizontal
	(VString s) -> (Hint msg @>> viewInformation [] s @ VString) <<@ ApplyLayout arrangeHorizontal
	(VDate s) -> (Hint msg @>> viewInformation [] s @ VDate) <<@ ApplyLayout arrangeHorizontal
	(VPair a b) ->
		(		Hint msg @>> viewInformation [] ()
			||-
				(evalTaskFunc (ViewF "" Identity) a -&&- evalTaskFunc (ViewF "" Identity) b)
			@ \(a, b) -> VPair a b
		)
		<<@ ApplyLayout arrangeHorizontal
evalTaskFunc (UpdateF msg func) val = case evalFunc val func of
	(VInt i) -> (Hint msg @>> updateInformation [] i @ VInt) <<@ ApplyLayout arrangeHorizontal
	(VBool b) -> (Hint msg @>> updateInformation [] b @ VBool) <<@ ApplyLayout arrangeHorizontal
	(VString s) -> (Hint msg @>> updateInformation [] s @ VString) <<@ ApplyLayout arrangeHorizontal
	(VDate s) -> (Hint msg @>> updateInformation [] s @ VDate) <<@ ApplyLayout arrangeHorizontal
	(VPair a b) ->
		(		Hint msg @>> viewInformation [] ()
			||-
				(evalTaskFunc (UpdateF "" Identity) a -&&- evalTaskFunc (UpdateF "" Identity) b)
			@ \(a, b) -> VPair a b
		)
		<<@ ApplyLayout arrangeHorizontal
evalTaskFunc (StoreF) (VInt i) =
  upd (cons i) globalValueShare @ (const VUnit)
evalTaskFunc (WatchF msg) val =
	Hint msg @>> viewSharedInformation [] globalValueShare @ (const VUnit)

evalExpr :: !Expr -> Value
evalExpr (Int i)                = VInt i
evalExpr (Bool b)               = VBool b
evalExpr (String s)             = VString s
evalExpr (Date d)               = VDate d
evalExpr (Pair fstExpr sndExpr) = VPair (evalExpr fstExpr) (evalExpr sndExpr)
evalExpr (Apply func expr)      = evalFunc (evalExpr expr) func

evalFunc :: !Value !Func -> Value
evalFunc val Identity = val

evalFunc (VInt i1) func = case func of
  Gt expr  = VBool $ i1 >  evalInt expr
  Ge expr  = VBool $ i1 >= evalInt expr
  Eq expr  = VBool $ i1 == evalInt expr
  Le expr  = VBool $ i1 <= evalInt expr
  Lt expr  = VBool $ i1 <  evalInt expr
  Add expr = VInt  $ i1 +  evalInt expr
  Sub expr = VInt  $ i1 -  evalInt expr
  Mul expr = VInt  $ i1 *  evalInt expr
  Div expr = VInt  $ i1 /  evalInt expr
where
	evalInt :: !Expr -> Int
	evalInt expr = case evalExpr expr of
		VInt i = i

evalFunc (VBool b1) func = case func of
	Eq expr   = VBool $ b1 == evalBool expr
	Conj expr = VBool $ b1 && evalBool expr
	Disj expr = VBool $ b1 || evalBool expr
	Not       = VBool $ not b1
where
	evalBool :: !Expr -> Bool
	evalBool expr = case evalExpr expr of
		VBool b = b

evalFunc (VString s1) func = case func of
	Eq expr = VBool $ s1 == evalString expr
where
	evalString :: !Expr -> String
	evalString expr = case evalExpr expr of
		VString s = s

evalFunc (VDate d1) func = case func of
	Eq expr = VBool $ d1 == evalDate expr
	Gt expr = VBool $ d1 >  evalDate expr
	Ge expr = VBool $ d1 >= evalDate expr
	Eq expr = VBool $ d1 == evalDate expr
	Le expr = VBool $ d1 <= evalDate expr
	Lt expr = VBool $ d1 <  evalDate expr
where
	evalDate :: !Expr -> Date
	evalDate expr = case evalExpr expr of
		VDate d = d

evalFunc (VPair x1 x2) func = case func of
	Fst = x1
	Snd = x2

instance toString Value where
	toString val = case val of
		VUnit     = "()"
		VInt i    = toString i
		VBool b   = toString b
		VString s = toString s
		VDate d   = toString d
		VPair x y = concat ["( ", toString x, ", ", toString y, " )"]
