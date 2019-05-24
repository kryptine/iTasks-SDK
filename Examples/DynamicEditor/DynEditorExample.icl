module DynEditorExample

import StdEnv
import Data.Func, Data.Functor
import Text
import iTasks, iTasks.Extensions.Editors.DynamicEditor

Start world = doTasks editTask world

editTask =		forever
		(		enterInformation () [EnterUsing id $ dynamicEditor taskEditor]
		>>=		evalTaskConstExpr o toValue taskEditor
		>>=		viewInformation "result of the completed task is:" []
		)

:: TaskConstExpr = Apply TaskFuncExpr Expr | EnterInformation Type | Bind TaskConstExpr TaskFuncExpr | Blind TaskConstExpr TaskConstExpr
				 | Or TaskConstExpr TaskConstExpr | And TaskConstExpr TaskConstExpr
				 | When TaskConstExpr FunExpr
:: TaskFuncExpr  = ViewInformation | UpdateInformation | Return
:: Expr          = Int Int | Bool Bool | Tuple Expr Expr | Fst Expr | Snd Expr | Eq Expr Expr
:: Value         = VInt Int | VBool Bool | VTuple Value Value
:: Type          = E.a: Type (a -> Value) & iTask a
:: Typed a b     =: Typed a
:: FunExpr	 	 = EqV Value | GrtV Value | LessV Value

derive class iTask TaskConstExpr, TaskFuncExpr, Expr, Value, Typed, FunExpr

// instances are never used
gDefault{|Type|} = undef
gEq{|Type|} _ _ = undef
JSONEncode{|Type|} _ _ = undef
JSONDecode{|Type|} _ _ = undef
gText{|Type|} _ _ = undef
gEditor{|Type|} = undef

taskEditor :: DynamicEditor TaskConstExpr
taskEditor = DynamicEditor conses
where
	conses =
		[ // This cons is used to provide untyped `TaskConstExpr` values.
		  DynamicCons $
			functionConsDyn "TaskConstExpr" "(enter task)"
				(dynamic \(Typed taskExpr) -> taskExpr :: A.a: (Typed TaskConstExpr a) -> TaskConstExpr)
		  <<@@@ HideIfOnlyChoice
		, DynamicConsGroup "Combinators"
			[ functionConsDyn "Bind" ">>="
				(	dynamic \(Typed task) (Typed taskFunc) -> Typed (Bind task taskFunc) ::
					A.a b:
						(Typed TaskConstExpr (Task a)) (Typed TaskFuncExpr (a -> Task b))
						-> Typed TaskConstExpr (Task b)
				)
			, functionConsDyn "Blind" ">>|"
				(	dynamic \(Typed task1) (Typed task2) -> Typed (Blind task1 task2) ::
					A.a b:
						(Typed TaskConstExpr (Task a)) (Typed TaskConstExpr (Task b))
						-> Typed TaskConstExpr (Task b)
				)
			, functionConsDyn "Or" "-||-"
				(	dynamic \(Typed task1) (Typed task2) -> Typed (Or task1 task2) ::
					A.a b:
						(Typed TaskConstExpr (Task a)) (Typed TaskConstExpr (Task a))
						-> Typed TaskConstExpr (Task a)
				)
			, functionConsDyn "And" "-&&-"
				(	dynamic \(Typed task1) (Typed task2) -> Typed (And task1 task2) ::
					A.a b:
						(Typed TaskConstExpr (Task a)) (Typed TaskConstExpr (Task b))
						-> Typed TaskConstExpr (Task (a,b))
				)
			, functionConsDyn "When" "when"
				(	dynamic \(Typed task1) (Typed funexpr) -> Typed (When task1 funexpr) ::
					A.a b:
						(Typed TaskConstExpr (Task a)) (Typed FunExpr a)
						-> Typed TaskConstExpr (Task a)
				)
				]
		, DynamicConsGroup "Editors"
			[ functionConsDyn "Apply" "apply"
				(	dynamic \(Typed taskFunc) (Typed expr) -> Typed (Apply taskFunc expr) ::
					A.a b: (Typed TaskFuncExpr (a -> Task b)) (Typed Expr a) -> Typed TaskConstExpr (Task b)
				) <<@@@ ApplyCssClasses["horizontal"] // don't know css class names to choose from 
			, functionConsDyn "EnterInformation" "enter information"
				(	dynamic \(Typed type) -> Typed (EnterInformation type) ::
					A.a: (Typed Type a) -> Typed TaskConstExpr (Task a)
				)
			, functionConsDyn "ViewInformation" "view information"
				(dynamic Typed ViewInformation :: A.a: Typed TaskFuncExpr (a -> Task a))
			, functionConsDyn "UpdateInformation" "update information"
				(dynamic Typed UpdateInformation :: A.a: Typed TaskFuncExpr (a -> Task a))
			, functionConsDyn "Return" "return"
				(dynamic Typed Return :: A.a: Typed TaskFuncExpr (a -> Task a))
			]

		// ordinary (non-task) expressions

		, DynamicCons $ functionConsDyn "EqV"   "equal"
			(dynamic \i -> Typed (EqV (VInt i))  :: Int  -> Typed FunExpr Int)
		, DynamicCons $ functionConsDyn "GrtV"   "greater"
			(dynamic \i -> Typed (GrtV (VInt i))  :: Int  -> Typed FunExpr Int)
		, DynamicCons $ functionConsDyn "LessV"   "less"
			(dynamic \i -> Typed (LessV (VInt i))  :: Int  -> Typed FunExpr Int)

		, DynamicCons $ functionConsDyn "int"   "enter integer:"
			(dynamic \i -> Typed (Int i)  :: Int  -> Typed Expr Int)
		, DynamicCons $ functionConsDyn "bool"  "enter boolean:"
			(dynamic \b -> Typed (Bool b) :: Bool -> Typed Expr Bool)
		, DynamicCons $ functionConsDyn "tuple" "enter tuple:"
			(	dynamic \(Typed a) (Typed b) -> Typed (Tuple a b) ::
				A.a b: (Typed Expr a) (Typed Expr b) -> Typed Expr (a, b)
			)
		, DynamicCons $ functionConsDyn "fst"   "fst"
			(dynamic \(Typed (Tuple a _)) -> Typed a :: A.a b: (Typed Expr (a, b)) -> Typed Expr a)
		, DynamicCons $ functionConsDyn "snd"   "snd"
			(dynamic \(Typed (Tuple _ b)) -> Typed b :: A.a b: (Typed Expr (a, b)) -> Typed Expr b)
		, DynamicCons $ functionConsDyn "=="    "=="
			(	dynamic \(Typed a) (Typed b) -> Typed (Eq a b) ::
				A.a: (Typed Expr a) (Typed Expr a) -> Typed Expr Bool
			)
		, DynamicCons $ customEditorCons "Int"   "(enter integer)" intEditor  <<@@@ HideIfOnlyChoice
		, DynamicCons $ customEditorCons "Bool"  "(enter boolean)" boolEditor <<@@@ HideIfOnlyChoice

		// type specifications for enterInformation

		, DynamicCons $ functionConsDyn "Type.Int"   "Int"   (dynamic Typed (Type VInt)  :: Typed Type Int)
		, DynamicCons $ functionConsDyn "Type.Bool"  "Bool"  (dynamic Typed (Type VBool) :: Typed Type Bool)
		, DynamicCons $ functionConsDyn "Type.Tuple" "Tuple"
			(	dynamic
					\(Typed (Type toValue1)) (Typed (Type toValue2)) ->
						Typed (Type \(x, y) -> VTuple (toValue1 x) (toValue2 y))
				::
					A.a b: (Typed Type a) (Typed Type b) -> Typed Type (a, b)
			)
		, DynamicCons $ functionConsDyn "Type.?" "(derived type)"
			(dynamic derivedType :: A.a: Typed Type a | iTask a)
		]

	derivedType :: Typed Type a | iTask a
	derivedType = case dynToValue of
		(toValue :: a^ -> Value | iTask a^) = Typed (Type toValue)
	where
		dynToValue = dynamic ()

	intEditor :: Editor Int
	intEditor = gEditor{|*|}

	boolEditor :: Editor Bool
	boolEditor = gEditor{|*|}

evalTaskConstExpr :: TaskConstExpr -> Task Value
evalTaskConstExpr (EnterInformation (Type toValue)) = enterInformation () [] @ toValue
evalTaskConstExpr (Apply taskFunc expr)             = evalTaskFuncExpr taskFunc $ evalExpr expr
evalTaskConstExpr (Bind task taskFunc)              = evalTaskConstExpr task >>= evalTaskFuncExpr taskFunc
evalTaskConstExpr (Blind task1 task2)             	= evalTaskConstExpr task1 >>| evalTaskConstExpr task2
evalTaskConstExpr (Or task1 task2)             		= evalTaskConstExpr task1 -||- evalTaskConstExpr task2
evalTaskConstExpr (And task1 task2)             	= evalTaskConstExpr task1 -&&- evalTaskConstExpr task2 @ \(a,b) -> VTuple a b
evalTaskConstExpr (When task pred)					= evalTaskConstExpr task >>* [OnAction ActionOk (ifValue test return)]
where
	test (VInt i) = case pred of
						(LessV (VInt j))  = i<j
						(GrtV  (VInt j))  = i>j
						(EqV   (VInt j))  = i==j
	test (VBool i) = case pred of
						(EqV   (VBool j)) = i==j

evalTaskFuncExpr :: TaskFuncExpr Value -> Task Value
evalTaskFuncExpr ViewInformation (VInt i)		= viewInformation () [] i @ VInt
evalTaskFuncExpr ViewInformation (VBool b)		= viewInformation () [] b @ VBool
evalTaskFuncExpr ViewInformation (VTuple a b)	= evalTaskFuncExpr ViewInformation a -&&- evalTaskFuncExpr ViewInformation b @ \(a,b) -> VTuple a b 
evalTaskFuncExpr UpdateInformation (VInt i)		= updateInformation () [] i @ VInt
evalTaskFuncExpr UpdateInformation (VBool b)	= updateInformation () [] b @ VBool
evalTaskFuncExpr UpdateInformation (VTuple a b)	= evalTaskFuncExpr UpdateInformation a -&&- evalTaskFuncExpr UpdateInformation b @ \(a,b) -> VTuple a b
evalTaskFuncExpr Return value 					= return value


evalExpr :: Expr -> Value
evalExpr (Int i)                 = VInt i
evalExpr (Bool b)                = VBool b
evalExpr (Tuple fstExpr sndExpr) = VTuple (evalExpr fstExpr) (evalExpr sndExpr)
evalExpr (Fst expr)              = fst
where
	(VTuple fst _) = evalExpr expr
evalExpr (Snd expr)              = snd
where
	(VTuple _ snd) = evalExpr expr
evalExpr (Eq expr1 expr2)        = VBool $ evalExpr expr1 === evalExpr expr2

