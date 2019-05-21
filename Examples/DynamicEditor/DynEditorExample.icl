module DynEditorExample

import StdEnv
import Data.Func, Data.Functor
import Text
import iTasks, iTasks.Extensions.Editors.DynamicEditor

Start world = doTasks editTask world

editTask =
	enterInformation () [EnterUsing id $ dynamicEditor taskEditor] >>=
	evalTaskConstExpr o toValue taskEditor

:: TaskConstExpr = Apply TaskFuncExpr Expr
:: TaskFuncExpr  = ViewInformation
:: Expr          = Int Int | Bool Bool | Tuple Expr Expr | Fst Expr | Snd Expr | Eq Expr Expr
:: Value         = VInt Int | VBool Bool | VTuple Value Value

:: Typed a b =: Typed a

derive class iTask TaskConstExpr, TaskFuncExpr, Expr, Value, Typed

taskEditor :: DynamicEditor x | TC x
taskEditor = DynamicEditor conses
where
	conses =
		[ // This cons is used to provide untyped `TaskConstExpr` values.
		  DynamicCons $
			functionConsDyn "TaskConstExpr" "(enter task)"
				(dynamic \(Typed taskExpr) -> taskExpr :: A.a: (Typed TaskConstExpr a) -> TaskConstExpr)
		  <<@@@ HideIfOnlyChoice
		, DynamicConsGroup "Combinators"
			[ functionConsDyn "Apply" "$"
				(	dynamic \(Typed taskFunc) (Typed expr) -> Typed (Apply taskFunc expr) ::
					A.a b: (Typed TaskFuncExpr (a -> Task b)) (Typed Expr a) -> Typed TaskConstExpr (Task b)
				)
			]
		, DynamicConsGroup "Editors"
			[ functionConsDyn "ViewInformation" "view information"
				(dynamic Typed ViewInformation :: A.a: Typed TaskFuncExpr (a -> Task a))
			]
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
		]

	intEditor :: Editor Int
	intEditor = gEditor{|*|}

	boolEditor :: Editor Bool
	boolEditor = gEditor{|*|}

evalTaskConstExpr :: TaskConstExpr -> Task Value
evalTaskConstExpr (Apply taskFunc expr) = evalTaskFuncExpr taskFunc $ evalExpr expr

evalTaskFuncExpr :: TaskFuncExpr Value -> Task Value
evalTaskFuncExpr ViewInformation value = viewInformation () [] value

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

/*
import Data.Func, Data.Functor, Data.Maybe
import iTasks, iTasks.UI.Editor.Modifiers
import iTasks.Extensions.Editors.DynamicEditor

:: TE  	= UpdEdit |  ViewEdit | Return //  EnterEdit 
		| Apply TE TE
		| Bind TE TE | Blind TE TE
		| Or TE TE | And TE TE 
		| I Int | B Bool | T (TE, TE) | Fst TE | Snd TE 

:: RTE a =: RTE TE
:: Tsk a =: Tsk a
:: Val a =: Val a

derive class iTask  TE, RTE, Tsk, Val 

tEditor :: DynamicEditor (RTE  a) | TC a
tEditor 
	= DynamicEditor ( 
	[ DynamicConsGroup "Editors"
		[ functionConsDyn "updEdit" "update"
		                ( dynamic RTE UpdEdit 
		                :: A.b: (RTE ((Val b) -> (Tsk (Val b))))
		                ) <<@@@ UseAsDefault
		, functionConsDyn "viewEdit" "view"
		                ( dynamic (RTE ViewEdit)
		                :: A.b : (RTE ((Val b) -> (Tsk (Val b))))
		                )
		, functionConsDyn "return"  "return"     
						( dynamic (RTE Return) 
						:: A.b : (RTE ((Val b) -> (Tsk (Val b))))
						) 
//			, functionConsDyn "enterEdit" "enter"
//			                ( dynamic (RTE EnterEdit)
//			                :: A.b : (RTE (Tsk (Val b)))
//			                ) <<@@@ UseAsDefault
		, functionConsDyn "apply" "editor:"
		                ( dynamic \(RTE f) (RTE v) = (RTE (Apply f v)) 
		                :: A.b c: (RTE ((Val b) -> (Tsk (Val c)))) (RTE (Val b)) -> (RTE (Tsk (Val c))) // crashes, type c is to general
						) <<@@@ LayoutVertical
		]
	] ++
	[ DynamicConsGroup "Sequencial Tasks"
		[ functionConsDyn "bind"  ">>="     
						( dynamic \(RTE  b) (RTE c) = RTE (Bind b c) 
						:: A.b c: (RTE (Tsk (Val b))) (RTE ((Val b) -> (Tsk (Val c)))) -> RTE (Tsk (Val c))
						) <<@@@ LayoutVertical
		, functionConsDyn "blind"  ">>|"     
						( dynamic \(RTE  b) (RTE c) = RTE (Blind b c) 
						:: A.b c: (RTE (Tsk (Val b))) (RTE (Tsk (Val c))) -> RTE (Tsk (Val c))
						)  <<@@@ LayoutVertical
		]
	] ++
	[ DynamicConsGroup "Parallel Tasks"
		[ functionConsDyn "or" "-||-"
		                ( dynamic \(RTE t1) (RTE t2) = RTE (Or t1 t2) 
		                :: A.b: (RTE (Tsk (Val b))) (RTE (Tsk (Val b))) -> (RTE (Tsk (Val b)))  
		                ) <<@@@ LayoutVertical
		, functionConsDyn "and" "-&&-"
		                ( dynamic \(RTE t1) (RTE t2) = RTE (And t1 t2) 
		                :: A.b c: (RTE (Tsk (Val b))) (RTE (Tsk (Val b))) -> RTE (Tsk (Val (b,c)))
		                ) <<@@@ LayoutVertical
		]
	] ++
	[ DynamicConsGroup "Functions"
		[ functionConsDyn "fst" "first"
		                ( dynamic \(RTE t)  = RTE (Fst t) 
		                :: A.b c: (RTE (Val (b,c))) -> (RTE (Val b))
		                )
		, functionConsDyn "snd" "second"
		                ( dynamic \(RTE t)  = RTE (Snd t) 
		                :: A.b c: (RTE (Val (b,c))) -> (RTE (Val c))
		                )
		]
	] ++
	[ DynamicConsGroup "Basic Types"
		[ functionConsDyn "int"  "integer" 	
						( dynamic (RTE (I 0)) 
						:: RTE (Val Int) 
						) <<@@@ UseAsDefault
		, functionConsDyn "bool" "bool"
						( dynamic (RTE (B False)) 
						:: RTE (Val Bool) 
						)
		, functionConsDyn "tuple" "tuple"
						( dynamic \(RTE t1) (RTE t2) = RTE (T (t1, t2))
						::  A.b c: (RTE (Val b)) (RTE (Val c)) -> (RTE (Val (b,c)))
						) <<@@@ LayoutVertical
		]
	])


interpret :: (RTE (Tsk (Val Int))) -> Task TE  // TO DO for all cases
interpret (RTE v) = interper v 
where
	interper :: TE -> Task TE
//		interper EnterEdit 
//			= enterInformation "enter value" []
	interper (Apply f v) 
		= case f of
			UpdEdit  -> updateInformation "update value" [] v
			ViewEdit -> viewInformation "view value" [] v
			Return	 -> return v
	interper (Bind t1 t2) 
		= interper t1 >>= \v -> interper (Apply t2 v)
	interper (Blind t1 t2) 
		= interper t1 >>| interper t2 
	interper (Or t1 t2) 
		= interper t1 -||- interper t2 
	interper (And t1 t2) 
		= (interper t1 -&&- interper t2) @ T
	interper (Blind t1 t2) 
		= interper t1 >>| interper t2 

	interper (I i)
		= return (I i)
	interper (B b)
		= return (B b)
	interper (T (a,b))
		= return (T (a,b))


Start world = doTasks enterTask world

//enterTask :: Task TE
enterTask 
	= 	(enterInformation     () [EnterUsing id $ dynamicEditor tEditor] 
	>&> viewSharedInformation () [ViewAs $ fmap $ toValue tEditor])
    >>= \val -> case val of
    			(Just te) -> interpret (toValue tEditor te) >>= viewInformation "result = " [] 



// non-typesafe expression
:: Expr = IntLit Int | RealLit Real | Plus Expr Expr | ToInt Expr | ToReal Expr | Eq Expr Expr

// expression with phantom type
:: TypedExpr a =: TypedExpr Expr

derive class iTask Expr, TypedExpr

dslEditor :: DynamicEditor (TypedExpr a)
dslEditor = DynamicEditor  
    [ DynamicConsGroup "Fixed"
      [ functionConsDyn "plus" "plus"
                        ( dynamic \(TypedExpr x) (TypedExpr y) -> TypedExpr (Plus x y) ::
                          A.b: (TypedExpr b) (TypedExpr b) -> TypedExpr b
                        )
      , functionCons "toInt"  "to integer" toIntExpr
      , functionCons "toReal" "to decimal" toRealExpr
      , customEditorCons "int"  "(enter integer)"
                         (bijectEditorValue (\(TypedExpr (IntLit i)) -> i)  intLit  gEditor{|*|})
      , customEditorCons "real" "(enter decimal)"
                         (bijectEditorValue (\(TypedExpr (RealLit r)) -> r) realLit gEditor{|*|})
      , functionConsDyn "eq" "are equal"
                        ( dynamic \(TypedExpr x) (TypedExpr y) -> TypedExpr (Eq x y) ::
                          A.b: (TypedExpr b) (TypedExpr b) -> TypedExpr Bool
                        )
      ]
    ]
where
    toIntExpr :: (TypedExpr Real) -> TypedExpr Int
    toIntExpr (TypedExpr x) = TypedExpr (ToInt x)

    toRealExpr :: (TypedExpr Int) -> TypedExpr Real
    toRealExpr (TypedExpr x) = TypedExpr (ToReal x)

    intLit :: Int -> TypedExpr Int
    intLit i = TypedExpr (IntLit i)

    realLit :: Real -> TypedExpr Real
    realLit r = TypedExpr (RealLit r)*/
