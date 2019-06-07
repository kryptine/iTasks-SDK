module DynEditorExample

import StdEnv
import Data.Func, Data.Functor
import Text
import iTasks, iTasks.Extensions.Editors.DynamicEditor


Start world = doTasks editTask world

editTask =    forever
    (      viewInformation "Contruct a Task expression:" [] ()
          ||-
          enterInformation () [EnterUsing id $ dynamicEditor taskEditor]
    >>=  \v ->  viewInformation "Evaluate the Expression:" [] ()
          ||-
          evalTaskConstExpr (toValue taskEditor v)
    >>=      viewInformation "Result of the Task is:" []
    >>=      return
    ) // <<@ ApplyLayout frameCompact

:: TaskConstExpr = Apply TaskFuncExpr Expr | EnterInformation String Type | Bind TaskConstExpr TaskFuncExpr | Blind TaskConstExpr TaskConstExpr
         | Or TaskConstExpr TaskConstExpr | And TaskConstExpr TaskConstExpr
         | When TaskConstExpr [(FunExpr, String, TaskFuncExpr)]
:: TaskFuncExpr  = ViewInformation String | UpdateInformation String | Return

:: Expr          = Int Int | Bool Bool | String String | Tuple Expr Expr | Fst Expr | Snd Expr | Eq Expr Expr

:: Value         = VInt Int | VBool Bool | VString String | VTuple Value Value
:: FunExpr      = EqV Value | GrtV Value | LessV Value

:: Type          = E.a: Type (a -> Value) & iTask a
:: Typed a b     =: Typed a

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
        (  dynamic \(Typed task) (Typed taskFunc) -> Typed (Bind task taskFunc) ::
          A.a b:
            (Typed TaskConstExpr (Task a)) (Typed TaskFuncExpr (a -> Task b))
            -> Typed TaskConstExpr (Task b)
        )
      , functionConsDyn "Blind" ">>|"
        (  dynamic \(Typed task1) (Typed task2) -> Typed (Blind task1 task2) ::
          A.a b:
            (Typed TaskConstExpr (Task a)) (Typed TaskConstExpr (Task b))
            -> Typed TaskConstExpr (Task b)
        )
      , functionConsDyn "Or" "-||-"
        (  dynamic \(Typed task1) (Typed task2) -> Typed (Or task1 task2) ::
          A.a b:
            (Typed TaskConstExpr (Task a)) (Typed TaskConstExpr (Task a))
            -> Typed TaskConstExpr (Task a)
        )
      , functionConsDyn "And" "-&&-"
        (  dynamic \(Typed task1) (Typed task2) -> Typed (And task1 task2) ::
          A.a b:
            (Typed TaskConstExpr (Task a)) (Typed TaskConstExpr (Task b))
            -> Typed TaskConstExpr (Task (a,b))
        )
      , functionConsDyn "When" "when"
        (  dynamic \(Typed task1) (Typed steps) -> Typed (When task1 [(expr,pred,tfExpr)\\ (Typed expr,pred,Typed tfExpr) <- steps]) ::
          A.a b:
            (Typed TaskConstExpr (Task a)) (Typed [(Typed FunExpr (a -> Bool) , String, Typed TaskFuncExpr (a -> Task a))] (a -> Task b))
            -> Typed TaskConstExpr (Task b)
        )   <<@@@ applyHorizontalClasses
      , listConsDyn "[(FunExpr, String, TaskFuncExpr)]" "[(FunExpr, String, TaskFuncExpr)]"
        (  dynamic \typedSteps -> Typed ((\(Typed expr) -> expr) <$> typedSteps) ::
          A.a b:
            [Typed (FunExpr, String, TaskFuncExpr) (a -> Task b)] -> Typed [(FunExpr, String, TaskFuncExpr)] (a -> Task b)
        )
        <<@@@ HideIfOnlyChoice
      , functionConsDyn "(FunExpr, String, TaskFuncExpr)" "(FunExpr, String, TaskFuncExpr)"
        (  dynamic \(Typed funExpr) s (Typed taskFunc) -> Typed (funExpr, s, taskFunc) ::
          A.a b:
            (Typed FunExpr a) String (Typed TaskFuncExpr (a -> Task b))
            -> Typed (FunExpr, String, TaskFuncExpr) (a -> Task b)
        )
        <<@@@ HideIfOnlyChoice
      ]
    , DynamicConsGroup "Editors"
      [ functionConsDyn "Apply" "apply"
        (  dynamic \(Typed taskFunc) (Typed expr) -> Typed (Apply taskFunc expr) ::
          A.a b: (Typed TaskFuncExpr (a -> Task b)) (Typed Expr a) -> Typed TaskConstExpr (Task b)
        )
      , functionConsDyn "EnterInformation" "enter information"
        (  dynamic \s (Typed type) -> Typed (EnterInformation s type) ::
          A.a: String (Typed Type a) -> Typed TaskConstExpr (Task a)
        ) <<@@@ applyHorizontalClasses
      , functionConsDyn "ViewInformation" "view information"
        (  dynamic \s -> Typed (ViewInformation s) ::
          A.a: String -> Typed TaskFuncExpr (a -> Task a)
        )  <<@@@ applyHorizontalClasses
      , functionConsDyn "UpdateInformation" "update information"
        (  dynamic \s -> Typed (UpdateInformation s) ::
          A.a: String -> Typed TaskFuncExpr (a -> Task a)
        )  <<@@@ applyHorizontalClasses
      , functionConsDyn "Return" "return"
        (  dynamic Typed Return ::
          A.a: Typed TaskFuncExpr (a -> Task a)
        )
      ]

    // ordinary (non-task) expressions

    , DynamicCons $ functionConsDyn "EqV"   "equal"
      (dynamic \i -> Typed (EqV (VInt i))  :: Int  -> Typed FunExpr Int)
      <<@@@ applyHorizontalClasses
    , DynamicCons $ functionConsDyn "GrtV"   "greater"
      (dynamic \i -> Typed (GrtV (VInt i))  :: Int  -> Typed FunExpr Int)
      <<@@@ applyHorizontalClasses
    , DynamicCons $ functionConsDyn "LessV"   "less"
      (dynamic \i -> Typed (LessV (VInt i))  :: Int  -> Typed FunExpr Int)
      <<@@@ applyHorizontalClasses
    , DynamicCons $ functionConsDyn "int"   "enter an integer:"
      (dynamic \i -> Typed (Int i)  :: Int  -> Typed Expr Int)
    , DynamicCons $ functionConsDyn "bool"  "enter a boolean:"
      (dynamic \b -> Typed (Bool b) :: Bool -> Typed Expr Bool)
    , DynamicCons $ functionConsDyn "string"  "enter a string:"
      (dynamic \s -> Typed (String s) :: String -> Typed Expr String)
    , DynamicCons $ functionConsDyn "tuple" "enter tuple:"
      (  dynamic \(Typed a) (Typed b) -> Typed (Tuple a b) ::
        A.a b: (Typed Expr a) (Typed Expr b) -> Typed Expr (a, b)
      )
    , DynamicCons $ functionConsDyn "fst"   "fst"
      (dynamic \(Typed (Tuple a _)) -> Typed a :: A.a b: (Typed Expr (a, b)) -> Typed Expr a)
      <<@@@ applyHorizontalClasses
    , DynamicCons $ functionConsDyn "snd"   "snd"
      (dynamic \(Typed (Tuple _ b)) -> Typed b :: A.a b: (Typed Expr (a, b)) -> Typed Expr b)
      <<@@@ applyHorizontalClasses
    , DynamicCons $ functionConsDyn "=="    "=="
      (  dynamic \(Typed a) (Typed b) -> Typed (Eq a b) ::
        A.a: (Typed Expr a) (Typed Expr a) -> Typed Expr Bool
      )
      <<@@@ applyHorizontalClasses
    , DynamicCons $ customEditorCons "Int"   "(enter integer)" intEditor  <<@@@ HideIfOnlyChoice
    , DynamicCons $ customEditorCons "Bool"  "(enter boolean)" boolEditor <<@@@ HideIfOnlyChoice
    , DynamicCons $ customEditorCons "String""(enter string )" stringEditor <<@@@ HideIfOnlyChoice

    // type specifications for enterInformation

    , DynamicCons $ functionConsDyn "Type.Int"     "Int"     (dynamic Typed (Type VInt)  :: Typed Type Int)
    , DynamicCons $ functionConsDyn "Type.Bool"    "Bool"    (dynamic Typed (Type VBool) :: Typed Type Bool)
    , DynamicCons $ functionConsDyn "Type.String"    "String"    (dynamic Typed (Type VString) :: Typed Type String)
    , DynamicCons $ functionConsDyn "Type.Tuple"   "Tuple"
      (  dynamic
          \(Typed (Type toValue1)) (Typed (Type toValue2)) ->
            Typed (Type \(x, y) -> VTuple (toValue1 x) (toValue2 y))
        ::
          A.a b: (Typed Type a) (Typed Type b) -> Typed Type (a, b)
      ) <<@@@ applyHorizontalClasses
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

  stringEditor :: Editor String
  stringEditor = gEditor{|*|}


applyHorizontalClasses = ApplyCssClasses ["itasks-horizontal", "itasks-wrap-width", "itasks-panel"]

evalTaskConstExpr :: TaskConstExpr -> Task Value
evalTaskConstExpr (EnterInformation prompt (Type toValue)) = enterInformation prompt [] @ toValue
evalTaskConstExpr (Apply taskFunc expr)             =  evalTaskFuncExpr taskFunc $ evalExpr expr
evalTaskConstExpr (Bind task taskFunc)              =   evalTaskConstExpr task
                          >>= evalTaskFuncExpr taskFunc
evalTaskConstExpr (Blind task1 task2)               =   evalTaskConstExpr task1
                          >>| evalTaskConstExpr task2
evalTaskConstExpr (Or task1 task2)                 =   evalTaskConstExpr task1
                          -||-
                            evalTaskConstExpr task2
evalTaskConstExpr (And task1 task2)               =   evalTaskConstExpr task1
                          -&&-
                            evalTaskConstExpr task2 @ \(a,b) -> VTuple a b
evalTaskConstExpr (When task1 options)        =   evalTaskConstExpr task1
                          >>* [  OnAction (Action butName) (ifValue (test pred)(evalTaskFuncExpr taskFunc))
                            \\ (pred, butName, taskFunc) <- options
                            ]
where
  test pred (VInt i) = case pred of
            (LessV (VInt j))  = i<j
            (GrtV  (VInt j))  = i>j
            (EqV   (VInt j))  = i==j
  test pred (VBool i) = case pred of
            (EqV   (VBool j))  = i==j
            (LessV (VBool j))  = False
            (GrtV  (VBool j))  = False

evalTaskFuncExpr :: TaskFuncExpr Value -> Task Value
evalTaskFuncExpr (ViewInformation p) (VInt i)    = (viewInformation p [] i @ VInt)  <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (ViewInformation p) (VBool b)    = (viewInformation p [] b @ VBool)  <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (ViewInformation p) (VString s)  = (viewInformation p [] s @ VString) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (ViewInformation p) (VTuple a b)  = (viewInformation p [] () ||-
                             evalTaskFuncExpr (ViewInformation "") a
                             -&&-
                             evalTaskFuncExpr (ViewInformation "") b @ \(a,b) -> VTuple a b) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (UpdateInformation p) (VInt i)    = (updateInformation p [] i @ VInt) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (UpdateInformation p) (VBool b)  = (updateInformation p [] b @ VBool) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (UpdateInformation p) (VString s)  = (updateInformation p [] s @ VString) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (UpdateInformation p) (VTuple a b)  = (viewInformation p [] () ||-
                             evalTaskFuncExpr (UpdateInformation "") a
                             -&&-
                             evalTaskFuncExpr (UpdateInformation "" )b @ \(a,b) -> VTuple a b) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr Return value             = return value


evalExpr :: Expr -> Value
evalExpr (Int i)                 = VInt i
evalExpr (Bool b)                = VBool b
evalExpr (String s)              = VString s
evalExpr (Tuple fstExpr sndExpr) = VTuple (evalExpr fstExpr) (evalExpr sndExpr)
evalExpr (Fst expr)              = fst
where
  (VTuple fst _) = evalExpr expr
evalExpr (Snd expr)              = snd
where
  (VTuple _ snd) = evalExpr expr
evalExpr (Eq expr1 expr2)        = VBool $ evalExpr expr1 === evalExpr expr2
