module DynEditorExample

import StdEnv
import Data.Func
import Data.Functor
import Text
import iTasks
import iTasks.Extensions.Editors.DynamicEditor


// Synonyms ////////////////////////////////////////////////////////////////////

:: List a :== [a]


// Main ////////////////////////////////////////////////////////////////////////

Start world = doTasks editTask world

editTask =
  forever
    ( enterInformation ("Contruct a task", info1) [EnterUsing id $ dynamicEditor taskEditor]
      >>= \v ->
          viewInformation ("Evaluate the task", info2) [] ()
            ||- (evalTaskConstExpr (toValue taskEditor v) <<@ ApplyLayout frameCompact)
            >>= viewInformation ("Done!", info3) []
            >>= return
    )
where
  info1 :: String
  info1 = "Select the editors and combinators you'd like to use. When you're ready, push the 'Continue' button below to run your program."
  info2 :: String
  info2 = "Now step through the task you just created to test it."
  info3 :: String
  info3 = "The program is done, the result is given below."


// Data ////////////////////////////////////////////////////////////////////////

:: TaskExpr
  = Apply TaskFuncExpr Expr
  | EnterInfo String Ty
  | Then TaskExpr TaskFuncExpr
  | Or TaskExpr TaskExpr
  | And TaskExpr TaskExpr
  | When TaskExpr (List TaskContExpr)

:: TaskFuncExpr
  = ViewInfo String
  | UpdateInfo String

:: TaskContExpr
  = { name :: String, pred :: FuncExpr, cont :: TaskFuncExpr}

:: Expr
  = Int Int
  | Bool Bool
  | String String
  | Tuple Expr Expr
  | Fst Expr
  | Snd Expr
  | Eq Expr Expr

:: Value
  = VInt Int
  | VBool Bool
  | VString String
  | VTuple Value Value

:: FuncExpr
  = EqV Value
  | GrtV Value
  | LessV Value

:: Ty
  = E.a: Ty (a -> Value) & iTask a

:: Typed a b
  =: Typed a

derive class iTask TaskExpr, TaskFuncExpr, TaskContExpr, Expr, FuncExpr, Value, Typed

// These instances cannot be auto derived because of the existential quantifier.
// However, they will be never used, so we make them undefined.
gDefault{|Ty|} = undef
gEq{|Ty|} _ _ = undef
JSONEncode{|Ty|} _ _ = undef
JSONDecode{|Ty|} _ _ = undef
gText{|Ty|} _ _ = undef
gEditor{|Ty|} = undef


// Editor //////////////////////////////////////////////////////////////////////

taskEditor :: DynamicEditor TaskExpr
taskEditor = DynamicEditor conses
where
  conses =
    [ // This cons is used to provide untyped `TaskExpr` values.
      DynamicCons
        $ functionConsDyn "TaskExpr" "(enter task)" (dynamic \(Typed taskExpr) -> taskExpr ::  A.a: (Typed TaskExpr a) -> TaskExpr)
        <<@@@ HideIfOnlyChoice
    , DynamicConsGroup "Combinators"
        [ functionConsDyn "Then" "sequence"
            ( dynamic \(Typed task) (Typed taskFunc) ->
              Typed (Then task taskFunc) ::
                A.a b:
                (Typed TaskExpr (Task a)) (Typed TaskFuncExpr (a -> Task b))
                -> Typed TaskExpr (Task b)
            )
        , functionConsDyn "When" "guarded sequence"
            ( dynamic \(Typed task1) (Typed steps) ->
              Typed (When task1 steps) ::
              // Typed (When task1 [(expr, pred, tfExpr) \\ (Typed expr, pred, Typed tfExpr) <- steps]) ::
                A.a b:
                (Typed TaskExpr (Task a))
                (Typed (List TaskContExpr) (a -> Task b))
                // (Typed (List (Typed FuncExpr (a -> Bool), String, Typed TaskFuncExpr (a -> Task a))) (a -> Task b))
                -> Typed TaskExpr (Task b)
            )
            <<@@@ applyHorizontalClasses
        , functionConsDyn "Or" "or"
            ( dynamic \(Typed task1) (Typed task2) ->
              Typed (Or task1 task2) ::
                A.a b:
                (Typed TaskExpr (Task a))
                (Typed TaskExpr (Task a))
                -> Typed TaskExpr (Task a)
            )
            <<@@@ applyHorizontalClasses
        , functionConsDyn "And" "and"
            ( dynamic \(Typed task1) (Typed task2) ->
              Typed (And task1 task2) ::
                A.a b:
                (Typed TaskExpr (Task a))
                (Typed TaskExpr (Task b))
                -> Typed TaskExpr (Task (a, b))
            )
            <<@@@ applyHorizontalClasses
        , listConsDyn "List TaskContExpr" "continuations"
            ( dynamic \typedSteps ->
              Typed ((\(Typed expr) -> expr) <$> typedSteps) ::
                A.a b:
                (List (Typed TaskContExpr (a -> Task b)))
                -> Typed (List TaskContExpr) (a -> Task b)
            )
            <<@@@ HideIfOnlyChoice
        , functionConsDyn "TaskContExpr" "continuation"
            ( dynamic \s (Typed func) (Typed taskFunc) ->
              Typed {name = s, pred = func, cont = taskFunc} ::
                A.a b:
                String
                (Typed FuncExpr a)
                (Typed TaskFuncExpr (a -> Task b))
                -> Typed TaskContExpr (a -> Task b)
            )
            <<@@@ HideIfOnlyChoice
            <<@@@ AddLabels [Just "name", Just "predicate", Just "continuation"]
        ]
    , DynamicConsGroup "Editors"
        [ functionConsDyn "Apply" "apply"
            ( dynamic \(Typed taskFunc) (Typed expr) ->
              Typed (Apply taskFunc expr) ::
                A.a b:
                (Typed TaskFuncExpr (a -> Task b))
                (Typed Expr a)
                -> Typed TaskExpr (Task b)
            )
        , functionConsDyn "EnterInfo" "enter information"
            ( dynamic \s (Typed ty) ->
              Typed (EnterInfo s ty) ::
                A.a:
                String (Typed Ty a)
                -> Typed TaskExpr (Task a)
            )
            <<@@@ applyHorizontalClasses
        , functionConsDyn "ViewInfo" "view information"
            ( dynamic \s ->
              Typed (ViewInfo s) ::
                A.a:
                String
                -> Typed TaskFuncExpr (a -> Task a)
            )
            <<@@@ applyHorizontalClasses
        , functionConsDyn "UpdateInfo" "update information"
            ( dynamic \s ->
              Typed (UpdateInfo s) ::
                A.a:
                String
                -> Typed TaskFuncExpr (a -> Task a)
            )
            <<@@@ applyHorizontalClasses
        ]
    // ordinary (non-task) expressions
    , DynamicCons
        $ functionConsDyn "EqV" "equal" (dynamic \i -> Typed (EqV (VInt i)) :: Int -> Typed FuncExpr Int)
        <<@@@ applyHorizontalClasses
    , DynamicCons
        $ functionConsDyn "GrtV" "greater" (dynamic \i -> Typed (GrtV (VInt i)) :: Int -> Typed FuncExpr Int)
        <<@@@ applyHorizontalClasses
    , DynamicCons
        $ functionConsDyn "LessV" "less" (dynamic \i -> Typed (LessV (VInt i)) :: Int -> Typed FuncExpr Int)
        <<@@@ applyHorizontalClasses
    , DynamicCons
        $ functionConsDyn "int" "enter an integer:" (dynamic \i -> Typed (Int i) :: Int -> Typed Expr Int)
    , DynamicCons
        $ functionConsDyn "bool" "enter a boolean:" (dynamic \b -> Typed (Bool b) :: Bool -> Typed Expr Bool)
    , DynamicCons
        $ functionConsDyn "string" "enter a string:" (dynamic \s -> Typed (String s) :: String -> Typed Expr String)
    , DynamicCons
        $ functionConsDyn "tuple" "enter tuple:"
            ( dynamic \(Typed a) (Typed b) ->
              Typed (Tuple a b) ::
                A.a b:
                  (Typed Expr a) (Typed Expr b) -> Typed Expr (a, b)
            )
    , DynamicCons
        $ functionConsDyn "fst" "fst" (dynamic \(Typed (Tuple a _)) -> Typed a ::  A.a b: (Typed Expr (a, b)) -> Typed Expr a)
        <<@@@ applyHorizontalClasses
    , DynamicCons
        $ functionConsDyn "snd" "snd" (dynamic \(Typed (Tuple _ b)) -> Typed b ::  A.a b: (Typed Expr (a, b)) -> Typed Expr b)
        <<@@@ applyHorizontalClasses
    , DynamicCons
        $ functionConsDyn "==" "=="
            ( dynamic \(Typed a) (Typed b) ->
              Typed (Eq a b) ::
                A.a:
                  (Typed Expr a) (Typed Expr a) -> Typed Expr Bool
            )
        <<@@@ applyHorizontalClasses
    , DynamicCons $ customEditorCons "Int" "(enter integer)" intEditor <<@@@ HideIfOnlyChoice
    , DynamicCons $ customEditorCons "Bool" "(enter boolean)" boolEditor <<@@@ HideIfOnlyChoice
    , DynamicCons $ customEditorCons "String" "(enter string )" stringEditor <<@@@ HideIfOnlyChoice
    // type specifications for enterInformation
    , DynamicCons $ functionConsDyn "Ty.Int" "Int" (dynamic Typed (Ty VInt) :: Typed Ty Int)
    , DynamicCons $ functionConsDyn "Ty.Bool" "Bool" (dynamic Typed (Ty VBool) :: Typed Ty Bool)
    , DynamicCons $ functionConsDyn "Ty.String" "String" (dynamic Typed (Ty VString) :: Typed Ty String)
    , DynamicCons
        $ functionConsDyn "Ty.Tuple" "Tuple"
            ( dynamic \(Typed (Ty toValue1)) (Typed (Ty toValue2)) ->
              Typed (Ty \(x, y) -> VTuple (toValue1 x) (toValue2 y)) ::
                A.a b:
                  (Typed Ty a) (Typed Ty b) -> Typed Ty (a, b)
            )
        <<@@@ applyHorizontalClasses
    ]


// Helpers //

derivedType :: Typed Ty a | iTask a
derivedType = case dynToValue of
  (toValue :: a^ -> Value | iTask a^) = Typed (Ty toValue)
where
  dynToValue = dynamic ()


intEditor :: Editor Int
intEditor = gEditor{|*|}

boolEditor :: Editor Bool
boolEditor = gEditor{|*|}

stringEditor :: Editor String
stringEditor = gEditor{|*|}

applyHorizontalClasses = ApplyCssClasses ["itasks-horizontal", "itasks-wrap-width", "itasks-panel"]


// Evaluation //////////////////////////////////////////////////////////////////

evalTaskConstExpr :: TaskExpr -> Task Value
evalTaskConstExpr (EnterInfo msg (Ty toValue)) = enterInformation msg [] @ toValue
evalTaskConstExpr (Apply taskFunc expr) = evalTaskFuncExpr taskFunc $ evalExpr expr
evalTaskConstExpr (Then task taskFunc) = evalTaskConstExpr task >>= evalTaskFuncExpr taskFunc
evalTaskConstExpr (Or task1 task2) = (evalTaskConstExpr task1 -||- evalTaskConstExpr task2 <<@ ApplyLayout arrangeHorizontal)
evalTaskConstExpr (And task1 task2) = (evalTaskConstExpr task1 -&&- evalTaskConstExpr task2 <<@ ApplyLayout arrangeHorizontal) @ \(a, b) -> VTuple a b
evalTaskConstExpr (When task1 options) = evalTaskConstExpr task1
  >>* [ OnAction (Action name) (ifValue (test pred) (evalTaskFuncExpr cont))
      \\ {name, pred, cont} <- options
      ]
where
  test pred (VInt i) = case pred of
    LessV (VInt j) -> i < j
    GrtV (VInt j) -> i > j
    EqV (VInt j) -> i == j

  test pred (VBool i) = case pred of
    EqV (VBool j) -> i == j
    LessV (VBool j) -> False
    GrtV (VBool j) -> False


evalTaskFuncExpr :: TaskFuncExpr Value -> Task Value
evalTaskFuncExpr (ViewInfo p) (VInt i) = (viewInformation p [] i @ VInt) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (ViewInfo p) (VBool b) = (viewInformation p [] b @ VBool) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (ViewInfo p) (VString s) = (viewInformation p [] s @ VString) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (ViewInfo p) (VTuple a b) =
  ( viewInformation p [] ()
    ||- evalTaskFuncExpr (ViewInfo "") a
    -&&- evalTaskFuncExpr (ViewInfo "") b
    @ \(a, b) -> VTuple a b
  )
    <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (UpdateInfo p) (VInt i) = (updateInformation p [] i @ VInt) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (UpdateInfo p) (VBool b) = (updateInformation p [] b @ VBool) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (UpdateInfo p) (VString s) = (updateInformation p [] s @ VString) <<@ ApplyLayout arrangeHorizontal
evalTaskFuncExpr (UpdateInfo p) (VTuple a b) =
  ( viewInformation p [] ()
    ||- evalTaskFuncExpr (UpdateInfo "") a
    -&&- evalTaskFuncExpr (UpdateInfo "") b
    @ \(a, b) -> VTuple a b
  )
    <<@ ApplyLayout arrangeHorizontal


evalExpr :: Expr -> Value
evalExpr (Int i) = VInt i
evalExpr (Bool b) = VBool b
evalExpr (String s) = VString s
evalExpr (Tuple fstExpr sndExpr) = VTuple (evalExpr fstExpr) (evalExpr sndExpr)
evalExpr (Fst expr) = fst
where
  (VTuple fst _) = evalExpr expr
evalExpr (Snd expr) = snd
where
  (VTuple _ snd) = evalExpr expr
evalExpr (Eq expr1 expr2) = VBool $ evalExpr expr1 === evalExpr expr2
