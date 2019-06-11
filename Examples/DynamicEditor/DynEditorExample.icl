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

editTask :: Task Value
editTask = forever $
  enterInformation ("Contruct a task", info1) [EnterUsing id $ dynamicEditor taskEditor]
    >>= \v ->
        viewInformation ("Evaluate the task", info2) [] ()
          ||- (evalTaskExpr (toValue taskEditor v) <<@ ApplyLayout frameCompact)
          >>= viewInformation ("Done!", info3) []
          >>= return  // Extra return to disable `Continue` button
where
  info1 :: String
  info1 = "Select the editors and combinators you'd like to use. When you're ready, push the 'Continue' button below to run your program."
  info2 :: String
  info2 = "Now step through the task you just created to test it."
  info3 :: String
  info3 = "The program is done, the result is given below."


// Data ////////////////////////////////////////////////////////////////////////

:: TaskExpr
  = Done Expr
  | EnterInfo String Ty
  | Then TaskExpr TaskFunc
  | Both TaskExpr TaskExpr
  | Any TaskExpr TaskExpr
  | One TaskExpr TaskExpr
  // | Apply TaskFunc Expr

:: TaskFunc
  = ViewF String Func
  | UpdateF String
  | ThenF TaskFunc TaskFunc

:: Expr
  = Int Int
  | Bool Bool
  | String String
  | Tuple Expr Expr
  | Fst Expr
  | Snd Expr
  | Eq Expr Expr

:: Func
  = Identity
  | And Value
  | Or Value
  | GtF Value
  | GeF Value
  | EqF Value
  | LeF Value
  | LtF Value
  | Add Value
  | Sub Value
  | Mul Value
  | Div Value

:: Value
  = VInt Int
  | VBool Bool
  | VString String
  | VTuple Value Value

:: Ty
  = E.a: Ty (a -> Value) & iTask a

:: Typed a b
  =: Typed a

derive class iTask TaskExpr, TaskFunc, Expr, Func, Value, Typed

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
taskEditor = DynamicEditor
  [ // This cons is used to provide untyped `TaskExpr` values.
    DynamicCons
      $ functionConsDyn "TaskExpr" "(enter task)" (dynamic \(Typed taskExpr) -> taskExpr ::  A.a: (Typed TaskExpr a) -> TaskExpr)
      <<@@@ HideIfOnlyChoice
  , DynamicConsGroup "Combinators"
      [ functionConsDyn "Then" "sequence"
          ( dynamic \(Typed task) (Typed taskFunc) -> Typed (Then task taskFunc) ::
              A.a b:
              (Typed TaskExpr (Task a)) (Typed TaskFunc (a -> Task b))
              -> Typed TaskExpr (Task b)
          )
      , functionConsDyn "ThenF" "sequence"
          ( dynamic \(Typed taskFunc1) (Typed taskFunc2) -> Typed (ThenF taskFunc1 taskFunc2) ::
              A.a b c:
              (Typed TaskFunc (a -> Task b)) (Typed TaskFunc (b -> Task c))
              -> Typed TaskFunc (a -> Task c)
          )
      , functionConsDyn "Both" "both"
          ( dynamic \(Typed task1) (Typed task2) -> Typed (Both task1 task2) ::
              A.a b:
              (Typed TaskExpr (Task a))
              (Typed TaskExpr (Task b))
              -> Typed TaskExpr (Task (a, b))
          )
          <<@@@ applyHorizontalClasses
      , functionConsDyn "Any" "any of"
          ( dynamic \(Typed task1) (Typed task2) -> Typed (Any task1 task2) ::
              A.a b:
              (Typed TaskExpr (Task a))
              (Typed TaskExpr (Task a))
              -> Typed TaskExpr (Task a)
          )
          <<@@@ applyHorizontalClasses
      , functionConsDyn "One" "one of"
          ( dynamic \(Typed task1) (Typed task2) -> Typed (One task1 task2) ::
              A.a b:
              (Typed TaskExpr (Task a))
              (Typed TaskExpr (Task a))
              -> Typed TaskExpr (Task a)
          )
          <<@@@ applyHorizontalClasses
      // , functionConsDyn "When" "guarded sequence"
      //     ( dynamic \(Typed task1) (Typed steps) -> Typed (When task1 steps) ::
      //       // Typed (When task1 [(expr, pred, tfExpr) \\ (Typed expr, pred, Typed tfExpr) <- steps]) ::
      //         A.a b:
      //         (Typed TaskExpr (Task a))
      //         (Typed (List TaskContExpr) (a -> Task b))
      //         // (Typed (List (Typed Func (a -> Bool), String, Typed TaskFunc (a -> Task a))) (a -> Task b))
      //         -> Typed TaskExpr (Task b)
      //     )
      //     <<@@@ applyHorizontalClasses
      // , listConsDyn "List TaskContExpr" "continuations"
      //     ( dynamic \typedSteps -> Typed ((\(Typed expr) -> expr) <$> typedSteps) ::
      //         A.a b:
      //         (List (Typed TaskContExpr (a -> Task b)))
      //         -> Typed (List TaskContExpr) (a -> Task b)
      //     )
      //     <<@@@ HideIfOnlyChoice
      // , functionConsDyn "TaskContExpr" "continuation"
      //     ( dynamic \s (Typed func) (Typed taskFunc) -> Typed {name = s, pred = func, cont = taskFunc} ::
      //         A.a b:
      //         String
      //         (Typed Func a)
      //         (Typed TaskFunc (a -> Task b))
      //         -> Typed TaskContExpr (a -> Task b)
      //     )
      //     <<@@@ HideIfOnlyChoice
      //     <<@@@ AddLabels [Just "name", Just "predicate", Just "continuation"]
      ]
  , DynamicConsGroup "Editors"
      [ functionConsDyn "Enter" "enter"
          ( dynamic \s (Typed ty) -> Typed (EnterInfo s ty) ::
              A.a:
              String (Typed Ty a)
              -> Typed TaskExpr (Task a)
          )
          <<@@@ applyHorizontalClasses
      , functionConsDyn "ViewF" "view"
          ( dynamic \s (Typed func) -> Typed (ViewF s func) ::
              A.a b:
              String
              (Typed Func (a -> b))
              -> Typed TaskFunc (a -> Task b)
          )
          <<@@@ applyHorizontalClasses
      , functionConsDyn "UpdateF" "update"
          ( dynamic \s -> Typed (UpdateF s) ::
              A.a:
              String
              -> Typed TaskFunc (a -> Task a)
          )
          <<@@@ applyHorizontalClasses
      ]
    , DynamicConsGroup "Special"
      [ functionConsDyn "Done" "done"
          ( dynamic \(Typed expr) -> Typed (Done expr) ::
              A.a:
              (Typed Expr a)
              -> Typed TaskExpr (Task a)
          )
      // , functionConsDyn "Apply" "apply"
      //     ( dynamic \(Typed taskFunc) (Typed expr) ->
      //       Typed (Apply taskFunc expr) ::
      //         A.a b:
      //         (Typed TaskFunc (a -> Task b))
      //         (Typed Expr a)
      //         -> Typed TaskExpr (Task b)
      //     )
      ]
  // Non-task functions:
  , DynamicConsGroup "Basics"
      [ functionConsDyn "Identity" "this value"
          (dynamic Typed Identity ::
            A.a:
            Typed Func (a -> a)
          )
      ]
  , DynamicConsGroup "Comparison"
      [ functionConsDyn "GtF" "greater than"
          (dynamic \i -> Typed (GtF (VInt i)) :: Int -> Typed Func Int)
          <<@@@ applyHorizontalClasses
      , functionConsDyn "GeF" "greater or equal"
          (dynamic \i -> Typed (GeF (VInt i)) :: Int -> Typed Func Int)
          <<@@@ applyHorizontalClasses
      , functionConsDyn "EqF" "equal to"
          (dynamic \i -> Typed (EqF (VInt i)) :: Int -> Typed Func Int)
          <<@@@ applyHorizontalClasses
      , functionConsDyn "LeF" "lesser than"
          (dynamic \i -> Typed (LeF (VInt i)) :: Int -> Typed Func Int)
          <<@@@ applyHorizontalClasses
      , functionConsDyn "LtF" "lesser than"
          (dynamic \i -> Typed (LtF (VInt i)) :: Int -> Typed Func Int)
          <<@@@ applyHorizontalClasses
      ]
  // Non-task expressions:
  , DynamicConsGroup "Values"
      [ functionConsDyn "Int" "the integer"
          (dynamic \i -> Typed (Int i) :: Int -> Typed Expr Int)
          <<@@@ applyHorizontalClasses
      , functionConsDyn "Bool" "the boolean"
          (dynamic \b -> Typed (Bool b) :: Bool -> Typed Expr Bool)
          <<@@@ applyHorizontalClasses
      , functionConsDyn "String" "the string"
          (dynamic \s -> Typed (String s) :: String -> Typed Expr String)
          <<@@@ applyHorizontalClasses
      , functionConsDyn "Tuple" "the tuple"
          ( dynamic \(Typed a) (Typed b) ->
            Typed (Tuple a b) ::
              A.a b:
                (Typed Expr a) (Typed Expr b) -> Typed Expr (a, b)
          )
          <<@@@ applyHorizontalClasses
      , functionConsDyn "Fst" "fst" (dynamic \(Typed (Tuple a _)) -> Typed a ::  A.a b: (Typed Expr (a, b)) -> Typed Expr a)
          <<@@@ applyHorizontalClasses
      , functionConsDyn "Snd" "snd" (dynamic \(Typed (Tuple _ b)) -> Typed b ::  A.a b: (Typed Expr (a, b)) -> Typed Expr b)
          <<@@@ applyHorizontalClasses
      , functionConsDyn "Eq" "=="
          ( dynamic \(Typed a) (Typed b) ->
            Typed (Eq a b) ::
              A.a:
                (Typed Expr a) (Typed Expr a) -> Typed Expr Bool
          )
          <<@@@ applyHorizontalClasses
    ]
  // Types
  , DynamicConsGroup "Types"
      [ functionConsDyn "Ty.Int" "Int"
          (dynamic Typed (Ty VInt) :: Typed Ty Int)
      , functionConsDyn "Ty.Bool" "Bool"
          (dynamic Typed (Ty VBool) :: Typed Ty Bool)
      , functionConsDyn "Ty.String" "String"
          (dynamic Typed (Ty VString) :: Typed Ty String)
      , functionConsDyn "Ty.Tuple" "Tuple"
          ( dynamic \(Typed (Ty toValue1)) (Typed (Ty toValue2)) -> Typed (Ty \(x, y) -> VTuple (toValue1 x) (toValue2 y)) ::
              A.a b:
              (Typed Ty a) (Typed Ty b) -> Typed Ty (a, b)
          )
          <<@@@ applyHorizontalClasses
      ]
  // Internal helper editors
  , DynamicConsGroup "Helpers"
    [ customEditorCons "int" "(enter integer)" intEditor <<@@@ HideIfOnlyChoice
    , customEditorCons "bool" "(enter boolean)" boolEditor <<@@@ HideIfOnlyChoice
    , customEditorCons "string" "(enter string )" stringEditor <<@@@ HideIfOnlyChoice
    ]
  ]
where
  intEditor :: Editor Int
  intEditor = gEditor{|*|}

  boolEditor :: Editor Bool
  boolEditor = gEditor{|*|}

  stringEditor :: Editor String
  stringEditor = gEditor{|*|}

  applyHorizontalClasses = ApplyCssClasses ["itasks-horizontal", "itasks-wrap-width", "itasks-panel"]


// Evaluation //////////////////////////////////////////////////////////////////

evalTaskExpr :: TaskExpr -> Task Value
evalTaskExpr (Done expr) = return $ evalExpr expr
evalTaskExpr (EnterInfo msg (Ty toValue)) = enterInformation msg [] @ toValue
evalTaskExpr (Then task taskFunc) = evalTaskExpr task >>= evalTaskFunc taskFunc
evalTaskExpr (Any task1 task2) = (evalTaskExpr task1 -||- evalTaskExpr task2 <<@ ApplyLayout arrangeHorizontal)
evalTaskExpr (Both task1 task2) = (evalTaskExpr task1 -&&- evalTaskExpr task2 <<@ ApplyLayout arrangeHorizontal) @ \(a, b) -> VTuple a b
// evalTaskExpr (Apply taskFunc expr) = evalTaskFunc taskFunc $ evalExpr expr
// evalTaskExpr (When task1 options) = evalTaskExpr task1
//   >>* [ OnAction (Action name) (ifValue (test pred) (evalTaskFunc cont))
//       \\ {name, pred, cont} <- options
//       ]
where
  test pred (VInt i) = case pred of
    LtF (VInt j) -> i < j
    GtF (VInt j) -> i > j
    EqF (VInt j) -> i == j
  test pred (VBool i) = case pred of
    EqF (VBool j) -> i == j
    LtF (VBool j) -> False
    GtF (VBool j) -> False


evalTaskFunc :: TaskFunc Value -> Task Value
evalTaskFunc (ThenF this next) val = evalTaskFunc this val >>= evalTaskFunc next
evalTaskFunc (ViewF msg func) val = case evalFunc val func of
  (VInt i) -> (viewInformation msg [] i @ VInt) <<@ ApplyLayout arrangeHorizontal
  (VBool b) -> (viewInformation msg [] b @ VBool) <<@ ApplyLayout arrangeHorizontal
  (VString s) -> (viewInformation msg [] s @ VString) <<@ ApplyLayout arrangeHorizontal
  (VTuple a b) ->
    ( viewInformation msg [] ()
      ||- evalTaskFunc (ViewF "" Identity) a -&&- evalTaskFunc (ViewF "" Identity) b
      @ \(a, b) -> VTuple a b
    )
      <<@ ApplyLayout arrangeHorizontal
evalTaskFunc (UpdateF msg) val = case val of
  (VInt i) -> (updateInformation msg [] i @ VInt) <<@ ApplyLayout arrangeHorizontal
  (VBool b) -> (updateInformation msg [] b @ VBool) <<@ ApplyLayout arrangeHorizontal
  (VString s) -> (updateInformation msg [] s @ VString) <<@ ApplyLayout arrangeHorizontal
  (VTuple a b) ->
    ( viewInformation msg [] ()
      ||- evalTaskFunc (UpdateF "") a
      -&&- evalTaskFunc (UpdateF "") b
      @ \(a, b) -> VTuple a b
    )
      <<@ ApplyLayout arrangeHorizontal


evalExpr :: Expr -> Value
evalExpr (Int i) = VInt i
evalExpr (Bool b) = VBool b
evalExpr (String s) = VString s
evalExpr (Tuple fstExpr sndExpr) = VTuple (evalExpr fstExpr) (evalExpr sndExpr)
evalExpr (Fst expr) = let (VTuple fst _) = evalExpr expr in fst
evalExpr (Snd expr) = let (VTuple _ snd) = evalExpr expr in snd
evalExpr (Eq expr1 expr2) = evalFunc (evalExpr expr1) (EqF (evalExpr expr2))


evalFunc :: Value Func -> Value
evalFunc val Identity = val
evalFunc (VInt i1) func = case func of
  (GtF (VInt i2)) -> VBool $ i1 > i2
  (GeF (VInt i2)) -> VBool $ i1 >= i2
  (EqF (VInt i2)) -> VBool $ i1 == i2
  (LeF (VInt i2)) -> VBool $ i1 <= i2
  (LtF (VInt i2)) -> VBool $ i1 < i2
evalFunc (VBool b1) func = case func of
  (EqF (VBool b2)) -> VBool $ b1 == b2
evalFunc (VString s1) func = case func of
  (EqF (VString s2)) -> VBool $ s1 == s2
