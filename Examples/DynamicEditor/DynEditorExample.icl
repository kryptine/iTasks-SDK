module DynEditorExample

import StdEnv
import Data.Func
import Data.Functor
import iTasks
import iTasks.Extensions.Editors.DynamicEditor


// Helpers /////////////////////////////////////////////////////////////////////

:: List a :== [a]
:: Message :== String
:: Button :== String

always x :== const True x

(>?>) infixl 1 :: (Task a) (List ( Button, a -> Bool, a -> Task b )) -> Task b | iTask a & iTask b
(>?>) task options = task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)


// Main ////////////////////////////////////////////////////////////////////////

Start world = doTasks (editTaskExpr Nothing) world

editTaskExpr :: (Maybe (DynamicEditorValue TaskExpr)) -> Task (Maybe (DynamicEditorValue TaskExpr))
editTaskExpr mv =
  enterOrUpdateExpr ("Contruct a task", info1) mv >?>
    [ ( "Run", always, \v -> viewInformation ("Evaluate the task", info2) [] () ||- (evalTaskExpr (toValue taskEditor v) <<@ ApplyLayout frameCompact) >?>
        [ ( "Finish", always, \r -> viewInformation ("Done!", info3) [] r >?>
            [ ( "Back", always, \_ -> editTaskExpr (Just v) ) ]
          )
        ]
      )
    ]
where
  info1 :: String
  info1 = "Select the editors and combinators you'd like to use. When you're ready, push the 'Continue' button below to run your program."
  info2 :: String
  info2 = "Now step through the task you just created to test it."
  info3 :: String
  info3 = "The program is done, the result is given below."

  enterOrUpdateExpr msg Nothing = enterInformation msg [EnterUsing id $ dynamicEditor taskEditor]
  enterOrUpdateExpr msg (Just v) = updateInformation msg [UpdateUsing id (curry fst) (dynamicEditor taskEditor)] v


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

:: Func
  = IdentityF
  | ConjF Expr
  | DisjF Expr
  | NotF
  | GtF Expr
  | GeF Expr
  | EqF Expr
  | LeF Expr
  | LtF Expr
  | AddF Expr
  | SubF Expr
  | MulF Expr
  | DivF Expr

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
              String
              (Typed Ty a)
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
      [ functionConsDyn "IdentityF" "this value"
          (dynamic Typed IdentityF ::
            A.a:
            Typed Func (a -> a)
          )
      ]
  , DynamicConsGroup "Arithmetic"
      [ functionConsDyn "AddF" "add"
          (dynamic \(Typed i) -> Typed (AddF i) :: (Typed Expr Int) -> Typed Func (Int -> Int)) //XXX (Typed Expr Int) -> Typed Func (Int -> Int)
          <<@@@ applyHorizontalClasses
      , functionConsDyn "SubF" "sub"
          (dynamic \(Typed i) -> Typed (SubF i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalClasses
      , functionConsDyn "MulF" "mul"
          (dynamic \(Typed i) -> Typed (MulF i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalClasses
      , functionConsDyn "DivF" "div"
          (dynamic \(Typed i) -> Typed (DivF i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalClasses
      ]
  , DynamicConsGroup "Logic"
      [ functionConsDyn "ConjF" "and"
          (dynamic \(Typed b) -> Typed (ConjF b) :: (Typed Expr Bool) -> Typed Func (Bool -> Bool))
          <<@@@ applyHorizontalClasses
      , functionConsDyn "DisjF" "or"
          (dynamic \(Typed b) -> Typed (DisjF b) :: (Typed Expr Bool) -> Typed Func (Bool -> Bool))
          <<@@@ applyHorizontalClasses
      , functionConsDyn "NotF" "not"
          (dynamic Typed (NotF) :: Typed Func (Bool -> Bool))
          <<@@@ applyHorizontalClasses
      ]
  , DynamicConsGroup "Comparison"
      [ functionConsDyn "GtF" "greater than"
          (dynamic \(Typed i) -> Typed (GtF i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalClasses
      , functionConsDyn "GeF" "greater or equal"
          (dynamic \(Typed i) -> Typed (GeF i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalClasses
      , functionConsDyn "EqF" "equal to"
          (dynamic \(Typed i) -> Typed (EqF i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalClasses
      , functionConsDyn "LeF" "lesser than"
          (dynamic \(Typed i) -> Typed (LeF i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalClasses
      , functionConsDyn "LtF" "lesser than"
          (dynamic \(Typed i) -> Typed (LtF i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
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
      ||- evalTaskFunc (ViewF "" IdentityF) a -&&- evalTaskFunc (ViewF "" IdentityF) b
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


evalFunc :: Value Func -> Value

evalFunc val IdentityF = val

evalFunc (VInt i1) func = case func of
  (GtF expr) -> VBool $ i1 > evalInt expr
  (GeF expr) -> VBool $ i1 >= evalInt expr
  (EqF expr) -> VBool $ i1 == evalInt expr
  (LeF expr) -> VBool $ i1 <= evalInt expr
  (LtF expr) -> VBool $ i1 < evalInt expr
  (AddF expr) -> VInt $ i1 + evalInt expr
  (SubF expr) -> VInt $ i1 - evalInt expr
  (MulF expr) -> VInt $ i1 * evalInt expr
  (DivF expr) -> VInt $ i1 / evalInt expr
where
  evalInt :: Expr -> Int
  evalInt expr = case evalExpr expr of
    (VInt i) -> i

evalFunc (VBool b1) func = case func of
  (EqF expr) -> VBool $ b1 == evalBool expr
  (ConjF expr) -> VBool $ b1 && evalBool expr
  (DisjF expr) -> VBool $ b1 || evalBool expr
  (NotF) -> VBool $ not b1
where
  evalBool :: Expr -> Bool
  evalBool expr = case evalExpr expr of
    (VBool b) -> b

evalFunc (VString s1) func = case func of
  (EqF expr) -> VBool $ s1 == evalString expr
where
  evalString :: Expr -> String
  evalString expr = case evalExpr expr of
    (VString s) -> s
