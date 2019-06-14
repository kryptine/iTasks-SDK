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

cons x xs :== [x:xs]

(>?>) infixl 1 :: (Task a) (List ( Button, a -> Bool, a -> Task b )) -> Task b | iTask a & iTask b
(>?>) task options = task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)


// Main ////////////////////////////////////////////////////////////////////////

Start world = doTasks (editTaskExpr Nothing) world

editTaskExpr :: (Maybe (DynamicEditorValue TaskExpr)) -> Task (Maybe (DynamicEditorValue TaskExpr))
editTaskExpr mv =
  enterOrUpdateExpr ("Contruct a task", info1) mv >?>
    [ ( "Run", const True, \v -> viewInformation ("Evaluate the task", info2) [] () ||- (evalTaskExpr (toValue taskEditor v) <<@ ApplyLayout frameCompact) >>*
        [ OnAction (Action "Back") (always (editTaskExpr (Just v)))
        , OnAction (Action "Finish") (ifValue (const True) (\r -> viewInformation ("Done!", info3) [] r >?>
            [ ( "Back", const True, \_ -> editTaskExpr (Just v) ) ]
          ))
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
  enterOrUpdateExpr msg (Just v) = updateInformation msg [UpdateUsing id (curry snd) (dynamicEditor taskEditor)] v


// Data ////////////////////////////////////////////////////////////////////////

:: TaskExpr
  = Done Expr
  | EnterInfo Ty String
  | Then TaskExpr TaskFunc
  | Both TaskExpr TaskExpr
  | Any TaskExpr TaskExpr
  | One Button TaskExpr Button TaskExpr
  // | Init Ty TaskExpr
  | Watch String
  // | Change String
  | Forever TaskExpr

:: TaskFunc
  = ThenF TaskFunc TaskFunc
  | ViewF String Func
  | UpdateF String Func
  | StoreF
  | WatchF String

:: Expr
  = Int Int
  | Bool Bool
  | String String
  | Tuple Expr Expr
  | Apply Func Expr

:: Func
  = Identity
  | Conj Expr
  | Disj Expr
  | Not
  | Gt Expr
  | Ge Expr
  | Eq Expr
  | Le Expr
  | Lt Expr
  | Add Expr
  | Sub Expr
  | Mul Expr
  | Div Expr
  | Fst
  | Snd

:: Value
  = VUnit
  | VInt Int
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
gDefault{|Ty|} = abort "Typed task editor: internal error with gDefault of Ty"
gEq{|Ty|} _ _ = abort "Typed task editor: internal error with gEq of Ty"
JSONEncode{|Ty|} _ _ = abort "Typed task editor: internal error with JSONEncode of Ty"
JSONDecode{|Ty|} _ _ = abort "Typed task editor: internal error with JSONDecode of Ty"
gText{|Ty|} _ _ = abort "Typed task editor: internal error with gText of Ty"
gEditor{|Ty|} = abort "Typed task editor: internal error with gEditor of Ty"


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
          <<@@@ applyVerticalBoxedLayout
      , functionConsDyn "ThenF" "sequence"
          ( dynamic \(Typed taskFunc1) (Typed taskFunc2) -> Typed (ThenF taskFunc1 taskFunc2) ::
              A.a b c:
              (Typed TaskFunc (a -> Task b)) (Typed TaskFunc (b -> Task c))
              -> Typed TaskFunc (a -> Task c)
          )
          <<@@@ applyVerticalBoxedLayout
      , functionConsDyn "Both" "both"
          ( dynamic \(Typed task1) (Typed task2) -> Typed (Both task1 task2) ::
              A.a b:
              (Typed TaskExpr (Task a))
              (Typed TaskExpr (Task b))
              -> Typed TaskExpr (Task (a, b))
          )
          <<@@@ applyVerticalBoxedLayout
      , functionConsDyn "Any" "any of"
          ( dynamic \(Typed task1) (Typed task2) -> Typed (Any task1 task2) ::
              A.a b:
              (Typed TaskExpr (Task a))
              (Typed TaskExpr (Task a))
              -> Typed TaskExpr (Task a)
          )
          <<@@@ applyVerticalBoxedLayout
      , functionConsDyn "One" "one of"
          ( dynamic \button1 (Typed task1) button2 (Typed task2) -> Typed (One button1 task1 button2 task2) ::
              A.a b:
              String
              (Typed TaskExpr (Task a))
              String
              (Typed TaskExpr (Task a))
              -> Typed TaskExpr (Task a)
          )
          <<@@@ applyVerticalBoxedLayout
      , functionConsDyn "Forever" "forever"
          ( dynamic \(Typed taskExpr) -> Typed (Forever taskExpr) ::
              A.a:
              (Typed TaskExpr (Task a))
              -> Typed TaskExpr (Task a)
          )
          <<@@@ applyVerticalBoxedLayout
      ]
      // , functionConsDyn "When" "guarded sequence"
      //     ( dynamic \(Typed task1) (Typed steps) -> Typed (When task1 steps) ::
      //       // Typed (When task1 [(expr, pred, tfExpr) \\ (Typed expr, pred, Typed tfExpr) <- steps]) ::
      //         A.a b:
      //         (Typed TaskExpr (Task a))
      //         (Typed (List TaskContExpr) (a -> Task b))
      //         // (Typed (List (Typed Func (a -> Bool), String, Typed TaskFunc (a -> Task a))) (a -> Task b))
      //         -> Typed TaskExpr (Task b)
      //     )
      //     <<@@@ applyHorizontalBoxedLayout
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
      // ]
  , DynamicConsGroup "Editors"
      [ functionConsDyn "Enter" "enter"
          ( dynamic \(Typed ty) s -> Typed (EnterInfo ty s) ::
              A.a:
              (Typed Ty a)
              String
              -> Typed TaskExpr (Task a)
          )
          <<@@@ applyHorizontalBoxedLayout
          <<@@@ AddLabels [Nothing, Just "message"]
      , functionConsDyn "ViewF" "view"
          ( dynamic \s (Typed func) -> Typed (ViewF s func) ::
              A.a b:
              String
              (Typed Func (a -> b))
              -> Typed TaskFunc (a -> Task b)
          )
          <<@@@ applyHorizontalBoxedLayout
          <<@@@ AddLabels [Just "message"]
      , functionConsDyn "UpdateF" "update"
          ( dynamic \s (Typed func) -> Typed (UpdateF s func) ::
              A.a b:
              String
              (Typed Func (a -> b))
              -> Typed TaskFunc (a -> Task b)
          )
          <<@@@ applyHorizontalBoxedLayout
          <<@@@ AddLabels [ Just "message" ]
      , functionConsDyn "Done" "done"
          ( dynamic \(Typed expr) -> Typed (Done expr) ::
              A.a:
              (Typed Expr a)
              -> Typed TaskExpr (Task a)
          )
          <<@@@ applyHorizontalBoxedLayout
      ]
  // Task expressions and functions on shares
  , DynamicConsGroup "Shares"
      // [ functionConsDyn "Init" "initialise"
      //     ( dynamic \(Typed sharedTy) (Typed taskExpr) -> Typed (Init sharedTy taskExpr) ::
      //         A.s a:
      //         (Typed Ty s)
      //         (Typed TaskExpr (Task a))
      //         -> Typed TaskExpr (Task a)
      //     )
      //     <<@@@ applyVerticalBoxedLayout
      [ functionConsDyn "StoreF" "store"
          (dynamic Typed StoreF :: Typed TaskFunc (Int -> Task ()))
          <<@@@ applyHorizontalBoxedLayout
          <<@@@ AddLabels [ Just "message" ]
      , functionConsDyn "Watch" "watch"
          ( dynamic \msg -> Typed (Watch msg) ::
              A.a:
              String
              -> Typed TaskExpr (Task ())
          )
          <<@@@ applyHorizontalBoxedLayout
          <<@@@ AddLabels [ Just "message" ]
      , functionConsDyn "WatchF" "watch"
          ( dynamic \msg -> Typed (WatchF msg) ::
              A.a:
              String
              -> Typed TaskFunc (a -> Task ())
          )
          <<@@@ applyHorizontalBoxedLayout
          <<@@@ AddLabels [ Just "message" ]
      ]
  // Non-task functions:
  , DynamicConsGroup "Basics"
      [ functionConsDyn "Identity" "this value"
          (dynamic Typed Identity :: A.a: Typed Func (a -> a))
          <<@@@ applyHorizontalLayout
      , functionConsDyn "Apply" "apply"
          ( dynamic \(Typed func) (Typed expr) ->
            Typed (Apply func expr) ::
              A.a b:
              (Typed Func (a -> b))
              (Typed Expr a)
              -> Typed Expr b
          )
          <<@@@ applyHorizontalBoxedLayout
          <<@@@ AddLabels [ Just "the function", Just "to" ]
      , functionConsDyn "Fst" "first element"
          (dynamic Typed Fst :: A.a b: Typed Func ((a, b) -> a))
          <<@@@ applyHorizontalLayout
      , functionConsDyn "Snd" "second element"
          (dynamic Typed Snd :: A.a b: Typed Func ((a, b) -> b))
          <<@@@ applyHorizontalLayout
      ]
  , DynamicConsGroup "Arithmetic"
      [ functionConsDyn "Add" "add"
          (dynamic \(Typed i) -> Typed (Add i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalBoxedLayout
      , functionConsDyn "Sub" "subtract"
          (dynamic \(Typed i) -> Typed (Sub i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalBoxedLayout
      , functionConsDyn "Mul" "multiply with"
          (dynamic \(Typed i) -> Typed (Mul i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalBoxedLayout
      , functionConsDyn "Div" "divide by"
          (dynamic \(Typed i) -> Typed (Div i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalBoxedLayout
      ]
  , DynamicConsGroup "Logic"
      [ functionConsDyn "Conj" "and"
          (dynamic \(Typed b) -> Typed (Conj b) :: (Typed Expr Bool) -> Typed Func (Bool -> Bool))
          <<@@@ applyHorizontalBoxedLayout
      , functionConsDyn "Disj" "or"
          (dynamic \(Typed b) -> Typed (Disj b) :: (Typed Expr Bool) -> Typed Func (Bool -> Bool))
          <<@@@ applyHorizontalBoxedLayout
      , functionConsDyn "Not" "negate"
          (dynamic Typed Not :: Typed Func (Bool -> Bool))
          <<@@@ applyHorizontalLayout
      ]
  , DynamicConsGroup "Comparison"
      [ functionConsDyn "Gt" "is greater than"
          (dynamic \(Typed i) -> Typed (Gt i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalBoxedLayout
      , functionConsDyn "Ge" "is greater or equal"
          (dynamic \(Typed i) -> Typed (Ge i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalBoxedLayout
      , functionConsDyn "Eq" "is equal to"
          (dynamic \(Typed i) -> Typed (Eq i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalBoxedLayout
      , functionConsDyn "Le" "is lesser than"
          (dynamic \(Typed i) -> Typed (Le i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalBoxedLayout
      , functionConsDyn "Lt" "is lesser than"
          (dynamic \(Typed i) -> Typed (Lt i) :: (Typed Expr Int) -> Typed Func (Int -> Int))
          <<@@@ applyHorizontalBoxedLayout
      ]
  // Non-task expressions:
  , DynamicConsGroup "Values"
      [ functionConsDyn "Int" "the integer"
          (dynamic \i -> Typed (Int i) :: Int -> Typed Expr Int)
          <<@@@ applyHorizontalLayout
      , functionConsDyn "Bool" "the boolean"
          (dynamic \b -> Typed (Bool b) :: Bool -> Typed Expr Bool)
          <<@@@ applyHorizontalLayout
      , functionConsDyn "String" "the string"
          (dynamic \s -> Typed (String s) :: String -> Typed Expr String)
          <<@@@ applyHorizontalLayout
      , functionConsDyn "Tuple" "the tuple"
          ( dynamic \(Typed a) (Typed b) ->
            Typed (Tuple a b) ::
              A.a b:
                (Typed Expr a) (Typed Expr b) -> Typed Expr (a, b)
          )
          <<@@@ applyHorizontalBoxedLayout
          <<@@@ AddLabels [ Just "with", Just "and" ]
    ]
  // Types
  , DynamicConsGroup "Types"
      [ functionConsDyn "Ty.Int" "Int"
          (dynamic Typed (Ty VInt) :: Typed Ty Int)
          <<@@@ applyHorizontalLayout
      , functionConsDyn "Ty.Bool" "Bool"
          (dynamic Typed (Ty VBool) :: Typed Ty Bool)
          <<@@@ applyHorizontalLayout
      , functionConsDyn "Ty.String" "String"
          (dynamic Typed (Ty VString) :: Typed Ty String)
          <<@@@ applyHorizontalLayout
      , functionConsDyn "Ty.Tuple" "Tuple"
          ( dynamic \(Typed (Ty toValue1)) (Typed (Ty toValue2)) -> Typed (Ty \(x, y) -> VTuple (toValue1 x) (toValue2 y)) ::
              A.a b:
              (Typed Ty a) (Typed Ty b) -> Typed Ty (a, b)
          )
          <<@@@ applyHorizontalBoxedLayout
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

  basicClasses = [ "typedtasks-base", "itasks-wrap-width", "itasks-wrap-height" ]
  horizontalClasses = [ "typedtasks-horizontal", "itasks-horizontal" ]
  verticalClasses = [ "typedtasks-vertical", "itasks-vertical" ]
  boxedClasses = [ "typedtasks-boxed" ]

  applyHorizontalBoxedLayout = ApplyCssClasses $ basicClasses ++ horizontalClasses ++ boxedClasses
  applyVerticalBoxedLayout = ApplyCssClasses $ basicClasses ++ verticalClasses ++ boxedClasses
  applyHorizontalLayout = ApplyCssClasses $ basicClasses ++ horizontalClasses
  applyVerticalLayout = ApplyCssClasses $ basicClasses ++ verticalClasses


// Evaluation //////////////////////////////////////////////////////////////////

// globalValueShare :: SimpleSDSLens ( Ty, List Value )
// globalValueShare = sharedStore "global share for typed task editor" ( abort "Global share not initialised", [] )
globalValueShare :: SimpleSDSLens (List Value)
globalValueShare = sharedStore "global share for typed task editor" []

evalTaskExpr :: TaskExpr -> Task Value
evalTaskExpr (Done expr) = return $ evalExpr expr
evalTaskExpr (EnterInfo (Ty toValue) msg) = enterInformation msg [] @ toValue
evalTaskExpr (Then task taskFunc) = evalTaskExpr task >>= evalTaskFunc taskFunc
evalTaskExpr (Both task1 task2) = (evalTaskExpr task1 -&&- evalTaskExpr task2) <<@ ApplyLayout arrangeHorizontal @ \(a, b) -> VTuple a b
evalTaskExpr (Any task1 task2) = (evalTaskExpr task1 -||- evalTaskExpr task2) <<@ ApplyLayout arrangeHorizontal
evalTaskExpr (One button1 task1 button2 task2) = viewInformation "Make a choice" [] () >?>
  [ ( button1, const True, \_ -> evalTaskExpr task1 )
  , ( button2, const True, \_ -> evalTaskExpr task2 )
  ]
// evalTaskExpr (Init sharedTy task) = set ( sharedTy, [] ) globalValueShare >>| evalTaskExpr task
evalTaskExpr (Watch msg) = viewSharedInformation msg [] globalValueShare @ (const VUnit)
evalTaskExpr (Forever task) = forever (evalTaskExpr task)

// evalTaskExpr (When task1 options) = evalTaskExpr task1
//   >>* [ OnAction (Action name) (ifValue (test pred) (evalTaskFunc cont))
//       \\ {name, pred, cont} <- options
//       ]
// where
//   test pred (VInt i) = case pred of
//     Lt (VInt j) -> i < j
//     Gt (VInt j) -> i > j
//     Eq (VInt j) -> i == j
//   test pred (VBool i) = case pred of
//     Eq (VBool j) -> i == j
//     Lt (VBool j) -> False
//     Gt (VBool j) -> False


evalTaskFunc :: TaskFunc Value -> Task Value
evalTaskFunc (ThenF this next) val =
  evalTaskFunc this val >>= evalTaskFunc next

evalTaskFunc (ViewF msg func) val = case evalFunc val func of
  (VInt i) -> (viewInformation msg [] i @ VInt) <<@ ApplyLayout arrangeHorizontal
  (VBool b) -> (viewInformation msg [] b @ VBool) <<@ ApplyLayout arrangeHorizontal
  (VString s) -> (viewInformation msg [] s @ VString) <<@ ApplyLayout arrangeHorizontal
  (VTuple a b) ->
    ( viewInformation msg [] ()
      ||- (evalTaskFunc (ViewF "" Identity) a -&&- evalTaskFunc (ViewF "" Identity) b)
      @ \(a, b) -> VTuple a b
    )
      <<@ ApplyLayout arrangeHorizontal

evalTaskFunc (UpdateF msg func) val = case evalFunc val func of
  (VInt i) -> (updateInformation msg [] i @ VInt) <<@ ApplyLayout arrangeHorizontal
  (VBool b) -> (updateInformation msg [] b @ VBool) <<@ ApplyLayout arrangeHorizontal
  (VString s) -> (updateInformation msg [] s @ VString) <<@ ApplyLayout arrangeHorizontal
  (VTuple a b) ->
    ( viewInformation msg [] ()
      ||- (evalTaskFunc (UpdateF "" Identity) a -&&- evalTaskFunc (UpdateF "" Identity) b)
      @ \(a, b) -> VTuple a b
    )
      <<@ ApplyLayout arrangeHorizontal

evalTaskFunc (StoreF) val =
  // upd (\( sharedTy, values ) -> ( sharedTy, cons val values)) globalValueShare @ (const VUnit)
  upd (cons val) globalValueShare @ (const VUnit)

evalTaskFunc (WatchF msg) val =
  viewSharedInformation msg [] globalValueShare @ (const VUnit)



evalExpr :: Expr -> Value
evalExpr (Int i) = VInt i
evalExpr (Bool b) = VBool b
evalExpr (String s) = VString s
evalExpr (Tuple fstExpr sndExpr) = VTuple (evalExpr fstExpr) (evalExpr sndExpr)
evalExpr (Apply func expr) = evalFunc (evalExpr expr) func


evalFunc :: Value Func -> Value
evalFunc val Identity = val

evalFunc (VInt i1) func = case func of
  (Gt expr) -> VBool $ i1 > evalInt expr
  (Ge expr) -> VBool $ i1 >= evalInt expr
  (Eq expr) -> VBool $ i1 == evalInt expr
  (Le expr) -> VBool $ i1 <= evalInt expr
  (Lt expr) -> VBool $ i1 < evalInt expr
  (Add expr) -> VInt $ i1 + evalInt expr
  (Sub expr) -> VInt $ i1 - evalInt expr
  (Mul expr) -> VInt $ i1 * evalInt expr
  (Div expr) -> VInt $ i1 / evalInt expr
where
  evalInt :: Expr -> Int
  evalInt expr = case evalExpr expr of
    (VInt i) -> i

evalFunc (VBool b1) func = case func of
  (Eq expr) -> VBool $ b1 == evalBool expr
  (Conj expr) -> VBool $ b1 && evalBool expr
  (Disj expr) -> VBool $ b1 || evalBool expr
  (Not) -> VBool $ not b1
where
  evalBool :: Expr -> Bool
  evalBool expr = case evalExpr expr of
    (VBool b) -> b

evalFunc (VString s1) func = case func of
  (Eq expr) -> VBool $ s1 == evalString expr
where
  evalString :: Expr -> String
  evalString expr = case evalExpr expr of
    (VString s) -> s

evalFunc (VTuple x1 x2) func = case func of
  Fst -> x1
  Snd -> x2
