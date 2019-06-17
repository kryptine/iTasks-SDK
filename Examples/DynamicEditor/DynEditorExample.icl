module DynEditorExample

import StdEnv
import Data.Func
import Data.Functor
import iTasks
import iTasks.Extensions.DateTime
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
    [ ( "Run", const True, \v ->
          viewInformation ("Evaluate the task", info2) [] ()
            ||- evalTaskExpr (toValue taskEditor v) >>*
        [ OnAction (Action "Back") (always (editTaskExpr (Just v)))
        , OnAction (Action "Finish") (ifValue (const True) (\r -> viewInformation ("Done!", info3) [] (toString r) >?>
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

:: Name
  :== String

:: TaskExpr
  = Done Expr
  | Bind TaskExpr Name TaskExpr

:: Expr
  = Int Int
  | Bool Bool
  | String String
  | Pair Expr Expr

:: Value
  = VUnit
  | VInt Int
  | VBool Bool
  | VString String
  | VPair Value Value

:: Ty
  = E.a: Ty (a -> Value) & iTask a

:: Typed a b
  =: Typed a

derive class iTask TaskExpr, Expr, Value, Typed

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
  , DynamicConsGroup "Basics"
      [ functionConsDyn "Bind" "bind"
          ( dynamic \(Typed task) (Typed name) (Typed cont) -> Typed (Bind task name cont) ::
              A.a b:
              (Typed TaskExpr (Task a))
              (Typed String a)
              (Typed TaskExpr (a -> Task b))
              -> Typed TaskExpr (Task b)
          )
          <<@@@ applyVerticalBoxedLayout
      , functionConsDyn "Done" "done"
          ( dynamic \(Typed expr) -> Typed (Done expr) ::
              A.a:
              (Typed Expr a)
              -> Typed TaskExpr (Task a)
          )
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
      , functionConsDyn "Pair" "the pair"
          ( dynamic \(Typed a) (Typed b) ->
            Typed (Pair a b) ::
              A.a b:
                (Typed Expr a) (Typed Expr b) -> Typed Expr (a, b)
          )
          <<@@@ applyHorizontalBoxedLayout
          <<@@@ AddLabels [ Just "with", Just "and" ]
    ]
  // Types
  , DynamicConsGroup "Types"
      [ functionConsDyn "Ty.Int" "Integer"
          (dynamic Typed (Ty VInt) :: Typed Ty Int)
          <<@@@ applyHorizontalLayout
      , functionConsDyn "Ty.Bool" "Boolean"
          (dynamic Typed (Ty VBool) :: Typed Ty Bool)
          <<@@@ applyHorizontalLayout
      , functionConsDyn "Ty.String" "String"
          (dynamic Typed (Ty VString) :: Typed Ty String)
          <<@@@ applyHorizontalLayout
      , functionConsDyn "Ty.Pair" "Pair"
          ( dynamic \(Typed (Ty toValue1)) (Typed (Ty toValue2)) -> Typed (Ty \(x, y) -> VPair (toValue1 x) (toValue2 y)) ::
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

  basicClasses = [ "typedtasks-base" ]
  horizontalClasses = [ "typedtasks-horizontal" ]
  verticalClasses = [ "typedtasks-vertical" ]
  boxedClasses = [ "typedtasks-boxed" ]

  applyHorizontalBoxedLayout = ApplyCssClasses $ basicClasses ++ horizontalClasses ++ boxedClasses
  applyVerticalBoxedLayout = ApplyCssClasses $ basicClasses ++ verticalClasses ++ boxedClasses
  applyHorizontalLayout = ApplyCssClasses $ basicClasses ++ horizontalClasses
  applyVerticalLayout = ApplyCssClasses $ basicClasses ++ verticalClasses


// Evaluation //////////////////////////////////////////////////////////////////

evalTaskExpr :: TaskExpr -> Task Value
evalTaskExpr (Done expr) = return $ evalExpr expr
// evalTaskExpr (Bind task fund) = ... //evalTaskExpr task >>= evalTaskFunc taskFunc


evalExpr :: Expr -> Value
evalExpr (Int i) = VInt i
evalExpr (Bool b) = VBool b
evalExpr (String s) = VString s
evalExpr (Pair fstExpr sndExpr) = VPair (evalExpr fstExpr) (evalExpr sndExpr)



instance toString Value where
  toString val = case val of
    VUnit -> "()"
    VInt i -> toString i
    VBool b -> toString b
    VString s -> toString s
    VPair x y -> "( " +++ toString x +++ ", " +++ toString y +++ " )"
