implementation module GinFlowLibrary

import StdFunc
import StdTuple

import GinTypes
import GinSyntax
import GinAbstractSyntax
import GinParser
import GinBindings

addDefaultLibrary :: GModule -> GModule
addDefaultLibrary mod = { GModule 
                        | mod & imports = map importDeclarations library
                        }

importBindings :: GImport -> GParseState Bindings
importBindings imp
# name = imp.GImport.name
= case [ mb.bindings \\ mb <- library | mb.ModuleBindings.name == name ] of
    [] = parseError ("Imported module " +++ name +++ "not found")
    [bindings] = ret bindings
    _ = parseError ("Multiple declarations of module " +++ name)
    
library :: [ModuleBindings]
library = [ clean
	      , coreCombinators
          , commonCombinators
          , DateTimeTasks
          , interactionTasks
          , systemTasks
          ]

mkZeroArityBinding :: String GTypeExpression String -> Binding
mkZeroArityBinding name type icon = NodeBinding 
    { NodeBinding
    | declaration = mkZeroArityDeclaration name type icon
    , parameterMap = NBPrefixApp
    }
    
mkZeroArityDeclaration :: String GTypeExpression String -> GDeclaration
mkZeroArityDeclaration name type icon = 
  { GDeclaration 
    | name = name
    , returnType = gTask  type
    , formalParams = []
    , icon = icon
    , shape = "app:task"
    }

/**
* Module bindings for Clean expressions 
*/

clean :: ModuleBindings
clean = { ModuleBindings
        | name = "(Clean language)"
        , bindings = [bCase]
        , types = predefinedTypes
        }
        
predefinedTypes :: [GTypeDefinition]
predefinedTypes = [ { name = "Bool", expression = GBasicTypeExpression "Bool" }
				  , { name = "Char", expression = GBasicTypeExpression "Char" }
				  , { name = "Int", expression = GBasicTypeExpression "Int" }
				  , { name = "Real", expression = GBasicTypeExpression "Real" }
				  , { name = "String", expression = GBasicTypeExpression "String" }
				  , { name = "Task", expression = GBasicTypeExpression "Task" }
				  , { name = "Void", expression = GBasicTypeExpression "Task" }				  
				  ]

bCase :: Binding
bCase = ParallelBinding
  { split = { GDeclaration 
              | name = "case split"
              , returnType = GUndefinedTypeExpression
              , formalParams = [ { GFormalParameter 
                                 | name = "a"
                                 , type = GTypeVariable "a" 
                                 }
                               ]
              , icon = "case"
              , shape = "embed:rhombus"
              }
    , merge = { GDeclaration 
              | name = "case merge"
              , returnType = GUndefinedTypeExpression
              , formalParams = []
              , icon = "merge"
              , shape = "icon:merge"
              }
    , type = GTypeVariable "b"
    , fixedNrBranches = Nothing
    , parameterMap = Extension (PBApply mkCase)
    }
    where
      mkCase :: [AExpression Void] [AExpression Void] [(Maybe APattern, AExpression Void)] -> GParseState (AExpression Void)
      mkCase splitParams mergeParams alts = 
        if (length (filter isNothing (map fst alts)) > 1)
          (parseError "A case expression can have at most one default case")
          //"otherwise" alternative is put at the end of the list
          (ret (let sortalts = filter (isJust o fst) alts ++ filter (isNothing o fst) alts
                in  Case (splitParams !! 0) (map mkCaseAlt alts)))
      
      mkCaseAlt :: (Maybe APattern, AExpression Void) -> ACaseAlt Void
      mkCaseAlt (Just pat, exp) = CaseAlt pat exp
      mkCaseAlt (Nothing, exp)  = CaseAlt "otherwise" exp

/**
* Module bindings for coreCombinators module
*/
coreCombinators :: ModuleBindings
coreCombinators = { ModuleBindings
               | name = "CoreCombinators"
               , types = []
               , bindings = [bReturn, bForever, /*bRepeatUntil, bIterateUntil,*/ bSequence]
               }

bReturn :: Binding
bReturn = NodeBinding { NodeBinding
                      | declaration = 
                          { GDeclaration 
                          | name = "return"
                          , returnType = gTask  (GTypeVariable "a")
                          , formalParams = [ { GFormalParameter 
                                             | name = "a"
                                             , type = GTypeVariable "a" 
                                             }
                                           ]
                          , icon = "return"
                          , shape = "embed:ellipse"
                          }
                      , parameterMap = NBPrefixApp
                      }

bForever :: Binding
bForever = NodeBinding { NodeBinding
                       | declaration = 
                           { GDeclaration 
                           | name = "forever"
                           , returnType = gTask  (GTypeVariable "a")
                           , formalParams = [ { GFormalParameter 
                                              | name = "task"
                                              , type = gTask  (GTypeVariable "a")
                                              } 
                                            ]
                           , icon = "forever"
                           , shape = "app:task"
                           }
                       , parameterMap = NBPrefixApp
                       }
bSequence :: Binding
bSequence = NodeBinding { NodeBinding
                        | declaration = 
                            { GDeclaration 
                            | name = "sequence"
                            , returnType = gTask  (GTypeVariable "a")
                            , formalParams = [ { GFormalParameter 
                                               | name = "label"
                                               , type = GBasicTypeExpression "String"
                                               }
                                             , { GFormalParameter 
                                               | name = "task"
                                               , type = GList (gTask  (GTypeVariable "a"))
                                               } 
                                             ]
                            , icon = "sequence"
                            , shape = "app:task"
                           }
                        , parameterMap = NBPrefixApp
                        }


/**
* Module bindings for coreCombinators module
*/
commonCombinators :: ModuleBindings
commonCombinators = { ModuleBindings
               | name     = "CommonCombinators"
               , types    = []
               , bindings = [bFirstCompleted, bLeft, bRight, bAllTasks, bAllTuple]
               }
               
split :: GDeclaration
split = mkZeroArityDeclaration "split" GUndefinedTypeExpression "split"

bFirstCompleted :: Binding
bFirstCompleted = ParallelBinding { ParallelBinding
                                  | split = split
                                  , merge = mkZeroArityDeclaration "first completed" GUndefinedTypeExpression "filter"
                                  , type = gTask  (GTypeVariable "a")
                                  , fixedNrBranches = Nothing
                                  , parameterMap = App [Lit "anyTask", Extension PBBranchList]
                                  }
               
bLeft :: Binding
bLeft = ParallelBinding { ParallelBinding
                        | split = split
                        , merge = mkZeroArityDeclaration "filter left" GUndefinedTypeExpression "filter"
                        , type = gTask  (GTypeVariable "a")
                        , fixedNrBranches = Just 2
                        , parameterMap = AppInfix "-||" Infixl 3 (Extension (PBBranch 0)) (Extension (PBBranch 1))
                        }

bRight :: Binding
bRight = ParallelBinding { ParallelBinding
                         | split = split
                         , merge = mkZeroArityDeclaration "filter right" GUndefinedTypeExpression "filter"
                         , type = gTask  (GTypeVariable "a")
                         , fixedNrBranches = Just 2
                         , parameterMap = AppInfix "||-" Infixr 3 (Extension (PBBranch 0)) (Extension (PBBranch 1))
                         }

bAllTasks :: Binding
bAllTasks = ParallelBinding { ParallelBinding
                            | split = split
                            , merge = mkZeroArityDeclaration "merge (list)" GUndefinedTypeExpression "merge"
                            , type = gTask  (GList (GTypeVariable "a"))
                            , fixedNrBranches = Nothing
                            , parameterMap = App [Lit "allTasks", Extension PBBranchList]
                            }
                            
bAllTuple :: Binding
bAllTuple = ParallelBinding { ParallelBinding
                            | split = split
                            , merge = mkZeroArityDeclaration "merge (tuple)" GUndefinedTypeExpression "merge"
                            , type = gTask  (GTuple [GTypeVariable "a",GTypeVariable "a"])
                            , fixedNrBranches = Just 2
                            , parameterMap = AppInfix "-&&-" Infixr 4 (Extension (PBBranch 0)) (Extension (PBBranch 1))
                            }

/**
* Module bindings for InteractionTasks module
*/
interactionTasks :: ModuleBindings
interactionTasks = { ModuleBindings
               | name = "InteractionTasks"
               , types = []
               , bindings = [ bEnterInformation
                            , bUpdateInformation
                            , bEnterChoice
                            , bUpdateChoice
                            , bShowMessage
                            , bShowInstruction
                            ]
               }

bEnterInformation :: Binding
bEnterInformation = NodeBinding { NodeBinding
                                | declaration = 
                                    { GDeclaration 
                                    | name = "enterInformation"
                                    , returnType = gTask  (GTypeVariable "a")
                                    , formalParams = [ { GFormalParameter 
                                                       | name = "subject"
                                                       , type = GBasicTypeExpression "String"
                                                       }
                                                     , { GFormalParameter 
                                                       | name = "description"
                                                       , type = GTypeVariable "description"
                                                       }
                                                     ]
                                    , icon = "keyboard"
                                    , shape = "app:task"
                                    }
                                , parameterMap = NBPrefixApp
                                }

bUpdateInformation :: Binding
bUpdateInformation = NodeBinding { NodeBinding
                                 | declaration = 
                                     { GDeclaration 
                                     | name = "updateInformation"
                                     , returnType = gTask  (GTypeVariable "a")
                                     , formalParams = [ { GFormalParameter 
                                                        | name = "subject"
                                                        , type = GBasicTypeExpression "String"
                                                        }
                                                      , { GFormalParameter 
                                                        | name = "description"
                                                        , type = GTypeVariable "description"
                                                        }
                                                      , { GFormalParameter 
                                                        | name = "initial"
                                                        , type = GTypeVariable "a"
                                                        }
                                                      ]
                                     , icon = "keyboard"
                                     , shape = "app:task"
                                     }
                                 , parameterMap = NBPrefixApp
                                 }
                                
brequestConfirmation :: Binding
brequestConfirmation = NodeBinding { NodeBinding
                                   | declaration = 
                                       { GDeclaration 
                                       | name = "requestConfirmation"
                                       , returnType = gTask  (GBasicTypeExpression "Bool")
                                       , formalParams = [ { GFormalParameter 
                                                          | name = "subject"
                                                          , type = (GBasicTypeExpression "String")
                                                          }
                                                        , { GFormalParameter 
                                                          | name = "description"
                                                          , type = GTypeVariable "description"
                                                          }
                                                       ]
                                       , icon = "confirm"
                                       , shape = "app:task"
                                       }
                                   , parameterMap = NBPrefixApp
                                   }
                                   
bEnterChoice :: Binding
bEnterChoice = NodeBinding { NodeBinding
                           | declaration = 
                               { GDeclaration 
                               | name = "enterChoice"
                               , returnType = gTask  (GTypeVariable "a")
                               , formalParams = [ { GFormalParameter 
                                                  | name = "subject"
                                                  , type = GBasicTypeExpression "String"
                                                  }
                                                , { GFormalParameter 
                                                  | name = "description"
                                                  , type = GTypeVariable "description"
                                                  }
                                                , { GFormalParameter 
                                                  | name = "options"
                                                  , type = GList (GTypeVariable "a")
                                                  }
                                                ]
                               , icon = "choice"
                               , shape = "app:task"
                               }
                           , parameterMap = NBPrefixApp
                           }

bUpdateChoice :: Binding
bUpdateChoice = NodeBinding { NodeBinding
                            | declaration = 
                                { GDeclaration 
                                | name = "updateChoice"
                                , returnType = gTask  (GTypeVariable "a")
                                , formalParams = [ { GFormalParameter 
                                                   | name = "subject"
                                                   , type = GBasicTypeExpression "String"
                                                   }
                                                 , { GFormalParameter 
                                                   | name = "description"
                                                   , type = GTypeVariable "description"
                                                   }
                                                 , { GFormalParameter 
                                                   | name = "options"
                                                   , type = GList (GTypeVariable "a")
                                                   }
                                                 , { GFormalParameter 
                                                   | name = "index"
                                                   , type = GBasicTypeExpression "Int"
                                                   }
                                                 ]
                                , icon = "choice"
                                , shape = "app:task"
                                }
                            , parameterMap = NBPrefixApp
                            }

bShowMessage :: Binding
bShowMessage = NodeBinding { NodeBinding
                           | declaration = 
                               { GDeclaration 
                               | name = "showMessage"
                               , returnType = gTask  (GTypeVariable "a")
                               , formalParams = [ { GFormalParameter 
                                                  | name = "subject"
                                                  , type = GBasicTypeExpression "String"
                                                  }
                                                , { GFormalParameter 
                                                  | name = "message"
                                                  , type = GTypeVariable "message"
                                                  } 
                                                , { GFormalParameter 
                                                  | name = "value"
                                                  , type = GTypeVariable "a"
                                                  } 
                                                ]
                               , icon = "information"
                               , shape = "app:task"
                               }
                           , parameterMap = NBPrefixApp
                           }

bShowInstruction :: Binding
bShowInstruction = NodeBinding { NodeBinding
                               | declaration = 
                                   { GDeclaration 
                                   | name = "showInstruction"
                                   , returnType = gTask  (GTypeVariable "a")
                                   , formalParams = [ { GFormalParameter 
                                                      | name = "title"
                                                      , type = GBasicTypeExpression "String"
                                                      }
                                                    , { GFormalParameter 
                                                      | name = "instruction"
                                                      , type = GTypeVariable "Instruction"
                                                      }
                                                    , { GFormalParameter 
                                                      | name = "value"
                                                      , type = GTypeVariable "a"
                                                      } 
                                                    ]
                                   , icon = "information"
                                   , shape = "app:task"
                                   }
                               , parameterMap = NBPrefixApp
                               }


/**
* Module bindings for coreCombinators module
*/
DateTimeTasks :: ModuleBindings
DateTimeTasks = { ModuleBindings
               | name = "DateTimeTasks"
              , types = [ { name = "Time", expression = GAbstractTypeExpression "Time" }
						, { name = "Date", expression = GAbstractTypeExpression "Date" }
						, { name = "DateTime", expression = GAbstractTypeExpression "DateTime" }
						]
               , bindings = [ mkZeroArityBinding "getCurrentTime" (GConstructor "Time") "clock"
                            , mkZeroArityBinding "getCurrentDate" (GConstructor "Date") "date"
                            , mkZeroArityBinding "getCurrentDateTime" (GConstructor "DateTime") "date"
                            , bWaitForTime
                            , bWaitForDate
                            , bWaitForTimer
                            ]
               }

bWaitForTime :: Binding
bWaitForTime = NodeBinding { NodeBinding
                           | declaration = 
                               { GDeclaration 
                               | name = "waitForTime"
                               , returnType = gTask  gVoid
                               , formalParams = [ { GFormalParameter 
                                                  | name = "time"
                                                  , type = GConstructor "Time"
                                                  } 
                                                ]
                               , icon = "clock_go"
                               , shape = "app:task"
                               }
                           , parameterMap = NBPrefixApp
                           }

bWaitForDate :: Binding
bWaitForDate = NodeBinding { NodeBinding
                           | declaration = 
                               { GDeclaration 
                               | name = "waitForDate"
                               , returnType = gTask  gVoid
                               , formalParams = [ { GFormalParameter 
                                                  | name = "date"
                                                  , type = GConstructor "Date"
                                                  }
                                                ]
                               , icon = "date_go"
                               , shape = "app:task"
                               }
                           , parameterMap = NBPrefixApp
                           }

bWaitForTimer :: Binding
bWaitForTimer = NodeBinding { NodeBinding
                            | declaration = 
                                { GDeclaration 
                                | name = "waitForTimer"
                                , returnType = gTask  gVoid
                                , formalParams = [ { GFormalParameter 
                                                   | name = "time"
                                                   , type = GConstructor "Time"
                                                   } 
                                                 ]
                                , icon = "clock_go"
                                , shape = "app:task"
                                }
                            , parameterMap = NBPrefixApp
                            }

/**
* Module bindings for SystemTasks module
*/
systemTasks :: ModuleBindings
systemTasks = { ModuleBindings
              | name = "SystemTasks"
              , types = [ { name = "User", expression = GAbstractTypeExpression "User" }
						, { name = "UserName", expression = GAbstractTypeExpression "UserName" }
						]
              , bindings = [ mkZeroArityBinding "getCurrentUser" (GConstructor "User") "user"
                           , mkZeroArityBinding "getCurrentProcessId" (GConstructor "ProcessId") "cog"
                           , mkZeroArityBinding "getContextWorker" (GConstructor "UserName") "user"
                           , mkZeroArityBinding "getContextManager" (GConstructor "UserName") "user"
                           , mkZeroArityBinding "getDefaultValue" (GTypeVariable "a") "page_white"
                           ]
              }
