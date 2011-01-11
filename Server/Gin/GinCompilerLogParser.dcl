definition module GinCompilerLogParser

import StdMaybe

:: CompilerErrorContext :== (CompilerError, ErrorContext)

:: CompilerError = ParseError String
                 | UndefinedError String
                 | OverloadingError String
                 | TypeError TypeError
                 | OtherError String

:: ErrorContext = { filename :: String
                  , line :: Int
                  , pos :: Maybe Int
                  , context :: String
                  }
                 
:: TypeError = { expectedType :: String
               , inferredType :: String
               , position :: TypeErrorPosition
               }
               
:: TypeErrorPosition = TypeErrorArgument Int String | TypeErrorNear String

parseCleanIDELog :: String -> [CompilerErrorContext]
parseCleanCompilerLog :: String -> [CompilerErrorContext]

printErrors :: [CompilerErrorContext] -> String

:: PathError :== (String, String)

findPathErrors :: [CompilerErrorContext] String -> [PathError]
