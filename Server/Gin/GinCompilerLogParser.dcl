definition module GinCompilerLogParser

from Maybe import :: Maybe

import GinParser
import GinPrinter
import GinAbstractSyntax

:: CompilerErrorContext :== (CompilerError, ErrorContext)

:: CompilerError = ParseError String
                 | UndefinedError String
                 | OverloadingError String
                 | TypeError TypeError
                 | OtherError String

:: TypeError = { expectedType  :: String
               , inferredType  :: String
               , position      :: TypeErrorPosition
               }
               
:: ErrorContext = { filename  :: String
                  , line      :: Int
                  , pos       :: Maybe Int
                  , context   :: String
                  }

:: TypeErrorPosition = TypeErrorArgument Int String | TypeErrorNear String

parseCleanIDELog :: String -> [CompilerErrorContext]
parseCleanCompilerLog :: String -> [CompilerErrorContext]

:: PathError :== (GPath, String)

findPathErrors :: [CompilerErrorContext] FunctionMap LineMap -> [PathError]
