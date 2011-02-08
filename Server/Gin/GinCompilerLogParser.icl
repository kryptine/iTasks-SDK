implementation module GinCompilerLogParser

import StdEnv
import Maybe
import StdParsComb
import GenPrint

import StdDebug

parseCleanIDELog :: String -> [CompilerErrorContext]
parseCleanIDELog s = (snd o hd) (begin1 parser (fromString s))
where
	parser :: CParser Char [CompilerErrorContext] t
	parser = <*> parseHeader &> <*> parseCompilerError <& parseFooter

parseCleanCompilerLog :: String -> [CompilerErrorContext]
parseCleanCompilerLog s = (snd o hd) (begin1 parser (fromString s))
where
	parser :: CParser Char [CompilerErrorContext] t
	parser = <*> parseCompilerError

parseHeader :: CParser Char Char t
parseHeader = newline
			  <!> token ['Bring up to date...'] &> newline
	          <!> token ['Compiling...'] &> newline
	          <!> symbol '[' &> parseStringUntilCut (token [',]: module has changed']) &> newline
	          <!> symbol '[' &> parseStringUntilCut (token [',]: no abc file']) &> newline
	          <!> token ['Compiling \''] &> parseStringUntilCut (token  ['\'.']) &> newline
	
parseFooter :: CParser Char Char t
parseFooter = token ['Linking...'] &> newline
		      <!> token ['Finished making.'] &> newline

parseCompilerError :: CParser Char CompilerErrorContext t
parseCompilerError = parseParseError
                     <!> parseUndefinedError
                     <!> parseOverloadingError
                     <!> parseTypeError
                     <!> parseOtherError

parseParseError :: CParser Char CompilerErrorContext t
parseParseError = parseSingleLineError "Parse error" "" ParseError

parseUndefinedError :: CParser Char CompilerErrorContext t
parseUndefinedError = parseSingleLineError "Error" " undefined" UndefinedError

parseOverloadingError :: CParser Char CompilerErrorContext t
parseOverloadingError = parseSingleLineError "Overloading error" "" OverloadingError

parseTypeError :: CParser Char CompilerErrorContext t
parseTypeError = 
	token ['Type error '] &> 
    parseErrorContext <&> \context = 
    token [':'] &> parseTypeErrorPosition <&> \position = 
    token [' cannot unify types:\n'] &>
    symbol ' ' &> parseStringUntilCut (symbol '\n') <&> \expectedType = 
    symbol ' ' &> parseStringUntilCut newlineOrEof <&> \inferredType = 
    yield ((TypeError { expectedType = expectedType, inferredType = inferredType, position = position }), context)
    where
	parseTypeErrorPosition :: CParser Char TypeErrorPosition t
	parseTypeErrorPosition = parseArgument <!> parseNear
	parseArgument :: CParser Char TypeErrorPosition t
	parseArgument = token ['"argument '] &> int <&> \argnr = 
		token [' of '] &> parseStringUntilCut (symbol '"') <&> \identifier = 
		yield (TypeErrorArgument argnr identifier)
	parseNear :: CParser Char TypeErrorPosition t
	parseNear = token [' near '] &> parseStringUntilCut (token [' :']) <&> \identifier = 
		yield (TypeErrorNear identifier)
		
parseOtherError :: CParser Char CompilerErrorContext t
parseOtherError = token ['Error '] &> parseErrorContext <&> \context = token [': '] &> parseUntilCut newlineOrEof <&> \message = 
	yield (OtherError (toString message), context)

parseSingleLineError :: String String (String -> CompilerError) -> CParser Char CompilerErrorContext t
parseSingleLineError prefix postfix f = 
	token (fromString prefix) &> symbol ' ' &> 
    parseErrorContext <&> \context = 
    token [': '] &> parseUntilCut (token (fromString postfix) &> newlineOrEof) <&> \message = 
                  yield (f (toString message), context)

parseErrorContext :: CParser Char ErrorContext t
parseErrorContext =
	symbol '[' &>
    parseStringUntilCut (symbol ',') <&> \filename =
    int <&> \line =
    parseMaybe (symbol ';' &> int) <&> \pos =
    parseMaybe (symbol ',') &>
    parseStringUntilCut (symbol ']') <&> \context =
    yield { ErrorContext | filename = filename, line = line, pos = pos, context = context }
                        
derive gPrint CompilerError, ErrorContext, TypeError, TypeErrorPosition, Maybe, (,)

printErrors :: [CompilerErrorContext] -> String
printErrors errs = printToString errs

printError :: CompilerError -> String
printError (ParseError err) = "Parse error: " +++ err
printError (UndefinedError v) = "Undefined variable: " +++ v 
printError (OverloadingError err) = "Overloading error: " +++ err
printError (TypeError te) = "Type error:\nExpected type:" +++ te.expectedType +++ "\nActual type:" +++ te.inferredType
printError (OtherError err) = err

findPathErrors :: [CompilerErrorContext] String -> [PathError]
findPathErrors errorContexts source =  map findPathError errorContexts
where
	findPathError :: CompilerErrorContext -> (String,String)
	findPathError (error,context) = case findPathCommentInSource context.line source of
		Just path = (path, printError error)
		Nothing = ("/", printError error)

findPathCommentInSource :: Int String -> Maybe String
findPathCommentInSource line source = (snd o hd) (begin1 (doFind 1 Nothing) (fromString source))
where
	doFind :: Int (Maybe String) -> CParser Char (Maybe String) t
	doFind curline curpath = if (curline > line) 
    	(yield curpath)
    	(	(token ['/* PATH:'] &> parseUntilCut (token [' */']) <&> \newPath = doFind curline (Just (toString newPath)))
    		<!> newlineOrEof &> doFind (inc curline) curpath
    		<!> char &> doFind curline curpath
    	)

//Parse utilities
char :: CParser Char Char t
char = psymbol where
	psymbol sc xc ac [x:ss] = sc x xc ac ss
	psymbol sc xc ac _      = xc ac

newline :: CParser Char Char t
newline = symbol '\n'

newlineOrEof :: CParser Char Char t
newlineOrEof = (newline <!> eof <@ fromInt)

parseUntilCut :: (CParser Char u t) -> CParser Char [Char] t
parseUntilCut p = p <@ (\_ -> []) <!> char <:&> parseUntilCut p

parseStringUntilCut :: (CParser Char u t) -> CParser Char String t
parseStringUntilCut p = parseUntilCut p <@ toString

parseMaybe :: (CParser s u t) -> CParser s (Maybe u) t
parseMaybe p = (p <&> \r = yield (Just r)) <!> yield Nothing
