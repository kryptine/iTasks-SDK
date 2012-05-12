definition module CodeGeneratorJS

import StringAppender

/**
* Generates JS from Sapl source
*
* @param Sapl source
* @return (JS source / error message, error)
*/
generateJS :: String -> (StringAppender, Bool)

/**
* Generates JS from Sapl source of sapl expression only
*
* @param souce of Sapl expression
* @return (JS source / error message, error)
*/
exprGenerateJS :: String -> (StringAppender, Bool)