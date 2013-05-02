implementation module BuiltInJS_fHS

import StdEnv, Map, StringAppender, BuiltInJS

builtInFunctions_fHS :: Map String (String, Arity)
builtInFunctions_fHS = fromList
				  []

inlineFunctions_fHS :: Map String (InlineCoderFunc, Arity)
inlineFunctions_fHS = fromList
				  [("==#", (inline_eq, 2))
				  ,("eqWord#", (inline_eq, 2)) 
				  ,("/=#", (inline_ne, 2))
				  ,("<#", (inline_less, 2))
				  ,("ltWord#", (inline_less, 2))
				  ,("ltChar#", (inline_less, 2))
				  ,("<=#", (inline_le, 2))
				  ,("leWord#", (inline_le, 2))
				  ,("leChar#", (inline_le, 2))			  
				  ,(">#", (inline_greater, 2))
				  ,("gtWord#", (inline_less, 2))
				  ,("gtChar#", (inline_less, 2))
				  ,(">=#", (inline_ge, 2))
				  ,("geWord#", (inline_ge, 2))
				  ,("geChar#", (inline_ge, 2))
				  ,("-#", (inline_sub, 2))
				  ,("minusWord#", (inline_sub, 2))
				  ,("+#", (inline_add, 2))
				  ,("plusFloat#", (inline_add, 2))
				  ,("plusWord#", (inline_add, 2))
				  ,("+##", (inline_add, 2))
				  ,("*#", (inline_mult, 2))	
				  ,("timesFloat#", (inline_mult, 2))					  
				  ,("*##", (inline_mult, 2))					  
				  ,("negateInt#", (inline_neg, 1))	
				  ,("negateFloat#", (inline_neg, 1))	
				  ,("negateDouble#", (inline_neg, 1))	
				  ,("and#", (inline_and, 2))
				  ,("xor#", (inline_xor, 2))
				  ,("or#", (inline_or, 2))
				  ,("uncheckedShiftL#", (inline_shiftL, 2))
				  ,("uncheckedShiftRL#", (inline_shiftRL, 2))
				  ,("word2Int#", (inline_id, 1))
				  ,("int2Word#", (inline_id, 1))
				  ,("int2Double#", (inline_id, 1))
				  ,("int2Float#", (inline_id, 1))
				  ,("**##", (inline_power, 2))
				  ,("powerFloat#", (inline_power, 2))
				  ,("not#", (inline_not, 1))
				  ,("ord#", (inline_ord, 1))
				  ,("chr#", (inline_chr, 1))
				  ,("remInt#", (inline_rem, 2))				  
				  ,("quotInt#", (inline_quot, 2)) 
				  ,("decodeFloat_Int#", (inline_id, 1)) // TODO
				  ,("decodeDouble_2Int#", (inline_id, 1)) // TODO
			
/*	  
Ensures that the suspensions under evaluation by the current thread
are unique; that is, the current thread is not evaluating anything
that is also under evaluation by another thread that has also executed
'noDuplicate'.

This operation is used in the definition of 'unsafePerformIO' to
prevent the IO action from being executed multiple times, which is usually
undesirable.				  
*/			  
				  ,("noDuplicate#", (inline_id, 1))
				  ]

a1 args :== hd (args)
a2 args :== hd (tl args)
a3 args :== hd (tl (tl args))

inline_id eval args a
	= a <++ eval (a1 args)
				
inline_eq eval args a
	= a <++ eval (a1 args) <++ "==" <++ eval (a2 args)

inline_ne eval args a
	= a <++ eval (a1 args) <++ "!=" <++ eval (a2 args)

inline_less eval args a
	= a <++ eval (a1 args) <++ "<" <++ eval (a2 args)

inline_le eval args a
	= a <++ eval (a1 args) <++ "<=" <++ eval (a2 args)

inline_greater eval args a
	= a <++ eval (a1 args) <++ ">" <++ eval (a2 args)

inline_ge eval args a
	= a <++ eval (a1 args) <++ ">=" <++ eval (a2 args)

inline_sub eval args a
	= a <++ eval (a1 args) <++ "-" <++ eval (a2 args)

inline_add eval args a
	= a <++ eval (a1 args) <++ "+" <++ eval (a2 args)

inline_mult eval args a
	= a <++ eval (a1 args) <++ "*" <++ eval (a2 args)

inline_neg eval args a
	= a <++ eval (a1 args) <++ "* -1"

inline_and eval args a
	= a <++ eval (a1 args) <++ "&" <++ eval (a2 args)

inline_xor eval args a
	= a <++ eval (a1 args) <++ "^" <++ eval (a2 args)

inline_or eval args a
	= a <++ eval (a1 args) <++ "|" <++ eval (a2 args)

inline_shiftL eval args a
	= a <++ eval (a1 args) <++ "<<" <++ eval (a2 args)

inline_shiftRL eval args a
	= a <++ eval (a1 args) <++ ">>" <++ eval (a2 args)
	
inline_power eval args a
	= a <++ "Math.pow(" <++ eval (a1 args) <++ "," <++ eval (a2 args) <++ ")"

inline_not eval args a
	= a <++ "!" <++ eval (a1 args)

inline_ord eval args a
	= a <++ eval (a1 args) <++ ".charCodeAt(0)"

inline_chr eval args a
	= a <++ "String.fromCharCode(" <++ eval (a1 args) <++ ")"

inline_rem eval args a
	= a <++ eval (a1 args) <++ "%" <++ eval (a2 args)

inline_quot eval args a
	= a <++ "Math.floor(" <++ eval (a1 args) <++ "/" <++ eval (a2 args) <++ ")"
	