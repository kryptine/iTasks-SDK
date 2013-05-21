definition module BuiltInJS_fHS

import SaplTokenizer, SaplParser, BuiltInJS

/**
 * Definitions of built-in and inline functions for the JavaScript code generator.
 * Haskell (GHC) flavor.
 *
 * In GHC primitive functions are strict and saturated, they can be inlined any time.
 */

import SaplParser, Text.StringAppender, Data.Map

builtInFunctions_fHS :: Map String (String, Arity)

inlineFunctions_fHS :: Map String (InlineCoderFunc, Arity)