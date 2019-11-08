implementation module iTasks.Util.DeferredJSON

import StdString
import iTasks.Internal.Generic.Visualization
import Text.GenJSON
import Data.GenDefault
import Data.GenEq
import Data.Maybe
import Data.Func, Data.Functor

instance toString DeferredJSON where
    toString (DeferredJSON x)        = toString $ toJSON x
    toString (DeferredJSONNode json) = toString json

fromDeferredJSON :: !DeferredJSON -> Maybe a | TC, JSONDecode{|*|} a
fromDeferredJSON (DeferredJSON x) = case dynamic x of
    (x :: a^) -> Just x
    _         -> Nothing
fromDeferredJSON (DeferredJSONNode json)  = fromJSON json

JSONEncode{|DeferredJSON|} _ (DeferredJSON a)
	= JSONEncode{|*|} False a
JSONEncode{|DeferredJSON|} _ (DeferredJSONNode json)
	= [json]

JSONDecode{|DeferredJSON|} _ []
	= (Just (DeferredJSONNode JSONNull), [])
JSONDecode{|DeferredJSON|} _ [x:xs]
	= ((Just (DeferredJSONNode x)), xs)
JSONDecode{|DeferredJSON|} _ l
	= (Nothing, l)

gEq{|DeferredJSON|} x y = toJSON x === toJSON y
gText{|DeferredJSON|} f djson = gText{|*|} f $ toJSON <$> djson
