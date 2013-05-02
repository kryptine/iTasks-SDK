definition module StdMaybe

import Data.Maybe

//TODO: Fix uniqueness annotations in Functor such that mapMaybe :== fmap
mapMaybe :: .(.x -> .y) !(Maybe .x) -> Maybe .y
