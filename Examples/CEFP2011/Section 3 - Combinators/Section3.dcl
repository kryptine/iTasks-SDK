definition module Chapter3

// Examples showing the usage of frequently used iTask combinators

import iTasks

flows3   :: [Workflow]

show     :: (Task a) -> Task a | iTask a

positive :: Task Int
