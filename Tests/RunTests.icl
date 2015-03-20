module RunTests

import qualified iTasks as i
import gast

Start = ppResults (TestList [] allTests)
  where
  ppResults = foldr (\x acc -> x +++ "\n" +++ acc) ""

allTests =
  [ \x -> x == True ]
