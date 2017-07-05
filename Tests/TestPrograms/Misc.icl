module Misc
import iTasks.Internal.Test.Definition

Start world = execTestSuite (testsuite "Miscellaneous" "All tests that don't fit anywhere else, but should not get lost"
	[jsonDecodeOfJSONField]) world


:: TestRecord =
	{ a :: String
	, b :: JSONNode
	}
derive class iTask TestRecord
derive gPrettyTrace Maybe, TestRecord, JSONNode

jsonDecodeOfJSONField
	= assertEqual "Encoding and decoding of record fields of type JSONNode with value JSONNull"
	  (Just rec)
	  (fromJSON (toJSON rec))
where
	rec = {TestRecord|a = "Foo", b = JSONNull}
