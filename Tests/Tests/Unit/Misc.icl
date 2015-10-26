implementation module Tests.Unit.Misc
import TestFramework

testMisc :: TestSuite
testMisc = testsuite "Miscellaneous" "All tests that don't fit anywhere else, but should not get lost"
	[jsonDecodeOfJSONField]


:: TestRecord =
	{ a :: String
	, b :: JSONNode
	}
derive class iTask TestRecord

jsonDecodeOfJSONField
	= assertEqual "Encoding and decoding of record fields of type JSONNode with value JSONNull"
	  (Just rec)
	  (fromJSON (toJSON rec))
where
	rec = {TestRecord|a = "Foo", b = JSONNull}
