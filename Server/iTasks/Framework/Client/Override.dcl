definition module iTasks.Framework.Client.Override

import StdDynamic, iTasks.API.Core.Types

// The functions of this module have different implementation
// at clint side. It is achieved by excluding this module from linking.

cast :: a -> b | TC a & TC b
cast_to_TaskValue :: a -> TaskValue b | TC a & TC b

unwrapTask :: Dynamic -> Task a | TC a



