definition module iTasks.Internal.Client.Serialization

/**
 * This module provides functions that serialize Clean values to send to a
 * browser client running Clean in the WebAssembly interpreter. The values are
 * serialized using GraphCopy after which the descriptors are replaced by the
 * descriptors of the client.
 */

from Data.Error import :: MaybeError, :: MaybeErrorString
from iTasks.Internal.IWorld import :: IWorld
from iTasks.UI.Editor import :: VSt

/**
 * Serialize an expression to send to a WebAssembly client.
 * @param The expression to serialize.
 * @result `Error` if the bytecode could not be loaded (which may happen due to
 *   incorrect project settings); otherwise a serialized string.
 */
serializeForClient :: f !*IWorld -> *(!MaybeErrorString String, !*IWorld)

/**
 * Like `serializeForClient`, but in a `VSt`.
 */
serializeInVSt :: f !*VSt -> *(!String, !*VSt)
