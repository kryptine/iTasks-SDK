definition module TUIEncode

import JSON, TUIDefinition, TUIDiff

// encodes a tui definition in a format readable by the client
encodeTUIDefinition :: !TUIDef -> JSONNode

// encodes a set of updates in a format readable by the client
encodeTUIUpdates :: ![TUIUpdate] -> JSONNode