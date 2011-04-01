definition module TaskPanelClientEncode

import JSON, TaskPanel

// encodes a task panel in a format readable by the client
clientEncodeTaskPanel :: !TaskPanel -> JSONNode
