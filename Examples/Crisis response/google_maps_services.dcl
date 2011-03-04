definition module google_maps_services

import iTasks

reverse_geocoding :: !String !String !Bool !String !(String -> a) -> Task (ReadOnlyShared (Maybe a)) | iTask a

