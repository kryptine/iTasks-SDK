definition module Incidone.DeviceBased.VideoWall

import iTasks
import Incidone.OP.Concepts

//This type represents the
:: WallContent
    = WallClock
    | WallOverview ContactMapPerspective
    | WallContactSummary (Maybe ContactNo)
    | WallIncidentSummary (Maybe IncidentNo)
    | WallCountDown DateTime

derive class iTask WallContent

//Current content of the video wall
wallContent :: Shared WallContent

//View content that was selected for viewing on the video wall
viewVideoWallContent :: Task WallContent
