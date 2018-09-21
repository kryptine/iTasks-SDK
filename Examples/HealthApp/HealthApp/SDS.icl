implementation module HealthApp.SDS

import iTasks

import HealthApp.Definition

clientShare :: SDSLens () [Client] [Client]
clientShare = sharedStore "" []