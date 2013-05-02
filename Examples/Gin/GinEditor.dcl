definition module GinEditor

from iTasks import :: Task, :: WorkflowContainer

import iTasks.Gin.Config
import iTasks.Gin.Syntax
import iTasks.API.Extensions.Gin.Domain

getConfig :: Task GinConfig
ginEditor :: WorkflowContainer Void
