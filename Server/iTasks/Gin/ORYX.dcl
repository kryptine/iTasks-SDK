definition module iTasks.Gin.ORYX

from iTasks import :: JSONNode
from Data.Maybe import :: Maybe
from iTasks.API.Core.SystemTypes import :: InteractionMask, :: VerifyMask, :: VerifyOptions, :: ConsPos, :: StaticVisualizationMode, :: VSt, :: VisualizationResult
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from iTasks.Framework.GenVisualize import generic gVisualizeText, generic gVisualizeEditor
from iTasks.Framework.GenUpdate import generic gUpdate, generic gDefault, generic gHeaders, generic gGridRows
from iTasks.Framework.GenVerify import generic gVerify
from GenEq import generic gEq

import iTasks.Gin.Types
import iTasks.Gin.Syntax
import iTasks.Gin.Parser

:: ORYXEditor =	{ diagram		:: ORYXDiagram
				, stencilset	:: ORYXStencilSetReference
				, errors		:: [ORYXError]
				}

:: ORYXBound =	
	{ x :: !Real
	, y :: !Real
	}

:: ORYXBounds =	
	{ lowerRight	:: !ORYXBound
	, upperLeft		:: !ORYXBound
	}

:: ORYXChildShape =	
	{ resourceId	:: !ORYXResourceId
	, properties	:: !ORYXProperties
	, stencil		:: !ORYXStencilReference
	, childShapes	:: ![ORYXChildShape]
	, outgoing		:: ![ORYXOutgoing]
	, bounds		:: !ORYXBounds
	, dockers		:: ![ORYXDocker]
	, target		:: Maybe ORYXTarget
	}
	
:: ORYXProperties = ORYXProperties [ORYXProperty]

:: ORYXProperty =
	{ key	:: !String
	, value	:: !JSONNode
	}
	
:: ORYXDiagram = 
	{ resourceId	:: !ORYXResourceId
	, properties	:: !ORYXProperties
	, stencil	 	:: !ORYXStencilReference
	, childShapes	:: ![ORYXChildShape]
	, bounds		:: !ORYXBounds
	, stencilset	:: !ORYXStencilSetReference
	, ssextensions	:: ![ORYXStencilSetExtension]
	}
	
:: ORYXDocker = 
	{ x	:: !Real
	, y :: !Real
	}
	
:: ORYXOutgoing =
	{ resourceId	:: !ORYXResourceId
	}
 
:: ORYXResourceId :== String

:: ORYXStencilReference = 
	{ id			:: !ORYXResourceId
	}

:: ORYXStencilSetReference =
	{ url		:: !String
	, namespace	:: !String
	}

:: ORYXStencilSetExtension :== String

:: ORYXTarget =
	{ resourceId	:: !ORYXResourceId
	}
	
:: ORYXError = 
	{ resourceId	:: !ORYXResourceId
	, message		:: !String
	, paramName		:: !Maybe String
	}

// Stencil definition types

derive gEq		 		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive JSONEncode		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive JSONDecode 		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive gVisualizeText	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
//derive gVisualizeHtml	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError TODO
derive gVisualizeEditor	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive gUpdate	    	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
//derive gDefaultMask		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError TODO
derive gVerify  		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive gDefault  		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError

oryxDiagramToGraph :: !Bindings !ORYXDiagram -> GGraph

emptyORYXEditor :: ORYXEditor

petriNetORYXEditor :: ORYXEditor

bpmnORYXEditor :: ORYXEditor

xmasORYXEditor :: ORYXEditor

ginORYXDiagram :: ORYXDiagram

ginORYXEditor :: !ORYXDiagram /*!(ORYXEditor *IWorld -> *(WorldPredicateResult,*IWorld))*/-> ORYXEditor

updateDiagramExtensions :: !GModule -> GModule

makeORYXError :: !ORYXDiagram !(GPath,String) -> ORYXError
