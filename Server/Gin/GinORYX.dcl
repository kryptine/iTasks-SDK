definition module GinORYX

import GenEq
import JSON

from iTasks import ::JSONNode, ::VerSt, ::UpdateMask, ::USt, ::UpdateMode, ::VSt, ::Visualization
from iTasks import class iTask, generic gVisualize, generic gUpdate, generic gDefaultMask, generic gVerify, generic JSONEncode, generic JSONDecode, generic gEq

from GinSyntax import ::GDeclaration, ::GModule, ::GGraph, ::GImport, ::Binding, ::Bindings

::ORYXEditor = { diagram	:: ORYXDiagram
			   , stencilset	:: ORYXStencilSetReference
			   
			   , toString	:: (ORYXEditor -> String)
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

// Stencil definition types

derive gEq		 		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget
derive JSONEncode		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget
derive JSONDecode 		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget
derive gVisualize  	 	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget
derive gUpdate	    	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget
derive gDefaultMask		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget
derive gVerify  		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget

oryxDiagramToGraph :: !Bindings !ORYXDiagram -> GGraph

newGinORYXDiagram :: ORYXDiagram

newORYXDiagram :: ORYXStencilSetReference -> ORYXDiagram

petriNetORYXEditor :: ORYXEditor

bpmnORYXEditor :: ORYXEditor

ginORYXEditor :: ![GImport] ORYXDiagram -> ORYXEditor

updateDiagramExtensions :: !GModule -> GModule
