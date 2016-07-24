'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 23 September 2001 at 10:12:52 pm'!
"Change Set:		11MetaToInclusion
Date:			17 September 2001
Author:			Brian T. Rice

Changes the name of MetaGraph and its relatives to use the more specific and descriptive term InclusionGraph."!

Smalltalk renameClassNamed: #BaseToMetaGraph as: #BaseToInclusionGraph!

!BaseToInclusionGraph commentStamp: '<historical>' prior: 0!
This graph type encapsulates ArrowGraph>>inclusionGraph and delegates to GraphInclusionGraph to manage the results.!

Smalltalk renameClassNamed: #MetaGraph as: #GraphInclusionGraph!

!GraphInclusionGraph commentStamp: '<historical>' prior: 0!
This graph type's arrows represent the relationship that constitute what other graphs are, conceptually. So, all the arrows should lead from one arrow representing the graph to all the arrows which are elements of the graph. This is something that should only be done lazily. Technically, it counts as the graph type of #includes: from ArrowGraph, however iteration over the graph's elements is not always provided except lazily (since it would be impossible to handle in general).

Alternatively, the InclusionGraph could be strictly specified, with the graph generated lazily from the InclusionGraph's information.

Instance Variables:
	baseGraph -- the object-level ArrowGraph which structure this graph represents.!

Smalltalk renameClassNamed: #MetaGraphSansBase as: #InclusionGraphSansBase!

!ArrowGraph methodsFor: 'converting' stamp: 'btr 9/16/2001 15:41'!
asInclusionStructure
	"Provide a downward method of reifying an arrow structure when  
	taking it to be the inclusion-graph (or structure in general) of the resulting graph."
	^ self as: InclusionGraphSansBase! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:36'!
inclusionArrowFor: anArrow 
	"Answer the arrow in the meta-graph defining the argument's  
	membership."
	^ self inclusionGraph inclusionArrowFor: anArrow! !


!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/16/2001 15:37'!
inclusionGraph
	"Sugaring; delegation to a graph type."
	^ GraphInclusionGraph of: self! !


!BaseToInclusionGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:38'!
applyTo: anArrowGraph 
	"Answer the meta-level representation of the argument."
	^ GraphInclusionGraph of: anArrowGraph! !

!BaseToInclusionGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:38'!
invertedApplyTo: anArrowGraph 
	anArrowGraph class == GraphInclusionGraph
		ifTrue: [^ anArrowGraph graph].
	"^ anArrowGraph asMetaGraph graph"
	"The previous line is a correct intent, but the expression does not seem to 
	have the right semantics."
	^ self notYetImplemented! !

!BaseToInclusionGraph methodsFor: 'testing' stamp: 'btr 9/16/2001 15:38'!
intensionallyIncludes: anArrow 
	"All arrows leading from graphs to their meta-graphs are members."
	(anArrow domainElement isKindOf: ArrowGraph)
		ifFalse: [^ false].
	^ self intensionallyIncludes: anArrow using: #inclusionGraph! !


!GraphInclusionGraph methodsFor: 'accessing' stamp: 'btr 9/19/2001 20:23'!
apexNode
	"The apex of this graph is the base graph's apex arrow initialized when 
	setting up the graph-to-inclusion-graph relationship."
	^ apex! !

!GraphInclusionGraph methodsFor: 'accessing' stamp: 'btr 9/16/2001 16:34'!
inclusionArrowFor: anArrow 
	"Returns the first (and only?) arrow whose head references the  
	argument."
	(graph includes: anArrow)
		ifFalse: [^ nil].
	^ cache
		detect: [:eachArrow | eachArrow codomainElement == anArrow]
		ifNone: [self add: self apexNode -> anArrow]! !

!GraphInclusionGraph methodsFor: 'accessing' stamp: 'btr 9/19/2001 20:24'!
inclusionArrowFor: anArrow WRT: anArrowGraph 
	"Handles #inclusionArrowFor: for any graph's inclusion-graph by 
	delegating to it."
	anArrowGraph == graph
		ifTrue: [^ self inclusionArrowFor: anArrow].
	^ (GraphInclusionGraph of: anArrowGraph)
		inclusionArrowFor: anArrow! !

!GraphInclusionGraph methodsFor: 'adding' stamp: 'btr 9/19/2001 20:24'!
add: anArrow 
	"Provides a hook for using inclusion-graph protocol to actually instantiate the base graph, in cases where the inclusion-graph is intended to drive the definition instead of reifying an existing graph."
	graph
		ifNil: [super add: anArrow]
		ifNotNil: [graph apex == anArrow domainElement
				ifTrue: [graph add: anArrow codomainElement.
					super add: anArrow]].
	^ anArrow! !

!GraphInclusionGraph methodsFor: 'comparing' stamp: 'btr 9/19/2001 20:25'!
% anArrowGraph 
	"Sugaring."
	^ self isInclusionGraphOf: anArrowGraph! !

!GraphInclusionGraph methodsFor: 'comparing' stamp: 'btr 9/19/2001 20:24'!
isInclusionGraphOf: anArrowGraph 
	"Shortcut super's method with a quick check against the base graph."
	self isInitialized
		ifFalse: [^ false].
	^ anArrowGraph == graph! !

!GraphInclusionGraph methodsFor: 'initialize' stamp: 'btr 9/16/2001 15:39'!
graph: anArrowGraph 
	"Initializes me or returns a new (possibly non-unique) instance that  
	covers a certain ArrowGraph."
	graph
		ifNil: [graph _ anArrowGraph.
			apex _ graph apex.
			^ self].
	^ GraphInclusionGraph of: anArrowGraph! !

!GraphInclusionGraph methodsFor: 'operations' stamp: 'btr 9/19/2001 20:26'!
applyTo: anArrow 
	"The only arrow that an inclusion graph can be applied to effectively is the apex node of the graph. Any other case is not handled yet."
	self apexNode == anArrow
		ifTrue: [^ graph].
	^ ArrowGraph bottom! !

!GraphInclusionGraph methodsFor: 'operations' stamp: 'btr 9/19/2001 20:26'!
invertedApplyTo: anArrow 
	"The only arrow that an inclusion graph can answer is the apex node of the graph. Any arrow in the base graph results in it."
	^ (graph includes: anArrow)
		ifTrue: [SingletonGraph of: self apexNode]
		ifFalse: [ArrowGraph bottom]! !


!InclusionGraphSansBase methodsFor: 'comparing' stamp: 'btr 9/19/2001 20:25'!
% anArrowGraph 
	"Sugaring."
	^ self isInclusionGraphOf: anArrowGraph! !

!InclusionGraphSansBase methodsFor: 'comparing' stamp: 'btr 9/19/2001 20:25'!
isInclusionGraphOf: anArrowGraph 
	"The #graph method lazily generates but does not cache its argument, so 
	this method's semantics are somewhat broken. TODO: fix this."
	^ self graph = anArrowGraph! !

!InclusionGraphSansBase methodsFor: 'initialize' stamp: 'btr 9/16/2001 15:39'!
graph: anArrowGraph 
	"Deprecated? A protected mutator. If you have an actual graph to make  
	an inclusion-graph from, this gives you a real inclusion-graph, not a pretender like GraphInclusionGraphSansBase."
	anArrowGraph = self graph
		ifTrue: [^ self].
	^ GraphInclusionGraph of: anArrowGraph! !

!InclusionGraphSansBase methodsFor: 'operations' stamp: 'btr 9/16/2001 15:51'!
applyTo: anArrow 
	"If applied to my apexNode, return the graph that my arrows specify."
	anArrow == apexNode
		ifTrue: [^ self graph].
	^ ArrowGraph bottom! !

!InclusionGraphSansBase methodsFor: 'operations' stamp: 'btr 9/16/2001 16:02'!
invertedApplyTo: anArrow 
	"All my arrows lead from my apexNode. This performs a simple check 
	against the cache."
	cache
		do: [:each | each codomainElement == anArrow
				ifTrue: [^ SingletonGraph of: apexNode]].
	^ ArrowGraph bottom! !

InclusionGraphSansBase removeSelector: #isMetaGraphOf:!
GraphInclusionGraph removeSelector: #isMetaGraphOf:!
GraphInclusionGraph removeSelector: #metaArrowFor:!
GraphInclusionGraph removeSelector: #metaArrowFor:WRT:!
ArrowGraphMain removeSelector: #metaGraph!
ArrowGraph removeSelector: #asMetaStructure!
