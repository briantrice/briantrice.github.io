'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 23 September 2001 at 10:59:47 pm'!
"Change Set:		8ExtendedCleanUps-btr
Date:			17 September 2001
Author:			Brian T. Rice

Various clean-ups and additions, mostly minor, towards version 0.2."!

Object subclass: #ArrowInteractor
	instanceVariableNames: 'worldGraph maps cursor root '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Interaction'!

!ArrowInteractor commentStamp: '<historical>' prior: 0!
Prototype Arrow-world manipulator.

Structure:
 worldGraph	<ArrowGraph> -- meta-level of the graph namespace
 cursor			<Arrow> -- the location of the current context
 root			<Arrow> -- the location of the 'home' context (to be removed?)

(Add more notes here as the idea becomes more mature.)!


!ArrowGraphMain commentStamp: '<historical>' prior: 0!
Arrows can always be referenced as members of graphs. Graphs are sets of arrows that are intensionally (and lazily) implemented, so that they may contain an infinite number of arrows. The implementation as for access will be to first search the underlying finite set of chosen arrows. If the arrow is not found there, then the graph 'declaration' is consulted (whether implemented by graphs or SmallTalk objects). If neither of these violates the predication of the arrow, then it is added.

Each graph's contents represent a (possibly reflective) relation, providing the fundamental constraint construct for specification.  Graphs are relative, in that they only represent the resulting relation computed by a declarative inference scheme.  In this way, ArrowGraphs are an implementational hack, in that they encode the information of the arrows they replace (which is represented by its GraphInclusionGraph).

Note that the entire protocol in a complete Arrow system can be represented and used as graphs. However, graphs are not special as arrow structures in the semantic sense. They are merely a convenient starting point for this implementation. At the implementation level, ArrowGraphs are merely compact notations for GraphInclusionGraphs, with Set-membership encoding the inclusion-arrows. This makes it a compression algorithm somewhat like f(size)->size/2 +1 where n is the number of references in the structure. Basically the apex encodes all of the domainElements (tails) of the inclusion-arrows encoded. The implementation plan in general is to create more versatile data structures for encoding arrow structures, until the system is expressive enough to allow fully-general arrow encoding of foreign objects and their structure (part of the function of SmalltalkMOP).

An interpretation of the intensional/extensional distinction is the notion of an inductive vs. co-inductive definition. (Perhaps this should be an abstract superclass of an inductive and a co-inductive pair of class branches.) In an inductive graph, an arrow is an element of the graph if and only if some program can generate it. So, arrows in inductively-defined graphs are constructive expressions. In a co-inductive graph, an arrow is an element of the graph until system interactions result in an inconsistency that invalidates that relationship's existence. This basically amounts to backtracking the supposition of the arrow's existence in the first place. So, it's also an indication of the need for laziness in arrow evaluation.

Notice that ArrowGraphs are extensions/annotations of Arrows, in that each ArrowGraph's apex node is the arrow it annotates. The ArrowGraph as Arrow represents the node of the inclusion-graph that reifies its structure. 'Its structure' means the notion that each arrow in the graph is there because of another relation that holds between an arrow representing that graph conceptually and those arrows as independent entities.

ArrowGraph is also intended to rely heavily on other classes to perform its work, for complexity's sake. So, as many methods are delegations as possible, which results in some very simple expressions in this type. By contrast, Arrow does nearly everything itself.

Structure:
	apex		This is the arrow in the graph's inclusion relation representation (the inclusion-
				graph). The graph is essentially an annotation of its apex in another sense.
				Should always satisfy the equation:
					graph apex == graph inclusionMeta apexNode.
	infinitary	Describes whether the graph is infinitary or finite.
				Iteration behavior is controlled by this.
	intension	Collaborates with arrows from SmalltalkMOP to encode meanings of graphs.
				The intension should be an arrow with exactly the necessary information
				that all of the graph's elements satisfy and no more.
	cache		When extensional, the cache is the immutable set of arrows that the graph is
				intended to represent. When intensional, it is treated as a proper cache.
				The cache can be a <Set> or <WeakSet> transparently to other code.

Protocol:
	Set-theoretic:
	&			Intersection.
	|			Union.
	>=			Superset(graph) of.
	<=			Subset(graph) of.
	=			Equals.
	includes:	Has as an element.
	anyOne		Existential quantifier.
	+			Immutable addition of elements.
	>>			Filtering over a block of Smalltalk code.
	intensionallyIncludes:	Whether the argument semantically belongs.

	Relational:
	inv			Inversion.
	**			Composition.
	*			Application to graphs or arrows (wrapped in singleton graphs).
	restrictedTo:	Restricting the domain to those of some set(graph).

	Book-keeping:
	holdWeakly		coerces the cache to be a <WeakSet>
	holdStrongly	coerces the cache to be a <Set>

TODO:
	Reduce my subclasses to instance-specific behavior for my operations' results.
	Improve the generality and robustness of the interaction between ArrowGraph and GraphInclusionGraph. For example, what count for ArrowGraphs should be relative to context, but obviously isn't.!
]style[(4061 13 313 10 176 12 377)f1,f1u,f1,f1u,f1,f1u,f1!

Smalltalk renameClassNamed: #ApplicationGraph as: #GraphApplicationGraph!

!InvertedGraph commentStamp: '<historical>' prior: 0!
I wrap a graph to produce a lazy implementation of graph inversion. My instances' operations delegate to the original properly and generically. Also, each instance is unique per argument graph.

In order for a graph type to delegate to InvertedGraph its #invert operation, it must provide #applyTo: and #invertedApplyTo:. Notice that most methods just replace the use of the argument arrow with its inverse. This works in almost every case.!


!ArrowInteractor methodsFor: 'accessing' stamp: 'btr 9/14/2001 16:33'!
atSelector: anObject 
	"Take any Smalltalk object and 'look up' its wrapper arrow in the 
	domain of the currently-selected graph, and return the result of the 
	application."
	self notYetImplemented! !

!ArrowInteractor methodsFor: 'accessing' stamp: 'btr 9/14/2001 15:25'!
here
	^ cursor! !

!ArrowInteractor methodsFor: 'accessing' stamp: 'btr 9/14/2001 15:24'!
home
	^ root! !

!ArrowInteractor methodsFor: 'initialize-release' stamp: 'btr 9/16/2001 15:40'!
initialize
	root _ Arrow new.
	cursor _ root.
	"worldGraph contains arrows from the current context to the arrows of 
	lookup-accessibility to the next context."
	worldGraph _ GraphInclusionGraph of: (ArrowGraph newFromArrow: root) holdStrongly! !


!ArrowStub methodsFor: 'initialize' stamp: 'btr 9/16/2001 17:10'!
initialize
	"Support for naming: useful for object browsers."
	name _ super name! !


!Arrow methodsFor: 'accessing' stamp: 'btr 9/16/2001 16:34'!
allMetaArrows
	"Answer every allocated arrow that points to me.  
	TODO: abstract arrows should not need to be skipped.  
	TODO: the graph abstraction should not have to be explicitly consulted."
	| temp |
	temp _ WeakSet newFrom: {}.
	Arrow
		allSubInstancesDo: [:each | each class == Arrow
				ifFalse: [(each head = self
							or: [each tail = self])
						ifTrue: [temp add: each]]].
	ArrowGraph
		allSubInstancesDo: [:each | (each includes: self)
				ifTrue: [temp
						add: (each inclusionGraph inclusionArrowFor: self)]].
	^ temp! !

!Arrow methodsFor: 'converting' stamp: 'btr 9/16/2001 14:14'!
asAssociation
	"Take myself as a tail-head pair and make a key-value pair out of it."
	^ Association key: self tail value: self head! !

!Arrow methodsFor: 'converting' stamp: 'btr 9/16/2001 14:12'!
fromAssociation: anAssociation in: anObjectGraph
	^ Arrow fromAssociation: anAssociation in: anObjectGraph! !


!Arrow class methodsFor: 'conversion' stamp: 'btr 9/14/2001 21:50'!
newFromAssociation: anAssociation in: anObjectGraph 
	"Permits the use of dictionaries and the association sugaring to make 
	arrows. Create wrappers by default, to avoid confusion."
	^ Arrow
		from: (Arrow wrapping: anAssociation key in: anObjectGraph)
		to: (Arrow wrapping: anAssociation value in: anObjectGraph)! !


!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:30'!
raisedApplicationTo: anArrow
	"Answer the arrows from the argument to its results lazily."
	^ (GraphInclusionGraph of: self * anArrow) apex: anArrow! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:32'!
raisedInverseApplicationTo: anArrow 
	"Answer the arrows from the argument to its results lazily."
	^ (GraphInclusionGraph of: self inv * anArrow)
		apex: anArrow! !


!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/16/2001 15:12'!
applyToGraph: anArrowGraph 
	"Generalizes #applyTo: so that a graph can be applied to all the elements 
	of another graph. I always answer a new graph, and always cache any possible values."
	^ (GraphApplicationGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph in: self world objects)) cache: (ArrowGraph new beExtensional cache: cache) * (ArrowGraph new beExtensional cache: anArrowGraph cache)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/14/2001 21:55'!
graphCompose: anArrowGraph 
	"Answer a graph of arrows which are those resulting from all possible  
	compositions of arrows from the receiver and argument respectively. 
	#asGraph coerces arrows to singleton graphs."
	^ GraphCompositionGraph newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/16/2001 15:29'!
invertedApplyTo: anArrow 
	"A method for UNapplying a graph as a function to an arrow. This  
	amounts to returning a graph of all arrows referenced by the tails of  
	those arrows whose heads reference the argument. This implementation  
	is the most generic case of graph-application. If the graph is  
	intensional, coerce the arrow to a singleton graph and re-cast the call."
	self isExtensional
		ifTrue: [^ ArrowGraph new
				cache: (cache
						select: [:each | each codomainElement == anArrow]
						thenCollect: [:each | each domainElement])].
	^ self invertedApplyToGraph: anArrow asGraph! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/16/2001 15:31'!
invertedApplyToGraph: anArrowGraph 
	"Generalizes #invertedApplyTo: so that a graph can be applied to all the  
	elements of another graph. I always answer a new graph, and will always cache already-knowable results."
	^ (GraphApplicationGraph
		newFromArrow: (Arrow newFromAssociation: anArrowGraph -> self in: self world objects))
		cache: (ArrowGraph new beExtensional cache: cache) inv
				* (ArrowGraph new beExtensional cache: anArrowGraph cache) inv! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/14/2001 22:01'!
restrictedTo: anArrowGraph 
	"The argument acts as a filter over the set of nodes (arrows referred to  
	by the receiver's arrows) that the receiver applies to."
	^ FilteredOverGraph newFromArrow: (Arrow newFromAssociation: anArrowGraph -> self in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/16/2001 15:32'!
successorsOf: anArrow 
	"Answer the nodes following the given node using my arrows by one  
	step."
	^ self * anArrow! !


!GraphApplicationGraph methodsFor: 'accessing' stamp: 'btr 9/14/2001 21:52'!
argument
	^ self apex head object! !

!GraphApplicationGraph methodsFor: 'accessing' stamp: 'btr 9/14/2001 21:52'!
operator
	^ self apex tail object! !


!GraphCompositionGraph methodsFor: 'accessing' stamp: 'btr 9/14/2001 21:53'!
firstArg
	^ apex tail object! !

!GraphCompositionGraph methodsFor: 'accessing' stamp: 'btr 9/14/2001 21:53'!
secondArg
	^ apex head object! !


!MetaGraphGraph class methodsFor: 'instance creation' stamp: 'btr 9/16/2001 16:46'!
of: anArrowGraph 
	"Sugaring."
	^ (self new initialize graph: anArrowGraph)
		name: 'A one-to-one projection of ' , anArrowGraph name! !


!InvertedGraph class methodsFor: 'instance creation' stamp: 'btr 9/16/2001 16:45'!
of: anArrowGraph 
	"Ensures that only one meta-graph of this type exists per graph  
	instance."
	self
		allInstancesDo: [:eachMeta | anArrowGraph == eachMeta graph
				ifTrue: [^ eachMeta]].
	^ (self new initialize graph: anArrowGraph)
		name: 'A meta-graph of ' , anArrowGraph name! !

ArrowGraphMain removeSelector: #sucessorsOf:!

!Arrow class reorganize!
('accessing' null unspecified)
('conversion' newFromAssociation:in:)
('instance creation' from: from:to: head:tail: to: wrapping:in:)
!

Smalltalk removeClassNamed: #MappingGraph!
