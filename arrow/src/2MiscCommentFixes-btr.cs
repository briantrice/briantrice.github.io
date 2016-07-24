'From Squeak3.1alpha of 7 March 2001 [latest update: #4081] on 21 June 2001 at 7:30:40 pm'!
"Change Set:		2MiscCommentFixes-btr
Date:			18 June 2001
Author:			Brian T. Rice

Adjusted comments to be more clear and up to date."!


!ArrowGraph commentStamp: 'btr 6/17/2001 23:34' prior: 0!
ArrowGraph has been refactored into two classes. This Stub class defines the set-theoretic operations and the minimum set of graph operations with exception to certain ones that are handled regularly according to set-theoretic operations. See UnionGraph and IntersectionGraph etc.

Basically the protocol remaining in ArrowGraphMain is that which set-theoretic operators can delegate to regularly, as well as those protocol elements which should not be inherited by the set-theoretic types, due to e.g. cache manipulation.

It is intended for all graphs to be subclasses of ArrowGraph, to allow for straightforward testing of type. (i.e. someVariable isKindOf: ArrowGraph) However, there should never be direct instances of ArrowGraph.!


!ArrowGraphMain commentStamp: 'btr 6/21/2001 00:15' prior: 0!
Arrows can always be referenced as members of graphs. Graphs are sets of arrows that are intensionally (and lazily) implemented, so that they may contain an infinite number of arrows. The implementation as for access will be to first search the underlying finite set of chosen arrows. If the arrow is not found there, then the graph 'declaration' is consulted (whether implemented by graphs or SmallTalk objects). If neither of these violates the predication of the arrow, then it is added.

Each graph's contents represent a (possibly reflective) relation, providing the fundamental constraint construct for specification.  Graphs are relative, in that they only represent the resulting relation computed by a declarative inference scheme.  In this way, ArrowGraphs are an implementational hack, in that they encode the information of the arrows they replace (which is represented by its MetaGraph).

Note that the entire protocol in a complete Arrow system can be represented and used as graphs. However, graphs are not special as arrow structures in the semantic sense. They are merely a convenient starting point for this implementation. At the implementation level, ArrowGraphs are merely compact notations for MetaGraphs, with Set-membership encoding the meta-arrows. This makes it a compression algorithm somewhat like f(size)->size/2 +1 where n is the number of references in the structure. Basically the apex encodes all of the domainElements (tails) of the meta-arrows encoded. The implementation plan in general is to create more versatile data structures for encoding arrow structures, until the system is expressive enough to allow fully-general arrow encoding of foreign objects and their structure (part of the function of SmalltalkMOP).

An interpretation of the intensional/extensional distinction is the notion of an inductive vs. co-inductive definition. (Perhaps this should be an abstract superclass of an inductive and a co-inductive pair of class branches.) In an inductive graph, an arrow is an element of the graph if and only if some program can generate it. So, arrows in inductively-defined graphs are constructive expressions. In a co-inductive graph, an arrow is an element of the graph until system interactions result in an inconsistency that invalidates that relationship's existence. This basically amounts to backtracking the supposition of the arrow's existence in the first place. So, it's also an indication of the need for laziness in arrow evaluation.

Notice that ArrowGraphs are extensions/annotations of Arrows, in that each ArrowGraph's apex node is the arrow it annotates. The ArrowGraph as Arrow represents the node of the meta-graph that reifies its structure. 'Its structure' means the notion that each arrow in the graph is there because of another relation that holds between an arrow representing that graph conceptually and those arrows as independent entities.

ArrowGraph is also intended to rely heavily on other classes to perform its work, for complexity's sake. So, as many methods are delegations as possible, which results in some very simple expressions in this type. By contrast, Arrow does nearly everything itself.

Structure:
	apex		This is the arrow in the graph's inclusion relation representation (the meta-
				graph). The graph is essentially an annotation of its apex in another sense.
				Should always satisfy the equation:
					graph apex == graph meta apexNode.
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
	Improve the generality and robustness of the interaction between ArrowGraph and MetaGraph. For example, what count for ArrowGraphs should be relative to context, but obviously isn't.!
]style[(4012 13 313 10 176 12 367)f1,f1u,f1,f1u,f1,f1u,f1!


!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 6/21/2001 00:36'!
metaGraph
	"Sugaring; delegation to a graph type."
	^ MetaGraph of: self! !


!MetaGraphGraph class methodsFor: 'instance creation' stamp: 'btr 6/21/2001 00:42'!
of: anArrowGraph 
	"Ensures that only one meta-graph of each type exists per graph 
	instance."
	self
		allInstancesDo: [:eachMeta | anArrowGraph == eachMeta graph
				ifTrue: [^ eachMeta]].
	^ (self new initialize graph: anArrowGraph)
		name: 'A meta-graph of ' , anArrowGraph name! !

