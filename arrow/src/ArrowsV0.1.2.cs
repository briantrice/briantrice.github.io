'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 9 February 2002 at 12:39:45 pm'!
"Change Set:		ArrowsV0.1.2
Date:			9 February 2002
Author:			Brian T. Rice

This release compiles everything through change-set number 22. Applying those change-sets consecutively has been tested very thoroughly, but this combined release is considered beta. Some major features and safety and optimization tweaks were added. Until 0.2 comes out, use the individual change sets to find what's different."!

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

Object subclass: #ArrowStub
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Core'!

!ArrowStub commentStamp: 'btr 10/17/2001 06:37' prior: 0!
!

ArrowStub subclass: #Arrow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Core'!

!Arrow commentStamp: '<historical>' prior: 0!
Arrows reference exactly two objects that must be other arrows.  Arrows are instantiated (as extensional entities) for user-choice purposes only.  This includes calculations and user-interface choices.

References are actually implemented as Reference objects, which are containers for the actual objects desired.  This allows self-reference by arrows and reference on a graph that contains the particular arrow without causing the VM to go into an infinite recursion (this is a Smalltalk-specific problem; Self avoids this primitively).
Also, arrows are immutable: all mutation operations beyond initialization return new instances. (However, arrows are not guaranteed to have unique values, except with respect to certain contexts.)

Instance variables:
	Just to be clear, an arrow "looks" like this:
		tail -> head
	Instantiate it like this:
		t->h, or
		t=>h, or
		t@h, or
		Arrow head: h tail: t, or
		Arrow from: t to: h
	which give you an arrow leading from 't' to 'h'.
	Abstract arrows are made by sending the message 'new' to the class 'Arrow':
		Arrow new
	use it when you don't care what its values are or do not consider it constructively.

NOTE: @ is included for Smalltalk sugaring (due to similarity to Point/Number>>@), and is not necessary.

Protocol:
	isIdentity		identity
	inv				invert
	++				compose
	-> or @			pair arrows as an arrow; analogous to CONS
	*				apply with a graph
	raiseHead		reify the head reference as an arrow
	raiseTail		reify the tail reference as an arrow
	raiseIdentity	reify the identity of the arrow as a node

	coincidesWith:coords:	Provides the co-incidence relationship. x R<i,j> y IFF x[i]==y[j].
	incidesUpon:coord:		Provides the incidence relationship. x R<i> y IFF x[i]==y.

	domainElement		tail		graph-application synonym
	codomainElement	head	graph-application synonym

	source				state-transition synonym for tail
	target, destination	state-transition synonyms for head
!
]style[(1521 404)f1,f1cblack;!

Arrow class
	instanceVariableNames: ''!
ArrowStub subclass: #ArrowFrame
	instanceVariableNames: 'arrows assertions compositions graphs heads inversions nodes objects tails values '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!

!ArrowFrame commentStamp: '<historical>' prior: 0!
An ArrowFrame is the closure of information regarding all the elements of a graph that it wraps. It provides the base model of functionality for arrows and graphs by managing their information's implementation transparently. In arrow logic, a frame is the set of knowledge of the ++, inv, and Identity relations over some set of arrows W (i.e. F(W)=(W,I,R,C)). This implementation attempts to do a similar thing for reflective arrow systems in a way that allows for serious flexibility in specification source.

This class is (currently) a singleton whose sole instance is the class variable Instance and which is accessible as 'ArrowWorld' or as 'ArrowFrame lobby'.

However, it is more crucial to the meaning of this type that it manage context-related activities. Basically, this type records information about restrictions in context which makes quicker work of brute-force searches performed as arrow queries.

Instance variables:
	arrows		-<ArrowGraph> that is the basis of the world
	assertions	-<AssertionGraph> extensional graph to provide a place to store facts and
			queries (unverified facts)
	compositions	-<CompositionGraph> denoting the compositions possible in the given world
	graphs		-mapping from apex arrows to graphs that the frame acknowledges;
			without this, graphs have no place in the arrow world
	heads, tails	-<HeadGraph> and <TailGraph> for the world
	inversions	-<InversionGraph> denoting the inversion relations possible in the given world
	nodes		-<MonoidGraph> that constitutes the ground type for the particular world
	objects		-<ObjectGraph> that manages the frame's Smalltalk MOP facility
	values		-<SquareGraph> that constitutes the value representation for the world

TODO:
	Clarify the role of 'graphs'.
	This implementation is completely blind to the notion of multiple worlds. While the only intent of this class is to handle the Smalltalk business of tracking Arrow objects image-wide, it should be friendly to the notion of taking some graph-relationship over it as another virtual world. Using the term 'virtual' however, is not to mean to deprecate its information value WRT the ArrowWorld. It often should allow for more arrows to exist than allocated, giving single arrow objects (regardless of uniqueness of value per object) multiple identities. So...
	(1) Implement a world-creation protocol.
	(2) Implement a reasonable policy for world-inclusion and de-resolution.!
]style[(95 2069 4 250)f1c138036000,f1,f1u,f1!

ArrowStub subclass: #ArrowGraph
	instanceVariableNames: 'apex frame infinitary intension cache '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!

!ArrowGraph commentStamp: 'btr 10/10/2001 04:52' prior: 0!
ArrowGraph has been refactored into two classes. This Stub class defines the set-theoretic operations and the minimum set of graph operations with exception to certain ones that are handled regularly according to set-theoretic operations. See UnionGraph and IntersectionGraph etc.

Basically the protocol remaining in ArrowGraphMain is that which set-theoretic operators can delegate to regularly (that protocol that can be distributed over set operations), as well as those protocol elements which should not be inherited by the set-theoretic types, due to e.g. cache manipulation.

It is intended for all graphs to be subclasses of ArrowGraph, to allow for straightforward testing of type. (i.e. someVariable isKindOf: ArrowGraph) However, there should never be direct instances of ArrowGraph.!

ArrowGraph subclass: #ArrowGraphMain
	instanceVariableNames: 'domain codomain '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!

!ArrowGraphMain commentStamp: 'btr 9/28/2001 03:15' prior: 0!
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
	/\			Intersection.
	\/			Union.
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
	\			Left Residual of composition.
	/			Right Residual of composition.
	!!>			Left Conjugate of composition.
	<!!			Right Conjugate of composition.
	*			Application to graphs or arrows (wrapped in singleton graphs).
	restrictedTo:	Restricting the domain to those of some set(graph).

	Book-keeping:
	holdWeakly		coerces the cache to be a <WeakSet>
	holdStrongly	coerces the cache to be a <Set>

TODO:
	Reduce my subclasses to instance-specific behavior for my operations' results.
	Improve the generality and robustness of the interaction between ArrowGraph and GraphInclusionGraph. For example, what count for ArrowGraphs should be relative to context, but obviously isn't.!
]style[(4061 13 315 10 322 12 377)f1,f1u,f1,f1u,f1,f1u,f1!

ArrowGraphMain subclass: #ApplyToGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Unfinished'!

!ApplyToGraph commentStamp: '<historical>' prior: 0!
This graph object should abstractly encapsulate the functionality of the #applyTo: protocol of ArrowGraph and subclasses. Basically, it reifies and represents graph-applications to arrows as arrows themselves. Instances of this graph represent single applications.

TODO:
	make sure that it delegates to #applyTo: properly, in order to have, say, metagraphs return their base graphs whenever they are applied to their apex nodes.!


!BaseToInclusionGraph commentStamp: '<historical>' prior: 0!
This graph type encapsulates ArrowGraph>>inclusionGraph and delegates to GraphInclusionGraph to manage the results.!

ArrowGraphMain subclass: #BinaryOperatorGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!BinaryOperatorGraph commentStamp: 'btr 9/26/2001 12:13' prior: 0!
Lightweight abstract class for graphs that operate on a pair of graphs. This graph type is an annotation of the arrow-pair of those graphs.!

ArrowGraphMain subclass: #CategoryGraph
	instanceVariableNames: 'nodes identities compositions '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Unfinished'!

!CategoryGraph commentStamp: '<historical>' prior: 0!
Graphs to represent categories must have special semantics: every arrow must be over a set of nodes that the category graph knows, and no meta-arrows of those arrows can be part of it. Also, the category contains one special identity arrow over each node, so 'identities' and 'nodes' are one-for-one to represent identity mappings. Of course there may be other identity arrows over nodes which are not identity mappings and so are not part of 'identities'. Finally, any composition of two of its arrows must also belong to the category. However, equality of compositions must be supposed and proven or refuted through the use of the 'compositions' graph.!

Arrow subclass: #ConcreteArrow
	instanceVariableNames: 'tail head '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Core'!

!ConcreteArrow commentStamp: '<historical>' prior: 0!
This type represents arrows made constructively, where the head and tail references are already known. This makes them usable by recursive-access algorithms, but much more difficult to use (without the abstract protocol) for equational pattern matching. Users should not have to manipulate this class explicitly.

Any method directly accessing the head and tail slots must be here.

NOTE about the implementation: ConcreteArrow code is entirely independent of even the existence of ArrowGraph and its variants except where syntactic sugaring is provided to make things simpler to express for the user. In every case, however, ConcreteArrow is only responsible for its own instances and basic operations.
!
]style[(382 49 273)f1,f1cblack;,f1!

ArrowGraphMain subclass: #EpsilonGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Unfinished'!

!EpsilonGraph commentStamp: '<historical>' prior: 0!
This class should be the basis for a "give me one of these" type statement: an existential quantification. See ArrowGraph>>anyOne.!

Arrow subclass: #EquationalArrow
	instanceVariableNames: 'tailsTo headsTo '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Unfinished'!

!EquationalArrow commentStamp: 'btr 10/14/2001 14:53' prior: 0!
Variant of an abstract arrow which knows arrows that point to it and register themselves for later usage. This is meant to improve query results through extended caching.

The implementation also enforces abstraction of the head and tail references as arrows.!

ArrowGraphMain subclass: #FilteredObjectGraph
	instanceVariableNames: 'objectGraph testBlock '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-SmalltalkMOP'!

!FilteredObjectGraph commentStamp: '<historical>' prior: 0!
This graph type represents lazy results of queries on wrapped objects. This class has a strange place in the Arrow class hierarchy because it is a subgraph of its ObjectGraph while at the same time should not inherit from it and is not a proper MetaObjectGraph since it does not hold an operation over WrapperArrows' objects. See also <FilteredGraph>. However, note that FilteredObjectGraph differs from FilteredGraph in requiring that it's arrows be wrappers within the objectGraph originally filtered, and not any intermediate filteredObjectGraph.

Filters are created from any graph and their testBlocks are composed via the >> selector.

Structure:
	objectGraph	<ObjectGraph> or <FilteredObjectGraph> which is the operand of this
			filter. The accessor method always returns the original ObjectGraph in the
			filter chain.
	testBlock	Determines membership by the call 'testBlock value: anArrow object'.
		Example: If all objects of class OrderedCollection are desired, I can use:
			testBlock _ [:object | object class == OrderedCollection]
			so that the mechanism is basically a lazy filter or set comprehension.!
]style[(336 13 771)f1,f1LFilteredGraph Comment;,f1!

ArrowGraphMain subclass: #FilteredOverGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!

!FilteredOverGraph commentStamp: '<historical>' prior: 0!
My graph type implements the #restrictedTo: protocol of ArrowGraphs, which produces subsets of the graph over a restricted set of nodes.!

Smalltalk renameClassNamed: #ApplicationGraph as: #GraphApplicationGraph!

!GraphApplicationGraph commentStamp: 'btr 10/9/2001 23:45' prior: 0!
This is graph type holds the results of graph applications. It is responsible for ensuring the laziness of the operation by never allocating arrows until they are absolutely necessary. It should keep its results consistent with updates to the original graph. Instances of this graph type represent the result of single applications.

The apex node holds my argument list. The tail is the operator and the head is the argument.!

BinaryOperatorGraph subclass: #GraphLeftConjugateGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!GraphLeftConjugateGraph commentStamp: 'btr 9/28/2001 02:46' prior: 0!
This graph type is the left residual (!!>) of graph composition. This means that its arrows are those that lead from the ends of arrows in the left graph of the composition to the ends of the result graph of the composition.

To put it simply, A !!> C is the graph that completes the composition equation A ** B <= C.

This graph type has a dual, GraphRightConjugateGraph.!

BinaryOperatorGraph subclass: #GraphLeftQuotientGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!GraphLeftQuotientGraph commentStamp: 'btr 9/26/2001 13:04' prior: 0!
This graph type is the left residual (\) of graph composition. This means that its arrows are those that lead from the ends of arrows in the left graph of the composition to the ends of the result graph of the composition.

To put it simply, A \ C is the graph that completes the composition equation A ** B <= C.

This graph type has a dual, GraphRightQuotientGraph.!

BinaryOperatorGraph subclass: #GraphRightConjugateGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!GraphRightConjugateGraph commentStamp: 'btr 9/28/2001 02:49' prior: 0!
This graph type is the right residual (<!!) of graph composition. This means that its arrows are those that lead from the starts of arrows in the result graph of the composition to the starts of the right graph of the composition.

To put it simply, A <!! C is the graph that completes the composition equation A ** B <= C.

This graph type has a dual, GraphLeftConjugateGraph.!

BinaryOperatorGraph subclass: #GraphRightQuotientGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!GraphRightQuotientGraph commentStamp: 'btr 9/26/2001 13:09' prior: 0!
This graph type is the right residual (/) of graph composition. This means that its arrows are those that lead from the starts of arrows in the result graph of the composition to the starts of the right graph of the composition.

To put it simply, C / B is the graph that completes the composition equation A ** B <= C.

This graph type has a dual, GraphLeftQuotientGraph.!

ArrowGraphMain subclass: #InductionGraph
	instanceVariableNames: 'kernel monoid '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!

!InductionGraph commentStamp: '<historical>' prior: 0!
This graph type provides a recursive single-threaded test for membership. It should be isomorphic to the natural numbers, and the class supports conversion to and from Smaltlalk naturals. There is basic support for variables instantiated by #anyOne. Note that this class is odd in that it is more concerned with its nodes than with its arrows. However, it's purpose is to encapsulate information over them in a modular way (multiple induction graphs can deal with the same (immutable) objects cleanly).

This is implemented as a relation over a MonoidGraph (actually its frame's ObjectGraph).
!

ArrowGraphMain subclass: #MetaFrameGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!

!MetaFrameGraph commentStamp: '<historical>' prior: 0!
This abstract graph type is the kind for meta-graphs of ArrowFrames, which are used to encode systematic knowledge about a particular universe of arrow information. So, all subclasses of this type encode the various basic relationships among arrows, and also must know of the other relationships among the same arrows. If the graph type does not need to interact in this way, it should be a subtype of MetaGraphGraph.!

MetaFrameGraph subclass: #AssertionGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!

!AssertionGraph commentStamp: '<historical>' prior: 0!
Dummy class for the use of the assertions slot of frames. If changes are made to this class, evaluate whether they should be rolled back into MetaFrameGraph which is just as useful. Potentially remove this class if differences from MetaFrameGraph are not found useful or necessary, and update the frame code to use it.!

MetaFrameGraph subclass: #CompositionGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!

!CompositionGraph commentStamp: 'btr 9/30/2001 13:39' prior: 0!
This graph type encapsulates the #composeElements protocol for frames. It attempts or rather promises a resulting arrow which is the composition of the arrows found as elements of the given domain arrow.

Notation Convention: Cxyz reads "x composes with y into z" or "x ** y == z"

More abstractly, all my arrows must be over value arrows, whose head and tail refer to the elements of the composition. The semantics of composition should be specified in the Assertions of the frame. Inversions and Identities can be expressed as Compositions, where Ix == Cxxx and Rxy == Cxyx and possibly Cyxx.!

MetaFrameGraph subclass: #HeadGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!

!HeadGraph commentStamp: '<historical>' prior: 0!
This graph type encapsulates the ability to create arrows representing the structure of the head reference of any arrow in the system. See Arrow>>raiseHead.

When an instance is searched, it either finds a meta-head arrow or subsequently asks the arrow itself if it knows.!

MetaFrameGraph subclass: #InversionGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!

!InversionGraph commentStamp: '<historical>' prior: 0!
This graph type encapsulates Arrow>>#inv.!

ArrowGraphMain subclass: #MetaGraphGraph
	instanceVariableNames: 'graph '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!

!MetaGraphGraph commentStamp: '<historical>' prior: 0!
This abstract graph type encapsulates all graph types whose function it is to provide information or operations over another specific graph. Common protocol for those types is collected here.!

MetaGraphGraph subclass: #FilteredGraph
	instanceVariableNames: 'testBlock '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!

!FilteredGraph commentStamp: '<historical>' prior: 0!
This graph type represents lazy results of queries on arrows. See also <FilteredObjectGraph>.

Filters are created from any graph and their testBlocks are composed via the >> selector.

Structure:
	<testBlock>	Determines membership by the call 'testBlock value: anArrow'.
		Example: If all arrows whose heads point to foo are desired, I can use:
			testBlock _ [:value | value head == foo]
			so that the mechanism is basically a lazy filter or set comprehension.!
]style[(72 19 372)f1,f1LFilteredObjectGraph Comment;,f1!


!GraphInclusionGraph commentStamp: '<historical>' prior: 0!
This graph type's arrows represent the relationship that constitute what other graphs are, conceptually. So, all the arrows should lead from one arrow representing the graph to all the arrows which are elements of the graph. This is something that should only be done lazily. Technically, it counts as the graph type of #includes: from ArrowGraph, however iteration over the graph's elements is not always provided except lazily (since it would be impossible to handle in general).

Alternatively, the InclusionGraph could be strictly specified, with the graph generated lazily from the InclusionGraph's information.

Instance Variables:
	baseGraph -- the object-level ArrowGraph which structure this graph represents.!

MetaGraphGraph subclass: #IdentityGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!IdentityGraph commentStamp: '<historical>' prior: 0!
This graph encapsulates Arrow>>#identity functionality.!

MetaGraphGraph subclass: #InvertedGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!InvertedGraph commentStamp: '<historical>' prior: 0!
I wrap a graph to produce a lazy implementation of graph inversion. My instances' operations delegate to the original properly and generically. Also, each instance is unique per argument graph.

In order for a graph type to delegate to InvertedGraph its #invert operation, it must provide #applyTo: and #invertedApplyTo:. Notice that most methods just replace the use of the argument arrow with its inverse. This works in almost every case.!

ArrowGraphMain subclass: #MetaObjectGraph
	instanceVariableNames: 'objectGraph '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-SmalltalkMOP'!

!MetaObjectGraph commentStamp: '<historical>' prior: 0!
This abstract graph type is the kind for meta-graphs of ObjectGraph, which itself is used as a monoid reifying Smalltalk objects. So, all subclasses of this type encode the relationships among objects that define them.

Because ObjectGraph is not implemented as a singleton, this graph type must know the objectGraph that it manages.!

MetaObjectGraph subclass: #ClassInstanceGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-SmalltalkMOP'!

!ClassInstanceGraph commentStamp: '<historical>' prior: 0!
I am the graph type of the class-to-instance relationship.!

MetaObjectGraph subclass: #ClassSubclassGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-SmalltalkMOP'!

!ClassSubclassGraph commentStamp: '<historical>' prior: 0!
I am the graph type of the class-to-subclass (inheritance) relationship.!

MetaObjectGraph subclass: #ClosureResultGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-SmalltalkMOP'!

!ClosureResultGraph commentStamp: '<historical>' prior: 0!
I am the graph type of the mapping of block- and method-closures to their results.

NOTE: The implementation of this type is difficult due to the multiplicity of selector arities. Currently, only completed closures (vice partial closures) are supported. Also, inverse graph application is undecidable in general.!

Arrow subclass: #MonoidArrow
	instanceVariableNames: 'graph '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Core'!

!MonoidArrow commentStamp: '<historical>' prior: 0!
This arrow type is responsible for grounding arrow structures in worlds by knowing which world it belongs to, since it knows which monoid it belongs to. The head and tail are determined dynamically on the basis of that information.!

ArrowGraphMain subclass: #MonoidGraph
	instanceVariableNames: 'kernel '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!

!MonoidGraph commentStamp: '<historical>' prior: 0!
The arrows of this graph type's instances all lead to and from a single arrow, so they all have the same extensional value, and are all identity arrows. What distinguishes any of these arrows from the others is the presence of meta-arrows: arrows that refer to arrows in this graph. (See InductionGraph for an example.)!
]style[(288 14 17)f1,f1LInductionGraph Comment;,f1!

MonoidGraph subclass: #ObjectGraph
	instanceVariableNames: 'closureTo instanceOf subclassOf slotOf '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-SmalltalkMOP'!

!ObjectGraph commentStamp: '<historical>' prior: 0!
This is a MonoidGraph designed to manage the reification of objects into the Arrow system. The objects are the elements of the monoid graph, and the relations that the Smalltalk VM supports over them are collaborating graphs. Arrow identity in this graph is the same as Smalltalk object identity with exceptions for instantiation of arrows within it.

Those graphs should not be manipulated except through this interface.

Also note that the nature of information is such that it does not handle state as usually presented in Smalltalk without a little assistance. For now, all MetaObjectGraph instances work only for objects which do not have or use their variables. Even accessing of variables can lead to code which mutes their values, so basically safety is impossible in that case.!

MetaObjectGraph subclass: #ObjectInstVarGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-SmalltalkMOP'!

!ObjectInstVarGraph commentStamp: '<historical>' prior: 0!
This is the graph type of mappings from objects to the objects which are their instance variables.

These graph instances are also one of the main focal points of mutability concern for the arrow system. Note that ClassSubclass and ClassInstance are just special cases of this graph that are fundamental to evaluation.

TODO: Implement an immutable model of the mutable state centered here, and support it in the system.!

MetaObjectGraph subclass: #PartialClosureResultGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-SmalltalkMOP'!

!PartialClosureResultGraph commentStamp: '<historical>' prior: 0!
This graph type encapsulates all closures, block and method, full and partial, as one arrow abstraction. ClosureResultGraph has an obvious embedding into me.

This is also the first attempt at creating a structured graph, i.e. one where the arrows are themselves only representatives of other compactified arrow data structures (in this case, graphs).!

ArrowGraphMain subclass: #PluggableGraph
	instanceVariableNames: 'selector args block '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!

!PluggableGraph commentStamp: '<historical>' prior: 0!
This graph type should consist of potentially all graphs which generalizes the inductive relationship that one of the Smalltalk methods of Arrow or ArrowGraph to a more equational semantics.

Structure:
	block	a <BlockContext> taking any input/output arrow pair of other arrows and verifying
			that it fulfills the intension of the graph instance.
!

MetaObjectGraph subclass: #PluggableObjectGraph
	instanceVariableNames: 'selector args block '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-SmalltalkMOP'!

!PluggableObjectGraph commentStamp: '<historical>' prior: 0!
Similar to PluggableGraph, but knows its objectGraph and is of course intended as a MetaObjectGraph to handle things like filters over ObjectGraph and its auxiliaries.

Structure:
	block	-<BlockContext> testing for intensional inclusion by sending 'perform: selector withArguments: args' to the domain arrow and comparing with the codomain arrow. This graph type can also be instantiated with an arbitrary block and no selector/args pair.
	selector, args	-<Symbol>, <Array>: Define what basic application is.!

MetaGraphGraph subclass: #ProjectionGraph
	instanceVariableNames: ''
	classVariableNames: 'CodomainResultTraits '
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!ProjectionGraph commentStamp: '<historical>' prior: 0!
This graph type encapsulates one-to-one embeddings from existing graphs-as-sets to new ones. Inversions of this graph type should be fully functional without provision by InvertedGraph for a special case.

TODO: Add a class variable that holds a specialized #anyOne method to give to my instances' #codomain results, since there should be a straightforward answer in that case.!

ArrowGraphMain subclass: #RSTGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Unfinished'!

!RSTGraph commentStamp: '<historical>' prior: 0!
I am the graph type for RST (=Reflexive, Symmetric, Transitive) relations, which include all equivalence relations. In the terms of arrow logic, I represent the class of relativised square frames. My instances' elements therefore must satisfy:
	a _ R anyOne domainElement
	(1) aRa					R includes: (a raiseIdentity)
	(2) aRb <=> bRa			R includes: (R anyOne inv)
	(3) aRb & bRc => aRc	(x ++ y) ifNotNil: [^ R includes: (x ++ y)] ifNil: [^ false]
within the frame that it basically defines (all graphs form frames).

Incidentally, for every graph Q, the union of Q's inverse and its transitive closure is always an equivalence relation.

It is significant that equivalence relations establish a notion of identity of objects within one theory as expressed within some other theory. In this way, RST is an important basis for forming ontologies.

TODO: Implement it. ;)
	Plans: R is the graph representing the desired relation. Queries over this graph basically would perform a lazy search for a proof that a given arrow is an element of it. It is known from logic programming language implementation experience that the search is obviously deterministic, so opening it up is the ultimate goal. In the meantime, however, symmetry of relations is an obvious point where recursive or iterative search could lead to looping non-deterministically.

!

Object subclass: #Reference
	instanceVariableNames: 'value '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-References'!

!Reference commentStamp: '<historical>' prior: 0!
This is a box class for objects (and by extension, arrows and graphs), to help with creating circular structures and with polymorphism with suspensions. These should support lazy evaluation, and the implementation can be shifted to a lazy block closure instead of a box per se.

Reference is the abstract superclass of both Value- and SuspendedReferences. It provides the common behavior for instance-copying, and allows one to instantiate for both lazy and strict cases using the simpler interface here. You should not be playing with direct instances of me.!

Reference class
	instanceVariableNames: ''!
Smalltalk renameClassNamed: #GraphCompositionGraph as: #RelCompositionGraph!
BinaryOperatorGraph subclass: #RelDisjointSumGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!
BinaryOperatorGraph subclass: #RelJoinGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!
BinaryOperatorGraph subclass: #RelProductGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!
BinaryOperatorGraph subclass: #RelSplitGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!
MessageSend subclass: #SafeMessageSend
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-References'!

!SafeMessageSend commentStamp: '<historical>' prior: 0!
Intended as a safe substitute for BlockContexts or Suspensions for lazy references. It basically protects its potentially mutable receiver object and uses a restricted initialization protocol.!

Smalltalk renameClassNamed: #GraphDisjointSumGraph as: #SetDisjointSumGraph!
ArrowGraph subclass: #SetOpsGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Set Ops'!

!SetOpsGraph commentStamp: '<historical>' prior: 0!
Handles some common protocol between Intersection- and UnionGraphs.!

SetOpsGraph subclass: #IntersectionGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Set Ops'!

!IntersectionGraph commentStamp: 'btr 10/12/2001 06:56' prior: 0!
I represent the result of applying the intersection relation onto pairs of graphs, resulting in set-theoretic intersection (for first-order graphs, anyway). /\ operations generate this type.

Whether appropriate or not, it currently uses its apex-node as the argument pairing. TODO: Investigate if this is inline with the rest of the theory and update the theory or implementation as appropriate.!

Smalltalk renameClassNamed: #GraphProductGraph as: #SetProductGraph!
ArrowGraphMain subclass: #SingletonGraph
	instanceVariableNames: 'singleton '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!

!SingletonGraph commentStamp: '<historical>' prior: 0!
This is the graph type of intensionally single-element graphs. It is implemented for very quick access, since it is meant for combining arrows into graphs one-by-one. This is also essential for polymorphic calls to graphs so that they apply to graphs and arrows equally well.!

MetaFrameGraph subclass: #SquareGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!

!SquareGraph commentStamp: '<historical>' prior: 0!
This is a speculative definition of a graph type. It represents pairings of arrows. It is called "square" because the pairing of objects in this graph coincides with the square frame from arrow logic. So, there exists for each two arrows in the domain a unique arrow representing their pairing which is an element of this graph. Furthermore, all pairs of arrows have a unique arrow in this graph. This will likely collaborate with RSTGraph.

What is questionable is the identity of square-pair arrows. They should obviously not be strictly identifiable (via #==) with any arrow that is not in the SquareGraph.
!

Reference subclass: #SuspendedReference
	instanceVariableNames: 'suspension '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-References'!

!SuspendedReference commentStamp: '<historical>' prior: 0!
I am a variant of Reference which uses the instance variable 'value' as a cache for my block closure's return value. I store the block closure as 'suspension', and delegate accessing my value to it on initialization. Instance creation should occur by passing the block to the class, which in turn wraps that block in an instance.
Lazy evaluation is supposed to occur by the following mechanism. 'value' requests should trigger (force) the evaluation of the blocks in each reference. The SuspendedReference forwards 'value' to the block it owns, which in turn should forward it on... however there is no mechanism in the Reference hierarchy yet for ensuring that only the appropriate kinds of blocks are used. One possibility is to meta-program the blocks, providing the interface to the meta-program in the class instantiation protocol. Anyway, SuspendedReferences evaluate to ValueReferences to their answers; this helps ensure that no more than one ValueReference exists for any given object. Naturally, there can be no such protection for SuspendedReferences, since it supposes knowledge of value ahead of time.!

BlockContext variableSubclass: #Suspension
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-References'!
MetaFrameGraph subclass: #TailGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!

!TailGraph commentStamp: '<historical>' prior: 0!
This graph type encapsulates the ability to create arrows representing the structure of the tail reference of any arrow in the system. See Arrow>>#raiseTail.

When an instance is searched, it either finds a meta-tail arrow or subsequently asks the arrow itself if it knows.!

ArrowGraphMain subclass: #TopOrBottomGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Set Ops'!

!TopOrBottomGraph commentStamp: '<historical>' prior: 0!
This abstract graph class represents the top and bottom of the set-theoretic lattice of possible graphs.!

TopOrBottomGraph subclass: #EmptyGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Set Ops'!

!EmptyGraph commentStamp: '<historical>' prior: 0!
This graph is an intensionally empty one. Methods are only implemented here to speed its run-time use and to prevent errors.!

TopOrBottomGraph subclass: #TopGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Set Ops'!

!TopGraph commentStamp: '<historical>' prior: 0!
This graph type just contains everything you feed it. It rejects nothing and contains every arrow you test for. This graph type basically represents 'Arrow allSubInstances' (and also any that are not allocated). Note that this graph does not behave normally at all.!

MetaGraphGraph subclass: #TransitiveClosureGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!TransitiveClosureGraph commentStamp: '<historical>' prior: 0!
This graph type represents the closure of recursive self-compositions of my argument graph. Designed to support recursion, since application will produce a lazy result chain of compositions of the core graph, and testing for inclusion results in a recursive search of that chain.

i.e. (TransitiveClosureGraph of: a) = a | (a ** a) | (a ** a ** a) | ...!
]style[(353)f1cblack;!

SetOpsGraph subclass: #UnionGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Set Ops'!

!UnionGraph commentStamp: 'btr 10/12/2001 06:56' prior: 0!
I represent the result of applying the union relation onto pairs of graphs, resulting in set-theoretic union (for first-order graphs, anyway). \/ operations generate this type.

Whether appropriate or not, it currently uses its apex-node as the argument pairing. TODO: Investigate if this is inline with the rest of the theory and update the theory or implementation as appropriate.!

Reference subclass: #ValueReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-References'!

!ValueReference commentStamp: '<historical>' prior: 0!
I am a box for values: a strictly-evaluating reference. I also am a cached result of a block that is wrapped as a SuspendedReference. In either case, I am immutable and attempts to mute me return the result of a request to my class for a new reference.
About my class: requests for references to an object should return any existing reference to that object before allocating a new reference to it. By routing all instantiation through one method which finds one existing duplicate if there is any, all duplications are avoided.!

MonoidArrow subclass: #WrapperArrow
	instanceVariableNames: 'object '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-SmalltalkMOP'!

!WrapperArrow commentStamp: '<historical>' prior: 0!
This arrow implementation type is solely responsible for managing proxies of objects in the arrow type space. Notice that this arrow has a specialized notion of its head and tail.

Instance Variables:
	object	-the wrapped object
	graph	-the monoid which the arrow is tied to (the objectGraph of various metaObjectGraphs)!


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

!Arrow methodsFor: 'accessing' stamp: 'btr 5/15/2001 09:58'!
at: anIndex 
	"Answers the reference indexed by the argument. This supports the  
	generalization to multi-arrows. Since this arrow type is abstract, it must pretend to know its slots."
	^ anIndex caseOf: {
		[0] -> [self tail].
		[1] -> [self head]}! !

!Arrow methodsFor: 'accessing' stamp: 'btr 4/4/2001 01:53'!
at: anIndex put: anArrow 
	"Changes the indexed reference to the argument. Type-checking and  
	mutation protection performed by head:tail:. Returns a new arrow."
	^ anIndex caseOf:
		{[0]->[self tail: anArrow].
		[1]->[self head: anArrow]}! !

!Arrow methodsFor: 'accessing' stamp: 'btr 6/10/2001 10:31'!
atBitString: anOrderedCollection 
	"Accesses the arrow obtained by using the addresses specified by each  
	element of the array; so that array must only contain 0's and 1's.  
	e.g. self atBitString: #(1 0 0 1 0 1)."
	| a indexer |
	a _ self.
	"Provide automatic coercion."
	indexer _ anOrderedCollection asArray.
	1
		to: indexer size
		do: [:each | a _ a
						at: (indexer at: each)].
	^ a! !

!Arrow methodsFor: 'accessing' stamp: 'btr 5/27/2001 21:08'!
frame
	"For now, the frame construct and the world construct are the same."
	^ self world! !

!Arrow methodsFor: 'accessing' stamp: 'btr 5/21/2001 11:50'!
head
	"A lot of methods rely on this abstract lazy query."
	self world heads applyTo: self! !

!Arrow methodsFor: 'accessing' stamp: 'btr 4/27/2001 13:10'!
object
	"Designed to handle error cases of trying to treat an ordinary arrow as  
	a wrapper arrow. Return self instead of an error to allow for easy 
	recursion testing. No arrow object will represent itself."
	^ self! !

!Arrow methodsFor: 'accessing' stamp: 'btr 5/21/2001 11:51'!
tail
	"A lot of methods rely on this abstract lazy query."
	self world tails applyTo: self! !

!Arrow methodsFor: 'accessing' stamp: 'btr 5/27/2001 21:01'!
value
	"The responsible world ensures that value comparisons only mean something within the same world / frame. Naturally, values of arrows have value semantics."
	^ self world valueOf: self! !

!Arrow methodsFor: 'accessing' stamp: 'btr 5/15/2001 18:49'!
world
	"This is a recursive method to determine what world an arrow is in  
	without having to perform searches throuh all frames and graphs."
	"For now, perform the search here as last resort."
	^ ArrowFrame allInstances
		detect: [:each | each includes: self]! !

!Arrow methodsFor: 'accessing-synonyms' stamp: 'btr 3/13/2001 17:11'!
codomainElement
	"If I were an element of a function or relation applied as a function,  
	my head would point to the result side."
	^ self head! !

!Arrow methodsFor: 'accessing-synonyms' stamp: 'btr 5/23/2001 21:47'!
destination
	"If I were the element of a category, this is my destination object."
	^ self head! !

!Arrow methodsFor: 'accessing-synonyms' stamp: 'btr 3/13/2001 16:59'!
domainElement
	"If I were an element of a function or relation applied as a function, my tail would point to the argument side."
	^ self tail! !

!Arrow methodsFor: 'accessing-synonyms' stamp: 'btr 3/13/2001 16:59'!
rangeElement
	"If I were an element of a function or relation applied as a function, 
	my head would point to the result side."
	^ self head! !

!Arrow methodsFor: 'accessing-synonyms' stamp: 'btr 5/23/2001 21:27'!
source
	"If I were the element of a category, this is my source object."
	^ self tail! !

!Arrow methodsFor: 'accessing-synonyms' stamp: 'btr 5/23/2001 21:27'!
target
	"If I were the element of a category, this is my destination object."
	^ self head! !

!Arrow methodsFor: 'adding' stamp: 'btr 9/28/2001 01:42'!
+ anArrow 
	"Polymorphic operator to construct graphs. This assumes no intension  
	other than user-driven composition."
	(anArrow isKindOf: Arrow)
		ifTrue: [^ self asGraph \/ anArrow asGraph].
	^ (anArrow isKindOf: ArrowGraph)
		ifTrue: [anArrow addImmutable: self]
		ifFalse: [self error: 'Arrows can only be composed with other arrows and graphs.']! !

!Arrow methodsFor: 'comparing' stamp: 'btr 5/29/2001 05:51'!
equals: anArrow 
	"Recursive test for value equality on an arrow which has no concrete 
	value."
	^ (self head equals: anArrow head)
		and: [self tail equals: anArrow tail]! !

!Arrow methodsFor: 'converting' stamp: 'btr 5/27/2001 21:07'!
asAbstract
	"I'm already abstractly specified."
	^ self! !

!Arrow methodsFor: 'converting' stamp: 'btr 5/27/2001 21:10'!
asArrow
	"Of course I'm an arrow. Implemented for polymorphism with ArrowGraphs."
	^ self! !

!Arrow methodsFor: 'converting' stamp: 'btr 9/16/2001 14:14'!
asAssociation
	"Take myself as a tail-head pair and make a key-value pair out of it."
	^ Association key: self tail value: self head! !

!Arrow methodsFor: 'converting' stamp: 'btr 4/25/2001 11:55'!
asConcrete
	"Finds out if self head and self tail return something coherent. If so,  
	make a concrete version of me and forward pointers to me to the  
	concrete instance."
	^ self
		becomeForward: (ConcreteArrow from: self head to: self tail)! !

!Arrow methodsFor: 'converting' stamp: 'btr 6/10/2001 11:27'!
asGraph
	"This is not conversion per se."
	^ self wrapInGraph! !

!Arrow methodsFor: 'converting' stamp: 'btr 6/15/2001 07:03'!
asGraphForArrow: anArrow 
	"This is not conversion per se. This differs from #asGraph in initializing 
	the apex."
	^ self wrapInGraph apex: anArrow! !

!Arrow methodsFor: 'converting' stamp: 'btr 6/10/2001 11:26'!
asReference
	"This is not conversion per se."
	^ self boxMeUp! !

!Arrow methodsFor: 'converting' stamp: 'btr 9/16/2001 14:12'!
fromAssociation: anAssociation in: anObjectGraph
	^ Arrow fromAssociation: anAssociation in: anObjectGraph! !

!Arrow methodsFor: 'initialize' stamp: 'btr 4/25/2001 11:58'!
head: anArrow 
	"Changes the second reference to the argument. Delegates to head:tail:."
	(anArrow isKindOf: Arrow)
		ifFalse: [self error: 'Arrows may only reference other arrows.'].
	^ self head: anArrow tail: self tail! !

!Arrow methodsFor: 'initialize' stamp: 'btr 6/10/2001 11:13'!
head: firstArrow tail: secondArrow 
	"Relies on Arrow class>>head:tail: creating a ConcreteArrow. 
	TODO: implement checking for knowledge of head and tail. 
	UNSAFE AS IS (expect to see this in a debugger near you ;)."
	^ self
		becomeForward: (self class head: firstArrow tail: secondArrow)! !

!Arrow methodsFor: 'initialize' stamp: 'btr 4/25/2001 11:58'!
headKnown
	"Answers whether the receiver has previously determined that it has a  
	unique head value specified.""^ (head isKindOf: Reference) 
	and: [head value isKindOf: Arrow]"
	^ self error: 're-implement this'! !

!Arrow methodsFor: 'initialize' stamp: 'btr 4/25/2001 11:56'!
tail: anArrow 
	"Changes the first reference to the argument. Delegates to head:tail:."
	(anArrow isKindOf: Arrow)
		ifFalse: [self error: 'Arrows may only reference other arrows.'].
	^ self head: self head value tail: anArrow! !

!Arrow methodsFor: 'initialize' stamp: 'btr 4/25/2001 11:59'!
tailKnown
	"Answers whether the receiver has previously determined that it has a  
	unique tail value specified."
	"^ (tail isKindOf: Reference)
		and: [tail value isKindOf: Arrow]"
	^ self error: 're-implement this'! !

!Arrow methodsFor: 'operations' stamp: 'btr 3/29/2001 20:30'!
++ anArrow 
	"Sugaring; polymorphic."
	(anArrow isKindOf: Arrow)
		ifTrue: [^ self compose: anArrow].
	(anArrow isKindOf: ArrowGraph)
		ifTrue: [^ self notYetImplemented].
	^ nil! !

!Arrow methodsFor: 'operations' stamp: 'btr 3/23/2001 12:59'!
-> anArrow 
	"Sugaring; intended to be conceptually similar to Point>>@."
	^ self pairWith: anArrow! !

!Arrow methodsFor: 'operations' stamp: 'btr 4/7/2001 22:25'!
=> anArrow 
	"Sugaring; intended to be conceptually similar to Point>>@."
	^ self pairWith: anArrow! !

!Arrow methodsFor: 'operations' stamp: 'btr 3/12/2001 10:34'!
@ anArrow
	"Sugaring; intended to be conceptually similar to Point>>@."
	^ self pairWith: anArrow! !

!Arrow methodsFor: 'operations' stamp: 'btr 6/10/2001 09:50'!
addToGraph: anArrowGraph 
	"Both #add: and #addToGraph: return the arrow vice the graph for compatibility with Smalltalk collection class behavior."
	"This ensures that a == a addToGraph: b == b add: a."
	^ anArrowGraph add: self! !

!Arrow methodsFor: 'operations' stamp: 'btr 3/23/2001 13:01'!
compose: anArrow 
	"Answers the arrow composition of two arrows, the receiver being the  
	second arrow in the sequence and the argument the first."
	(anArrow isKindOf: Arrow)
		ifFalse: [self error: 'Arrows can only be composed with other arrows.'].
	self head == anArrow tail
		ifTrue: [^ self tail -> anArrow head].
	"Don't return self, as it would be confused with an appropriate result."
	^ nil! !

!Arrow methodsFor: 'operations' stamp: 'btr 6/10/2001 14:38'!
composeElements
	"Abstract specification of the composition of elements is not yet handled."
	self notYetImplemented! !

!Arrow methodsFor: 'operations' stamp: 'btr 6/6/2001 18:22'!
identity
	"The simplest possible operation."
	^ self! !

!Arrow methodsFor: 'operations' stamp: 'btr 3/2/2001 20:49'!
inv
	"Sugaring."
	^ self invert! !

!Arrow methodsFor: 'operations' stamp: 'btr 6/16/2001 09:59'!
inverse
	"Sugaring."
	^ self invert! !

!Arrow methodsFor: 'operations' stamp: 'btr 5/26/2001 11:58'!
invert
	"Answers a new arrow with transposed references."
	^ self head -> self tail! !

!Arrow methodsFor: 'operations' stamp: 'btr 6/10/2001 09:55'!
pairWith: anArrow 
	"Answers #(self anArrow) as an arrow. This should be compatible with  
	all of my subclasses without modification. #-> is a sugaring of this."
	(anArrow isKindOf: Arrow)
		ifFalse: [^ nil].
	^ Arrow head: anArrow tail: self! !

!Arrow methodsFor: 'operations' stamp: 'btr 3/23/2001 13:01'!
raiseNode
	"Create an identity over me. Deprecated for now to raiseIdentity."
	^ self -> self! !

!Arrow methodsFor: 'reifications' stamp: 'btr 6/10/2001 09:56'!
raise: aSelector 
	"This is the basis for all reifications."
	((aSelector isKindOf: Symbol) not
			or: [aSelector numArgs < 0])
		ifTrue: [^ self error: aSelector asString , ' is not a valid selector.'].
	^ self
		-> (self perform: aSelector)! !

!Arrow methodsFor: 'reifications' stamp: 'btr 6/6/2001 18:18'!
raiseHead
	"This performs the basic operation of reifying the head reference as an Arrow. Related context management of the result should be handled with a HeadGraph. This evaluates self head value"
	^ self -> self head! !

!Arrow methodsFor: 'reifications' stamp: 'btr 4/2/2001 12:59'!
raiseIdentity
	"Create an identity over me. This does NOT evaluate my references."
	^ self -> self! !

!Arrow methodsFor: 'reifications' stamp: 'btr 6/6/2001 18:18'!
raiseTail
	"This performs the basic operation of reifying the tail reference as an 
	Arrow. Related context management of the result should be handled 
	with a TailGraph. This evaluates self tail value"
	^ self -> self tail! !

!Arrow methodsFor: 'reifications' stamp: 'btr 6/10/2001 14:38'!
raiseValue
	"The arrow leading from me to my value."
	^ self -> self value! !

!Arrow methodsFor: 'testing' stamp: 'btr 6/6/2001 18:25'!
coincidesWith: anArrow coords: aPoint 
	"Provides the co-incidence relationship. x R<i,j> y IFF x[i]==y[j].
	e.g. (a -> b) coincidesWith: (b -> c) coords: 1@0"
	^ (self at: aPoint x)
		== (anArrow at: aPoint y)! !

!Arrow methodsFor: 'testing' stamp: 'btr 6/10/2001 09:57'!
incidesUpon: anArrow 
	"Sugaring; also similarity with #coincidesWith:coords:"
	^ self references: anArrow! !

!Arrow methodsFor: 'testing' stamp: 'btr 3/18/2001 01:46'!
incidesUpon: anArrow coord: anIndex 
	"Provides the incidence relationship. x R<i> y IFF x[i]==y."
	^ self at: anIndex == anArrow! !

!Arrow methodsFor: 'testing' stamp: 'btr 9/30/2001 13:51'!
isAbstract
	^ self class == Arrow! !

!Arrow methodsFor: 'testing' stamp: 'btr 6/15/2001 07:00'!
isApexOfGraph
	"This is the most general algorithm for finding all graphs using me as  
	an apex arrow. Frames should introduce specialized versions to form a 
	constructor for context-restriction."
	ArrowGraph
		allSubInstancesDo: [:eachGraph | eachGraph apex == self
				ifTrue: [^ true]].
	^ false! !

!Arrow methodsFor: 'testing' stamp: 'btr 6/2/1999 12:00'!
isCompositionOf: anArrow1 and: anArrow2 
	"Syntactic sugar."
	^ self isCompositionOf: anArrow1 with: anArrow2! !

!Arrow methodsFor: 'testing' stamp: 'btr 6/10/2001 10:01'!
isCompositionOf: firstArrow with: secondArrow 
	"Answers whether the receiver is a valid composition result of the  
	arguments in the given order."
	"^ firstArrow coincidesWith: secondArrow coords: 0@1"
	^ self equals: (firstArrow ++ secondArrow)! !

!Arrow methodsFor: 'testing' stamp: 'btr 9/30/2001 13:52'!
isConcrete
	^ self isAbstract not! !

!Arrow methodsFor: 'testing' stamp: 'btr 6/13/2001 21:52'!
isElementOf: anArrowGraph
	"Sugaring."
	^ anArrowGraph includes: self! !

!Arrow methodsFor: 'testing' stamp: 'btr 6/10/2001 09:48'!
isIdentity
	"Answers whether the receiver's references are identical. Identity  
	arrows represent nul information transitions."
	"^ self coincidesWith: self coords: 0@1"
	^ self head == self tail! !

!Arrow methodsFor: 'testing' stamp: 'btr 10/10/2001 04:36'!
isIn: anArrowGraph 
	"Sugaring."
	^ anArrowGraph includes: self! !

!Arrow methodsFor: 'testing' stamp: 'btr 6/10/2001 10:03'!
isInverseOf: anArrow 
	"Answers whether the receiver is a valid inverse of the argument."
	"^ self raiseIdentity isCompositionOf: self with: anArrow"
	^ self inv equals: anArrow! !

!Arrow methodsFor: 'testing' stamp: 'btr 3/28/2001 09:46'!
isNodeTo: anArrowGraph 
	"Sugaring; another delegation to a graph."
	^ anArrowGraph treatsAsNode: self! !

!Arrow methodsFor: 'testing' stamp: 'btr 6/13/2001 14:52'!
isWrapper
	"Wrappers form a strict tower, so any arrow answering itself will not be 
	a wrapper arrow."
	^ self object ~~ self! !

!Arrow methodsFor: 'testing' stamp: 'btr 4/7/2001 22:12'!
references: anArrow 
	"Answers whether or not the receiver points to the argument."
	^ (self head == anArrow)
		or: [self tail == anArrow]! !

!Arrow methodsFor: 'wrapping' stamp: 'btr 3/28/2001 09:49'!
boxMeUp
	"Sugaring."
	^ Reference to: self! !

!Arrow methodsFor: 'wrapping' stamp: 'btr 6/10/2001 11:25'!
wrapInGraph
	"Answer a graph whose meaning is that I am its contents."
	^ SingletonGraph of: self! !


!Arrow class methodsFor: 'accessing' stamp: 'btr 3/28/2001 09:52'!
null
	"Answer a nul arrow, distinct from an arrow pointing to a ground arrow."
	^ self head: nil tail: nil! !

!Arrow class methodsFor: 'accessing' stamp: 'btr 5/27/2001 21:10'!
unspecified
	"New arrow instances have no information in their head and tail."
	^ self new! !

!Arrow class methodsFor: 'conversion' stamp: 'btr 9/14/2001 21:50'!
newFromAssociation: anAssociation in: anObjectGraph 
	"Permits the use of dictionaries and the association sugaring to make 
	arrows. Create wrappers by default, to avoid confusion."
	^ Arrow
		from: (Arrow wrapping: anAssociation key in: anObjectGraph)
		to: (Arrow wrapping: anAssociation value in: anObjectGraph)! !

!Arrow class methodsFor: 'instance creation' stamp: 'btr 4/11/2001 23:56'!
from: tailArrow
	"Create a new arrow according to this sugaring."
	^ self head: nil tail: tailArrow! !

!Arrow class methodsFor: 'instance creation' stamp: 'btr 4/4/2001 01:36'!
from: tailArrow to: headArrow 
	"Create a new arrow according to this sugaring."
	^ self head: headArrow tail: tailArrow! !

!Arrow class methodsFor: 'instance creation' stamp: 'btr 4/2/2001 12:39'!
head: firstArrow tail: secondArrow 
	"Create a new arrow and initialize it."
	^ ConcreteArrow new head: firstArrow tail: secondArrow! !

!Arrow class methodsFor: 'instance creation' stamp: 'btr 4/11/2001 23:56'!
to: headArrow 
	"Create a new arrow according to this sugaring."
	^ self head: headArrow tail: nil! !

!Arrow class methodsFor: 'instance creation' stamp: 'btr 4/24/2001 18:37'!
wrapping: anObject in: anObjectGraph 
	"Introduce a wrapper for any object."
	^ WrapperArrow for: anObject in: anObjectGraph! !


!ArrowFrame methodsFor: 'accessing' stamp: 'btr 5/27/2001 21:14'!
arrows
	"The core graph of arrows that I represent."
	^ arrows! !

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 10/17/2001 06:54'!
assertions
	assertions
		ifNil: [self initializeAssertions].
	^ assertions! !

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 10/17/2001 06:54'!
compositions
	"A graph representing abstractly-specified composition relationships."
	compositions
		ifNil: [self initializeCompositions].
	^ compositions! !

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 6/18/2001 16:32'!
graphs
	"A relational mapping from arrows to the arrow-wrappings of ArrowGraph objects."
	^ graphs! !

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 10/17/2001 06:55'!
heads
	heads
		ifNil: [self initializeHeads].
	^ heads! !

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 10/17/2001 06:55'!
inversions
	"A graph representing abstractly-specified inversion relationships."
	inversions
		ifNil: [self initializeInversions].
	^ inversions! !

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 5/27/2001 21:14'!
nodes
	"The ground type handler for the frame. Abstract arrows can use this as a fallback."
	^ nodes! !

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 6/10/2001 14:40'!
objects
	"My objectGraph (my reflective interface handle)."
	^ objects! !

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 9/30/2001 14:25'!
root
	^ nodes root! !

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 10/17/2001 06:55'!
tails
	tails
		ifNil: [self initializeTails].
	^ tails! !

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 5/27/2001 21:13'!
valueOf: anArrow 
	"Frames should know the values of their arrows without the client objects having to know a frame's structure."
	^ values valueOf: anArrow! !

!ArrowFrame methodsFor: 'initialize' stamp: 'btr 6/10/2001 11:03'!
graph: anArrowGraph 
	"Mutation protection is all that is provided for frames. Frames cannot be 
	unique per graph since they represent informational interpretations  
	over that set. Of course this means the frame protocol needs to be  
	expanded to express these kinds of different possible interpretations."
	(anArrowGraph isKindOf: ArrowGraph)
		ifFalse: [^ self error: 'Frames must be based on graphs.'].
	arrows
		ifNotNil: [^ ArrowFrame for: anArrowGraph].
	arrows _ anArrowGraph! !

!ArrowFrame methodsFor: 'initialize' stamp: 'btr 10/17/2001 06:53'!
initialize
	"This initialization method is odd in that it sets up a lot of variables. Note 
	that #initialize should not be called in user code;  
	ArrowFrame class >> #for: initializes automatically."
	| root |
	super initialize.
	root _ Arrow new.
	root head: root tail: root.
	arrows
		ifNil: [arrows _ ArrowGraph new holdWeakly].
	nodes _ MonoidGraph of: root.
	objects _ ObjectGraph newFromArrow: nodes anyOne.
	graphs _ objects
				>> [:each | each isKindOf: ArrowGraph].
	graphs addFor: arrows.
	graphs addFor: nodes.
	graphs addFor: objects.
	graphs addFor: graphs.
	arrows add: root! !

!ArrowFrame methodsFor: 'initialize' stamp: 'btr 10/17/2001 06:51'!
initializeAssertions
	assertions _ AssertionGraph over: self.
	graphs addFor: assertions! !

!ArrowFrame methodsFor: 'initialize' stamp: 'btr 10/17/2001 06:52'!
initializeCompositions
	compositions _ CompositionGraph over: self.
	graphs addFor: compositions! !

!ArrowFrame methodsFor: 'initialize' stamp: 'btr 10/17/2001 06:52'!
initializeHeads
	heads _ HeadGraph over: self.
	graphs addFor: tails! !

!ArrowFrame methodsFor: 'initialize' stamp: 'btr 10/17/2001 06:52'!
initializeInversions
	inversions _ InversionGraph over: self.
	graphs addFor: inversions! !

!ArrowFrame methodsFor: 'initialize' stamp: 'btr 10/17/2001 06:53'!
initializeTails
	tails _ TailGraph over: self.
	graphs addFor: heads! !

!ArrowFrame methodsFor: 'initialize' stamp: 'btr 10/17/2001 06:53'!
initializeValues
	values _ SquareGraph over: self.
	graphs addFor: values! !

!ArrowFrame methodsFor: 'initialize' stamp: 'btr 6/15/2001 19:34'!
name: aString 
	"Protected mutator for frames' names. TODO: make this redundant by 
	the equivalent of Dictionaries reified as arrow objects (graphs from  
	Smalltalk symbols to values)."
	name
		ifNotNil: [self error: 'I already have a name.'].
	name _ aString asString! !

!ArrowFrame methodsFor: 'testing' stamp: 'btr 5/8/2001 14:26'!
includes: anArrow
	"The frame is the closure of all of the information of the ArrowGraph it wraps, so this catch-all method allows for getting that information transparently. Alternatively, the frame structure can be accessed publically for more refined searches by the sender."
	(arrows includes: anArrow) ifTrue: [^true].
	(nodes includes: anArrow) ifTrue: [^true].
	(heads includes: anArrow) ifTrue: [^true].
	(tails includes: anArrow) ifTrue: [^true].
	^ values includes: anArrow! !


!ArrowFrame class methodsFor: 'accessing instances and variables' stamp: 'btr 6/6/2001 15:41'!
lobby
	"My sole or default instance can be referred to as 'ArrowWorld'."
	^ Smalltalk at: #ArrowWorld! !

!ArrowFrame class methodsFor: 'class initialization' stamp: 'btr 10/17/2001 06:33'!
initialize
	"ArrowFrame initialize."
	"Provides a default arrow frame to play with, which can be directly  
	referred to as 'ArrowWorld'. Try this: 'ArrowWorld explore.'"
	Smalltalk at: #ArrowWorld put: (self new initialize)! !

!ArrowFrame class methodsFor: 'instance creation' stamp: 'btr 6/10/2001 11:05'!
for: anArrowGraph 
	"Frames are not even unique per graph handled. Beware. However,  
	mutation is certainly protected."
	^ self new graph: anArrowGraph; initialize! !


!ArrowGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 14:20'!
anyOne
	"Returns an example member of this graph. First look for examples in  
	the cache; if none are there, make up an arrow if the set is  
	intensional. This arrow should have exactly the semantics of the graph 
	definition: no more, no less. Any further information should be a 
	potential for a unification (matching with a known arrow) or 
	invalidation, but neither should be possible with the answer this method 
	gives.
	NOTE: Any graph subclass implementing this should ensure that 
	self anyOne satisfies self intensionallyIncludes:."
	(self isIntensional
			and: [cache isNil])
		ifTrue: [^ intension].
	^ cache anyOne! !

!ArrowGraph methodsFor: 'accessing' stamp: 'btr 6/15/2001 07:00'!
apex
	"An accessor; initializing the apex arrow implicitly is debatable."
	apex
		ifNil: [^ apex _ Arrow new].
	^ apex! !

!ArrowGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 11:29'!
cache
	"Allow collaborating graphs to manipulate my cache if necessary.  
	If security is an issue, check all calls to this method. No object calling 
	this method should be effecting the cache, only inspecting it."
	^ cache
		ifNil: [cache _ WeakSet new. cache]! !

!ArrowGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 14:43'!
frame
	"Worlds and frames are synonymous."
	^ self world! !

!ArrowGraph methodsFor: 'accessing' stamp: 'btr 10/17/2001 06:41'!
world
	"This is a recursive method to determine what world a graph is in  
	without having to perform searches throuh all frames and graphs."
	"For now, assume that the graph belongs to the same world as its  
	apex."
	^ frame
		ifNil: [self apex world]! !

!ArrowGraph methodsFor: 'adding' stamp: 'btr 3/26/2001 23:24'!
+ anArrow 
	"Sugaring; #add: was not sugared this way because it resembles Collection>>add: rather than Number>>+."
	^ self addImmutable: anArrow! !

!ArrowGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 05:15'!
add: anArrow 
	"Just use the default error handler."
	^ self
		add: anArrow
		ifFail: [self error: 'This arrow does not satisfy the graph definition.']! !

!ArrowGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:31'!
add: anArrow ifFail: errorBlock 
	"This modifies the cache of the graph to include the argument as well. It 
	is not meant to represent actual addition of elements to an enumerated  
	(extensional) graph, so the argument must satisfy the receiver's  
	intension. Provide the proof incrementally and intensionally."
	(anArrow isKindOf: Arrow)
		ifFalse: [self error: 'ArrowGraphs can only contain arrows'].
	(self intensionallyIncludes: anArrow)
		ifFalse: [errorBlock value].
	cache ifNil: [cache _ WeakSet with: anArrow.
			^ anArrow]
			ifNotNil: [(cache size = 0)
				ifTrue: [cache _ cache class with: anArrow. ^ anArrow]].
	^ cache add: anArrow! !

!ArrowGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 05:16'!
addAll: aCollection 
	"Iterate over the collection. Ignore errors, since this is designed to be a batch-assertion facility for inclusion."
	aCollection
		do: [:each | self add: each ifFail: []].
	^ aCollection! !

!ArrowGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:21'!
addImmutable: anArrow 
	"Supports immutability and reification of extensional graph structure as  
	arrows. Note that this returns the resulting graph instead of the added element, so it is unlike add:."
	self
		add: anArrow
		ifFail: [^ UnionGraph
				of: (SingletonGraph of: anArrow)
				with: self]! !

!ArrowGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 05:05'!
addImmutableAll: aCollection 
	"Add all of the elements of the collection successively without affecting 
	any of the intermediate graph results' semantics. Note that the order 
	counts: if an arrow that does not fit gets added before an arrow that 
	would, the latter arrow would not get add:ed because the semantics of 
	the intermediate result at that point would be unavailable. TODO: fix 
	this."
	^ aCollection
		inject: self
		into: [:graphResult :each | graphResult addImmutable: each]! !

!ArrowGraph methodsFor: 'comparing' stamp: 'btr 3/26/2001 23:59'!
<= anArrowGraph 
	"Sugaring."
	^ self isSubGraphOf: anArrowGraph! !

!ArrowGraph methodsFor: 'comparing' stamp: 'btr 3/26/2001 23:59'!
>= anArrowGraph
	"Sugaring."
	^ self isSuperGraphOf: anArrowGraph! !

!ArrowGraph methodsFor: 'comparing' stamp: 'btr 6/10/2001 14:41'!
isSubGraphOf: anArrowGraph 
	"This class exists to separate out the non-set-theoretic protocol."
	self subclassResponsibility! !

!ArrowGraph methodsFor: 'comparing' stamp: 'btr 6/10/2001 14:41'!
isSuperGraphOf: anArrowGraph 
	"This class exists to separate out the non-set-theoretic protocol."
	self subclassResponsibility! !

!ArrowGraph methodsFor: 'converting' stamp: 'btr 6/21/2001 00:12'!
asArrow
	"Answer a wrapper for me in my frame's MOP facility. This is the way to semantically identify me in the arrow type-system."
	^ Arrow wrapping: self in: self frame objects! !

!ArrowGraph methodsFor: 'converting' stamp: 'btr 6/15/2001 19:33'!
asFrame
	"Answer a frame representing me with no default semantics."
	^ ArrowFrame for: self! !

!ArrowGraph methodsFor: 'converting' stamp: 'btr 6/11/2001 10:23'!
asGraph
	"Supports the coercion of arrows to singleton graphs."
	^ self! !

!ArrowGraph methodsFor: 'converting' stamp: 'btr 6/15/2001 07:03'!
asGraphForArrow: anArrow 
	"Supports the coercion of arrows to singleton graphs coupled with 
	specification of the apex."
	^ self identity apex: anArrow! !

!ArrowGraph methodsFor: 'converting' stamp: 'btr 9/16/2001 15:41'!
asInclusionStructure
	"Provide a downward method of reifying an arrow structure when  
	taking it to be the inclusion-graph (or structure in general) of the resulting graph."
	^ self as: InclusionGraphSansBase! !

!ArrowGraph methodsFor: 'converting' stamp: 'btr 6/15/2001 07:01'!
asReference
	"This doesn't quite make sense. I also want to pass around graphs in a 
	boxed style. However, there is no direct use for that yet."
	^ Reference to: self apex! !

!ArrowGraph methodsFor: 'converting' stamp: 'btr 6/13/2001 15:39'!
asSet
	"Provide the ability to strip away the graph semantics overhead if 
	needed, while protecting against unsafe mutation."
	^ cache clone! !

!ArrowGraph methodsFor: 'filtering' stamp: 'btr 6/21/2001 01:16'!
>> aBlockContext 
	"Sugaring."
	^ self filteredBy: aBlockContext! !

!ArrowGraph methodsFor: 'initialize' stamp: 'btr 6/15/2001 07:03'!
apex: anArrow 
	"Protected mutation. Avoid if at all possible effects on the annotated  
	arrow."
	(anArrow isKindOf: Arrow)
		ifFalse: [(anArrow isKindOf: ArrowGraph)
				ifTrue: [^ self apex: ArrowGraph apex].
			^ self error: 'Only arrows can be used to instantiate graphs.'].
	apex
		ifNil: [apex _ anArrow.
			^ self].
	"Use an identity filter to make the new graph without copying over the  
	apex variable."
	^ self identity apex: anArrow! !

!ArrowGraph methodsFor: 'initialize' stamp: 'btr 6/21/2001 00:08'!
cache: aCollection 
	"Sets the cache to a known set. Note that this method allows for 
	non-weak sets, which is what is desired in many cases for 
	initialization."
	(cache isNil not
			and: [cache isEmpty not])
		ifTrue: [^ self class newFrom: aCollection asSet].
	cache _ aCollection asSet
				select: [:each | (each isKindOf: Arrow)
						and: [self intensionallyIncludes: each]]! !

!ArrowGraph methodsFor: 'initialize' stamp: 'btr 10/17/2001 06:43'!
frame: anArrowFrame
	"Initialize the graph for a particular frame. Note, however, that a graph may still be represented in multiple non-intersecting frames."
	frame
		ifNotNil: [self notYetImplemented].
	frame _ anArrowFrame! !

!ArrowGraph methodsFor: 'initialize' stamp: 'btr 10/21/2001 11:10'!
initialize
	"Do nothing by default."! !

!ArrowGraph methodsFor: 'initialize' stamp: 'btr 6/18/2001 17:12'!
initializeIntension
	"My frame must be known before this is called, which requires the apex 
	to be already given according to the current implementation. 
	TODO: method complexity here is unacceptable. refactor it."
	| asserts frame graphWrapper |
	apex
		ifNil: ["Either throw an error or silently initialize the apex."
			self notYetImplemented].
	frame _ self frame.
	asserts _ frame assertions.
	intension _ Arrow new addToGraph: frame arrows.
	frame arrows add: apex.
	graphWrapper _ Arrow wrapping: self in: frame objects.
	"Assert that my graph is accessible from my frame (maybe this should  
	be the basis for self frame instead of delegation to apex)."
	asserts
		add: (frame graphs add: apex -> graphWrapper).
	"Assert to my frame that my intension arrow belongs to me."
	asserts add: apex -> intension! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/28/2001 02:32'!
!!> anArrowGraph 
	"Sugaring."
	^ self leftConjugateWith: anArrowGraph! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 6/12/2001 07:01'!
* anArrow 
	"Sugaring; polymorphic."
	(anArrow isKindOf: Arrow)
		ifTrue: [^ self applyTo: anArrow].
	(anArrow isKindOf: ArrowGraph)
		ifTrue: [^ self applyToGraph: anArrow].
	^ nil!
]style[(2 7 3 24 41 4 10 7 5 7 11 10 15 4 15 7 6 3)f1b,f1cblack;b,f1,f1c148046000,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 3/26/2001 23:40'!
** anArrowGraph
	"Sugaring."
	^ self graphCompose: anArrowGraph! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 6/11/2001 10:25'!
++ anArrow 
	"Sugaring; polymorphic over-riding of Arrow>>++."
	(anArrow isKindOf: ArrowGraph)
		ifTrue: [^ self graphCompose: anArrow].
	^ nil! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/26/2001 12:39'!
/ anArrowGraph 
	"Sugaring."
	^ self rightQuotientWith: anArrowGraph! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/28/2001 02:32'!
<!! anArrowGraph 
	"Sugaring."
	^ self rightConjugateWith: anArrowGraph! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/26/2001 12:38'!
\ anArrowGraph
	"Sugaring."
	^ self leftQuotientWith: anArrowGraph! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/28/2001 01:43'!
applicativeClosureFor: anArrowGraph 
	"Answer the graph resulting from taking all of my elements and the  
	results of applying the argument to them. When the argument is a  
	transitive closure, this is particularly powerful."
	^ self \/ (anArrowGraph * self)! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 10/14/2001 13:58'!
diagonal
	"Return a diagonal relation over my elements. Identity relations are called diagonal because the (a,a) ordered pairs fill the diagonal in the 2-dimensional array of possibilities for relational-holding."
	^ IdentityGraph over: self! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 6/15/2001 06:57'!
identity
	"Answer my (intensional) self. This is preferred over cloning to allow 
	cache synchronization."
	^ self
		>> [:value | true]! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:36'!
inclusionArrowFor: anArrow 
	"Answer the arrow in the meta-graph defining the argument's  
	membership."
	^ self inclusionGraph inclusionArrowFor: anArrow! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 3/26/2001 23:22'!
inv
	"Sugaring."
	^ self invert! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 3/26/2001 23:21'!
invert
	"Answers a new graph with arrows in the opposite configuration of the  
	receiver."
	^ InvertedGraph of: self! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/25/2001 19:19'!
kleeneStar
	"Sugaring."
	^ self reflexiveTransitiveClosure! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:30'!
raisedApplicationTo: anArrow
	"Answer the arrows from the argument to its results lazily."
	^ (GraphInclusionGraph of: self * anArrow) apex: anArrow! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:32'!
raisedInverseApplicationTo: anArrow 
	"Answer the arrows from the argument to its results lazily."
	^ (GraphInclusionGraph of: self inv * anArrow)
		apex: anArrow! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 10/11/2001 01:02'!
reflexiveClosure
	"Adds the possibility of applying myself 0 times: returning the  
	argument as the result."
	^ (IdentityGraph over: self domain)
		\/ self! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 21:37'!
reflexiveTransitiveClosure
	^ self transitiveClosure reflexiveClosure! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 10/10/2001 04:48'!
rstClosure
	"Answers the smallest equivalence relation based on all my arrows."
	^ self transitiveClosure symmetricClosure reflexiveClosure! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 10/10/2001 04:46'!
symmetricClosure
	"Answer the smallest relation possible that includes my arrows and all their inverses."
	^ self \/ self inv! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 19:26'!
transitiveClosure
	"Answer the graph that would result if I were composed with myself ad  
	infinitum. This is a method to support recursion.  
	i.e. self transitiveClosure = self | self ** self | self ** self ** self | ... ."
	^ TransitiveClosureGraph of: self! !

!ArrowGraph methodsFor: 'set operations' stamp: 'btr 9/28/2001 01:34'!
/\ anArrowGraph 
	"Set-theoretic intersection."
	^ IntersectionGraph of: self with: anArrowGraph! !

!ArrowGraph methodsFor: 'set operations' stamp: 'btr 9/28/2001 01:34'!
\/ anArrowGraph 
	"Set-theoretic union."
	^ UnionGraph of: self with: anArrowGraph! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/7/2001 02:42'!
? anArrow
	"Sugaring."
	^ self includes: anArrow! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 3/28/2001 09:56'!
contains: anArrow
	"Sugaring; supports the notion of graph as a space."
	^ self includes: anArrow! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/9/2001 23:56'!
hasFixedPointFor: anArrow 
	"A fixed point for a relation is where application returns its argument."
	^ self includes: anArrow -> anArrow! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 6/13/2001 19:28'!
holdsStrongly
	^ cache class isWeak not! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 6/13/2001 19:28'!
holdsWeakly
	^ cache class isWeak! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/23/2001 19:08'!
intensionallyIncludes: anArrow 
	"Default answer for all graphs; subclasses should override according to  
	their semantic intent."
	"TODO: alter this to use 'intension' (which requires building an intension 
	system first)."
	^ self isIntensional
		and: [(self treatsAsDomainElement: anArrow domainElement)
			and: [self treatsAsCodomainElement: anArrow codomainElement]]! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 6/13/2001 12:11'!
intensionallyIncludes: anArrow using: aSelector 
	"This is an abstract method parametrized by the selector that the class  
	knows it encodes. This begs to have reflective equational semantics."
	"^ anArrow tail <foo> = anArrow head."
	"^ anArrow domainElement <foo> = anArrow rangeElement."
	"^ domainElement -(foo)-> rangeElement at anArrow."
	^ (anArrow tail perform: aSelector)
		= anArrow head! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 5/26/2001 08:55'!
intensionallyIncludes: anArrow using: aSelector withArgs: anArray 
	^ (anArrow tail perform: aSelector withArguments: anArray) = anArrow head! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/11/2001 01:35'!
isContinuousFor: anArrowGraph
	"Continuity is a topological property where applying an operation to any set returns a subset of it. This expresses local continuity."
	^ self * anArrowGraph <= anArrowGraph! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 11:46'!
isCoreflexive
	"This is directly copied from the equational relational algebraic  
	definition, so it will not be useful in general to attempt to evaluate  
	this."
	^ (IdentityGraph over: self domain)
		>= self! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 3/27/2001 00:37'!
isDegenerate
	"Answers whether the graph 'speaks about' its own arrows. If true, then 
	the graph cannot represent a first-order relation."
	^ self hasMetaArrowsFor: self! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 6/13/2001 13:07'!
isExtensional
	"Graphs without intensions are extensional. See #isIntensional."
	^ intension isNil not! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 3/27/2001 00:57'!
isFinite
	"Answers whether the internal cache should be treated as the graph 
	itself. "
	^ infinitary not! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 6/15/2001 07:01'!
isFirstOrder
	"Answers whether the receiver constitutes a valid first-order logical  
	relation. It also addresses whether the graph's arrows refer to the  
	graph itself."
	^ self isNonDegenerate & (self includesReferencesTo: self apex) not! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/9/2001 23:55'!
isFixedPointFor: anArrow 
	"A fixed point for a relation is where application returns its argument."
	| temp |
	temp _ (self * anArrow).
	^ temp class == SingletonGraph and: [temp singleton == anArrow]! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 3/27/2001 00:57'!
isInfinitary
	"Answers if evaluation should be strictly lazy."
	^ infinitary! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 6/13/2001 13:08'!
isIntensional
	"Any graph with an intension is checked for #intensionallyIncludes: for membership, whether finite or not."
	^ intension isNil! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 11:53'!
isIsomorphism
	"The equational relational algebraic definition."
	^ self isTotalFunction
		and: [self inv isTotalFunction]! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 3/27/2001 03:02'!
isNonDegenerate
	"Answers whether the receiver is valid as a first-order relation."
	^ self isDegenerate not! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 13:15'!
isPartialFunction
	"The equational relational algebraic definition."
	^ self isSimpleRelation! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 13:20'!
isPostOrderRelation
	"The equational relational algebraic definition."
	^ self isCoreflexive
		and: [self isTransitive]! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 13:16'!
isPreOrderRelation
	"The equational relational algebraic definition."
	^ self isReflexive
		and: [self isTransitive]! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 11:46'!
isReflexive
	"This is directly copied from the equational relational algebraic 
	definition, so it will not be useful in general to attempt to evaluate 
	this."
	^ self
		>= (IdentityGraph over: self domain)! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 13:15'!
isSimpleRelation
	"The equational relational algebraic definition."
	^ (IdentityGraph over: self domain)
		>= (self ** self inv)! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 13:15'!
isTotalFunction
	"The equational relational algebraic definition."
	^ self isSimpleRelation
		and: [self isTotalRelation]! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 13:15'!
isTotalRelation
	"The equational relational algebraic definition."
	^ self inv ** self
		>= (IdentityGraph over: self domain)! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 13:18'!
isTransitive
	"This is directly copied from the equational relational algebraic  
	definition, so it will not be useful in general to attempt to evaluate  
	this."
	^ (self selfComposeTimes: 1) <= self! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 6/10/2001 10:07'!
treatsAsNode: anArrow 
	"Nodes are a term for an arrow relative to another graph. Graphs  
	strictly contain their directed edges (their arrows) as a set does its  
	elements. The nodes of the graph are those arrows referenced by those  
	edges. System-wide graphs should include a separate intensional check."
	^ ((self treatsAsDomainElement: anArrow)
				or: [self treatsAsCodomainElement: anArrow])
			or: [self includesReferencesTo: anArrow]! !

!ArrowGraph methodsFor: 'private' stamp: 'btr 6/13/2001 13:10'!
beExtensional
	intension _ nil! !

!ArrowGraph methodsFor: 'private' stamp: 'btr 6/13/2001 13:10'!
beIntensional
	"Default answer until some expressive intension system is designed."
	intension _ true! !

!ArrowGraph methodsFor: 'private' stamp: 'btr 10/12/2001 05:47'!
holdStrongly
	"I convert my cache to a Set from whatever it was before (most likely a 
	WeakSet). This is useful when the graph is intensional but is passed 
	around linearly (usually as the result of a computation) so that it is 
	the primary source of reference for its contents."
	| tempCache |
	tempCache _ Set new.
	cache
		ifNotNil: [tempCache init: cache size.
			cache addAll: tempCache].
	infinitary _ false! !

!ArrowGraph methodsFor: 'private' stamp: 'btr 10/12/2001 05:48'!
holdWeakly
	"The cache is forced to be a WeakSet, whose references aren't valid to 
	the garbage collector. Allows evaluation to be lazy, treating the inner 
	set as a cache. This can be used whenever graph definition is 
	intensional, even if the extension of the graph is finite, since the 
	algorithm is only used when cache searches fail. However, if the 
	graph is the only source of references to the arrow elements and they 
	are not trivially recomputed, then my cache should #holdStrongly."
	| tempCache |
	tempCache _ WeakSet newFrom: {}.
	cache
		ifNotNil: [tempCache init: cache size.
			cache addAll: tempCache].
	infinitary _ true! !

!ArrowGraph methodsFor: 'private' stamp: 'btr 6/15/2001 07:09'!
orientTailSmaller
	"Use this to be lazy about iterating through the apex. This ensures 
	that the tail points to something with fewer elements. Use only when  
	the head/tail distinctions of the apex are not meaningful."
	apex head isExtensional
			& (apex tail isIntensional
					or: [apex head cache size < apex tail cache size])
		ifTrue: [apex _ Reference to: apex inv]! !


!ArrowGraph class methodsFor: 'accessing' stamp: 'btr 6/10/2001 16:27'!
bottom
	"Answer a bottom element of the set-theoretic lattice."
	^ EmptyGraph new! !

!ArrowGraph class methodsFor: 'accessing' stamp: 'btr 10/11/2001 01:08'!
id
	"Return the maximal identity graph."
	^ IdentityGraph over: self top! !

!ArrowGraph class methodsFor: 'accessing' stamp: 'btr 6/11/2001 10:03'!
top
	"Answer a top element of the set-theoretic lattice of graphs."
	^ TopGraph new! !

!ArrowGraph class methodsFor: 'instance creation' stamp: 'btr 9/24/2001 22:38'!
from: domainGraph to: codomainGraph 
	"Create an initialized empty graph, and include type-checking information. Let the user add the details later."
	^ self new
		 domain: domainGraph;
		 codomain: codomainGraph! !

!ArrowGraph class methodsFor: 'instance creation' stamp: 'btr 6/12/2001 07:11'!
new
	"Ensure that all my sub-instances are initialized automatically. Don't let 
	ArrowGraph itself be instantiated."
	^ ArrowGraphMain basicNew initialize! !

!ArrowGraph class methodsFor: 'instance creation' stamp: 'btr 10/16/2001 22:25'!
newFrom: aCollection 
	"Create a new extensional graph with the contents of aCollection. #as: methods delegate to class>>newFrom:, so this supports casting collections and collection literals to ArrowGraphs whenever possible."
	^ self new holdStrongly; beExtensional; cache: aCollection! !

!ArrowGraph class methodsFor: 'instance creation' stamp: 'btr 6/15/2001 07:03'!
newFromArrow: anArrow 
	"Annotate the argument with an initialized empty graph."
	^ self new apex: anArrow! !

!ArrowGraph class methodsFor: 'instance creation' stamp: 'btr 6/21/2001 00:05'!
newFromArrow: anArrow caching: anArrowGraph 
	"Annotate the argument with an initialized graph, caching some 
	elements."
	^ self new apex: anArrow;
		 cache: anArrowGraph asSet! !

!ArrowGraph class methodsFor: 'instance creation' stamp: 'btr 9/24/2001 22:37'!
newFromArrow: anArrow from: domainGraph to: codomainGraph
	"Annotate the argument with an initialized empty graph, and include type-checking information."
	^ self new apex: anArrow;
		domain: domainGraph;
		codomain: codomainGraph! !


!ArrowGraphMain methodsFor: 'accessing' stamp: 'btr 9/19/2001 20:44'!
codomain
	codomain
		ifNil: [self initializeCodomain].
	^ codomain! !

!ArrowGraphMain methodsFor: 'accessing' stamp: 'btr 9/19/2001 20:45'!
domain
	domain
		ifNil: [self initializeDomain].
	^ domain! !

!ArrowGraphMain methodsFor: 'comparing' stamp: 'btr 6/10/2001 14:02'!
isSubGraphOf: anArrowGraph 
	"Answers whether the argument contains all of the receiver's arrows.  
	The implementation so far is only extensional."
	(self isExtensional
			and: [cache size > anArrowGraph cache size])
		ifTrue: [^ false].
	(cache isEmpty
			or: [anArrowGraph cache isEmpty])
		ifTrue: [^ true].
	^ (cache
		anySatisfy: [:eachArrow | (anArrowGraph includes: eachArrow) not]) not! !

!ArrowGraphMain methodsFor: 'comparing' stamp: 'btr 6/10/2001 12:43'!
isSuperGraphOf: anArrowGraph 
	"Set-theoretic inclusion."
	^ anArrowGraph isSubGraphOf: self! !

!ArrowGraphMain methodsFor: 'filtering' stamp: 'btr 9/20/2001 16:06'!
codomain: anArrowGraph 
	"Protected mutation and checking against existing elements."
	codomain
		ifNotNil: [| a b |
			a _ codomain.
			codomain _ nil.
			b _ self clone codomain: anArrowGraph.
			codomain _ a.
			^ b].
	self isExtensional
		ifTrue: [(cache
					allSatisfy: [:each | anArrowGraph includes: each codomainElement])
				ifFalse: [self error: 'The argument graph does not include all of the codomain elements in the receiver.']].
	"Are more checks needed here?"
	codomain _ anArrowGraph! !

!ArrowGraphMain methodsFor: 'filtering' stamp: 'btr 9/20/2001 16:06'!
domain: anArrowGraph 
	"Protected mutation and checking against existing elements."
	domain
		ifNotNil: [| a b | 
			a _ domain.
			domain _ nil.
			b _ self clone domain: anArrowGraph.
			domain _ a.
			^ b].
	self isExtensional
		ifTrue: [(cache
					allSatisfy: [:each | anArrowGraph includes: each domainElement])
				ifFalse: [self error: 'The argument graph does not include all of the domain elements in the receiver.']].
	"Are more checks needed here?"
	domain _ anArrowGraph! !

!ArrowGraphMain methodsFor: 'filtering' stamp: 'btr 10/11/2001 02:24'!
domain: domainGraph codomain: codomainGraph
	"Sugaring."
	^ (self domain: domainGraph) codomain: codomainGraph! !

!ArrowGraphMain methodsFor: 'filtering' stamp: 'btr 6/21/2001 01:17'!
filteredBy: aBlockContext 
	"Provide for filtering of my arrows by a test block taking the arrow as  
	argument. In this generic case, no cache or set of example values is  
	given."
	^ FilteredGraph
		newFrom: {}
		of: self
		for: aBlockContext! !

!ArrowGraphMain methodsFor: 'filtering' stamp: 'btr 6/21/2001 01:17'!
filteredBy: aBlockContext caching: anArrowGraph 
	"Provide for filtering of my arrows by a test block taking the arrow as argument and an example graph of arrows satisfying the test."
	^ FilteredGraph
		newFrom: anArrowGraph asSet
		of: self
		for: aBlockContext! !

!ArrowGraphMain methodsFor: 'filtering' stamp: 'btr 9/14/2001 22:01'!
restrictedTo: anArrowGraph 
	"The argument acts as a filter over the set of nodes (arrows referred to  
	by the receiver's arrows) that the receiver applies to."
	^ FilteredOverGraph newFromArrow: (Arrow newFromAssociation: anArrowGraph -> self in: self world objects)! !

!ArrowGraphMain methodsFor: 'filtering' stamp: 'btr 10/11/2001 02:28'!
testFor: aBlockContext 
	"Answer the relation's graph representing the test mode of a predicate,  
	given as a block closure. The result contains exactly the identity arrows 
	over arrows satisfying the predicate."
	^ (PluggableGraph
		for: [:eachArrow | eachArrow isIdentity])
		domain: self
		codomain: self >> aBlockContext! !

!ArrowGraphMain methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:45'!
initializeCodomain
	| tempCache | 
			tempCache _ self cache
						collect: [:each | each codomainElement].
			self isExtensional
				ifTrue: [^ ArrowGraph new cache: tempCache].
			codomain _ ArrowGraph top
						>> [:each | self treatsAsCodomainElement: each] cache: tempCache! !

!ArrowGraphMain methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:45'!
initializeDomain
	| tempCache | 
			tempCache _ self cache
						collect: [:each | each domainElement].
			self isExtensional
				ifTrue: [^ ArrowGraph new cache: tempCache].
			domain _ ArrowGraph top
						>> [:each | self treatsAsDomainElement: each] cache: tempCache! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 6/11/2001 10:29'!
applyTo: anArrow 
	"A method for applying a graph as a function to an arrow. This  
	amounts to returning a graph of all arrows referenced by the heads of 
	those arrows whose tails reference the argument. This implementation  
	is the most generic case of graph-application. If the graph is intensional, coerce the arrow to a singleton graph and re-cast the call."
	self isExtensional
		ifTrue: [^ ArrowGraph new
				cache: (cache
						select: [:each | each domainElement == anArrow]
						thenCollect: [:each | each codomainElement])].
	^ self applyToGraph: anArrow asGraph! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 10/21/2001 11:24'!
applyToGraph: anArrowGraph 
	"Generalizes #applyTo: so that a graph can be applied to all the elements 
	of another graph. I always answer a new graph, and always cache 
	any possible values."
	^ (GraphApplicationGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph in: self world objects))
		cache: (ArrowGraph new beExtensional cache: self cache)
				* (ArrowGraph new beExtensional cache: anArrowGraph cache)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 11/20/2001 21:03'!
graphCompose: anArrowGraph 
	"Answer a graph of arrows which are those resulting from all possible  
	compositions of arrows from the receiver and argument respectively.  
	#asGraph coerces arrows to singleton graphs."
	^ RelCompositionGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/16/2001 15:37'!
inclusionGraph
	"Sugaring; delegation to a graph type."
	^ GraphInclusionGraph of: self! !

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

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 11/20/2001 01:12'!
join: anArrowGraph 
	^ RelJoinGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/28/2001 01:31'!
leftConjugateWith: anArrowGraph 
	^ GraphLeftConjugateGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/26/2001 12:37'!
leftQuotientWith: anArrowGraph
	^ GraphLeftQuotientGraph newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 4/10/2001 20:54'!
predecessorsOf: anArrow 
	"Answer the nodes preceding the given node using my arrows by one 
	step. "
	^ self inv * anArrow! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/18/2001 07:11'!
projected
	^ (ProjectionGraph of: self) codomain! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/28/2001 01:32'!
rightConjugateWith: anArrowGraph 
	^ GraphRightConjugateGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/26/2001 12:38'!
rightQuotientWith: anArrowGraph 
	^ GraphRightQuotientGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/24/2001 01:29'!
selfComposeTimes: aNatural 
	"Apply composition to myself n times and answer the result."
	| temp |
	aNatural < 0
		ifTrue: [^ self inv selfComposeTimes: aNatural negated].
	aNatural = 0
		ifTrue: [^ self reflexiveClosure].
	temp _ self.
	1
		to: aNatural
		by: 1
		do: [temp _ self graphCompose: temp].
	^ temp! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 11/20/2001 01:12'!
split: anArrowGraph 
	^ RelSplitGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/16/2001 15:32'!
successorsOf: anArrow 
	"Answer the nodes following the given node using my arrows by one  
	step."
	^ self * anArrow! !

!ArrowGraphMain methodsFor: 'set operations' stamp: 'btr 11/20/2001 21:57'!
<*> anArrowGraph 
	"Cartesian Product."
	^ SetProductGraph of: self with: anArrowGraph! !

!ArrowGraphMain methodsFor: 'set operations' stamp: 'btr 11/20/2001 21:57'!
<+> anArrowGraph 
	"Disjoint sum."
	^ SetDisjointSumGraph of: self with: anArrowGraph! !

!ArrowGraphMain methodsFor: 'set operations' stamp: 'btr 11/20/2001 22:08'!
<<*>> anArrowGraph 
	"Cartesian Product of graphs as relations."
	^ RelProductGraph of: self with: anArrowGraph! !

!ArrowGraphMain methodsFor: 'set operations' stamp: 'btr 11/20/2001 22:08'!
<<+>> anArrowGraph 
	"Disjoint sum of graphs as relations."
	^ RelDisjointSumGraph of: self with: anArrowGraph! !

!ArrowGraphMain methodsFor: 'testing' stamp: 'btr 4/12/2001 14:49'!
hasMetaArrowsFor: anArrowGraph 
	"Answers whether the receiver contains arrows which reference any  
	arrows from the argument."
	(cache isEmpty
			or: [anArrowGraph cache isEmpty])
		ifTrue: [^ false].
	cache size ~= anArrowGraph cache size
		ifTrue: [^ false].
	^ cache
		anySatisfy: [:element | (anArrowGraph includes: element tail)
				or: [anArrowGraph includes: element head]]! !

!ArrowGraphMain methodsFor: 'testing' stamp: 'btr 6/13/2001 19:29'!
includes: anArrow 
	"Answers whether the argument is part of the receiver."
	"This method over-rides the set method to allow for intensional  
	description of graphs and their arrows, which will permit infinitary  
	graphs. The intensional query is performed after the cache is  
	searched. If intensional inclusion works, add it to the cache if it is weak (to avoid memory leaks in the strong case)."
	^ (cache isEmpty not
			and: [cache includes: anArrow])
		or: [self isIntensional
				and: [(self intensionallyIncludes: anArrow)
					ifTrue: [self holdsWeakly ifTrue: [cache add: anArrow]. true]
					ifFalse: [false]]]! !

!ArrowGraphMain methodsFor: 'testing' stamp: 'btr 4/12/2001 15:10'!
includesReferencesTo: anArrow 
	"Answers whether the receiver contains arrows which reference the  
	argument."
	"This method over-rides the set method to allow for intensional  
	description of graphs and their arrows, which will permit infinitary  
	graphs. The intensional query should be implemented after the internal 
	cache is searched."
	^ cache isEmpty not and: [cache
		anySatisfy: [:eachArrow | eachArrow references: anArrow]]! !

!ArrowGraphMain methodsFor: 'testing' stamp: 'btr 10/14/2001 12:37'!
treatsAsCodomainElement: anArrow 
	"Don't initialize the codomain if it hasn't already been done, and perform a basic type check."
	codomain
		ifNotNil: [^ self codomain includes: anArrow].
	^ anArrow isKindOf: Arrow! !

!ArrowGraphMain methodsFor: 'testing' stamp: 'btr 10/14/2001 12:38'!
treatsAsDomainElement: anArrow 
	"Don't initialize the domain if it hasn't already been done, and  
	perform a basic type check."
	domain
		ifNotNil: [^ self domain includes: anArrow].
	^ anArrow isKindOf: Arrow! !

!ArrowGraphMain methodsFor: 'testing' stamp: 'btr 6/10/2001 10:07'!
treatsAsGround: anArrow 
	"An arrow is a ground arrow in a graph iff it is not a meta-information 
	arrow of one of the graph's arrows."
	^ (self includes: anArrow)
		and: [(self includes: anArrow head) not
				and: [(self includes: anArrow tail) not]]! !

!ArrowGraphMain methodsFor: 'testing' stamp: 'btr 6/10/2001 10:07'!
treatsAsGroundNode: anArrow 
	"Answers whether arrows leading from the argument exist in the  
	receiver."
	^ (self * anArrow) size = 0! !


!ApplyToGraph methodsFor: 'testing' stamp: 'btr 3/8/2001 16:41'!
intensionallyIncludes: anArrow 
	"For now, count any pair of a graph and an arrow as an element."
	^ (anArrow tail isKindOf: ArrowGraph)
		& (anArrow head isKindOf: Arrow)! !


!ArrowGraphMain class methodsFor: 'instance creation' stamp: 'btr 6/12/2001 09:07'!
new
	"Ensure all my sub-instances are initialized. This overrides ArrowGraph 
	class>>#new which coerces instantiations to be of ArrowGraphMain type."
	^ self basicNew initialize! !


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


!BinaryOperatorGraph methodsFor: 'accessing' stamp: 'btr 9/26/2001 12:14'!
firstArg
	^ apex tail object! !

!BinaryOperatorGraph methodsFor: 'accessing' stamp: 'btr 10/17/2001 04:07'!
firstArgArrow
	^ apex tail! !

!BinaryOperatorGraph methodsFor: 'accessing' stamp: 'btr 9/26/2001 12:14'!
secondArg
	^ apex head object! !

!BinaryOperatorGraph methodsFor: 'accessing' stamp: 'btr 10/17/2001 04:07'!
secondArgArrow
	^ apex head! !


!BinaryOperatorGraph class methodsFor: 'instance creation' stamp: 'btr 10/16/2001 23:32'!
of: firstGraph with: secondGraph 
	^ self new apex: firstGraph -> secondGraph;
		 initialize! !


!CategoryGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 14:49'!
identities
	"The identity mappings over my nodes."
	^ identities! !

!CategoryGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 14:48'!
identityOf: anArrow 
	"Check for proper inclusion of the argument as a node of the category. 
	The answer should represent the unique-per-node identity mapping 
	over it, therefore 'identities' should not have arrows with duplicate 
	value."
	| temp |
	(nodes includes: anArrow)
		ifFalse: [^ nil].
	temp _ anArrow raiseIdentity.
	^ identities cache
		detect: [:each | each equals: temp]
		ifNone: [temp addToGraph: identities]! !

!CategoryGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 14:49'!
nodes
	"The arrows treated as nodes in this category."
	^ nodes! !

!CategoryGraph methodsFor: 'initialize' stamp: 'btr 6/13/2001 05:34'!
initialize
	super initialize.
	nodes _ ArrowGraph new holdStrongly.
	identities _ ArrowGraph new holdWeakly.
	compositions _ CompositionGraph new holdWeakly! !

!CategoryGraph methodsFor: 'initialize' stamp: 'btr 5/21/2001 11:30'!
nodes: anArrowGraph 
	"Protected mutator. This should also check for consistency against already added arrows."
	nodes
		ifNotNil: [^ self class over: anArrowGraph].
	nodes _ anArrowGraph! !

!CategoryGraph methodsFor: 'testing' stamp: 'btr 5/21/2001 11:32'!
includes: anArrow 
	"Check for normal inclusion, and scan for possible compositions that lead to the argument."
	(super includes: anArrow)
		ifTrue: [^ true].
	cache
		do: [:each | cache
				do: [:other | (each ++ other = anArrow
							or: [other ++ each = anArrow])
						ifTrue: [^ true]]].
	^ false! !

!CategoryGraph methodsFor: 'testing' stamp: 'btr 5/21/2001 11:33'!
intensionallyIncludes: anArrow
	"Ensure that the arrow is over my nodes graph's elements."
	^ (nodes includes: anArrow head)
			and: [nodes includes: anArrow tail]! !


!ConcreteArrow methodsFor: 'accessing' stamp: 'btr 5/15/2001 10:06'!
at: anIndex put: anArrow 
	"Changes the indexed reference to the argument. Type-checking and  
	mutation protection performed by head:tail:. Returns a new arrow."
	((super at: anIndex + 1) isKindOf: Reference)
		ifTrue: [^ self notYetImplemented].
	^ super
		at: anIndex + 1
		put: (Reference to: anArrow)! !

!ConcreteArrow methodsFor: 'accessing' stamp: 'btr 4/2/2001 12:30'!
head
	"Answers the second reference."
	^ head value! !

!ConcreteArrow methodsFor: 'accessing' stamp: 'btr 4/2/2001 12:30'!
tail
	"Answers the first reference."
	^ tail value! !

!ConcreteArrow methodsFor: 'accessing' stamp: 'btr 5/15/2001 09:56'!
world
	"This is a recursive method to determine what world an arrow is in  
	without having to perform searches throuh all frames and graphs."
	"For now, assume that constructed arrows have the same world as the  
	arrows they refer to. 
	WARNING: an arrow over two arrows that happen to be from different 
	worlds is ambiguous. Currently the implementation will return the tail's 
	world."
	(tail notNil
			and: [self tail world notNil])
		ifTrue: [^ self tail world].
	(head notNil
			and: [self head world notNil])
		ifTrue: [^ self head world].
	"Last resort: treat ArrowWorld as a lobby."
	^ ArrowWorld! !

!ConcreteArrow methodsFor: 'comparing' stamp: 'btr 4/2/2001 13:18'!
equals: anArrow 
	"Tests for extensional equality. That is, equality via comparing the  
	object's variables. Translates to Arrow system equality within square  
	and relativised square frames. Basically, two arrows are #equals: if 
	their references point to equal objects. Note that #= is the same as #==, object identity. "
	(anArrow isKindOf: self class)
		ifFalse: [^ false].
	^ self head = anArrow head
		and: [self tail = anArrow tail]! !

!ConcreteArrow methodsFor: 'converting' stamp: 'btr 5/26/2001 09:43'!
asAbstract
	"Conversion to abstract arrows takes the construction information  
	(the instance variables), and transmit / delegate that information to the 
	right head- and tail-graphs."
	self world heads add: self raiseHead.
	self world tails add: self raiseTail.
	^ self becomeForward: Arrow new! !

!ConcreteArrow methodsFor: 'converting' stamp: 'btr 6/10/2001 14:50'!
asConcrete
	"I am already concrete."
	^ self! !

!ConcreteArrow methodsFor: 'copying' stamp: 'btr 6/10/2001 09:45'!
deepCopy
	"Arrow structures will be cyclical at some point, so deep copies are 
	dangerous."
	self shouldNotImplement! !

!ConcreteArrow methodsFor: 'initialize' stamp: 'btr 4/2/2001 12:31'!
head: headArrow tail: tailArrow 
	"Sets both references. This supports immutability. Once both  
	head and tail are not nil, all calls to this method return a new arrow."
	| newArrow |
	self head isNil | self tail isNil
		ifTrue: [head _ Reference to: headArrow.
			tail _ Reference to: tailArrow.
			^ self].
	newArrow _ self class basicNew.
	"Allows adaptation to subclasses; #basicNew bypasses the current  
	disablement of #new in Arrow."
	self become: newArrow.
	head _ Reference to: headArrow.
	tail _ Reference to: tailArrow.
	self become: newArrow.
	^ newArrow! !

!ConcreteArrow methodsFor: 'initialize' stamp: 'btr 4/2/2001 12:33'!
initialize
	"Isolates use of assignment to prime areas. See Arrow>>head:tail:"
	super initialize.
	head _ Reference to: nil.
	tail _ Reference to: nil! !

!ConcreteArrow methodsFor: 'initialize' stamp: 'btr 4/7/2001 22:21'!
isInitialized
	"This does not catch Arrows like those produced by ArrowGraph>>anyOne 
	even though they reference nil."
	^ (head isKindOf: Reference)
			and: [tail isKindOf: Reference]! !

!ConcreteArrow methodsFor: 'operations' stamp: 'btr 5/29/2001 09:17'!
composeElements
	"Return the composition of my tail and head in that order."
	^ self source ++ self target! !

!ConcreteArrow methodsFor: 'reifications' stamp: 'btr 6/10/2001 14:50'!
raiseComposition
	"Answer the arrow leading to the composition of my elements."
	^ self -> self composeElements! !


!ConcreteArrow class methodsFor: 'instance creation' stamp: 'btr 4/2/2001 12:39'!
new
	"Don't let an un-initialized Arrow get made without some effort."
	^ super new initialize! !


!EquationalArrow methodsFor: 'accessing' stamp: 'btr 9/28/2001 01:43'!
allMetaArrows
	"Return all the arrows referring to me as their heads or tails."
	^ headsTo \/ tailsTo! !

!EquationalArrow methodsFor: 'accessing' stamp: 'btr 10/14/2001 14:50'!
headsTo
	headsTo
		ifNil: [self initializeHeadsTo].
	^ headsTo! !

!EquationalArrow methodsFor: 'accessing' stamp: 'btr 10/14/2001 14:50'!
tailsTo
	tailsTo
		ifNil: [self initializeTailsTo].
	^ tailsTo! !

!EquationalArrow methodsFor: 'initialize' stamp: 'btr 10/14/2001 14:49'!
initializeHeadsTo
	"Ensure that I don't hold my meta-arrows strongly since they should  
	strongly hold me. If the meta-arrows are needed for my semantics,  
	there should be back-arrows from me to them."
	headsTo _ ArrowGraph top >> [:each | each head == self]! !

!EquationalArrow methodsFor: 'initialize' stamp: 'btr 10/14/2001 14:49'!
initializeTailsTo
	"Ensure that I don't hold my meta-arrows strongly since they should  
	strongly hold me. If the meta-arrows are needed for my semantics,  
	there should be back-arrows from me to them."
	tailsTo _ ArrowGraph top
				>> [:each | each tail == self]! !


!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 9/23/2001 23:32'!
anyOne
	"If my cache is empty, I would have to provide an example of an object  
	satisfying my testBlock, which actually may have no answer."
	cache isEmpty
		ifTrue: [objectGraph cache
				detect: [:each | testBlock value: each object]
				ifNone: [self notYetImplemented]].
	^ cache anyOne! !

!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 01:25'!
arrowFor: anObject 
	"This performs the intended check for membership by using objectGraph  
	properly."
	^ self
		add: (Arrow wrapping: anObject in: self objectGraph)! !

!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 6/13/2001 12:59'!
block
	^ testBlock! !

!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 13:28'!
graph
	"Polymorphism with FilteredGraph>>#graph. However, #objectGraph looks for the original objectGraph in the filter chain."
	^ objectGraph! !

!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 01:14'!
kernel
	"This allows FilteredObjectGraphs to be used for ObjectGraphs transparently. objectGraph is accessed indirectly because of the way successive filter applications work."
	^ self objectGraph kernel! !

!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 6/13/2001 15:15'!
objectGraph
	"Polymorphism between ObjectGraphs and FilteredObjectGraphs is 
	supported here. See FilteredObjectGraph>>#over:."
	^ objectGraph class == ObjectGraph
		ifTrue: [objectGraph]
		ifFalse: [objectGraph objectGraph]! !

!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 01:37'!
operand
	"Ensure that other filteredObjectGraphs know what my direct argument 
	is, since #objectGraph bypasses all FilteredObjectGraphs for the original 
	ObjectGraph."
	^ objectGraph! !

!FilteredObjectGraph methodsFor: 'adding' stamp: 'btr 6/18/2001 13:33'!
add: aWrapperArrow
	"Perform the usual method, but also ensure that my operand(s) and the original objectGraph get this added to their caches. The latter should not fail if the former doesn't."
	super add: aWrapperArrow.
	^ self operand add: aWrapperArrow! !

!FilteredObjectGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:48'!
addFor: anObject 
	"Convenient method. Transparently returns a wrapping."
	^ self
		add: (Arrow wrapping: anObject in: self objectGraph)! !

!FilteredObjectGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:48'!
addForAll: aCollection 
	"Convenient method."
	^ aCollection do: [:each | self add: (Arrow wrapping: each in: self objectGraph)]! !

!FilteredObjectGraph methodsFor: 'comparing' stamp: 'btr 6/13/2001 18:58'!
isSubGraphOf: anArrowGraph 
	"My objectGraph and its objectGraph(s) are super-graphs of me."
	^ (anArrowGraph == objectGraph
		or: [objectGraph isSubGraphOf: anArrowGraph])
		or: [super isSubGraphOf: anArrowGraph]! !

!FilteredObjectGraph methodsFor: 'filtering' stamp: 'btr 6/15/2001 06:34'!
filteredBy: aBlockContext 
	"Provide for filtering of my object wrappers by a test block taking the  
	object as argument. In this generic case, my filtered cache is given.
	Note that the testBlock given is used for testing *after* testing with the 
	testBlocks of FilteredGraphs that I am based on, so that there is no need 
	to compose block filters manually."
	^ FilteredObjectGraph
		newFrom: (cache
				select: [:each | aBlockContext value: each object])
		over: self
		for: aBlockContext! !

!FilteredObjectGraph methodsFor: 'initialize' stamp: 'btr 6/13/2001 19:11'!
block: aBlockContext 
	"I can't let my cache invalidate the block I set, so perform some checks  
	beforehand."
	(cache isNil not
			and: [cache isEmpty])
		ifTrue: [(cache
					select: [:each | (testBlock value: each object) not]) size > 0
				ifTrue: [self error: 'Some cache elements are invalid by this test.']].
	testBlock
		ifNotNil: [^ self class
				newFrom: cache
				over: objectGraph
				for: aBlockContext].
	testBlock _ aBlockContext!
]style[(7 13 3 88 3 5 20 5 23 5 15 6 3 9 8 4 22 1 14 4 8 47 5 9 16 4 20 5 11 11 10 13 4 9 3 13)f1b,f1cblack;b,f1,f1c148046000,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;! !

!FilteredObjectGraph methodsFor: 'initialize' stamp: 'btr 10/16/2001 22:22'!
cache: aWrapperCollection 
	"Perform a few checks to ensure the cache's consistency."
	testBlock
		ifNil: [self error: 'Don''t set this graph''s cache before giving it a block to test against.'].
	objectGraph
		ifNil: [self error: 'Don''t set this graph''s cache before giving it an objectGraph to use.'].
	cache _ (aWrapperCollection
				removeAllSuchThat: [:each | ((testBlock value: each object)
						and: [each objectGraph == objectGraph]) not]) asSet! !

!FilteredObjectGraph methodsFor: 'initialize' stamp: 'btr 9/24/2001 01:18'!
initializeCodomain
	"Since I filter objectGraphs, which are monoids, this test just delegates to  
	them."
	codomain _ self objectGraph codomain! !

!FilteredObjectGraph methodsFor: 'initialize' stamp: 'btr 9/24/2001 01:18'!
initializeDomain
	"Since I filter objectGraphs, which are monoids, this test just delegates to  
	them."
	domain _ self objectGraph domain! !

!FilteredObjectGraph methodsFor: 'initialize' stamp: 'btr 10/21/2001 11:23'!
over: anObjectGraph 
	"Protected mutator. This also is polymorphic in allowing for composition  
	of filters (of FilteredObjectGraphs)."
	objectGraph
		ifNotNil: [^ self class
				newFrom: self cache
				over: anObjectGraph
				for: testBlock].
	anObjectGraph class == FilteredObjectGraph
		ifTrue: [objectGraph _ anObjectGraph.
			testBlock _ [:value | (testBlock value: value)
						and: [anObjectGraph
								includes: (Arrow wrapping: value in: self objectGraph)]].
			^ self].
	anObjectGraph class == ObjectGraph
		ifFalse: [self error: 'I cannot take that as an argument of my filter.'].
	objectGraph _ anObjectGraph! !

!FilteredObjectGraph methodsFor: 'operations' stamp: 'btr 9/24/2001 01:35'!
applyTo: anArrow 
	^ ((self raisedApplicationTo: anArrow)
		>> testBlock) codomain! !

!FilteredObjectGraph methodsFor: 'operations' stamp: 'btr 9/24/2001 01:35'!
applyToGraph: anArrowGraph 
	^ ((self raisedApplicationTo: anArrowGraph)
		>> testBlock) codomain! !

!FilteredObjectGraph methodsFor: 'operations' stamp: 'btr 9/24/2001 01:36'!
invertedApplyTo: anArrow 
	^ ((self raisedInverseApplicationTo: anArrow)
		>> testBlock) domain! !

!FilteredObjectGraph methodsFor: 'operations' stamp: 'btr 9/24/2001 01:37'!
invertedApplyToGraph: anArrowGraph 
	^ ((self raisedInverseApplicationTo: anArrowGraph)
		>> testBlock) domain! !

!FilteredObjectGraph methodsFor: 'set operations' stamp: 'btr 9/28/2001 01:37'!
/\ anArrowGraph 
	"My intersection with my objectGraph returns myself."
	anArrowGraph = objectGraph
		ifTrue: [^ self].
	((anArrowGraph isKindOf: FilteredObjectGraph)
			and: [anArrowGraph objectGraph == self objectGraph])
		ifTrue: ["Taking the intersection of two filteredObjectGraphs which are  
			over the same objectGraph is handled to take the intersection of 
			the testBlock and caches."
			^ FilteredObjectGraph
				newFrom: (cache intersect: anArrowGraph cache)
				over: (self operand intersect: anArrowGraph operand)
				for: [:value | (testBlock value: value)
						and: [anArrowGraph block value: value]]].
	^ super /\ anArrowGraph! !

!FilteredObjectGraph methodsFor: 'set operations' stamp: 'btr 9/28/2001 01:43'!
\/ anArrowGraph 
	"My union with my objectGraph returns the objectGraph."
	anArrowGraph = objectGraph
		ifTrue: [^ anArrowGraph].
	((anArrowGraph isKindOf: FilteredObjectGraph)
			and: [anArrowGraph objectGraph == self objectGraph])
		ifTrue: ["Taking the union of two filteredObjectGraphs which are  
			over the same objectGraph is handled to take the union of  
			the testBlock and caches."
			^ FilteredObjectGraph
				newFrom: (cache union: anArrowGraph cache)
				over: (self operand union: anArrowGraph operand)
				for: [:value | (testBlock value: value)
						or: [anArrowGraph block value: value]]].
	^ super \/ anArrowGraph! !

!FilteredObjectGraph methodsFor: 'testing' stamp: 'btr 6/18/2001 01:25'!
intensionallyIncludes: anArrow 
	"testBlock determines membership. Of course I check that the arrow is  
	a wrapper in the first place, and also that it belongs to the same  
	objectGraph as mine."
	^ (anArrow isWrapper
			and: [anArrow objectGraph = self objectGraph])
		and: [testBlock value: anArrow object]! !


!FilteredObjectGraph class methodsFor: 'instance creation' stamp: 'btr 6/13/2001 12:24'!
newFrom: aCollection
	^ self notYetImplemented! !

!FilteredObjectGraph class methodsFor: 'instance creation' stamp: 'btr 6/13/2001 19:11'!
newFrom: aCollection over: anObjectGraph for: aBlockContext 
	"Create a new extensional graph with the contents of aCollection that  
	verifies aBlockContext. The order of message sends is critical."
	^ self new holdStrongly block: aBlockContext;
		 over: anObjectGraph;
		 cache: aCollection!
]style[(9 11 7 13 6 13 3 137 4 4 25 13 11 13 12 11)f1b,f1cblack;b,f1b,f1cblack;b,f1b,f1cblack;b,f1,f1c148046000,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;! !

!FilteredObjectGraph class methodsFor: 'instance creation' stamp: 'btr 6/13/2001 22:11'!
over: anObjectGraph for: aBlockContext
	"Sugaring for when you don't have a cache to specify."
	^ self newFrom: {} over: anObjectGraph for: aBlockContext! !


!FilteredOverGraph methodsFor: 'accessing' stamp: 'btr 6/15/2001 07:01'!
argument
	^ self apex head! !

!FilteredOverGraph methodsFor: 'accessing' stamp: 'btr 6/15/2001 07:01'!
filter
	^ self apex tail! !

!FilteredOverGraph methodsFor: 'testing' stamp: 'btr 3/16/2001 13:30'!
intensionallyIncludes: anArrow
	(self filter includes: anArrow head) & (self filter includes: anArrow tail)
		ifFalse: [^ false.].
	^ self argument intensionallyIncludes: anArrow! !


!GraphApplicationGraph methodsFor: 'accessing' stamp: 'btr 10/9/2001 22:03'!
anyResultForArrow: anArrow
	"Forces evaluation of one result arrow."
	^ (self resultForArrow: anArrow) anyOne! !

!GraphApplicationGraph methodsFor: 'accessing' stamp: 'btr 9/14/2001 21:52'!
argument
	^ self apex head object! !

!GraphApplicationGraph methodsFor: 'accessing' stamp: 'btr 9/14/2001 21:52'!
operator
	^ self apex tail object! !

!GraphApplicationGraph methodsFor: 'accessing' stamp: 'btr 10/12/2001 06:23'!
resultForArrow: anArrow 
	"Apply my operator to the argument. Remember that this still always  
	returns a graph."
	| temp |
	(self argument includes: anArrow)
		ifFalse: [^ ArrowGraph bottom].
	temp _ self operator applyTo: anArrow.
	self addAll: temp cache.
	^ temp! !

!GraphApplicationGraph methodsFor: 'comparing' stamp: 'btr 10/9/2001 23:44'!
isSubGraphOf: anArrowGraph
	anArrowGraph class == self class
		ifTrue: [^ anArrowGraph operator == self operator
			and: [self argument isSubGraphOf: anArrowGraph argument]].
	^ super isSubGraphOf: anArrowGraph! !


!GraphLeftConjugateGraph methodsFor: 'converting' stamp: 'btr 9/28/2001 03:04'!
inTermsOfResiduals
	^ (self firstArg \ self secondArg inv) inv! !

!GraphLeftConjugateGraph methodsFor: 'testing' stamp: 'btr 9/28/2001 01:30'!
intensionallyIncludes: anArrow 
	"NOTES: My secondArg is the right of the composition.  
	My firstArg is the result.  
	The argument must complete (as left element of the composition) a  
	composition between some two arrows from the firstArg and secondArg."
	^ self secondArg * anArrow codomainElement * self firstArg inv includes: anArrow domainElement! !


!GraphLeftQuotientGraph methodsFor: 'converting' stamp: 'btr 9/28/2001 03:05'!
inTermsOfConjugates
	^ (self firstArg !!> self secondArg inv) inv! !

!GraphLeftQuotientGraph methodsFor: 'initialize' stamp: 'btr 9/26/2001 12:52'!
initializeCodomain
	codomain _ self secondArg codomain! !

!GraphLeftQuotientGraph methodsFor: 'initialize' stamp: 'btr 9/26/2001 12:54'!
initializeDomain
	domain _ self firstArg codomain! !

!GraphLeftQuotientGraph methodsFor: 'operations' stamp: 'btr 10/17/2001 04:34'!
applyTo: anArrow
	^ (self firstArg inv * anArrow) * (self secondArg)! !

!GraphLeftQuotientGraph methodsFor: 'operations' stamp: 'btr 9/26/2001 12:42'!
invert
	^ self secondArg inv / self firstArg inv! !

!GraphLeftQuotientGraph methodsFor: 'operations' stamp: 'btr 10/17/2001 04:34'!
invertedApplyTo: anArrow 
	^ self secondArg inv * anArrow * self firstArg! !

!GraphLeftQuotientGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 12:27'!
intensionallyIncludes: anArrow
	"I include all arrows that complete compositions, treating my first argument as the left of the composition and the second as the composition results."
	^ (self secondArg inv * anArrow codomainElement) >= (self firstArg inv * anArrow domainElement)! !


!GraphRightConjugateGraph methodsFor: 'converting' stamp: 'btr 9/28/2001 03:05'!
inTermsOfResiduals
	^ (self firstArg inv / self secondArg) inv! !

!GraphRightConjugateGraph methodsFor: 'testing' stamp: 'btr 9/28/2001 02:29'!
intensionallyIncludes: anArrow 
	"NOTES: My secondArg is the left of the composition.  
	My firstArg is the result.  
	The argument must complete (as right element of the composition) a  
	composition between some two arrows from the firstArg and secondArg."
	^ self firstArg inv * anArrow codomainElement * self secondArg includes: anArrow domainElement! !


!GraphRightQuotientGraph methodsFor: 'converting' stamp: 'btr 9/28/2001 03:06'!
inTermsOfConjugates
	^ (self firstArg inv <!! self secondArg) inv! !

!GraphRightQuotientGraph methodsFor: 'initialize' stamp: 'btr 9/26/2001 12:55'!
initializeCodomain
	codomain _ self secondArg domain! !

!GraphRightQuotientGraph methodsFor: 'initialize' stamp: 'btr 9/26/2001 12:55'!
initializeDomain
	domain _ self firstArg domain! !

!GraphRightQuotientGraph methodsFor: 'operations' stamp: 'btr 10/17/2001 04:37'!
applyTo: anArrow
	^ self firstArg * anArrow * self secondArg inv! !

!GraphRightQuotientGraph methodsFor: 'operations' stamp: 'btr 9/26/2001 12:42'!
invert
	^ self secondArg inv \ self firstArg inv! !

!GraphRightQuotientGraph methodsFor: 'operations' stamp: 'btr 10/17/2001 04:37'!
invertedApplyTo: anArrow 
	^ self secondArg * anArrow * self firstArg inv! !

!GraphRightQuotientGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 12:32'!
intensionallyIncludes: anArrow 
	"I include all arrows that complete compositions, treating my second  
	argument as the right of the composition and the first as the  
	composition results."
	^ self secondArg * anArrow codomainElement >= (self firstArg * anArrow domainElement)! !


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


!InductionGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 14:56'!
at: aNatural 
	"Treat the induction graph as indexable."
	^ self fromNatural: aNatural! !

!InductionGraph methodsFor: 'converting' stamp: 'btr 5/31/2001 05:37'!
asNatural: anArrow 
	"Determines which, if any, natural number corresponds to the given 
	arrow in this graph representing formal induction."
	(anArrow object isKindOf: Integer)
		ifTrue: [^ anArrow object]! !

!InductionGraph methodsFor: 'converting' stamp: 'btr 5/31/2001 05:40'!
fromNatural: aNatural 
	"Instantiates an arrow representing the given natural number in 
	inductive terms."
	((aNatural isKindOf: Integer)
		and: [aNatural negative not])
		ifTrue: [^ nil].
	^ Arrow wrapping: aNatural in: monoid! !

!InductionGraph methodsFor: 'initialize' stamp: 'btr 6/18/2001 01:19'!
initialize
	"Set up an underlying monoid from my frame's reflective interface, and 
	set the kernel to wrap 0."
	super initialize.
	monoid _ self frame objects >> [:value | (value isKindOf: Integer) and: [value >= 0]].
	kernel _ Arrow wrapping: 0 in: monoid! !

!InductionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:49'!
initializeCodomain
	codomain _ monoid! !

!InductionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:56'!
initializeDomain
	domain _ monoid! !

!InductionGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:51'!
applyTo: anArrow 
	"Successors should be unique per Arrow."
	(monoid includes: anArrow)
		ifFalse: [^ ArrowGraph bottom].
	anArrow isWrapper not
		ifTrue: [^ super * anArrow].
	^ (Arrow wrapping: anArrow object + 1 in: monoid) asGraph! !

!InductionGraph methodsFor: 'operations' stamp: 'btr 5/30/2001 10:43'!
identify: firstArrow with: secondArrow 
	"Here's a simple pattern-matching core. At least one of these must be a  
	variable. This is highly recursive."
	(self treatsAsVariable: firstArrow)
		ifTrue: ["Take all arrows connected to firstArrow and identify them with 
			the appropriate arrows connected to secondArrow. If both are  
			variables, turn them into the same object and let garbage  
			collection take care of the mess."
			secondArrow isWrapper not
				ifTrue: [firstArrow becomeForward: secondArrow.
					^ self].
			(self hasSuccOf: firstArrow)
				ifTrue: [self
						identify: (self successorOf: firstArrow)
						with: (self successorOf: secondArrow)].
			(self hasPredOf: firstArrow)
				ifTrue: [self
						identify: (self predecessorOf: firstArrow)
						with: (self predecessorOf: secondArrow)].
			firstArrow becomeForward: secondArrow.
			^ firstArrow].
	(self treatsAsVariable: secondArrow)
		ifTrue: [^ self identify: secondArrow with: firstArrow].
	^ self error: 'One of the arrows matched must be a variable.'! !

!InductionGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:02'!
invertedApplyTo: anArrow 
	"Predecessors should be unique per Arrow, and respect successor usage.  
	NOTE: either anArrow is the result of 'monoid anyOne', in which case it 
	is treated as a variable, or it is the result of a successor construction,  
	which numbers converted to my elements produce on their own."
	((monoid includes: anArrow)
			and: [anArrow ~~ kernel])
		ifFalse: [^ ArrowGraph bottom].
	cache
		do: [:each | each codomainElement == anArrow
				ifTrue: [^ each domainElement]].
	^ (cache add: monoid anyOne -> anArrow) domainElement asGraph! !

!InductionGraph methodsFor: 'operations' stamp: 'btr 6/10/2001 14:55'!
predecessorOf: anArrow 
	"Induction is the successor relation. This is its inverse."
	^ self inv * anArrow! !

!InductionGraph methodsFor: 'operations' stamp: 'btr 6/10/2001 14:56'!
successorOf: anArrow 
	"The induction process is the successor relation itself."
	^ self * anArrow! !

!InductionGraph methodsFor: 'testing' stamp: 'btr 5/30/2001 10:20'!
hasPredOf: anArrow 
	"Basic unit test to see if anArrow's successor is resident in memory.  
	Don't use this if you simultaneously need the predecessor, to avoid  
	unnecessary doubling of the work of predecessorOf:."
	^ cache
		anySatisfy: [:each | each codomainElement == anArrow]! !

!InductionGraph methodsFor: 'testing' stamp: 'btr 5/30/2001 10:20'!
hasSuccOf: anArrow 
	"Basic unit test to see if anArrow's successor is resident in memory.  
	Don't use this if you simultaneously need the predecessor, to avoid  
	unnecessary doubling of the work of predecessorOf:."
	^ cache
		anySatisfy: [:each | each domainElement == anArrow]! !

!InductionGraph methodsFor: 'testing' stamp: 'btr 6/13/2001 14:54'!
intensionallyIncludes: anArrow 
	"All my arrows must be concrete and must not lead to a wrapper of 0, 
	since 0 has no predecessor."
	^ anArrow class ~~ Arrow
		and: [anArrow codomainElement object ~= 0]! !

!InductionGraph methodsFor: 'testing' stamp: 'btr 5/30/2001 10:12'!
treatsAsCodomainElement: anArrow 
	"This graph's arrows range exactly over its underlying monoid's arrows."
	^ monoid intensionallyIncludes: anArrow! !

!InductionGraph methodsFor: 'testing' stamp: 'btr 5/30/2001 10:11'!
treatsAsDomainElement: anArrow 
	"This graph's arrows range exactly over its underlying monoid's arrows."
	^ monoid intensionallyIncludes: anArrow! !

!InductionGraph methodsFor: 'testing' stamp: 'btr 5/30/2001 10:38'!
treatsAsVariable: anArrow 
	"Determine if anArrow or any of its predecessors do not have other  
	pre-existing predecessors, unless that predecessor is 0. First check to  
	make sure that you are not checking an arrow that already wraps  
	a value."
	anArrow isWrapper
		ifTrue: [^ false].
	cache
		do: [:each | each codomainElement == anArrow
				ifTrue: [| temp | 
					temp _ each domainElement.
					temp object = 0
						ifTrue: [^ false].
					^ self treatsAsVariable: temp]].
	^ true! !


!MetaFrameGraph methodsFor: 'accessing' stamp: 'btr 5/29/2001 16:53'!
anyOne
	"Relies on my subclasses defining #applyTo: over abstract arrows as  
	well as the ordinary concrete case. Check the cache first to avoid creating new arrows if possible."
	| temp |
	cache isEmpty
		ifFalse: [^ cache anyOne].
	temp _ frame arrows anyOne.
	^ temp -> self * temp! !

!MetaFrameGraph methodsFor: 'accessing' stamp: 'btr 5/8/2001 00:23'!
frame
	^ frame! !

!MetaFrameGraph methodsFor: 'accessing' stamp: 'btr 6/12/2001 10:08'!
world
	"Worlds and frames are implemented the same way for now."
	^ frame! !

!MetaFrameGraph methodsFor: 'comparing' stamp: 'btr 5/29/2001 16:55'!
= anArrowGraph 
	"All MetaFrameGraphs are equivalent which cover the same frame."
	^ self class = anArrowGraph class
		and: [frame = anArrowGraph frame]! !

!MetaFrameGraph methodsFor: 'comparing' stamp: 'btr 5/29/2001 09:02'!
isSubGraphOf: anArrowGraph 
	"A proper superclass of a meta-frame graph's values of something is equivalent to it, unless it is a powerset, which this is not."
	^ self = anArrowGraph! !

!MetaFrameGraph methodsFor: 'initialize' stamp: 'btr 10/17/2001 06:32'!
frame: anArrowFrame 
	"Protected mutator."
	frame
		ifNotNil: [^ self class over: anArrowFrame].
	frame _ anArrowFrame! !

!MetaFrameGraph methodsFor: 'initialize' stamp: 'btr 6/15/2001 19:38'!
initialize
	"Each class should have a default cache semantics and set it up before use."
	super initialize.
	self initializeCache! !

!MetaFrameGraph methodsFor: 'initialize' stamp: 'btr 6/15/2001 19:37'!
initializeCache
	"Graphs of relationships over frames by default should be weak."
	self holdWeakly! !

!MetaFrameGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:58'!
initializeDomain
	domain _ frame! !


!AssertionGraph methodsFor: 'initialize' stamp: 'btr 6/15/2001 19:40'!
initializeCache
	"Unlike most relationships over frame arrows, assertions are intended to drive the caching of those other more basic pieces of information; therefore I should hold my arrows strongly."
	self holdStrongly! !


!CompositionGraph methodsFor: 'initialize' stamp: 'btr 9/30/2001 13:54'!
initializeDomain
	domain _ frame values! !

!CompositionGraph methodsFor: 'operations' stamp: 'btr 9/30/2001 13:53'!
applyTo: anArrow 
	"For pairs of concrete arrows, use the concrete method to handle 
	composition. Otherwise, set up the pattern of information to match 
	against later."
	(anArrow head isConcrete
			and: [anArrow tail isConcrete])
		ifTrue: [^ anArrow composeElements].
	^ self notYetImplemented! !

!CompositionGraph methodsFor: 'testing' stamp: 'btr 9/30/2001 13:58'!
intensionallyIncludes: anArrow 
	"This currently only works for non-abstract arrows. Arrows in this  
	graph type lead from pairs of arrows in the frame composed to the  
	result of their composition."
	(super intensionallyIncludes: anArrow)
		ifFalse: [^ false].
	(anArrow domainElement head isConcrete
			and: [anArrow domainElement tail isConcrete])
		ifTrue: [^ self intensionallyIncludes: anArrow using: #composeElements].
	"Finally check against assertions."
	self notYetImplemented! !


!HeadGraph methodsFor: 'operations' stamp: 'btr 5/29/2001 05:42'!
applyTo: anArrow 
	"Reifies the head of the arrow as another (unique) arrow. Notice that  
	the answer provided is not unique by necessity."
	"TODO: Abstract arrows rely on this being a lazy promise, which it is 
	not."
	cache
		detect: [:each | each domainElement == anArrow]
		ifNone: [| newArrow | 
			"Only works on ConcreteArrows."
			newArrow _ anArrow raiseHead.
			^ cache
				detect: [:eachArrow | eachArrow = newArrow]
				ifNone: [^ self add: newArrow]]! !

!HeadGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:01'!
invertedApplyTo: anArrow 
	"First check to make sure the argument is valid as a reified head arrow. 
	If so, return the arrow it would have been the result of."
	"TODO: Abstract arrows rely on this being a lazy promise, which it is  
	not."
	^ (self intensionallyIncludes: anArrow)
		ifTrue: [anArrow domainElement]
		ifFalse: [ArrowGraph bottom]! !

!HeadGraph methodsFor: 'testing' stamp: 'btr 5/15/2001 18:36'!
intensionallyIncludes: anArrow 
	"My arrows simply have to lead from one arrow in the frame to the  
	arrow that its head references (if there is one)."
	(frame includes: anArrow tail)
		ifFalse: [^ false].
	anArrow class == ConcreteArrow
		ifTrue: [^ self intensionallyIncludes: anArrow using: #head].
	^ false! !

!HeadGraph methodsFor: 'testing' stamp: 'btr 5/15/2001 18:36'!
treatsAsNode: anArrow 
	"All arrows in the frame can have their head references reified as 
	other arrows."
	^ frame includes: anArrow! !


!InversionGraph methodsFor: 'adding' stamp: 'btr 5/29/2001 09:14'!
add: anArrow 
	"Abstract arrows have to be handled with a pattern-matchin algorithm."
	anArrow class = Arrow
		ifFalse: [^ super add: anArrow].
	^ self notYetImplemented! !

!InversionGraph methodsFor: 'initialize' stamp: 'btr 9/30/2001 14:01'!
initializeCodomain
	domain _ codomain _ self frame! !

!InversionGraph methodsFor: 'initialize' stamp: 'btr 9/30/2001 14:01'!
initializeDomain
	domain _ codomain _ self frame! !

!InversionGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:51'!
applyTo: anArrow 
	"Abstract arrows have to be handled by the generic lazy case, looking 
	for arrows leading from it to other arrows. Concrete arrows of any 
	kind, however, are treated in the simplest way possible."
	anArrow class = Arrow
		ifTrue: [^ super applyTo: anArrow].
	^ SingletonGraph of: anArrow inv! !

!InversionGraph methodsFor: 'testing' stamp: 'btr 6/12/2001 10:05'!
intensionallyIncludes: anArrow 
	"I represent the action of Arrow>>#inv."
	^ self intensionallyIncludes: anArrow using: #inv! !


!MetaFrameGraph class methodsFor: 'instance creation' stamp: 'btr 5/27/2001 14:21'!
over: anArrowFrame 
	"Make a new instance of one of my subclasses for a particular frame."
	^ self new initialize frame: anArrowFrame! !


!MetaGraphGraph methodsFor: 'accessing' stamp: 'btr 5/15/2001 11:08'!
graph
	"Return the graph that this meta-graph was instantiated for, or  
	instantiate a graph that satisfies the definition."
	self isInitialized
		ifFalse: [^ self notYetImplemented].
	^ graph! !

!MetaGraphGraph methodsFor: 'initialize' stamp: 'btr 5/15/2001 11:05'!
graph: anArrowGraph 
	"There should only be one meta-graph of my kind per graph."
	graph
		ifNotNil: [^ self class of: anArrowGraph].
	graph _ anArrowGraph! !


!FilteredGraph methodsFor: 'accessing' stamp: 'btr 9/23/2001 23:30'!
anyOne
	"If my cache is empty, I would have to provide an example of an object  
	satisfying my testBlock, which actually may have no answer."
	cache isEmpty
		ifTrue: [graph cache detect: testBlock
			ifNone: [self notYetImplemented]].
	^ cache anyOne! !

!FilteredGraph methodsFor: 'accessing' stamp: 'btr 6/13/2001 16:26'!
block
	^ testBlock! !

!FilteredGraph methodsFor: 'accessing' stamp: 'btr 6/13/2001 16:27'!
graph
	^ graph! !

!FilteredGraph methodsFor: 'comparing' stamp: 'btr 6/13/2001 18:59'!
isSubGraphOf: anArrowGraph 
	"My graph and its graph(s) are super-graphs of me."
	^ (anArrowGraph == graph
			or: [graph isSubGraphOf: anArrowGraph])
		or: [super isSubGraphOf: anArrowGraph]! !

!FilteredGraph methodsFor: 'filtering' stamp: 'btr 6/13/2001 18:49'!
>> aBlockContext 
	"Sugaring. Polymorphic with ObjectGraph>>#>> and FilteredObject>>#>>."
	^ self filteredBy: aBlockContext! !

!FilteredGraph methodsFor: 'filtering' stamp: 'btr 6/15/2001 06:33'!
filteredBy: aBlockContext 
	"Provide for filtering of my object wrappers by a test block taking the  
	object as argument. In this generic case, my filtered cache is given. Note that the testBlock given is used for testing *after* testing with the testBlocks of FilteredGraphs that I am based on, so that there is no need to compose block filters manually."
	^ FilteredGraph
		newFrom: (cache
				select: [:each | aBlockContext value: each])
		of: self
		for: aBlockContext! !

!FilteredGraph methodsFor: 'initialize' stamp: 'btr 6/13/2001 16:28'!
block: aBlockContext 
	"I can't let my cache invalidate the block I set, so perform some checks  
	beforehand."
	(cache isNil not
			and: [cache isEmpty])
		ifFalse: [(cache
					select: [:each | (testBlock value: each) not]) size > 0
				ifTrue: [self error: 'Some cache elements are invalid by this test.']].
	testBlock
		ifNotNil: [^ self class
				newFrom: cache
				of: graph
				for: aBlockContext].
	testBlock _ aBlockContext! !

!FilteredGraph methodsFor: 'initialize' stamp: 'btr 10/16/2001 22:22'!
cache: aWrapperCollection 
	"Perform a few checks to ensure the cache's consistency."
	testBlock
		ifNil: [self error: 'Don''t set this graph''s cache before giving it a block to test against.'].
	graph
		ifNil: [self error: 'Don''t set this graph''s cache before giving it a graph to use.'].
	cache _ (aWrapperCollection
		removeAllSuchThat: [:each | ((testBlock value: each)
				and: [each graph == graph]) not]) asSet! !

!FilteredGraph methodsFor: 'initialize' stamp: 'btr 10/21/2001 11:22'!
graph: anArrowGraph 
	"Protected mutator. This also is polymorphic in allowing for composition  
	of filters (of FilteredGraphs)."
	graph
		ifNotNil: [^ self class
				newFrom: self cache
				of: anArrowGraph
				for: testBlock].
	anArrowGraph class == FilteredGraph
		ifTrue: [graph _ anArrowGraph.
			testBlock _ [:value | (testBlock value: value)
						and: [anArrowGraph includes: value]].
			^ self].
	(anArrowGraph isKindOf: ArrowGraph)
		ifFalse: [self error: 'I can only filter graphs.'].
	graph _ anArrowGraph! !

!FilteredGraph methodsFor: 'initialize' stamp: 'btr 9/24/2001 01:20'!
initializeCodomain
	codomain _ graph codomain! !

!FilteredGraph methodsFor: 'initialize' stamp: 'btr 9/24/2001 01:20'!
initializeDomain
	domain _ graph domain! !

!FilteredGraph methodsFor: 'operations' stamp: 'btr 9/24/2001 01:33'!
applyTo: anArrow
	^ ((self raisedApplicationTo: anArrow) >> testBlock) codomain! !

!FilteredGraph methodsFor: 'operations' stamp: 'btr 9/24/2001 01:34'!
applyToGraph: anArrowGraph 
	^ ((self raisedApplicationTo: anArrowGraph)
		>> testBlock) codomain! !

!FilteredGraph methodsFor: 'operations' stamp: 'btr 9/24/2001 01:37'!
invertedApplyTo: anArrow 
	^ ((self raisedInverseApplicationTo: anArrow)
		>> testBlock) domain! !

!FilteredGraph methodsFor: 'operations' stamp: 'btr 9/24/2001 01:37'!
invertedApplyToGraph: anArrowGraph 
	^ ((self raisedInverseApplicationTo: anArrowGraph)
		>> testBlock) domain! !

!FilteredGraph methodsFor: 'set operations' stamp: 'btr 9/28/2001 01:36'!
/\ anArrowGraph 
	"My intersection with my graph returns myself."
	anArrowGraph = graph
		ifTrue: [^ self].
	((anArrowGraph isKindOf: FilteredGraph)
			and: [anArrowGraph graph == graph])
		ifTrue: [| temp | 
			"Taking the intersection of two filteredGraphs which are over  
			the same graph is handled to take the intersection of the  
			testBlock and caches."
			temp _ anArrowGraph block.
			^ FilteredGraph
				newFrom: (cache intersect: anArrowGraph cache)
				of: graph
				for: [:value | (testBlock value: value)
						and: [temp value: value]]].
	^ super /\ anArrowGraph! !

!FilteredGraph methodsFor: 'set operations' stamp: 'btr 9/28/2001 01:36'!
\/ anArrowGraph 
	"My union with my graph returns the graph."
	anArrowGraph = graph
		ifTrue: [^ anArrowGraph].
	((anArrowGraph isKindOf: FilteredGraph)
			and: [anArrowGraph graph == graph])
		ifTrue: [| temp | 
			"Taking the union of two filteredGraphs which are over  
			the same graph is handled to take the union of the  
			testBlock and caches."
			temp _ anArrowGraph block.
			^ FilteredGraph
				newFrom: (cache union: anArrowGraph cache)
				of: graph
				for: [:value | (testBlock value: value)
						or: [temp value: value]]].
	^ super \/ anArrowGraph! !

!FilteredGraph methodsFor: 'testing' stamp: 'btr 6/13/2001 18:56'!
intensionallyIncludes: anArrow 
	"testBlock determines membership. Of course I check that the arrow belongs to the same graph as mine."
	^ (graph includes: anArrow)
		and: [testBlock value: anArrow]! !


!GraphInclusionGraph methodsFor: 'accessing' stamp: 'btr 9/19/2001 20:23'!
apexNode
	"The apex of this graph is the base graph's apex arrow initialized when 
	setting up the graph-to-inclusion-graph relationship."
	^ apex! !

!GraphInclusionGraph methodsFor: 'accessing' stamp: 'btr 9/19/2001 20:31'!
baseArrowFor: anArrow 
	"Return the arrow in the base graph which the argument represents the 
	set-inclusion of for me. The abstract case is not handled."
	(self includes: anArrow)
		ifFalse: [^ ArrowGraph bottom].
	^ self graph add: anArrow codomainElement! !

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

!GraphInclusionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:49'!
initializeCodomain
	codomain _ self graph! !

!GraphInclusionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:55'!
initializeDomain
	domain _ SingletonGraph of: apex! !

!GraphInclusionGraph methodsFor: 'operations' stamp: 'btr 9/19/2001 20:26'!
applyTo: anArrow 
	"The only arrow that an inclusion graph can be applied to effectively is the apex node of the graph. Any other case is not handled yet."
	self apexNode == anArrow
		ifTrue: [^ graph].
	^ ArrowGraph bottom! !

!GraphInclusionGraph methodsFor: 'operations' stamp: 'btr 9/20/2001 16:14'!
applyToGraph: anArrowGraph 
	"As in #invertedApplyToGraph:, error-handling is not proper here."
	((anArrowGraph isKindOf: SingletonGraph)
			and: [anArrowGraph singleton == self apexNode])
		ifTrue: [^ self graph].
	^ ArrowGraph bottom! !

!GraphInclusionGraph methodsFor: 'operations' stamp: 'btr 9/19/2001 20:26'!
invertedApplyTo: anArrow 
	"The only arrow that an inclusion graph can answer is the apex node of the graph. Any arrow in the base graph results in it."
	^ (graph includes: anArrow)
		ifTrue: [SingletonGraph of: self apexNode]
		ifFalse: [ArrowGraph bottom]! !

!GraphInclusionGraph methodsFor: 'operations' stamp: 'btr 9/20/2001 16:15'!
invertedApplyToGraph: anArrowGraph 
	"Perform a simple check before answering a graph of the apex node,  
	but the error case does not consider possible lazy concurrency of  
	evaluation. The more lenient case would produce an error in parallel  
	and allow any proper element of the argument to lead to the right  
	answer."
	(anArrowGraph isSubGraphOf: graph)
		ifFalse: [^ ArrowGraph bottom].
	^ SingletonGraph of: self apexNode! !


!IdentityGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 00:49'!
anyOne
	"Answer an identity over one of my graph's arrows."
	^ graph anyOne raiseIdentity! !

!IdentityGraph methodsFor: 'comparing' stamp: 'btr 6/18/2001 00:57'!
isSubGraphOf: anArrowGraph
	"Dispatch on my own type with a shortcut to check our arguments' inclusion."
	anArrowGraph class == self class
		ifTrue: [^ self graph isSubGraphOf: anArrowGraph graph].
	^ super isSubGraphOf: anArrowGraph! !

!IdentityGraph methodsFor: 'comparing' stamp: 'btr 6/18/2001 00:57'!
isSuperGraphOf: anArrowGraph 
	"Dispatch on my own type with a shortcut to check our arguments' 
	inclusion. "
	anArrowGraph class == self class
		ifTrue: [^ self graph isSuperGraphOf: anArrowGraph graph].
	^ super isSuperGraphOf: anArrowGraph! !

!IdentityGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:49'!
initializeCodomain
	codomain _ graph! !

!IdentityGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:55'!
initializeDomain
	domain _ graph! !

!IdentityGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:54'!
applyTo: anArrow 
	"Answer the argument. I am the identity operation. Also ensure the  
	arrow is in my graph."
	^ (graph includes: anArrow)
		ifTrue: [SingletonGraph of: anArrow]
		ifFalse: [ArrowGraph bottom]! !

!IdentityGraph methodsFor: 'operations' stamp: 'btr 6/18/2001 00:42'!
invert
	"Answer myself, since #applyTo: and #invertedApplyTo: are equivalent for identities."
	^ self! !

!IdentityGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:02'!
invertedApplyTo: anArrow 
	"Answer the argument. I am the identity operation. Also ensure the  
	arrow is in my graph."
	^ (graph includes: anArrow)
		ifTrue: [SingletonGraph of: anArrow]
		ifFalse: [ArrowGraph bottom]! !

!IdentityGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 23:21'!
reflexiveClosure
	"I am a (my) reflexive closure."
	^ self! !

!IdentityGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 23:12'!
selfComposeTimes: aNatural
	"All compositions with myself return myself."
	^ self! !

!IdentityGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 23:12'!
transitiveClosure
	"All compositions with myself return myself."
	^ self! !

!IdentityGraph methodsFor: 'testing' stamp: 'btr 9/19/2001 20:36'!
intensionallyIncludes: anArrow 
	"The arrow must be an identity over one of my graph's elements."
	^ anArrow isIdentity
		and: [self domain includes: anArrow domainElement]! !


!InvertedGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 15:01'!
anyOne
	"Delegate to my kernel and invert the result."
	^ graph anyOne inv! !

!InvertedGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 15:03'!
cache
	"This should not be called since it answers a copy of the set that 
	actually gets manipulated. However, no cache should be manipulated directly, anyway."
	^ graph cache
		collect: [:eachArrow | eachArrow inv]! !

!InvertedGraph methodsFor: 'adding' stamp: 'btr 6/10/2001 15:01'!
add: anArrow 
	"Add the inverse to my kernel."
	^ graph add: anArrow inv! !

!InvertedGraph methodsFor: 'adding' stamp: 'btr 6/13/2001 14:16'!
addImmutable: anArrow 
	"For value semantics, return a new inverted graph of the immutable  
	addition of the (UN-)inverse of the arrow to my kernel graph."
	^ InvertedGraph
		of: (UnionGraph of: (SingletonGraph of: anArrow inv)
					with: graph)! !

!InvertedGraph methodsFor: 'initialize' stamp: 'btr 6/10/2001 15:00'!
cache: aSet 
	"My cache should never be used."
	^ self shouldNotImplement! !

!InvertedGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:50'!
initializeCodomain
	codomain _ graph domain! !

!InvertedGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:57'!
initializeDomain
	domain _ graph codomain! !

!InvertedGraph methodsFor: 'operations' stamp: 'btr 6/10/2001 14:59'!
applyTo: anArrow 
	"Swap my graph's operations."
	^ graph invertedApplyTo: anArrow! !

!InvertedGraph methodsFor: 'operations' stamp: 'btr 6/10/2001 14:59'!
applyToGraph: anArrow 
	"Swap my graph's operations."
	^ graph invertedApplyToGraph: anArrow! !

!InvertedGraph methodsFor: 'operations' stamp: 'btr 3/18/2001 01:00'!
invert
	"This is the ultimate in lazy methods."
	^ graph! !

!InvertedGraph methodsFor: 'operations' stamp: 'btr 6/10/2001 15:03'!
invertedApplyTo: anArrow 
	"Swap my graph's operations."
	^ graph applyTo: anArrow! !

!InvertedGraph methodsFor: 'operations' stamp: 'btr 6/10/2001 15:03'!
invertedApplyToGraph: anArrow 
	"Swap my graph's operations."
	^ graph applyToGraph: anArrow! !

!InvertedGraph methodsFor: 'testing' stamp: 'btr 6/10/2001 15:01'!
includes: anArrow 
	"Test against the arrow's inverse."
	^ graph includes: anArrow inv! !

!InvertedGraph methodsFor: 'testing' stamp: 'btr 6/10/2001 15:00'!
intensionallyIncludes: anArrow 
	"Test against the arrow's inverse."
	^ graph intensionallyIncludes: anArrow inv! !


!MetaGraphGraph class methodsFor: 'instance creation' stamp: 'btr 9/23/2001 23:17'!
of: anArrowGraph 
	"Sugaring."
	^ self new initialize graph: anArrowGraph! !


!FilteredGraph class methodsFor: 'instance creation' stamp: 'btr 6/13/2001 16:24'!
newFrom: aCollection of: anArrowGraph for: aBlockContext
	^ self new graph: anArrowGraph;
		block: aBlockContext;
		cache: aCollection! !

!FilteredGraph class methodsFor: 'instance creation' stamp: 'btr 6/13/2001 16:21'!
of: anArrowGraph
	self shouldNotImplement! !

!FilteredGraph class methodsFor: 'instance creation' stamp: 'btr 6/21/2001 00:48'!
of: anArrowGraph for: aBlockContext 
	"Implicitly use the argument graph's cache to initialize the new instance's cache."
	^ self new graph: anArrowGraph;
		 block: aBlockContext;
		 cache: anArrowGraph asSet! !


!IdentityGraph class methodsFor: 'instance creation' stamp: 'btr 10/11/2001 01:07'!
over: anArrowGraph
	"Sugaring. This is more natural grammatically to speak of identities over a graph's arrows."
	^ self of: anArrowGraph! !


!InvertedGraph class methodsFor: 'instance creation' stamp: 'btr 9/16/2001 16:45'!
of: anArrowGraph 
	"Ensures that only one meta-graph of this type exists per graph  
	instance."
	self
		allInstancesDo: [:eachMeta | anArrowGraph == eachMeta graph
				ifTrue: [^ eachMeta]].
	^ (self new initialize graph: anArrowGraph)
		name: 'A meta-graph of ' , anArrowGraph name! !


!MetaObjectGraph methodsFor: 'accessing' stamp: 'btr 4/24/2001 19:01'!
objectGraph
	^ objectGraph! !

!MetaObjectGraph methodsFor: 'accessing' stamp: 'btr 6/12/2001 10:14'!
world
	"MetaObjectGraphs lie in the same world as their subject matter."
	^ objectGraph world! !

!MetaObjectGraph methodsFor: 'initialize' stamp: 'btr 10/10/2001 23:20'!
initializeDomain
	domain _ objectGraph! !

!MetaObjectGraph methodsFor: 'initialize' stamp: 'btr 10/10/2001 23:20'!
initializecodomain
	codomain _ objectGraph! !

!MetaObjectGraph methodsFor: 'initialize' stamp: 'btr 6/10/2001 14:34'!
over: anObjectGraph 
	"First, ensure that the argument is the appropriate type. Then ensure  
	that the wrapperGraph is the only of its type managing the argument."
	(anObjectGraph isKindOf: ObjectGraph)
		ifFalse: [^ self error: 'MetaObjectGraphs must manage only graphs of type ObjectGraph.'].
	self class
		allInstancesDo: [:each | each objectGraph == anObjectGraph
				ifTrue: [^ each]].
	objectGraph _ anObjectGraph! !

!MetaObjectGraph methodsFor: 'testing' stamp: 'btr 6/12/2001 10:14'!
treatsAsCodomainElement: anArrow 
	"The default is to just add wrapper-checking to the generic case."
	^ (super treatsAsCodomainElement: anArrow)
		and: [anArrow isWrapper]! !

!MetaObjectGraph methodsFor: 'testing' stamp: 'btr 6/12/2001 10:14'!
treatsAsDomainElement: anArrow 
	"The default is to just add wrapper-checking to the generic case."
	^ (super treatsAsDomainElement: anArrow)
		and: [anArrow isWrapper]! !


!ClassInstanceGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:49'!
applyTo: anArrow 
	"Answers a graph of wrappers of all the instances of the argument's  
	wrapped behavior."
	anArrow object isBehavior
		ifFalse: [^ ArrowGraph bottom].
	^ objectGraph
		>> [:value | value class = anArrow object]
		addAll: (anArrow object allInstances
				collect: [:each | Arrow wrapping: each in: objectGraph])! !

!ClassInstanceGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:03'!
invertedApplyTo: anArrow 
	"Answer a wrapper for the class of the argument's wrapped object."
	^ objectGraph >> [:value | value == anArrow object class]
			add: (Arrow wrapping: anArrow object class in: objectGraph)! !

!ClassInstanceGraph methodsFor: 'initialize' stamp: 'btr 10/10/2001 23:11'!
initializeDomain
	"Any subclass of behavior can have instances, and non-wrapper arrows 
	return themselves which are never behaviors themselves."
	domain _ objectGraph >> [:each | each isKindOf: Behavior]! !

!ClassInstanceGraph methodsFor: 'testing' stamp: 'btr 5/29/2001 17:32'!
intensionallyIncludes: anArrow 
	"This includes all arrows leading between wrappers of objects and 
	wrappers of their classes."
	^ anArrow codomainElement object class == anArrow domainElement object! !


!ClassSubclassGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:49'!
applyTo: anArrow 
	"Answers all wrapped subclasses of the argument's wrapped behavior."
	anArrow object isBehavior
		ifFalse: [^ ArrowGraph bottom].
	^ objectGraph
		>> [:value | value superclass == anArrow object]
		addAll: (anArrow object subclasses
				collect: [:each | Arrow wrapping: each in: objectGraph])! !

!ClassSubclassGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:00'!
invertedApplyTo: anArrow 
	"Answer the superclass of the argument's wrapped behavior."
	(anArrow object isKindOf: Behavior)
		ifFalse: [^ ArrowGraph bottom].
	^ objectGraph
		>> [:value | anArrow object superclass == value]
		add: (Arrow wrapping: anArrow object superclass in: objectGraph)! !

!ClassSubclassGraph methodsFor: 'initialize' stamp: 'btr 10/10/2001 23:15'!
initializeCodomain
	"All Behaviors can be superclassed except for nil (the superclass of  
	ProtoObject)."
	codomain _ self domain
		>> [:each | each isNil not]! !

!ClassSubclassGraph methodsFor: 'initialize' stamp: 'btr 10/10/2001 23:13'!
initializeDomain
	"Any subclass of behavior can have subclasses, and non-wrapper arrows return themselves which are never behaviors themselves."
	domain _ objectGraph
				>> [:each | each isKindOf: Behavior]! !

!ClassSubclassGraph methodsFor: 'testing' stamp: 'btr 5/29/2001 17:21'!
intensionallyIncludes: anArrow 
	"This graph type includes all arrows between wrappers that inherit  
	directly from each other (in one specific direction)."
	^ (self treatsAsDomainElement: anArrow domainElement)
		and:
	[anArrow codomainElement object superclass == anArrow domainElement object]! !


!ClosureResultGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:04'!
applyTo: anArrow 
	"Since this graph only represents completed closures, getting the result is 
	straightforward."
	^ objectGraph
		>> [:value | value = anArrow object value] add: (Arrow wrapping: anArrow object value in: objectGraph)! !

!ClosureResultGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 00:54'!
invertedApplyTo: anArrow 
	"This is undecidable, since the class of closures returning a given value  
	is not well-defined. Instead I answer something that lazily filters for  
	such."
	^ objectGraph >> [:value | value value = anArrow]! !

!ClosureResultGraph methodsFor: 'initialize' stamp: 'btr 10/10/2001 23:16'!
initializeDomain
	"BlockContexts are closures (this should change in future Squeaks and is  
	certainly not right in other Smalltalks, since they have proper  
	BlockClosures)."
	domain _ objectGraph >> [:each | each isKindOf: BlockContext]! !

!ClosureResultGraph methodsFor: 'testing' stamp: 'btr 5/29/2001 17:14'!
intensionallyIncludes: anArrow 
	"Sending #value to BlockContexts forces evaluation."
	^ (self treatsAsDomainElement: anArrow domainElement)
		and: [anArrow domainElement object value == anArrow codomainElement object]! !


!MetaObjectGraph class methodsFor: 'instance creation' stamp: 'btr 6/12/2001 05:22'!
newFrom: aCollection 
	"My objectGraph must be initialized before any objects can be added."
	self shouldNotImplement! !

!MetaObjectGraph class methodsFor: 'instance creation' stamp: 'btr 6/13/2001 05:34'!
newFrom: aCollection over: anObjectGraph 
	"Create a new extensional graph with the contents of aCollection."
	^ self new holdStrongly cache: aCollection;
		 over: anObjectGraph! !

!MetaObjectGraph class methodsFor: 'instance creation' stamp: 'btr 6/15/2001 07:03'!
newFromArrow: anArrow over: anObjectGraph 
	"Annotate the argument with an initialized empty graph."
	^ self new initialize apex: anArrow;
		 over: anObjectGraph! !

!MetaObjectGraph class methodsFor: 'instance creation' stamp: 'btr 6/15/2001 07:03'!
newFromArrow: anArrow wrapping: aCollection over: anObjectGraph 
	"Annotate the argument with a graph over a known collection of  
	objects."
	^ self new initialize apex: anArrow;
		 over: anObjectGraph;
		
		cache: (aCollection asSet
				collect: [:each | Arrow wrapping: each in: anObjectGraph])! !

!MetaObjectGraph class methodsFor: 'instance creation' stamp: 'btr 6/10/2001 14:33'!
newFromWrapping: aCollection over: anObjectGraph 
	"Wrap the elements of aCollection, using anObjectGraph as a base."
	^ self class new
		cache: (aCollection asSet
				collect: [:each | Arrow wrapping: each in: anObjectGraph]);
		 over: anObjectGraph! !

!MetaObjectGraph class methodsFor: 'instance creation' stamp: 'btr 6/10/2001 14:34'!
over: anObjectGraph 
	"Create a new instance for the given objectGraph. No duplication checks 
	are performed here, since it would require some odd parametrization on 
	my subclasses."
	^ self new initialize over: anObjectGraph! !


!MonoidArrow methodsFor: 'accessing' stamp: 'btr 4/26/2001 07:12'!
graph
	^ graph! !

!MonoidArrow methodsFor: 'accessing' stamp: 'btr 5/3/2001 15:25'!
head
	"I am an element of my MonoidGraph, so I am an identity over its 
	kernel."
	^ graph kernel! !

!MonoidArrow methodsFor: 'accessing' stamp: 'btr 5/3/2001 15:25'!
tail
	"I am an element of my MonoidGraph, so I am an identity over its 
	kernel. "
	^ graph kernel! !

!MonoidArrow methodsFor: 'accessing' stamp: 'btr 5/3/2001 15:38'!
world
	"This is a recursive method to determine what world an arrow is in 
	without having to perform searches throuh all frames and graphs."
	"For now, assume that the monoid knows its world."
	^ self graph world! !

!MonoidArrow methodsFor: 'converting' stamp: 'btr 4/27/2001 13:11'!
asConcrete
	"I already know my head and tail, and would lose information (namely my responsibility to my graph) by becoming a strictly concrete arrow."
	^ self! !

!MonoidArrow methodsFor: 'initialize' stamp: 'btr 5/30/2001 12:18'!
graph: aMonoidGraph 
	"This is a protected mutator. It initializes, but also can return a new  
	instance of the appropriate MonoidGraph membership if possible."
	graph
		ifNil: [graph _ aMonoidGraph. ^ self].
	(aMonoidGraph isKindOf: MonoidGraph)
		ifFalse: [^ self error: 'Monoid arrows can only meaningfully be owned by MonoidGraphs.'].
	^ self class new graph: aMonoidGraph!
]style[(7 12 3 139 2 5 11 5 3 20 5 12 11 11 16 4 8 63 6 4 18 12)f1b,f1cblack;b,f1,f1c146044000,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;! !

!MonoidArrow methodsFor: 'testing' stamp: 'btr 5/29/2001 08:53'!
isIdentity
	"Shortcut the usual test since all arrows of this class are identities."
	^ true! !


!MonoidArrow class methodsFor: 'instance creation' stamp: 'btr 6/12/2001 10:14'!
newFor: aMonoidGraph 
	"Sugaring."
	^ self new initialize graph: aMonoidGraph! !


!MonoidGraph methodsFor: 'accessing' stamp: 'btr 5/30/2001 12:08'!
anyOne
	"Answer kernel -> kernel"
	^ self add: (MonoidArrow newFor: self)! !

!MonoidGraph methodsFor: 'accessing' stamp: 'btr 5/15/2001 18:13'!
kernel
	"Answers my kernel arrow."
	^ kernel! !

!MonoidGraph methodsFor: 'adding' stamp: 'btr 6/13/2001 14:09'!
add: anArrow 
	"I can be initialized by adding an identity arrow. Otherwise, perform the 
	intension check and add a MonoidArrow. All identity ConcreteArrows  
	are then converted to proper MonoidArrows."
	anArrow isIdentity
		ifFalse: [^ self error: 'The added arrow cannot be added to any monoid.'].
	kernel
		ifNil: [kernel _ anArrow head].
	(self intensionallyIncludes: anArrow)
		ifFalse: [^ self error: 'The added arrow does not belong in this monoid.'].
	anArrow
		becomeForward: (MonoidArrow newFor: self).
	cache isEmpty
		ifTrue: [cache _ cache class with: anArrow]
		ifFalse: [cache add: anArrow].
	^ anArrow! !

!MonoidGraph methodsFor: 'comparing' stamp: 'btr 5/15/2001 18:12'!
= anArrowGraph 
	^ kernel == anArrowGraph kernel! !

!MonoidGraph methodsFor: 'comparing' stamp: 'btr 10/13/2001 13:02'!
hash
	"Supposedly this should allow for accurate comparing via =."
	^ kernel hash! !

!MonoidGraph methodsFor: 'comparing' stamp: 'btr 3/19/2001 00:16'!
isSubGraphOf: anArrowGraph 
	"Monoids with the same kernel are all equal. Other graphs are not  
	comparable without a transformation on them."
	anArrowGraph class == MonoidGraph
		ifFalse: [^ self error: 'That graph cannot be compared with a monoid.'].
	^ self = anArrowGraph! !

!MonoidGraph methodsFor: 'initialize' stamp: 'btr 6/13/2001 14:00'!
initialize
	self holdWeakly! !

!MonoidGraph methodsFor: 'initialize' stamp: 'btr 10/10/2001 23:05'!
initializeCodomain
	domain _ codomain _ SingletonGraph of: kernel! !

!MonoidGraph methodsFor: 'initialize' stamp: 'btr 10/10/2001 23:05'!
initializeDomain
	domain _ codomain _ SingletonGraph of: kernel! !

!MonoidGraph methodsFor: 'initialize' stamp: 'btr 10/21/2001 12:47'!
kernel: anArrow 
	"Protects the kernel arrow from modification after it is initialized."
	kernel
		ifNotNil: [^ self class of: anArrow].
	kernel _ anArrow! !

!MonoidGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:55'!
applyTo: anArrow 
	"All arrows in a monoid are identities over the kernel arrow."
	^ anArrow == kernel
		ifTrue: [SingletonGraph of: kernel]
		ifFalse: [ArrowGraph bottom]! !

!MonoidGraph methodsFor: 'operations' stamp: 'btr 4/24/2001 18:27'!
invert
	"Don't even bother with an InvertedGraph layer.
	TODO: possibly remove this overriding method if the significance of keeping the syntactic structure intact is important. modify InversionGraph accordingly to keep the optimization."
	^ self! !

!MonoidGraph methodsFor: 'operations' stamp: 'btr 6/11/2001 10:06'!
invertedApplyTo: anArrow 
	"A monoid only consists of identity arrows, so its inversion applies identically as itself."
	^ self applyTo: anArrow! !

!MonoidGraph methodsFor: 'testing' stamp: 'btr 6/11/2001 10:09'!
includesReferencesTo: anArrow 
	"All my arrows are identities over my kernel."
	^ anArrow == kernel! !

!MonoidGraph methodsFor: 'testing' stamp: 'btr 5/30/2001 12:30'!
intensionallyIncludes: anArrow 
	^ (anArrow isKindOf: MonoidArrow)
		and: [anArrow graph = self]! !

!MonoidGraph methodsFor: 'testing' stamp: 'btr 6/11/2001 10:08'!
treatsAsCodomainElement: anArrow 
	"All my arrows are identities over my kernel."
	^ anArrow == kernel! !

!MonoidGraph methodsFor: 'testing' stamp: 'btr 6/11/2001 10:08'!
treatsAsDomainElement: anArrow 
	"All my arrows are identities over my kernel."
	^ anArrow == kernel! !

!MonoidGraph methodsFor: 'testing' stamp: 'btr 6/11/2001 10:08'!
treatsAsNode: anArrow 
	"All my arrows are identities over my kernel."
	^ anArrow == kernel! !


!MonoidGraph class methodsFor: 'instance creation' stamp: 'btr 6/13/2001 05:36'!
of: anArrow 
	^ self new holdWeakly kernel: anArrow! !


!ObjectGraph methodsFor: 'accessing' stamp: 'btr 4/25/2001 12:04'!
anyOne
	"Answer a random object wrapper and cache it. TODO: find a better 
	randomizer over objects, since this seems to depend on properties of the garbage collection migration mechanism."
	^ self
		add: (Arrow wrapping: self someObject in: self)! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 11:42'!
arrowFor: anObject 
	"Answer an arrow wrapping the given object for me; avoids duplicates. 
	Note that wrapper arrows add: themselves to their objectGraphs 
	implicitly."
	^ WrapperArrow for: anObject in: self! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 12:44'!
arrowsFor: aCollection 
	"Add all arrows for each element of the collection, failing silently."
	aCollection
		do: [:each | self
				add: (self arrowFor: each)
				ifFail: []]! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 08:21'!
booleans
	"Return a graph of the Smalltalk boolean objects."
	^ self
		>> [:each | each isKindOf: Boolean]
		+ (self arrowFor: true)
		+ (self arrowFor: false)! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 12:38'!
characterMappings
	"Answers the map of integers to Smalltalk characters."
	^ (PluggableObjectGraph
		over: self
		for: [:each | (Character value: each domainElement object)
				== each codomainElement object])
		domain: self
				>> [:each | [each isKindOf: Integer]
						and: [each > 0]]
		codomain: self characters! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 12:33'!
characters
	"Answers the set of Smalltalk characters."
	^ self
		>> [:each | Character characterTable includes: each]
		addForAll: (Character allCharacters); yourself! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 6/12/2001 06:39'!
closureTo
	^ closureTo! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 6/12/2001 06:40'!
instanceOf
	^ instanceOf! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 10:47'!
instancesOf: aBehavior
	^ self >> [:each | self instanceOf * (self arrowFor: each) == aBehavior]! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 00:02'!
literals
	"Answer all the literals. However, PointerFinders also count due to a poor hack of #isLiteral to fix a bug in that class."
	^ self >> [:value | value isLiteral and: [(value class == PointerFinder) not]]! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 00:00'!
numbers
	"Return everything that is a number. This does not provide for distinguishment of types."
	^ self >> [:value | value isKindOf: Number]! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 6/13/2001 13:56'!
slotOf
	^ slotOf! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 6/12/2001 06:40'!
subclassOf
	^ subclassOf! !

!ObjectGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:27'!
add: aWrapperArrow 
	"This graph should only contain WrapperArrows. This may be confusing 
	if one wants to add a wrapper for a wrapper and so forth (which is  
	valid, but produces a confusing chain)."
	"TODO: This also could speculatively create arrows for the various  
	meaningful relations for the object wrapped."
	^ self add: aWrapperArrow
		ifFail: [self error: 'ObjectGraphs can only meaningfully contain wrapper arrows.'. aWrapperArrow]! !

!ObjectGraph methodsFor: 'adding' stamp: 'btr 10/21/2001 11:48'!
addFor: anObject 
	"Convenient method. Transparently returns a wrapping and adds it to 
	the cache."
	^ WrapperArrow for: anObject in: self! !

!ObjectGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:37'!
addForAll: aCollection 
	"Convenient method."
	^ aCollection do: [:each | cache add: (Arrow wrapping: each in: self)]! !

!ObjectGraph methodsFor: 'comparing' stamp: 'btr 10/13/2001 12:55'!
= anArrowGraph 
	"ObjectGraphs are only equivalent to other objectGraphs with the same 
	kernel."
	^ anArrowGraph class == self class
		and: [kernel == anArrowGraph kernel]! !

!ObjectGraph methodsFor: 'filtering' stamp: 'btr 6/13/2001 15:24'!
filteredBy: aBlockContext
	"Provide for filtering of my object wrappers by a test block taking the object as argument. In this generic case, no cache or set of example values is given."
	^ FilteredObjectGraph
		newFrom: {}
		over: self
		for: aBlockContext! !

!ObjectGraph methodsFor: 'filtering' stamp: 'btr 6/13/2001 15:35'!
filteredBy: aBlockContext caching: anArrowGraph
	"Provide for filtering of my object wrappers by a test block taking the  
	object as argument and an example graph of wrappers satisfying the  
	test."
	^ FilteredObjectGraph
		newFrom: anArrowGraph asSet
		over: self
		for: aBlockContext! !

!ObjectGraph methodsFor: 'initialize' stamp: 'btr 10/21/2001 12:49'!
initialize
	super initialize.
	self beIntensional.
	"Don't enumerate through objectGraphs."
	infinitary _ true.
	subclassOf _ ClassSubclassGraph over: self.
	slotOf _ ObjectInstVarGraph over: self.
	instanceOf _ ClassInstanceGraph over: self.
	closureTo _ ClosureResultGraph over: self! !

!ObjectGraph methodsFor: 'testing' stamp: 'btr 6/13/2001 13:52'!
intensionallyIncludes: aWrapperArrow 
	"Every wrapped Smalltalk object counts as an element, unless it is 
	initialized for another objectGraph already."
	^ (aWrapperArrow isWrapper)
		and: [aWrapperArrow objectGraph = self
				or: [aWrapperArrow objectGraph isNil]]! !


!ObjectInstVarGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:05'!
applyTo: anArrow 
	"This answers a graph of wrapper arrows for all the objects used in the  
	argument's slots."
	^ objectGraph >> [:value | anArrow object pointsTo: value]
			addAll: (anArrow object class allInstVarNames
				collect: [:each | Arrow
						wrapping: (self instVarNamed: each)
						in: objectGraph])! !

!ObjectInstVarGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:06'!
invertedApplyTo: anArrow 
	"This collects all objects having slots pointing to the argument slot,  
	answering a graph of arrow-wrappers for them."
	^ objectGraph >> [:value | value pointsTo: anArrow object]
			addAll: ((Smalltalk pointersTo: anArrow except: #()) asSet
				collect: [:each | Arrow wrapping: each in: objectGraph])! !

!ObjectInstVarGraph methodsFor: 'initialize' stamp: 'btr 10/10/2001 23:19'!
initializeDomain
	"Any object with size > 0 has slots."
	domain _ objectGraph >> [:each | each size > 0]! !

!ObjectInstVarGraph methodsFor: 'testing' stamp: 'btr 5/29/2001 17:16'!
intensionallyIncludes: anArrow 
	"This graph includes arrows from objects to all the objects in their slots."
	^ anArrow domainElement object pointsTo: anArrow codomainElement object! !


!PluggableGraph methodsFor: 'initialize' stamp: 'btr 5/30/2001 09:45'!
block: aBlockContext 
	"Protected mutator. Graphs' intensions must be immutable."
	block
		ifNotNil: [^ self class for: aBlockContext].
	block _ aBlockContext! !

!PluggableGraph methodsFor: 'initialize' stamp: 'btr 5/30/2001 09:42'!
selector: aSelector args: anArray 
	"Protected mutator. Graphs' intensions must be immutable."
	selector
		ifNotNil: [^ self class
				using: aSelector
				args: anArray].
	"Test to ensure that the symbol aSelector is a valid selector."
	aSelector numArgs < 0
		ifTrue: [^ self error: aSelector asString , ' is not a valid selector.'].
	selector _ aSelector.
	"Coerce anArray to an array."
	args _ anArray asArray.
	block
		ifNotNil: [self
				block: [:anArrow | (anArrow domainElement perform: selector withArguments: args)
						= anArrow codomainElement]]! !

!PluggableGraph methodsFor: 'operations' stamp: 'btr 6/13/2001 22:56'!
applyTo: anArrow 
	"If selector is defined, make a MessageSend that will apply to the  
	given arrow and answer its result."
	selector
		ifNil: [^ self notYetImplemented].
	^ FilteredGraph
		newFrom: {(MessageSend
			receiver: anArrow
			selector: selector
			arguments: args) value}
		of: ArrowGraph top
		for: [:value | (anArrow perform: selector withArguments: args) = value]! !

!PluggableGraph methodsFor: 'testing' stamp: 'btr 10/11/2001 02:17'!
intensionallyIncludes: anArrow 
	"The block verifies the input-output pair."
	^ (super intensionallyIncludes: anArrow) and: [block value: anArrow]! !

!PluggableGraph methodsFor: 'testing' stamp: 'btr 6/6/2001 11:23'!
treatsAsDomainElement: anArrow 
	"Checks that the arrow actually understands the selector. Since #ifNotNil: returns the receiver if it's nil, the logic contains a short-circuit."
	^ (super treatsAsDomainElement: anArrow)
		and: [selector
				ifNotNil: [^ anArrow respondsTo: selector].
			true]! !


!PluggableGraph class methodsFor: 'instance creation' stamp: 'btr 5/30/2001 09:45'!
for: aBlockContext 
	"Return a new instance of me for the given block closure. Notice that 
	this is never guaranteed to be unique by the class."
	^ self new block: aBlockContext! !

!PluggableGraph class methodsFor: 'instance creation' stamp: 'btr 5/30/2001 09:44'!
using: aSelector args: anArray 
	^ self new initialize
		selector: aSelector
		args: anArray! !


!PluggableObjectGraph methodsFor: 'initialize' stamp: 'btr 6/1/2001 14:02'!
block: aBlockContext 
	"Protected mutator. Graphs' intensions must be immutable."
	objectGraph
		ifNil: [^ self error: 'The objectGraph must be known.'].
	block
		ifNotNil: [
			^ self class over: objectGraph for: aBlockContext].
	block _ aBlockContext! !

!PluggableObjectGraph methodsFor: 'initialize' stamp: 'btr 10/10/2001 23:26'!
initializeDomain
	"Checks that the arrow's wrapped object actually understands the  
	selector if one is assigned."
	domain _ objectGraph
				>> [:each | (selector isNil not
						and: [each respondsTo: selector])
					or: [block isNil not]]! !

!PluggableObjectGraph methodsFor: 'initialize' stamp: 'btr 6/10/2001 14:34'!
over: anObjectGraph block: aBlockContext 
	"Coupled initializers for sugaring."
	^ (self over: anObjectGraph)
		block: aBlockContext! !

!PluggableObjectGraph methodsFor: 'initialize' stamp: 'btr 6/10/2001 14:35'!
over: anObjectGraph selector: aSelector args: anArray 
	"Coupled initializers for sugaring."
	^ (self over: anObjectGraph)
		selector: aSelector
		args: anArray! !

!PluggableObjectGraph methodsFor: 'initialize' stamp: 'btr 5/30/2001 09:17'!
selector: aSelector 
	"Protected mutator. Graphs' intensions must be immutable."
	^ self selector: aSelector args: #()! !

!PluggableObjectGraph methodsFor: 'initialize' stamp: 'btr 6/1/2001 14:02'!
selector: aSelector args: anArray 
	"Protected mutator. Graphs' intensions must be immutable."
	objectGraph
		ifNil: [^ self error: 'The objectGraph must be known.'].
	selector
		ifNotNil: [^ self class
				over: objectGraph
				using: aSelector
				args: anArray].
	"Test to ensure that the symbol aSelector is a valid selector."
	aSelector numArgs < 0
		ifTrue: [^ self error: aSelector asString , ' is not a valid selector.'].
	selector _ aSelector.
	"Coerce anArray to an array."
	args _ anArray asArray.
	block
		ifNotNil: [self
				block: [:anArrow | (anArrow domainElement object perform: selector withArguments: args)
						= anArrow codomainElement object]]! !

!PluggableObjectGraph methodsFor: 'operations' stamp: 'btr 10/12/2001 04:54'!
applyTo: anArrow 
	"If selector or block is defined, apply it to the given arrow's object and  
	answer its wrapped result."
	| a |
	selector
		ifNil: [block
				ifNil: [^ nil]
				ifNotNil: [a _ block value: anArrow object.
					^ objectGraph
						>> [:value | a = value]
						add: (Arrow wrapping: a in: objectGraph)]].
	(self treatsAsDomainElement: anArrow)
		ifFalse: [^ ArrowGraph bottom].
	a _ anArrow object perform: selector withArguments: args.
	^ objectGraph
		>> [:value | a = value]
		add: (Arrow wrapping: a in: objectGraph)! !

!PluggableObjectGraph methodsFor: 'operations' stamp: 'btr 10/12/2001 04:55'!
invertedApplyTo: anArrow 
	"If selector or block is defined, answer a graph which lazily filters  
	results."
	selector
		ifNil: [block
				ifNil: [^ nil]
				ifNotNil: [^ objectGraph
						>> [:value | (block value: value)
								= anArrow object]]].
	(self treatsAsDomainElement: anArrow)
		ifFalse: [^ ArrowGraph bottom].
	^ objectGraph
		>> [:value | (value perform: selector withArguments: args)
				= anArrow object]! !

!PluggableObjectGraph methodsFor: 'testing' stamp: 'btr 5/30/2001 05:58'!
intensionallyIncludes: anArrow 
	"The block verifies the input-output pair."
	^ block value: anArrow! !


!PluggableObjectGraph class methodsFor: 'instance creation' stamp: 'btr 5/30/2001 06:07'!
over: anObjectGraph
	"This class needs a parameter block closure to complete its semantics. See #over:for:."
	^ self over: anObjectGraph for: []! !

!PluggableObjectGraph class methodsFor: 'instance creation' stamp: 'btr 6/10/2001 14:35'!
over: anObjectGraph for: aBlockContext 
	"Answer a new instance with the given arguments completing the  
	semantics."
	^ self new initialize over: anObjectGraph block: aBlockContext! !

!PluggableObjectGraph class methodsFor: 'instance creation' stamp: 'btr 5/30/2001 09:20'!
over: anObjectGraph using: aSelector 
	^ self
		over: anObjectGraph
		using: aSelector
		args: #()! !

!PluggableObjectGraph class methodsFor: 'instance creation' stamp: 'btr 6/10/2001 14:35'!
over: anObjectGraph using: aSelector args: anArray 
	^ self new initialize
		over: anObjectGraph
		selector: aSelector
		args: anArray! !


!ProjectionGraph methodsFor: 'adding' stamp: 'btr 9/19/2001 19:41'!
add: anArrow
	"Delegate to the projection handling code, while remaining polymorphic with other add: methods."
	self project: anArrow domainElement onto: anArrow codomainElement.
	^ anArrow! !

!ProjectionGraph methodsFor: 'adding' stamp: 'btr 9/17/2001 19:26'!
project: anArrow onto: targetArrow
	"This method allows for the specification of existing arrows as the target of the projection, one at a time. It must preserve the graph's intension, of course."
	| projector |
	(self treatsAsDomainElement: anArrow)
		ifFalse: [self error: 'The arrow does not belong to this graph''s domain.'].
	projector _ cache detect: [:each | each domainElement == anArrow]
		ifNone: [^ cache add: anArrow -> targetArrow].
	"Both the case where the projector was already initialized to a new arrow and where it has already been project:onto:'d another meaningful arrow must be handled."
	self notYetImplemented! !

!ProjectionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:58'!
initializeDomain
	domain _ graph! !

!ProjectionGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:53'!
applyTo: anArrow 
	"Ensure first that the projection applies to the arrow. Then return the 
	first possible result, or a new arrow if an applicable projection arrow 
	is not found."
	(self treatsAsDomainElement: anArrow)
		ifFalse: [^ ArrowGraph bottom].
	cache
		do: [:each | each domainElement == anArrow
				ifTrue: [^ SingletonGraph of: each codomainElement]].
	^ SingletonGraph
		of: (cache add: anArrow -> Arrow new) codomainElement! !

!ProjectionGraph methodsFor: 'testing' stamp: 'btr 9/16/2001 16:58'!
intensionallyIncludes: anArrow 
	"All my arrows must lead from my graph to an arrow unique to the  
	original."
	^ (graph includes: anArrow domainElement)
		and:
	[(cache
		anySatisfy: [:each | (each domainElement == anArrow domainElement
					and: [each codomainElement ~~ anArrow codomainElement])]) not]! !

!ProjectionGraph methodsFor: 'testing' stamp: 'btr 9/16/2001 14:54'!
treatsAsDomainElement: anArrow
	"Shortcut case for one-for-one projections."
	^ graph includes: anArrow! !


!ProjectionGraph class methodsFor: 'instance creation' stamp: 'btr 9/23/2001 23:16'!
of: anArrowGraph 
	"Sugaring."
	^ (self new initialize graph: anArrowGraph)
		name: 'A one-to-one projection of ' , anArrowGraph name! !


!Reference methodsFor: 'accessing' stamp: 'btr 3/4/2001 21:20'!
value
	^ value! !

!Reference methodsFor: 'accessing' stamp: 'btr 3/28/2001 10:14'!
value: anObject 
	"Proper use of the Reference class will ensure that this abstract method never gets called."
	self subclassResponsibility! !

!Reference methodsFor: 'copying' stamp: 'btr 3/2/2001 20:15'!
clone
	"Answer with the receiver, because References are unique per object."
	^ self! !

!Reference methodsFor: 'copying' stamp: 'btr 3/2/2001 20:15'!
copy
	"Answer with the receiver, because References are unique per object."
	^ self! !

!Reference methodsFor: 'copying' stamp: 'btr 3/2/2001 20:30'!
deepCopy
	"Objects using the Reference class are obviously concerned with circularity or meta-circularity of reference, and so should not be trying to do this."
	^ self shouldNotImplement! !

!Reference methodsFor: 'copying' stamp: 'btr 3/2/2001 20:15'!
shallowCopy
	"Answer with the receiver, because References are unique per object."
	^ self! !

!Reference methodsFor: 'copying' stamp: 'btr 2/27/2001 22:14'!
veryDeepCopyWith: deepCopier 
	"Return self. I am immutable in the Morphic world. Do not record me."
	^ self! !

!Reference methodsFor: 'testing' stamp: 'btr 3/21/2001 15:20'!
isTo: anObject 
	"I could also answer by an #= comparison vice #==."
	^ value == anObject! !


!Reference class methodsFor: 'instance creation' stamp: 'btr 3/6/2001 17:52'!
for: aBlock 
	^ SuspendedReference for: aBlock! !

!Reference class methodsFor: 'instance creation' stamp: 'btr 3/6/2001 17:52'!
futureOf: aBlock 
	^ SuspendedReference for: aBlock! !

!Reference class methodsFor: 'instance creation' stamp: 'btr 3/6/2001 17:52'!
to: anObject 
	^ ValueReference to: anObject! !

!Reference class methodsFor: 'instance variables' stamp: 'btr 5/15/2001 10:18'!
removeDuplicates
	"There MUST be a better way to do this. This is O(n squared) at the very  
	least."
	self
		allSubInstancesDo: [:each | self class
				allSubInstancesDo: [:other | other = each
						ifTrue: [other becomeForward: each]]]! !


!RelDisjointSumGraph methodsFor: 'initialize' stamp: 'btr 11/20/2001 22:40'!
initializeCodomain
	codomain _ self firstArgArrow asGraph <*> self firstArg codomain \/ (self secondArgArrow asGraph <*> self secondArg codomain)! !

!RelDisjointSumGraph methodsFor: 'initialize' stamp: 'btr 11/20/2001 22:40'!
initializeDomain
	domain _ (self firstArgArrow asGraph <*> self firstArg domain)
		\/ (self secondArgArrow asGraph <*> self secondArg domain)! !

!RelDisjointSumGraph methodsFor: 'operations' stamp: 'btr 11/20/2001 19:54'!
applyTo: anArrow
	"Disjoint sums apply to tagged arrows and return tagged results."
	| tag |
	tag _ anArrow tail object.
	(tag == self firstArg
			or: [tag == self secondArg])
		ifFalse: [^ ArrowGraph bottom].
	^ anArrow tail -> (tag applyTo: anArrow head)! !

!RelDisjointSumGraph methodsFor: 'operations' stamp: 'btr 11/20/2001 19:55'!
invertedApplyTo: anArrow 
	"Disjoint sums apply to tagged arrows and return tagged results."
	| tag |
	tag _ anArrow tail object.
	(tag == self firstArg
			or: [tag == self secondArg])
		ifFalse: [^ ArrowGraph bottom].
	^ anArrow tail
		-> (tag invertedApplyTo: anArrow head)! !

!RelDisjointSumGraph methodsFor: 'testing' stamp: 'btr 11/20/2001 20:02'!
intensionallyIncludes: anArrow
	"My arrows point to taggings of input/output pairs."
	| tag |
	tag _ anArrow tail tail object.
	^ (tag == anArrow head tail object)
		and: [tag includes: anArrow tail head -> anArrow head head]! !


!RelJoinGraph methodsFor: 'initialize' stamp: 'btr 11/20/2001 22:48'!
initializeCodomain
	codomain _ self firstArg codomain \/ self secondArg codomain! !

!RelJoinGraph methodsFor: 'initialize' stamp: 'btr 11/20/2001 22:47'!
initializeDomain
	domain _ self firstArgArrow asGraph <*> self firstArg domain \/ (self secondArgArrow asGraph <*> self secondArg domain)! !

!RelJoinGraph methodsFor: 'operations' stamp: 'btr 11/20/2001 06:51'!
applyTo: anArrow
	"Joins apply to arrows leading from one of the join-arguments to one of their elements. Application delegates to the correct argument."
	| tag|
	tag _ anArrow domainElement object.
	(tag == self firstArg or: [tag == self secondArg])
		ifFalse: [^ ArrowGraph bottom].
	^ tag applyTo: anArrow codomainElement! !

!RelJoinGraph methodsFor: 'operations' stamp: 'btr 11/20/2001 06:55'!
invertedApplyTo: anArrow
	"Answers the tagged results of inverse application by each argument of the join."
	^ ((SingletonGraph of: self firstArgArrow) <*> self firstArg invertedApplyTo: anArrow)
		\/ ((SingletonGraph of: self secondArgArrow) <*> self secondArg invertedApplyTo: anArrow)! !

!RelJoinGraph methodsFor: 'testing' stamp: 'btr 11/20/2001 07:07'!
intensionallyIncludes: anArrow
	"All join arrows must lead from a tagged arrow to its result by applying the graph that tags it; also the tag must be a join argument."
	^ (anArrow domainElement tail == self firstArg
			or: [anArrow domainElement tail == self secondArg])
		and: [anArrow domainElement treatsAsDomainElement: anArrow codomainElement]! !


!RelProductGraph methodsFor: 'operations' stamp: 'btr 11/20/2001 20:08'!
applyTo: anArrow 
	"Relational products take pairs of inputs and answer pairs of results, 
	applying the inputs' elements to the product arguments."
	^ (self firstArg applyTo: anArrow tail)
		<*> (self secondArg applyTo: anArrow head)! !

!RelProductGraph methodsFor: 'operations' stamp: 'btr 11/20/2001 20:08'!
invertedApplyTo: anArrow 
	"Relational products take pairs of inputs and answer pairs of results,  
	applying the inputs' elements to the product arguments."
	^ (self firstArg invertedApplyTo: anArrow tail)
		<*> (self secondArg invertedApplyTo: anArrow head)! !

!RelProductGraph methodsFor: 'testing' stamp: 'btr 11/20/2001 19:45'!
intensionallyIncludes: anArrow 
	"My arrows map input-output pairs of both of my arguments."
	^ (self firstArg includes: anArrow head tail -> anArrow tail tail)
		or: [self secondArg includes: anArrow head head -> anArrow tail head]! !

!RelProductGraph methodsFor: 'initialize' stamp: 'btr 11/20/2001 22:31'!
initializeCodomain
	codomain _ self firstArg codomain <*> self secondArg codomain! !

!RelProductGraph methodsFor: 'initialize' stamp: 'btr 11/20/2001 22:31'!
initializeDomain
	domain _ self firstArg domain <*> self secondArg domain! !


!RelSplitGraph methodsFor: 'initialize' stamp: 'btr 11/20/2001 22:33'!
initializeCodomain
	codomain _ self firstArg codomain <*> self secondArg codomain! !

!RelSplitGraph methodsFor: 'initialize' stamp: 'btr 11/20/2001 22:33'!
initializeDomain
	domain _ self firstArg domain \/ self secondArg domain! !

!RelSplitGraph methodsFor: 'operations' stamp: 'btr 11/20/2001 21:21'!
applyTo: anArrow 
	^ (self firstArg applyTo: anArrow)
		<*> (self secondArg applyTo: anArrow)! !

!RelSplitGraph methodsFor: 'operations' stamp: 'btr 11/20/2001 21:20'!
invertedApplyTo: anArrow 
	^ (self firstArg invertedApplyTo: anArrow head)
		/\ (self secondArg invertedApplyTo: anArrow tail)! !

!RelSplitGraph methodsFor: 'testing' stamp: 'btr 11/20/2001 21:22'!
intensionallyIncludes: anArrow 
	^ (self firstArg includes: anArrow domainElement -> anArrow codomainElement tail)
		and: [self secondArg includes: anArrow domainElement -> anArrow codomainElement head]! !


!SafeMessageSend methodsFor: 'accessing' stamp: 'btr 6/9/2001 12:59'!
arguments: anArray
	^ self shouldNotImplement! !

!SafeMessageSend methodsFor: 'accessing' stamp: 'btr 6/9/2001 12:55'!
receiver
	"Attempts to access my receiver could mutate it, resulting in unsafe semantics. Keep in mind that #receiver: always makes a local clone to prevent other outside manipulations."
	^ self shouldNotImplement! !

!SafeMessageSend methodsFor: 'accessing' stamp: 'btr 6/9/2001 12:58'!
receiver: anObject
	^ self shouldNotImplement! !

!SafeMessageSend methodsFor: 'accessing' stamp: 'btr 6/9/2001 12:59'!
selector: aSymbol 
	^ self shouldNotImplement! !

!SafeMessageSend methodsFor: 'initialize' stamp: 'btr 6/9/2001 13:06'!
receiver: anObject selector: aSymbol arguments: anArray
	((aSymbol isKindOf: Symbol)
		and: [aSymbol numArgs >= 0])
		ifFalse: [^ aSymbol asString , ' is not a proper selector.'].
	(selector isNil & arguments isNil)
		ifFalse: [^ self error: 'I am already initialized.'].
	receiver _ anObject clone.
	selector _ aSymbol.
	arguments _ anArray asArray! !


!SetDisjointSumGraph methodsFor: 'initialize' stamp: 'btr 11/20/2001 21:36'!
initializeCodomain
	codomain _ self firstArg \/ self secondArg! !

!SetDisjointSumGraph methodsFor: 'initialize' stamp: 'btr 11/20/2001 21:36'!
initializeDomain
	domain _ self firstArgArrow asGraph \/ self secondArgArrow asGraph! !

!SetDisjointSumGraph methodsFor: 'operations' stamp: 'btr 11/20/2001 21:38'!
applyTo: anArrow 
	"Return the elements of the graph that the argument represents."
	| tag |
	tag _ anArrow domainElement.
	(self domain includes: tag)
		ifFalse: [^ ArrowGraph bottom].
	^ tag object! !

!SetDisjointSumGraph methodsFor: 'operations' stamp: 'btr 11/20/2001 21:41'!
invertedApplyTo: anArrow 
	"Return the tagged inverse application of the graph corresponding to the 
	'tag' in the arrow."
	(self firstArg includes: anArrow)
		ifTrue: [^ self firstArg apex asGraph].
	(self secondArg includes: anArrow)
		ifTrue: [^ self secondArg apex asGraph]
		ifFalse: [^ ArrowGraph bottom]! !


!SetOpsGraph methodsFor: 'accessing' stamp: 'btr 10/17/2001 05:00'!
anyOne
	"This simple randomizer will add the result to either or both arguments as possible. Note however that this randomizer only is evenly balanced against the two arguments and does not account for sub-arguments of any kind. So a union whose second argument is a union will treat the first argument 50% and the arguments of the latter at 25% each."
	^ self secondArg
		add: (self firstArg add: {self firstArg anyOne. self secondArg anyOne} atRandom)! !

!SetOpsGraph methodsFor: 'accessing' stamp: 'btr 6/13/2001 13:20'!
firstArg
	^ apex tail! !

!SetOpsGraph methodsFor: 'accessing' stamp: 'btr 6/13/2001 13:20'!
secondArg
	^ apex head! !

!SetOpsGraph methodsFor: 'adding' stamp: 'btr 10/17/2001 07:04'!
add: anArrow 
	"If someone wants to add an arrow, it could belong in both arguments of the set operation, but fails silently."
	^ self firstArg
		add: (self secondArg add: anArrow ifFail: [])
		ifFail: []! !


!IntersectionGraph methodsFor: 'comparing' stamp: 'btr 6/10/2001 14:11'!
isSubGraphOf: anArrowGraph 
	"The smaller argument restricted to the larger is equivalent and an  
	optimal handling for this relatively ambiguous case. However the case where the intersection is a subgraph, but neither of the intersection operands are subgraphs, is not handled in the generic intensional case."
	self orientTailSmaller.
	^ ((self firstArg isSubGraphOf: anArrowGraph)
		and: [self secondArg isSubGraphOf: anArrowGraph])
		or: [((self firstArg cache intersection: self secondArg cache)
			anySatisfy: [:each | (anArrowGraph includes: each) not]) not]! !

!IntersectionGraph methodsFor: 'comparing' stamp: 'btr 6/10/2001 14:14'!
isSuperGraphOf: anArrowGraph 
	"The generic intensional case is not handled."
	^ ((self firstArg isSuperGraphOf: anArrowGraph)
			and: [self secondArg isSuperGraphOf: anArrowGraph])
		and: [((self firstArg cache intersect: self secondArg cache)
				anySatisfy: [:each | (anArrowGraph includes: each) not]) not]! !

!IntersectionGraph methodsFor: 'initialize' stamp: 'btr 6/13/2001 13:31'!
initialize
	"My operands must be known before initializing. Use SetOpsGraph 
	class>>#of:with:."
	infinitary _ self firstArg isFinite
				or: [self secondArg isFinite].
	self beIntensional! !

!IntersectionGraph methodsFor: 'operations' stamp: 'btr 9/28/2001 01:40'!
doesNotUnderstand: aMessage 
	"Many of the messages of this class just delegate to its arguments with  
	the very same combination operator. e.g. applyTo: invertedApplyTo:  
	includes: intensionallyIncludes: and all the protocol that relies upon  
	them."
	"Also it performs lazy predicate evaluation in a definite, which is an  
	implementation hack since set operations should be commutative."
	| temp |
	temp _ aMessage sentTo: self firstArg.
	^ (temp isKindOf: Boolean)
		ifTrue: [temp
				and: [aMessage sentTo: self secondArg]]
		ifFalse: [temp
				/\ (aMessage sentTo: self secondArg)]! !


!SetOpsGraph class methodsFor: 'instance creation' stamp: 'btr 6/15/2001 19:42'!
new
	"Override the odd hack in ArrowGraph class>>#new."
	^ self basicNew initialize! !

!SetOpsGraph class methodsFor: 'instance creation' stamp: 'btr 6/13/2001 13:19'!
of: firstGraph and: secondGraph 
	"Sugaring."
	^ self of: firstGraph with: secondGraph ! !

!SetOpsGraph class methodsFor: 'instance creation' stamp: 'btr 6/15/2001 07:03'!
of: firstGraph with: secondGraph 
	^ self new apex: firstGraph -> secondGraph;
		 initialize! !


!SingletonGraph methodsFor: 'accessing' stamp: 'btr 5/22/2001 06:03'!
anyOne
	"An accessor that implicitly initializes the singleton arrow if necessary."
	self isInitialized
		ifFalse: [^ (self singleton: Arrow new) anyOne].
	^ singleton! !

!SingletonGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 12:27'!
cache
	^ singleton
		ifNil: [{}]
		ifNotNil: [WeakSet with: singleton]! !

!SingletonGraph methodsFor: 'accessing' stamp: 'btr 3/19/2001 18:36'!
singleton
	^ singleton! !

!SingletonGraph methodsFor: 'adding' stamp: 'btr 6/15/2001 06:46'!
add: anArrow 
	(self isInitialized
			and: [(anArrow == singleton) not])
		ifTrue: [^ self error: 'Singletons can only contain one element.'].
	self singleton: anArrow.
	^ anArrow! !

!SingletonGraph methodsFor: 'comparing' stamp: 'btr 3/19/2001 18:29'!
isSubGraphOf: anArrowGraph
	^ anArrowGraph includes: singleton! !

!SingletonGraph methodsFor: 'comparing' stamp: 'btr 6/11/2001 16:21'!
isSuperGraphOf: anArrowGraph
	"Only an empty graph could have fewer elements."
	^ anArrowGraph class = EmptyGraph
		or: [anArrowGraph isExtensional
			and: [anArrowGraph cache size = 0]]! !

!SingletonGraph methodsFor: 'initialize' stamp: 'btr 5/8/2001 10:03'!
initialize
	"I am intensional but my cache is not used. Don't re-send this message."
	infinitary _ true.
	cache _ nil! !

!SingletonGraph methodsFor: 'initialize' stamp: 'btr 6/10/2001 12:28'!
isInitialized
	^ singleton isNil not! !

!SingletonGraph methodsFor: 'initialize' stamp: 'btr 6/10/2001 12:24'!
singleton: anArrow 
	"Protected mutator. The cache isn't used by this graph type."
	self isInitialized
		ifFalse: [singleton _ anArrow.
			^ self].
	singleton = anArrow
		ifTrue: [^ self].
	^ SingletonGraph of: anArrow! !

!SingletonGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:52'!
applyTo: anArrow 
	"My singleton is the only arrow to check against."
	anArrow == singleton domainElement
		ifTrue: [^ SingletonGraph of: singleton codomainElement].
	^ ArrowGraph bottom! !

!SingletonGraph methodsFor: 'operations' stamp: 'btr 6/11/2001 16:15'!
applyToGraph: anArrowGraph 
	"My singleton is the only arrow to check against."
	(anArrowGraph includes: singleton domainElement)
		ifTrue: [^ singleton codomainElement].
	^ nil! !

!SingletonGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:03'!
invertedApplyTo: anArrow 
	"My singleton is the only arrow to check against."
	^ anArrow == singleton codomainElement
		ifTrue: [SingletonGraph of: singleton domainElement]
		ifFalse: [ArrowGraph bottom]! !

!SingletonGraph methodsFor: 'operations' stamp: 'btr 6/11/2001 16:16'!
invertedApplyToGraph: anArrowGraph 
	"My singleton is the only arrow to check against."
	(anArrowGraph includes: singleton codomainElement)
		ifTrue: [^ singleton domainElement].
	^ nil! !

!SingletonGraph methodsFor: 'set operations' stamp: 'btr 9/28/2001 01:47'!
/\ anArrowGraph 
	"An intersection of a singleton and another graph returns either the 
	singleton or an empty graph."
	^ (anArrowGraph includes: singleton)
		ifTrue: [self]
		ifFalse: [ArrowGraph bottom]! !

!SingletonGraph methodsFor: 'testing' stamp: 'btr 3/19/2001 18:27'!
includes: anArrow
	^ anArrow == singleton! !

!SingletonGraph methodsFor: 'testing' stamp: 'btr 3/28/2001 10:13'!
intensionallyIncludes: anArrow 
	"This graph type has no logical condition to satisfy."
	^ false! !


!SingletonGraph class methodsFor: 'instance creation' stamp: 'btr 9/20/2001 16:09'!
from: anApex to: anArrow
	"SingletonGraphs can be used as views of individual arrows."
	^ self new initialize apex: anApex; singleton: anArrow! !

!SingletonGraph class methodsFor: 'instance creation' stamp: 'btr 9/20/2001 16:19'!
fromArrow: anArrow
	^ self from: anArrow domainElement to: anArrow codomainElement! !

!SingletonGraph class methodsFor: 'instance creation' stamp: 'btr 3/19/2001 18:38'!
of: anArrow
	^ self new initialize singleton: anArrow! !


!SquareGraph methodsFor: 'accessing' stamp: 'btr 9/30/2001 14:08'!
anyOne
	"Try not to create new arrows if possible. Just return something that  
	works."
	cache isEmpty
		ifTrue: [^ (self domain anyOne -> self codomain anyOne) addToGraph: self].
	^ cache anyOne! !

!SquareGraph methodsFor: 'accessing' stamp: 'btr 9/30/2001 14:08'!
valueOf: anArrow 
	"Ensure that we get a unique instance with the same value."
	(self treatsAsNode: anArrow)
		ifFalse: [^ anArrow world valueOf: anArrow].
	anArrow class == Arrow
		ifTrue: [self error: 'Value-representation of abstract arrows is not yet implemented.'].
	^ self
		add: (cache
				detect: [:each | each equals: anArrow]
				ifNone: [^ self add: anArrow clone])! !

!SquareGraph methodsFor: 'adding' stamp: 'btr 5/27/2001 20:45'!
add: anArrow 
	"Don't allow duplicate values, and don't check for intensionallyIncludes:, 
	because any arrow is a legal value as long as the SquareGraph has no 
	duplicate instances for the same value."
	^ cache
		detect: [:each | each equals: anArrow]
		ifNone: [(cache isNil
			or: [cache size = 0])
		ifTrue: [^ cache _ WeakSet with: anArrow].
	^ cache add: anArrow]! !

!SquareGraph methodsFor: 'initialize' stamp: 'btr 9/30/2001 14:04'!
initializeCodomain
	domain _ codomain _ self frame! !

!SquareGraph methodsFor: 'initialize' stamp: 'btr 9/30/2001 14:04'!
initializeDomain
	domain _ codomain _ self frame! !

!SquareGraph methodsFor: 'operations' stamp: 'btr 6/11/2001 10:04'!
applyTo: anArrow 
	"Return all possible arrows. Arrow allInstances will not do in this case."
	^ ArrowGraph top! !

!SquareGraph methodsFor: 'operations' stamp: 'btr 5/8/2001 00:59'!
invertedApplyTo: anArrow 
	"All values belong, and inverses of values have values, so the inversion is identical to the original."
	^ self applyTo: anArrow! !

!SquareGraph methodsFor: 'testing' stamp: 'btr 3/23/2001 14:41'!
intensionallyIncludes: anArrow 
	"Answer false because only arrows that are in my cache already can be 
	guaranteed unique from all other arrow instances with the same value. This relies on SquareGraph overriding add: to not check for intensional inclusion."
	^ false! !


!SuspendedReference methodsFor: 'accessing' stamp: 'btr 3/6/2001 18:37'!
suspension
	^ suspension! !

!SuspendedReference methodsFor: 'accessing' stamp: 'btr 3/6/2001 18:40'!
suspension: aBlockContext 
	suspension
		ifNil: [(aBlockContext isKindOf: BlockContext)
				ifTrue: 
					[suspension _ aBlockContext.
					^ self]
				ifFalse: [^ self error: 'You must pass a block for a lazy reference to use.']].
	suspension = aBlockContext
		ifTrue: [^ self]
		ifFalse: [^ self class new suspension: aBlockContext]! !

!SuspendedReference methodsFor: 'accessing' stamp: 'btr 3/6/2001 18:42'!
value
	"When the value of the reference is not already calculated and cached,  
	the suspension should be executed to render and cache the value. This  
	is what makes a SuspendedReference a Lazy object wrapping. Notice 
	that this returns a ValueReference."
	value
		ifNil: 
			[suspension ifNil: [^ self error: 'No evaluation possible on un-initialized lazy references.'].
			value _ suspension value].
	^ Reference to: value! !

!SuspendedReference methodsFor: 'accessing' stamp: 'btr 3/6/2001 17:53'!
value: anObject 
	"Break the lazy evaluation rule to make sure we aren't duplicating  
	references. This doesn't seem a good way to handle it. Maybe I should 
	answer with a promise instead of a direct answer."
	anObject == self value ifTrue: [^ self].
	^ ValueReference to: anObject! !

!SuspendedReference methodsFor: 'comparing' stamp: 'btr 3/21/2001 15:22'!
= aReference 
	"This forces evaluation."
	^ self value == aReference value! !


!SuspendedReference class methodsFor: 'instance creation' stamp: 'btr 3/6/2001 18:39'!
for: aBlock 
	"This checks to return any existing Reference instance to the argument  
	before allocating a new one."
	"TODO: Re-implement using a weak collection as a class variable to track 
	all reference instances."
	self
		allInstancesDo: [:eachReference | eachReference suspension = aBlock ifTrue: [^ eachReference]].
	^ Reference new suspension: aBlock! !

!SuspendedReference class methodsFor: 'instance creation' stamp: 'btr 3/4/2001 22:23'!
futureOf: aBlock
	^ self for: aBlock! !

!SuspendedReference class methodsFor: 'instance creation' stamp: 'btr 3/4/2001 22:22'!
to: aBlock 
	"Lazy references should only be instantiated for things with future values, not to the values themselves as such (as values)."
	^ self shouldNotImplement! !


!TailGraph methodsFor: 'operations' stamp: 'btr 5/8/2001 10:28'!
applyTo: anArrow 
	"Reifies the tail of the arrow as another arrow. Notice that  
	the answer provided is not unique by necessity."
	cache
		detect: [:each | each domainElement == anArrow]
		ifNone: [| newArrow | 
			"Only works on ConcreteArrows."
	newArrow _ anArrow raiseTail.
	^ cache
		detect: [:eachArrow | eachArrow = newArrow]
		ifNone: [self add: newArrow]]! !

!TailGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:04'!
invertedApplyTo: anArrow 
	"First check to make sure the argument is valid as a reified head  
	reference. If so, return the arrow it would have been the result of."
	^ (self intensionallyIncludes: anArrow)
		ifTrue: [anArrow domainElement]
		ifFalse: [ArrowGraph bottom]! !

!TailGraph methodsFor: 'testing' stamp: 'btr 5/8/2001 14:22'!
intensionallyIncludes: anArrow 
	"My arrows simply have to lead from one arrow in the frame to the  
	arrow that its head references (if there is one). Also note that since all  
	arrows in the graph are in the frame automatically, it must also be  
	searched. TODO: implement this."
	(frame includes: anArrow tail)
		ifFalse: [^ false].
	anArrow class == ConcreteArrow
		ifTrue: [^ self intensionallyIncludes: anArrow using: #tail].
	^ false! !

!TailGraph methodsFor: 'testing' stamp: 'btr 5/15/2001 18:36'!
treatsAsNode: anArrow 
	"All arrows in the frame can have their tail references reified as other  
	arrows."
	^ frame includes: anArrow! !


!TopOrBottomGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 16:45'!
cache
	"Caches are useless for graphs of this kind. Don't implement unless it will prevent an error."
	self shouldNotImplement! !

!TopOrBottomGraph methodsFor: 'comparing' stamp: 'btr 6/10/2001 16:05'!
= anArrowGraph 
	"All graphs of this type are equal."
	^ anArrowGraph class == self class! !

!TopOrBottomGraph methodsFor: 'comparing' stamp: 'btr 6/10/2001 16:05'!
isSubGraphOf: anArrowGraph
	^ self = anArrowGraph! !

!TopOrBottomGraph methodsFor: 'initialize' stamp: 'btr 6/10/2001 16:10'!
cache: aSet 
	"Don't allow this to happen for now. Ideally this would be equivalent to muting the world or worlds, which would be ridiculously complex."
	^ self shouldNotImplement! !


!EmptyGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 16:44'!
anyOne
	"I am empty."
	^ nil! !

!EmptyGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 16:08'!
cache
	"I am empty."
	^ {}! !

!EmptyGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 05:32'!
add: anArrow 
	"My intension is to be empty."
	^ self
		add: anArrow
		ifFail: [self error: 'This graph is intended to be empty.']! !

!EmptyGraph methodsFor: 'adding' stamp: 'btr 6/10/2001 16:47'!
addImmutable: anArrow
	"Answer a graph containing just that arrow, since I have none."
	^ SingletonGraph of: anArrow! !

!EmptyGraph methodsFor: 'comparing' stamp: 'btr 6/13/2001 12:11'!
isSubGraphOf: anArrowGraph 
	"I am a subgraph of any non-empty graph or intensional graph."
	^ (anArrowGraph class = self class) not
		and: [anArrowGraph cache size > 0
				or: [anArrowGraph isIntensional]]! !

!EmptyGraph methodsFor: 'comparing' stamp: 'btr 6/10/2001 16:17'!
isSuperGraphOf: anArrowGraph 
	"I am only a superclass of other empty graphs, and even then only  
	improperly."
	^ anArrowGraph class == self class! !

!EmptyGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:47'!
initializeCodomain
	codomain _ ArrowGraph bottom! !

!EmptyGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:54'!
initializeDomain
	domain _ ArrowGraph bottom! !

!EmptyGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:50'!
applyTo: anArrow 
	"I am empty."
	^ ArrowGraph bottom! !

!EmptyGraph methodsFor: 'operations' stamp: 'btr 6/10/2001 16:07'!
applyToGraph: anArrowGraph 
	"I am empty."
	^ nil! !

!EmptyGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:00'!
invertedApplyTo: anArrow 
	"I am empty."
	^ ArrowGraph bottom! !

!EmptyGraph methodsFor: 'operations' stamp: 'btr 6/10/2001 16:06'!
invertedApplyToGraph: anArrowGraph 
	"I am empty."
	^ nil! !

!EmptyGraph methodsFor: 'set operations' stamp: 'btr 9/28/2001 01:47'!
/\ anArrowGraph 
	"Intersections with empty sets produce empty sets."
	(anArrowGraph isKindOf: ArrowGraph)
		ifFalse: [^ nil]! !

!EmptyGraph methodsFor: 'set operations' stamp: 'btr 9/28/2001 01:46'!
\/ anArrowGraph 
	"Unions with empty sets produce the identical input."
	(anArrowGraph isKindOf: ArrowGraph)
		ifFalse: [^ nil].
	^ anArrowGraph! !

!EmptyGraph methodsFor: 'testing' stamp: 'btr 6/10/2001 16:08'!
includes: anArrow 
	"I am empty."
	^ false! !

!EmptyGraph methodsFor: 'testing' stamp: 'btr 6/10/2001 16:07'!
intensionallyIncludes: anArrow
	"I am empty."
	^ false! !

!EmptyGraph methodsFor: 'testing' stamp: 'btr 6/10/2001 16:08'!
treatsAsCodomainElement: anArrow 
	"I am empty."
	^ false! !

!EmptyGraph methodsFor: 'testing' stamp: 'btr 6/10/2001 16:08'!
treatsAsDomainElement: anArrow 
	"I am empty."
	^ false! !


!TopGraph methodsFor: 'accessing' stamp: 'btr 6/10/2001 16:45'!
anyOne
	"I contain everything. So I just answer with something."
	^ Arrow new! !

!TopGraph methodsFor: 'adding' stamp: 'btr 5/8/2001 09:58'!
add: anArrow 
	"Do nothing. A cache is useless in this case."
	^ anArrow! !

!TopGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 05:35'!
add: anArrow ifFail: errorBlock
	"Do nothing. A cache is useless in this case."
	^ anArrow! !

!TopGraph methodsFor: 'adding' stamp: 'btr 6/10/2001 16:10'!
addImmutable: anArrow 
	"Do nothing, since this graph already includes every possible arrow."
	^ self! !

!TopGraph methodsFor: 'comparing' stamp: 'btr 6/10/2001 16:18'!
isSubGraphOf: anArrowGraph
	"I am only a subgraph of other all-inclusive graphs, and even then only improperly."
	^ anArrowGraph class == self class! !

!TopGraph methodsFor: 'comparing' stamp: 'btr 6/13/2001 14:56'!
isSuperGraphOf: anArrowGraph 
	"Any graph not of my type is a proper subclass."
	^ self class ~~ anArrowGraph class! !

!TopGraph methodsFor: 'initialize' stamp: 'btr 9/25/2001 12:23'!
initializeCodomain
	codomain _ domain _ ArrowGraph top! !

!TopGraph methodsFor: 'initialize' stamp: 'btr 9/25/2001 12:23'!
initializeDomain
	domain _ codomain _ ArrowGraph top! !

!TopGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:52'!
applyTo: anArrow 
	"I answer everything."
	^ ArrowGraph top! !

!TopGraph methodsFor: 'operations' stamp: 'btr 6/10/2001 16:41'!
applyToGraph: anArrowGraph 
	"I answer everything."
	^ self! !

!TopGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:04'!
invertedApplyTo: anArrow 
	"I answer everything."
	^ ArrowGraph top! !

!TopGraph methodsFor: 'operations' stamp: 'btr 6/10/2001 16:41'!
invertedApplyToGraph: anArrowGraph 
	"I answer everything."
	^ self! !

!TopGraph methodsFor: 'set operations' stamp: 'btr 9/28/2001 01:47'!
/\ anArrowGraph 
	"Intersections with top sets produce the identical input."
	(anArrowGraph isKindOf: ArrowGraph)
		ifFalse: [^ nil].
	^ anArrowGraph! !

!TopGraph methodsFor: 'set operations' stamp: 'btr 9/28/2001 01:46'!
\/ anArrowGraph 
	"Unions with top sets produce top sets."
	(anArrowGraph isKindOf: ArrowGraph)
		ifFalse: [^ nil]! !

!TopGraph methodsFor: 'testing' stamp: 'btr 6/10/2001 16:43'!
includes: anArrow 
	"I contain everything."
	^ anArrow isKindOf: Arrow! !

!TopGraph methodsFor: 'testing' stamp: 'btr 10/12/2001 05:34'!
intensionallyIncludes: anArrow 
	"I contain everything."
	^ true! !


!TransitiveClosureGraph methodsFor: 'comparing' stamp: 'btr 9/23/2001 21:40'!
isSuperGraphOf: anArrowGraph
	^ (anArrowGraph == graph
		or: [anArrowGraph class == self class
			and: [anArrowGraph graph == graph]])
		or: [super isSuperGraphOf: anArrowGraph]! !

!TransitiveClosureGraph methodsFor: 'initialize' stamp: 'btr 9/28/2001 01:44'!
initializeCodomain
	codomain _ graph codomain \/ (self * graph codomain)! !

!TransitiveClosureGraph methodsFor: 'initialize' stamp: 'btr 9/23/2001 19:48'!
initializeDomain
	domain _ graph domain! !

!TransitiveClosureGraph methodsFor: 'operations' stamp: 'btr 10/10/2001 04:23'!
applyTo: anArrow 
	"Recurses on the application of the original arrow and all following  
	arrows lazily."
	| temp |
	temp _ graph applyTo: anArrow.
	^ temp \/ (self applyToGraph: temp)! !

!TransitiveClosureGraph methodsFor: 'operations' stamp: 'btr 9/28/2001 01:44'!
applyToGraph: anArrowGraph 
	"Recurses on the application of the original graph and all following  
	arrows lazily."
	| temp |
	temp _ graph applyToGraph: anArrowGraph.
	^ temp
		\/ (self applyToGraph: temp)! !

!TransitiveClosureGraph methodsFor: 'operations' stamp: 'btr 10/10/2001 04:26'!
invertedApplyTo: anArrow 
	"Recurses on the inverse application of the original arrow and all  
	preceding arrows lazily."
	| temp |
	temp _ graph invertedApplyTo: anArrow.
	^ temp
		\/ (self invertedApplyToGraph: temp)! !

!TransitiveClosureGraph methodsFor: 'operations' stamp: 'btr 10/10/2001 04:16'!
invertedApplyToGraph: anArrowGraph 
	"Recurses on the inverse application of the original graph and all preceding arrows lazily."
	| temp |
	temp _ graph invertedApplyToGraph: anArrowGraph.
	^ temp
		\/ (self invertedApplyToGraph: temp)! !

!TransitiveClosureGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 21:42'!
selfComposeTimes: aNatural
	"This graph type includes all of its graph's self-compositions."
	^ self! !

!TransitiveClosureGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 21:35'!
transitiveClosure
	^ self! !

!TransitiveClosureGraph methodsFor: 'testing' stamp: 'btr 9/23/2001 21:35'!
intensionallyIncludes: anArrow
	"WARNING: this method has exceptionally high computational complexity. If the kernel graph does not contain the arrow, it must search for a possible sequence of arrows in the graph that composes into the result. So some checks are performed beforehand to potentially avoid this."
	| start end |
	start _ anArrow domainElement.
	(self treatsAsDomainElement: start)
		ifFalse: [^ false].
	end _ anArrow codomainElement.
	^ (self * start) includes: end! !

!TransitiveClosureGraph methodsFor: 'testing' stamp: 'btr 11/20/2001 00:20'!
isPreOrderRelation
	"Reflexive transitive closures are pre-orders."
	^ self graph reflexiveClosure <= self graph! !


!UnionGraph methodsFor: 'comparing' stamp: 'btr 6/10/2001 10:21'!
isSubGraphOf: anArrowGraph 
	"Both of a union's arguments must satisfy the same <= relationship.  
	Check the smaller first; check the larger lazily on failure of the former 
	condition."
	self orientTailSmaller.
	^ (self firstArg isSubGraphOf: anArrowGraph)
		and: [self secondArg isSubGraphOf: anArrowGraph]! !

!UnionGraph methodsFor: 'comparing' stamp: 'btr 6/10/2001 14:04'!
isSuperGraphOf: anArrowGraph 
	"Perform quick checks on the argument against my firstArg and  
	secondArg since they are encoded as subgraphs of me."
	^ (self firstArg = anArrowGraph
			or: [self secondArg = anArrowGraph])
		or: [(anArrowGraph cache
				anySatisfy: [:eachArrow | (self firstArg includes: eachArrow) not
						and: [(self secondArg includes: eachArrow) not]]) not]! !

!UnionGraph methodsFor: 'initialize' stamp: 'btr 6/13/2001 13:31'!
initialize
	"My operands must be known before initializing. Use SetOpsGraph 
	class>>#of:with:. "
	infinitary _ self firstArg isFinite not
				or: [self secondArg isFinite not].
	self beIntensional! !

!UnionGraph methodsFor: 'operations' stamp: 'btr 9/28/2001 01:44'!
doesNotUnderstand: aMessage 
	"Many of the messages of this class just delegate to its arguments with  
	the very same combination operator. e.g. applyTo: invertedApplyTo:  
	includes: intensionallyIncludes: and all the protocol that relies upon  
	them."
	"Also it performs lazy predicate evaluation in a definite, which is an  
	implementation hack since set operations should be commutative."
	| temp |
	temp _ aMessage sentTo: self firstArg.
	^ (temp isKindOf: Boolean)
		ifTrue: [temp
				or: [aMessage sentTo: self secondArg]]
		ifFalse: [temp
				\/ (aMessage sentTo: self secondArg)]! !


!ValueReference methodsFor: 'accessing' stamp: 'btr 3/4/2001 21:21'!
value
	^ value! !

!ValueReference methodsFor: 'accessing' stamp: 'btr 5/15/2001 10:14'!
value: anObject 
	"Protected mutation; use for initialization or creating new instances. 
	NOTE: Reference>>to: calls me."
	value
		ifNil: [value _ anObject.
			^ self].
	value == anObject
		ifTrue: [^ self].
	^ Reference to: anObject! !

!ValueReference methodsFor: 'comparing' stamp: 'btr 3/5/2001 15:42'!
= aReference 
	"References should be unique per object, so #= should do the same as 
	#==, but this supports debugging."
	^ value hash = aReference value hash! !

!ValueReference methodsFor: 'comparing' stamp: 'btr 3/21/2001 11:50'!
deprecatedHash
	"Hash is re-implemented because #= is implemented. This is also to allow 
	for more efficient searching of References for duplicates."
	^ value hash! !


!ValueReference class methodsFor: 'instance creation' stamp: 'btr 3/21/2001 15:18'!
to: anObject 
	"This checks to return any existing Reference instance to the argument  
	before allocating a new one."
	"TODO: Re-implement using a weak collection (a heap or hashtable?) as a 
	class variable to track all reference instances."
	self
		allInstancesDo: [:each | (each value == anObject)
				ifTrue: [^ each]].
	^ self new value: anObject!
]style[(4 8 3 228 2 4 20 6 3 4 10 8 17 4 7 4 12 8)f1b,f1cblack;b,f1,f1c134032000,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;! !


!WrapperArrow methodsFor: 'accessing' stamp: 'btr 4/25/2001 11:52'!
object
	"Answer the object I wrap/reify/represent."
	^ object! !

!WrapperArrow methodsFor: 'accessing' stamp: 'btr 6/21/2001 00:24'!
object: anObject graph: anObjectGraph 
	"This is a protected mutator. It initializes, but also can return a new  
	instance of the appropriate ObjectGraph membership if possible. Also  
	ensures that only one wrapper exists for every (object, objectGraph) 
	pair. "
	(anObjectGraph isKindOf: ObjectGraph)
		ifFalse: [^ self error: 'Wrapper arrows can only meaningfully be owned by ObjectGraphs.'].
	graph
		ifNil: [graph _ anObjectGraph]
		ifNotNil: [graph = anObjectGraph
				ifFalse: [^ Arrow wrapping: anObject in: anObjectGraph]].
	"At this point, 'graph' must be initialized."
	object
		ifNotNil: [^ Arrow wrapping: anObject in: graph].
	object _ anObject.
	^ graph cache
		detect: [:each | each object = object]
		ifNone: [self addToGraph: graph]! !

!WrapperArrow methodsFor: 'accessing' stamp: 'btr 4/26/2001 06:59'!
objectGraph
	"Answer the graph responsible for me."
	^ graph! !

!WrapperArrow methodsFor: 'initialize' stamp: 'btr 6/12/2001 09:44'!
graph: anObjectGraph
	self shouldNotImplement! !

!WrapperArrow methodsFor: 'initialize' stamp: 'btr 6/12/2001 09:42'!
object: anObject objectGraph: anObjectGraph 
	^ self object: anObject graph: anObjectGraph! !


!WrapperArrow class methodsFor: 'instance creation' stamp: 'btr 6/12/2001 09:43'!
for: anObject in: anObjectGraph 
	"Keep only one wrapper arrow per object per objectGraph."
	^ super new object: anObject graph: anObjectGraph! !

!WrapperArrow class methodsFor: 'instance creation' stamp: 'btr 4/24/2001 18:40'!
new
	"Ensure I don't answer a duplicate wrapper for an object."
	"The following has the unfortunate side-effect of putting it in a random ObjectGraph.
	^ self for: self someObject in: ObjectGraph someInstance"
	^ self shouldNotImplement! !

!WrapperArrow class methodsFor: 'instance creation' stamp: 'btr 6/12/2001 09:45'!
newFor: anObjectGraph
	"Use #for:in: instead."
	self shouldNotImplement! !

UnionGraph removeSelector: #add:!
UnionGraph removeSelector: #anyOne!
TopGraph removeSelector: #&!
TopGraph removeSelector: #treatsAsCodomainElement:!
TopGraph removeSelector: #treatsAsDomainElement:!
TopGraph removeSelector: #|!

!TopGraph reorganize!
('accessing' anyOne)
('adding' add: add:ifFail: addImmutable:)
('comparing' isSubGraphOf: isSuperGraphOf:)
('initialize' initializeCodomain initializeDomain)
('operations' applyTo: applyToGraph: invertedApplyTo: invertedApplyToGraph:)
('set operations' /\ \/)
('testing' includes: intensionallyIncludes:)
!

EmptyGraph removeSelector: #&!
EmptyGraph removeSelector: #|!

!EmptyGraph reorganize!
('accessing' anyOne cache)
('adding' add: addImmutable:)
('comparing' isSubGraphOf: isSuperGraphOf:)
('initialize' initializeCodomain initializeDomain)
('operations' applyTo: applyToGraph: invertedApplyTo: invertedApplyToGraph:)
('set operations' /\ \/)
('testing' includes: intensionallyIncludes: treatsAsCodomainElement: treatsAsDomainElement:)
!

SquareGraph removeSelector: #treatsAsNode:!
SingletonGraph removeSelector: #&!
IntersectionGraph removeSelector: #add:!
IntersectionGraph removeSelector: #anyOne!

!RelSplitGraph reorganize!
('initialize' initializeCodomain initializeDomain)
('operations' applyTo: invertedApplyTo:)
('testing' intensionallyIncludes:)
!


!RelJoinGraph reorganize!
('initialize' initializeCodomain initializeDomain)
('operations' applyTo: invertedApplyTo:)
('testing' intensionallyIncludes:)
!

PluggableObjectGraph removeSelector: #treatsAsDomainElement:!
ObjectInstVarGraph removeSelector: #treatsAsCodomainElement:!
ObjectInstVarGraph removeSelector: #treatsAsDomainElement:!
ObjectGraph class removeSelector: #of:!
ObjectGraph removeSelector: #>>!
ClosureResultGraph removeSelector: #treatsAsDomainElement:!
ClassSubclassGraph removeSelector: #treatsAsCodomainElement:!
ClassSubclassGraph removeSelector: #treatsAsDomainElement:!
ClassInstanceGraph removeSelector: #treatsAsDomainElement:!

!IdentityGraph reorganize!
('accessing' anyOne)
('comparing' isSubGraphOf: isSuperGraphOf:)
('initialize' initializeCodomain initializeDomain)
('operations' applyTo: invert invertedApplyTo: reflexiveClosure selfComposeTimes: transitiveClosure)
('testing' intensionallyIncludes:)
!

FilteredGraph removeSelector: #&!
FilteredGraph removeSelector: #|!
InversionGraph removeSelector: #treatsAsNode:!
MetaFrameGraph removeSelector: #treatsAsDomainElement:!
FilteredObjectGraph removeSelector: #&!
FilteredObjectGraph removeSelector: #>>!
FilteredObjectGraph removeSelector: #cache!
FilteredObjectGraph removeSelector: #treatsAsCodomainElement:!
FilteredObjectGraph removeSelector: #treatsAsDomainElement:!
FilteredObjectGraph removeSelector: #|!

!FilteredObjectGraph reorganize!
('accessing' anyOne arrowFor: block graph kernel objectGraph operand)
('adding' add: addFor: addForAll:)
('comparing' isSubGraphOf:)
('filtering' filteredBy:)
('initialize' block: cache: initializeCodomain initializeDomain over:)
('operations' applyTo: applyToGraph: invertedApplyTo: invertedApplyToGraph:)
('set operations' /\ \/)
('testing' intensionallyIncludes:)
!

EquationalArrow removeSelector: #initialize!

!EquationalArrow reorganize!
('accessing' allMetaArrows headsTo tailsTo)
('initialize' initializeHeadsTo initializeTailsTo)
('operations')
('testing')
!

ArrowGraphMain removeSelector: #/*\!
ArrowGraphMain removeSelector: #/+\!
ArrowGraphMain removeSelector: #sucessorsOf:!

!ArrowGraphMain reorganize!
('accessing' codomain domain)
('comparing' isSubGraphOf: isSuperGraphOf:)
('filtering' codomain: domain: domain:codomain: filteredBy: filteredBy:caching: restrictedTo: testFor:)
('initialize' initializeCodomain initializeDomain)
('operations' applyTo: applyToGraph: graphCompose: inclusionGraph invertedApplyTo: invertedApplyToGraph: join: leftConjugateWith: leftQuotientWith: predecessorsOf: projected rightConjugateWith: rightQuotientWith: selfComposeTimes: split: successorsOf:)
('set operations' <*> <+> <<*>> <<+>>)
('testing' hasMetaArrowsFor: includes: includesReferencesTo: treatsAsCodomainElement: treatsAsDomainElement: treatsAsGround: treatsAsGroundNode:)
!

ArrowGraph removeSelector: #&!
ArrowGraph removeSelector: #<*>!
ArrowGraph removeSelector: #<+>!
ArrowGraph removeSelector: #treatsAsCodomainElement:!
ArrowGraph removeSelector: #treatsAsDomainElement:!
ArrowGraph removeSelector: #|!
ArrowFrame class removeSelector: #for:named:!
ArrowFrame initialize!

!ArrowFrame class reorganize!
('accessing instances and variables' lobby)
('class initialization' initialize)
('instance creation' for:)
!


!Arrow class reorganize!
('accessing' null unspecified)
('conversion' newFromAssociation:in:)
('instance creation' from: from:to: head:tail: to: wrapping:in:)
!

Arrow removeSelector: #head:tail:name:!

!Arrow reorganize!
('accessing' allMetaArrows at: at:put: atBitString: frame head object tail value world)
('accessing-synonyms' codomainElement destination domainElement rangeElement source target)
('adding' +)
('comparing' equals:)
('converting' asAbstract asArrow asAssociation asConcrete asGraph asGraphForArrow: asReference fromAssociation:in:)
('initialize' head: head:tail: headKnown tail: tailKnown)
('operations' ++ -> => @ addToGraph: compose: composeElements identity inv inverse invert pairWith: raiseNode)
('reifications' raise: raiseHead raiseIdentity raiseTail raiseValue)
('testing' coincidesWith:coords: incidesUpon: incidesUpon:coord: isAbstract isApexOfGraph isCompositionOf:and: isCompositionOf:with: isConcrete isElementOf: isIdentity isIn: isInverseOf: isNodeTo: isWrapper references:)
('wrapping' boxMeUp wrapInGraph)
!

ArrowStub removeSelector: #initialize!
ArrowStub removeSelector: #name!
ArrowStub removeSelector: #name:!
