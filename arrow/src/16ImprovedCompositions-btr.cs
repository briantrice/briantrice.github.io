'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 9 February 2002 at 12:15:44 pm'!
"Change Set:		16ImprovedCompositions-btr
Date:			26 September 2001
Author:			Brian T. Rice

Added support for relational quotient operators and cleaned up inclusion testing for compositions."!


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

ArrowGraphMain subclass: #BinaryOperatorGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!BinaryOperatorGraph commentStamp: 'btr 9/26/2001 12:13' prior: 0!
Lightweight abstract class for graphs that operate on a pair of graphs. This graph type is an annotation of the arrow-pair of those graphs.!

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


!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/28/2001 02:32'!
!!> anArrowGraph 
	"Sugaring."
	^ self leftConjugateWith: anArrowGraph! !

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


!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/28/2001 01:31'!
leftConjugateWith: anArrowGraph 
	^ GraphLeftConjugateGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/26/2001 12:37'!
leftQuotientWith: anArrowGraph
	^ GraphLeftQuotientGraph newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/28/2001 01:32'!
rightConjugateWith: anArrowGraph 
	^ GraphRightConjugateGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/26/2001 12:38'!
rightQuotientWith: anArrowGraph 
	^ GraphRightQuotientGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !


!BinaryOperatorGraph methodsFor: 'accessing' stamp: 'btr 9/26/2001 12:14'!
firstArg
	^ apex tail object! !

!BinaryOperatorGraph methodsFor: 'accessing' stamp: 'btr 9/26/2001 12:14'!
secondArg
	^ apex head object! !


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

