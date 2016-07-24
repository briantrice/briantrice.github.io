'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 23 September 2001 at 10:12:57 pm'!
"Change Set:		14TransitiveClosures-btr
Date:			23 September 2001
Author:			Brian T. Rice

Adds support for reflexive and transitive closures for graphs as relations."!

MetaGraphGraph subclass: #TransitiveClosureGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!TransitiveClosureGraph commentStamp: 'btr 9/23/2001 21:55' prior: 0!
This graph type represents the closure of recursive self-compositions of my argument graph. Designed to support recursion, since application will produce a lazy result chain of compositions of the core graph, and testing for inclusion results in a recursive search of that chain.

i.e. (TransitiveClosureGraph of: a) = a | (a ** a) | (a ** a ** a) | ...!
]style[(353)f1cblack;!


!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 21:37'!
reflexiveClosure
	^ (IdentityGraph of: self domain) | self! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 21:37'!
reflexiveTransitiveClosure
	^ self transitiveClosure reflexiveClosure! !

!ArrowGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 19:26'!
transitiveClosure
	"Answer the graph that would result if I were composed with myself ad  
	infinitum. This is a method to support recursion.  
	i.e. self transitiveClosure = self | self ** self | self ** self ** self | ... ."
	^ TransitiveClosureGraph of: self! !


!TransitiveClosureGraph methodsFor: 'comparing' stamp: 'btr 9/23/2001 21:40'!
isSuperGraphOf: anArrowGraph
	^ (anArrowGraph == graph
		or: [anArrowGraph class == self class
			and: [anArrowGraph graph == graph]])
		or: [super isSuperGraphOf: anArrowGraph]! !

!TransitiveClosureGraph methodsFor: 'initialize' stamp: 'btr 9/23/2001 19:51'!
initializeCodomain
	codomain _ graph codomain | (self * graph codomain)! !

!TransitiveClosureGraph methodsFor: 'initialize' stamp: 'btr 9/23/2001 19:48'!
initializeDomain
	domain _ graph domain! !

!TransitiveClosureGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 19:51'!
applyTo: anArrow 
	"Recurses on the application of the original arrow and all following  
	arrows lazily."
	| temp |
	temp _ graph applyTo: anArrow.
	^ temp
		| (self * temp)! !

!TransitiveClosureGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 21:58'!
applyToGraph: anArrowGraph 
	"Recurses on the application of the original graph and all following  
	arrows lazily."
	| temp |
	temp _ graph applyToGraph: anArrowGraph.
	^ temp | (self applyToGraph: temp)! !

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


!TransitiveClosureGraph reorganize!
('comparing' isSuperGraphOf:)
('initialize' initializeCodomain initializeDomain)
('operations' applyTo: applyToGraph: selfComposeTimes: transitiveClosure)
('testing' intensionallyIncludes:)
!

