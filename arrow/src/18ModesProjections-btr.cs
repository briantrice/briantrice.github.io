'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 9 February 2002 at 12:15:49 pm'!
"Change Set:		18ModesProjections-btr
Date:			12 October 2001
Author:			Brian T. Rice

Various modes and projections as morphisms between predicates and programs (see Exploring Logical Dynamics by Johan van Benthem) were implemented."!


!ArrowGraph methodsFor: 'operations' stamp: 'btr 10/14/2001 13:58'!
diagonal
	"Return a diagonal relation over my elements. Identity relations are called diagonal because the (a,a) ordered pairs fill the diagonal in the 2-dimensional array of possibilities for relational-holding."
	^ IdentityGraph over: self! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/9/2001 23:56'!
hasFixedPointFor: anArrow 
	"A fixed point for a relation is where application returns its argument."
	^ self includes: anArrow -> anArrow! !

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

!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/9/2001 23:55'!
isFixedPointFor: anArrow 
	"A fixed point for a relation is where application returns its argument."
	| temp |
	temp _ (self * anArrow).
	^ temp class == SingletonGraph and: [temp singleton == anArrow]! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/26/2001 11:53'!
isIsomorphism
	"The equational relational algebraic definition."
	^ self isTotalFunction
		and: [self inv isTotalFunction]! !

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


!ArrowGraph class methodsFor: 'accessing' stamp: 'btr 10/11/2001 01:08'!
id
	"Return the maximal identity graph."
	^ IdentityGraph over: self top! !


!ArrowGraphMain methodsFor: 'filtering' stamp: 'btr 10/11/2001 02:28'!
testFor: aBlockContext 
	"Answer the relation's graph representing the test mode of a predicate,  
	given as a block closure. The result contains exactly the identity arrows 
	over arrows satisfying the predicate."
	^ (PluggableGraph
		for: [:eachArrow | eachArrow isIdentity])
		domain: self
		codomain: self >> aBlockContext! !


!PluggableGraph methodsFor: 'testing' stamp: 'btr 10/11/2001 02:17'!
intensionallyIncludes: anArrow 
	"The block verifies the input-output pair."
	^ (super intensionallyIncludes: anArrow) and: [block value: anArrow]! !


!TransitiveClosureGraph methodsFor: 'testing' stamp: 'btr 11/20/2001 00:20'!
isPreOrderRelation
	"Reflexive transitive closures are pre-orders."
	^ self graph reflexiveClosure <= self graph! !

