'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 23 September 2001 at 10:12:56 pm'!
"Change Set:		13MoreCleanUps-btr
Date:			23 September 2001
Author:			Brian T. Rice

Additional clean-ups: sugaring for SingletonGraphs, method category structure clean ups, provision for lazy predicate evaluation for set operation results."!


!IntersectionGraph methodsFor: 'adding' stamp: 'btr 9/23/2001 19:59'!
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
				& (aMessage sentTo: self secondArg)]! !


!SingletonGraph class methodsFor: 'instance creation' stamp: 'btr 9/20/2001 16:09'!
from: anApex to: anArrow
	"SingletonGraphs can be used as views of individual arrows."
	^ self new initialize apex: anApex; singleton: anArrow! !

!SingletonGraph class methodsFor: 'instance creation' stamp: 'btr 9/20/2001 16:19'!
fromArrow: anArrow
	^ self from: anArrow domainElement to: anArrow codomainElement! !


!UnionGraph methodsFor: 'operations' stamp: 'btr 9/23/2001 20:00'!
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
				| (aMessage sentTo: self secondArg)]! !


!FilteredObjectGraph reorganize!
('accessing' anyOne arrowFor: block cache graph kernel objectGraph operand)
('adding' add:)
('comparing' isSubGraphOf:)
('initialize' block: cache: over:)
('operations' >> filteredBy:)
('set operations' & |)
('testing' intensionallyIncludes: treatsAsCodomainElement: treatsAsDomainElement:)
!


!Arrow reorganize!
('accessing' allMetaArrows at: at:put: atBitString: frame head object tail value world)
('accessing-synonyms' codomainElement destination domainElement rangeElement source target)
('adding' +)
('comparing' equals:)
('converting' asAbstract asArrow asAssociation asConcrete asGraph asGraphForArrow: asReference fromAssociation:in:)
('initialize' head: head:tail: head:tail:name: headKnown tail: tailKnown)
('operations' ++ -> => @ addToGraph: compose: composeElements identity inv inverse invert pairWith: raiseNode)
('reifications' raise: raiseHead raiseIdentity raiseTail raiseValue)
('testing' coincidesWith:coords: incidesUpon: incidesUpon:coord: isApexOfGraph isCompositionOf:and: isCompositionOf:with: isElementOf: isIdentity isInverseOf: isNodeTo: isWrapper references:)
('wrapping' boxMeUp wrapInGraph)
!

