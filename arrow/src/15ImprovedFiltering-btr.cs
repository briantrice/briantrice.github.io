'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 9 February 2002 at 12:15:41 pm'!
"Change Set:		15ImprovedFiltering-btr
Date:			12 October 2001
Author:			Brian T. Rice

Filters were updated to take advantage of the latest semantics and protocols."!


!FilteredGraph methodsFor: 'accessing' stamp: 'btr 9/23/2001 23:30'!
anyOne
	"If my cache is empty, I would have to provide an example of an object  
	satisfying my testBlock, which actually may have no answer."
	cache isEmpty
		ifTrue: [graph cache detect: testBlock
			ifNone: [self notYetImplemented]].
	^ cache anyOne! !

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


!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 9/23/2001 23:32'!
anyOne
	"If my cache is empty, I would have to provide an example of an object  
	satisfying my testBlock, which actually may have no answer."
	cache isEmpty
		ifTrue: [objectGraph cache
				detect: [:each | testBlock value: each object]
				ifNone: [self notYetImplemented]].
	^ cache anyOne! !

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

FilteredObjectGraph removeSelector: #cache!
FilteredObjectGraph removeSelector: #treatsAsCodomainElement:!
FilteredObjectGraph removeSelector: #treatsAsDomainElement:!
