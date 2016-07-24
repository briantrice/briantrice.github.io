'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 23 September 2001 at 10:12:46 pm'!
"Change Set:		9Projections-btr
Date:			17 September 2001
Author:			Brian T. Rice

Adds direct support for one-to-one projections."!

MetaGraphGraph subclass: #ProjectionGraph
	instanceVariableNames: ''
	classVariableNames: 'CodomainResultTraits '
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!ProjectionGraph commentStamp: 'btr 9/18/2001 07:20' prior: 0!
This graph type encapsulates one-to-one embeddings from existing graphs-as-sets to new ones. Inversions of this graph type should be fully functional without provision by InvertedGraph for a special case.

TODO: Add a class variable that holds a specialized #anyOne method to give to my instances' #codomain results, since there should be a straightforward answer in that case.!


!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 9/18/2001 07:11'!
projected
	^ (ProjectionGraph of: self) codomain! !


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


!ProjectionGraph reorganize!
('accessing')
('adding' add: project:onto:)
('initialize' initializeDomain)
('operations' applyTo:)
('testing' intensionallyIncludes: treatsAsDomainElement:)
!

