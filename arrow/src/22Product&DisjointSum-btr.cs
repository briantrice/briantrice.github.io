'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 9 February 2002 at 12:16:02 pm'!
"Change Set:		22Product&DisjointSum-btr
Date:			17 October 2001
Author:			Brian T. Rice

Adds safe implementations of set- and relational products and disjoint sums."!

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
Smalltalk renameClassNamed: #GraphDisjointSumGraph as: #SetDisjointSumGraph!
Smalltalk renameClassNamed: #GraphProductGraph as: #SetProductGraph!

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 11/20/2001 21:03'!
graphCompose: anArrowGraph 
	"Answer a graph of arrows which are those resulting from all possible  
	compositions of arrows from the receiver and argument respectively.  
	#asGraph coerces arrows to singleton graphs."
	^ RelCompositionGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 11/20/2001 01:12'!
join: anArrowGraph 
	^ RelJoinGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 11/20/2001 01:12'!
split: anArrowGraph 
	^ RelSplitGraph
		newFromArrow: (Arrow newFromAssociation: self -> anArrowGraph asGraph in: self world objects)! !

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


!BinaryOperatorGraph methodsFor: 'accessing' stamp: 'btr 10/17/2001 04:07'!
firstArgArrow
	^ apex tail! !

!BinaryOperatorGraph methodsFor: 'accessing' stamp: 'btr 10/17/2001 04:07'!
secondArgArrow
	^ apex head! !


!BinaryOperatorGraph class methodsFor: 'instance creation' stamp: 'btr 10/16/2001 23:32'!
of: firstGraph with: secondGraph 
	^ self new apex: firstGraph -> secondGraph;
		 initialize! !


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

ArrowGraphMain removeSelector: #/*\!
ArrowGraphMain removeSelector: #/+\!

!ArrowGraphMain reorganize!
('accessing' codomain domain)
('comparing' isSubGraphOf: isSuperGraphOf:)
('filtering' codomain: domain: domain:codomain: filteredBy: filteredBy:caching: restrictedTo: testFor:)
('initialize' initializeCodomain initializeDomain)
('operations' applyTo: applyToGraph: graphCompose: inclusionGraph invertedApplyTo: invertedApplyToGraph: join: leftConjugateWith: leftQuotientWith: predecessorsOf: projected rightConjugateWith: rightQuotientWith: selfComposeTimes: split: successorsOf:)
('set operations' <*> <+> <<*>> <<+>>)
('testing' hasMetaArrowsFor: includes: includesReferencesTo: treatsAsCodomainElement: treatsAsDomainElement: treatsAsGround: treatsAsGroundNode:)
!

ArrowGraph removeSelector: #<*>!
ArrowGraph removeSelector: #<+>!
