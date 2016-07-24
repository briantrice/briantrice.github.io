'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 23 September 2001 at 10:12:43 pm'!
"Change Set:		10DomainCodomain-btr
Date:			17 September 2001
Author:			Brian T. Rice

Adds support for accessing domain and codomain of any graph-as-relation in a safe and lazy manner, as well as type-checking in the appropriate places."!

ArrowGraph subclass: #ArrowGraphMain
	instanceVariableNames: 'domain codomain '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!

!ArrowGraph methodsFor: 'accessing' stamp: 'btr 9/23/2001 19:21'!
cache
	"Allow collaborating graphs to manipulate my cache if necessary. 
	If security is an issue, check all calls to this method. No object calling this method should be effecting the cache, only inspecting it."
	^ cache! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 9/23/2001 19:08'!
intensionallyIncludes: anArrow 
	"Default answer for all graphs; subclasses should override according to  
	their semantic intent."
	"TODO: alter this to use 'intension' (which requires building an intension 
	system first)."
	^ self isIntensional
		and: [(self treatsAsDomainElement: anArrow domainElement)
			and: [self treatsAsCodomainElement: anArrow codomainElement]]! !


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

!ArrowGraphMain methodsFor: 'initialize' stamp: 'btr 9/20/2001 16:06'!
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

!ArrowGraphMain methodsFor: 'initialize' stamp: 'btr 9/20/2001 16:06'!
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

!ArrowGraphMain methodsFor: 'testing' stamp: 'btr 9/19/2001 20:11'!
treatsAsCodomainElement: anArrow 
	codomain
		ifNotNil: [^ self codomain includes: anArrow].
	^ super treatsAsCodomainElement: anArrow! !

!ArrowGraphMain methodsFor: 'testing' stamp: 'btr 9/19/2001 20:11'!
treatsAsDomainElement: anArrow 
	domain
		ifNotNil: [^ self domain includes: anArrow].
	^ super treatsAsDomainElement: anArrow! !


!EmptyGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:47'!
initializeCodomain
	codomain _ ArrowGraph bottom! !

!EmptyGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:54'!
initializeDomain
	domain _ ArrowGraph bottom! !


!GraphCompositionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:47'!
initializeCodomain
	codomain _  self secondArg codomain! !

!GraphCompositionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:54'!
initializeDomain
	domain _ self firstArg domain! !

!GraphCompositionGraph methodsFor: 'testing' stamp: 'btr 9/19/2001 20:18'!
treatsAsCodomainElement: anArrow 
	^ self codomain includes: anArrow! !

!GraphCompositionGraph methodsFor: 'testing' stamp: 'btr 9/19/2001 20:17'!
treatsAsDomainElement: anArrow
	^ self domain includes: anArrow! !


!GraphInclusionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:49'!
initializeCodomain
	codomain _ self graph! !

!GraphInclusionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:55'!
initializeDomain
	domain _ SingletonGraph of: apex! !


!IdentityGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:49'!
initializeCodomain
	codomain _ graph! !

!IdentityGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:55'!
initializeDomain
	domain _ graph! !

!IdentityGraph methodsFor: 'testing' stamp: 'btr 9/19/2001 20:36'!
intensionallyIncludes: anArrow 
	"The arrow must be an identity over one of my graph's elements."
	^ anArrow isIdentity
		and: [self domain includes: anArrow domainElement]! !


!InductionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:49'!
initializeCodomain
	codomain _ monoid! !

!InductionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:56'!
initializeDomain
	domain _ monoid! !


!InvertedGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:50'!
initializeCodomain
	codomain _ graph domain! !

!InvertedGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:57'!
initializeDomain
	domain _ graph codomain! !


!MetaFrameGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:58'!
initializeDomain
	domain _ frame! !


!ProjectionGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:58'!
initializeDomain
	domain _ graph! !


!TopGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:50'!
initializeCodomain
	codomain _ ArrowGraph top! !

!TopGraph methodsFor: 'initialize' stamp: 'btr 9/19/2001 20:59'!
initializeDomain
	domain _ ArrowGraph top! !

InvertedGraph removeSelector: #codomain!
InvertedGraph removeSelector: #domain!
IdentityGraph removeSelector: #codomain!
IdentityGraph removeSelector: #domain!
GraphInclusionGraph removeSelector: #codomain!
GraphInclusionGraph removeSelector: #domain!

!GraphCompositionGraph reorganize!
('accessing' firstArg secondArg)
('initialize' initializeCodomain initializeDomain)
('operations' applyTo: applyToGraph: invertedApplyTo: invertedApplyToGraph:)
('testing' includes: treatsAsCodomainElement: treatsAsDomainElement:)
!

EmptyGraph removeSelector: #codomain!

!ArrowGraphMain reorganize!
('accessing' codomain domain)
('comparing' isSubGraphOf: isSuperGraphOf:)
('initialize' codomain: domain: initializeCodomain initializeDomain)
('operations' applyTo: applyToGraph: filteredBy: filteredBy:caching: graphCompose: inclusionGraph invertedApplyTo: invertedApplyToGraph: predecessorsOf: projected restrictedTo: selfComposeTimes: successorsOf:)
('testing' hasMetaArrowsFor: includes: includesReferencesTo: treatsAsCodomainElement: treatsAsDomainElement: treatsAsGround: treatsAsGroundNode:)
!

