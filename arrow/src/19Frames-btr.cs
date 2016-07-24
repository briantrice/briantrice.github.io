'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 9 February 2002 at 12:15:51 pm'!
"Change Set:		19Frames-btr
Date:			12 October 2001
Author:			Brian T. Rice

Some belated improvements were implemented in the Frame-related classes, as well as some re-works toward a different frame architecture."!

Object subclass: #ArrowStub
	instanceVariableNames: 'name '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Core'!

!ArrowStub commentStamp: 'btr 10/17/2001 06:37' prior: 0!
!

ArrowStub subclass: #ArrowGraph
	instanceVariableNames: 'apex infinitary intension cache frame '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!

!EquationalArrow commentStamp: 'btr 10/14/2001 14:53' prior: 0!
Variant of an abstract arrow which knows arrows that point to it and register themselves for later usage. This is meant to improve query results through extended caching.

The implementation also enforces abstraction of the head and tail references as arrows.!

ArrowGraphMain subclass: #MetaFrameGraph
	instanceVariableNames: 'frame '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!

!CompositionGraph commentStamp: 'btr 9/30/2001 13:39' prior: 0!
This graph type encapsulates the #composeElements protocol for frames. It attempts or rather promises a resulting arrow which is the composition of the arrows found as elements of the given domain arrow.

Notation Convention: Cxyz reads "x composes with y into z" or "x ** y == z"

More abstractly, all my arrows must be over value arrows, whose head and tail refer to the elements of the composition. The semantics of composition should be specified in the Assertions of the frame. Inversions and Identities can be expressed as Compositions, where Ix == Cxxx and Rxy == Cxyx and possibly Cyxx.!


!Arrow methodsFor: 'testing' stamp: 'btr 9/30/2001 13:51'!
isAbstract
	^ self class == Arrow! !

!Arrow methodsFor: 'testing' stamp: 'btr 9/30/2001 13:52'!
isConcrete
	^ self isAbstract not! !

!Arrow methodsFor: 'testing' stamp: 'btr 10/10/2001 04:36'!
isIn: anArrowGraph 
	"Sugaring."
	^ anArrowGraph includes: self! !


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

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 9/30/2001 14:25'!
root
	^ nodes root! !

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 10/17/2001 06:55'!
tails
	tails
		ifNil: [self initializeTails].
	^ tails! !

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


!ArrowFrame class methodsFor: 'class initialization' stamp: 'btr 10/17/2001 06:33'!
initialize
	"ArrowFrame initialize."
	"Provides a default arrow frame to play with, which can be directly  
	referred to as 'ArrowWorld'. Try this: 'ArrowWorld explore.'"
	Smalltalk at: #ArrowWorld put: (self new initialize)! !


!ArrowGraph methodsFor: 'accessing' stamp: 'btr 10/17/2001 06:41'!
world
	"This is a recursive method to determine what world a graph is in  
	without having to perform searches throuh all frames and graphs."
	"For now, assume that the graph belongs to the same world as its  
	apex."
	^ frame
		ifNil: [self apex world]! !

!ArrowGraph methodsFor: 'initialize' stamp: 'btr 10/17/2001 06:43'!
frame: anArrowFrame
	"Initialize the graph for a particular frame. Note, however, that a graph may still be represented in multiple non-intersecting frames."
	frame
		ifNotNil: [self notYetImplemented].
	frame _ anArrowFrame! !

!ArrowGraph methodsFor: 'initialize' stamp: 'btr 10/21/2001 11:10'!
initialize
	"Do nothing by default."! !

!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/7/2001 02:42'!
? anArrow
	"Sugaring."
	^ self includes: anArrow! !


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


!MetaFrameGraph methodsFor: 'initialize' stamp: 'btr 10/17/2001 06:32'!
frame: anArrowFrame 
	"Protected mutator."
	frame
		ifNotNil: [^ self class over: anArrowFrame].
	frame _ anArrowFrame! !


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


!InversionGraph methodsFor: 'initialize' stamp: 'btr 9/30/2001 14:01'!
initializeCodomain
	domain _ codomain _ self frame! !

!InversionGraph methodsFor: 'initialize' stamp: 'btr 9/30/2001 14:01'!
initializeDomain
	domain _ codomain _ self frame! !


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

!SquareGraph methodsFor: 'initialize' stamp: 'btr 9/30/2001 14:04'!
initializeCodomain
	domain _ codomain _ self frame! !

!SquareGraph methodsFor: 'initialize' stamp: 'btr 9/30/2001 14:04'!
initializeDomain
	domain _ codomain _ self frame! !

SquareGraph removeSelector: #treatsAsNode:!
InversionGraph removeSelector: #treatsAsNode:!
MetaFrameGraph removeSelector: #treatsAsDomainElement:!
ArrowGraphMain subclass: #MetaFrameGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!
EquationalArrow removeSelector: #initialize!

!EquationalArrow reorganize!
('accessing' allMetaArrows headsTo tailsTo)
('initialize' initializeHeadsTo initializeTailsTo)
('operations')
('testing')
!

ArrowStub subclass: #ArrowGraph
	instanceVariableNames: 'apex frame infinitary intension cache '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Graphs'!
ArrowFrame class removeSelector: #for:named:!
ArrowFrame initialize!
Arrow removeSelector: #head:tail:name:!
ArrowStub removeSelector: #initialize!
ArrowStub removeSelector: #name!
ArrowStub removeSelector: #name:!
Object subclass: #ArrowStub
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Core'!
