'From Squeak3.1alpha of 7 March 2001 [latest update: #4081] on 17 September 2001 at 8:10:39 pm'!
"Change Set:		7Intensions-btr
Date:			17 September 2001
Author:			Brian T. Rice

Preliminary changes for support of intensions, aka existential quantification basis by example."!

ArrowStub subclass: #ArrowFrame
	instanceVariableNames: 'arrows assertions compositions heads inversions nodes objects tails values graphs '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!

!ArrowFrame methodsFor: 'accessing' stamp: 'btr 6/18/2001 16:32'!
graphs
	"A relational mapping from arrows to the arrow-wrappings of ArrowGraph objects."
	^ graphs! !

!ArrowFrame methodsFor: 'initialize' stamp: 'btr 6/18/2001 16:36'!
initialize
	"This initialization method is odd in that it sets up a lot of variables. Note 
	that #initialize should not be called in user code;  
	ArrowFrame >> #graph: and ArrowFrame class >> #for: initialize  
	automatically."
	| root |
	super initialize.
	root _ Arrow new.
	root head: root tail: root;
		 name: 'Root Node'.
	arrows
		ifNil: [arrows _ ArrowGraph new holdWeakly].
	assertions _ AssertionGraph over: self.
	compositions _ CompositionGraph over: self.
	inversions _ InversionGraph over: self.
	nodes _ MonoidGraph of: root.
	objects _ ObjectGraph newFromArrow: nodes anyOne.
	graphs _ MappingGraph from: arrows to: objects
					>> [:value | value isKindOf: ArrowGraph].
	tails _ TailGraph over: self.
	heads _ HeadGraph over: self.
	values _ SquareGraph over: self.
	arrows add: root! !


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

!ArrowGraph methodsFor: 'converting' stamp: 'btr 6/21/2001 00:12'!
asArrow
	"Answer a wrapper for me in my frame's MOP facility. This is the way to semantically identify me in the arrow type-system."
	^ Arrow wrapping: self in: self frame objects! !

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


!ArrowGraph class methodsFor: 'instance creation' stamp: 'btr 6/21/2001 00:05'!
newFromArrow: anArrow caching: anArrowGraph 
	"Annotate the argument with an initialized graph, caching some 
	elements."
	^ self new apex: anArrow;
		 cache: anArrowGraph asSet! !


!EquationalArrow methodsFor: 'accessing' stamp: 'btr 6/21/2001 00:28'!
allMetaArrows
	"Return all the arrows referring to me as their heads or tails."
	^ headsTo | tailsTo! !

!EquationalArrow methodsFor: 'initialize' stamp: 'btr 6/21/2001 00:30'!
initialize
	"Ensure that I don't hold me meta-arrows strongly since they should 
	strongly hold me. If the meta-arrows are needed for my semantics, 
	there should be back-arrows from me to them.
	TODO: implement this strategy."
	super initialize.
	headsTo _ ArrowGraph new holdWeakly.
	tailsTo _ ArrowGraph new holdWeakly! !

ArrowGraph removeSelector: #metaGraph!
ArrowStub subclass: #ArrowFrame
	instanceVariableNames: 'arrows assertions compositions graphs heads inversions nodes objects tails values '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Frames'!
