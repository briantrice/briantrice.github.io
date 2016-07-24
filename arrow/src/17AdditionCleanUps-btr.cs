'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 9 February 2002 at 12:15:46 pm'!
"Change Set:		17AdditionCleanUps-btr
Date:			12 October 2001
Author:			Brian T. Rice

Factored add: to include exception block support and mapping support through addAll: and addImmutableAll:."!


!ArrowGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 05:15'!
add: anArrow 
	"Just use the default error handler."
	^ self
		add: anArrow
		ifFail: [self error: 'This arrow does not satisfy the graph definition.']! !

!ArrowGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:31'!
add: anArrow ifFail: errorBlock 
	"This modifies the cache of the graph to include the argument as well. It 
	is not meant to represent actual addition of elements to an enumerated  
	(extensional) graph, so the argument must satisfy the receiver's  
	intension. Provide the proof incrementally and intensionally."
	(anArrow isKindOf: Arrow)
		ifFalse: [self error: 'ArrowGraphs can only contain arrows'].
	(self intensionallyIncludes: anArrow)
		ifFalse: [errorBlock value].
	cache ifNil: [cache _ WeakSet with: anArrow.
			^ anArrow]
			ifNotNil: [(cache size = 0)
				ifTrue: [cache _ cache class with: anArrow. ^ anArrow]].
	^ cache add: anArrow! !

!ArrowGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 05:16'!
addAll: aCollection 
	"Iterate over the collection. Ignore errors, since this is designed to be a batch-assertion facility for inclusion."
	aCollection
		do: [:each | self add: each ifFail: []].
	^ aCollection! !

!ArrowGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:21'!
addImmutable: anArrow 
	"Supports immutability and reification of extensional graph structure as  
	arrows. Note that this returns the resulting graph instead of the added element, so it is unlike add:."
	self
		add: anArrow
		ifFail: [^ UnionGraph
				of: (SingletonGraph of: anArrow)
				with: self]! !

!ArrowGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 05:05'!
addImmutableAll: aCollection 
	"Add all of the elements of the collection successively without affecting 
	any of the intermediate graph results' semantics. Note that the order 
	counts: if an arrow that does not fit gets added before an arrow that 
	would, the latter arrow would not get add:ed because the semantics of 
	the intermediate result at that point would be unavailable. TODO: fix 
	this."
	^ aCollection
		inject: self
		into: [:graphResult :each | graphResult addImmutable: each]! !

!ArrowGraph methodsFor: 'private' stamp: 'btr 10/12/2001 05:47'!
holdStrongly
	"I convert my cache to a Set from whatever it was before (most likely a 
	WeakSet). This is useful when the graph is intensional but is passed 
	around linearly (usually as the result of a computation) so that it is 
	the primary source of reference for its contents."
	| tempCache |
	tempCache _ Set new.
	cache
		ifNotNil: [tempCache init: cache size.
			cache addAll: tempCache].
	infinitary _ false! !

!ArrowGraph methodsFor: 'private' stamp: 'btr 10/12/2001 05:48'!
holdWeakly
	"The cache is forced to be a WeakSet, whose references aren't valid to 
	the garbage collector. Allows evaluation to be lazy, treating the inner 
	set as a cache. This can be used whenever graph definition is 
	intensional, even if the extension of the graph is finite, since the 
	algorithm is only used when cache searches fail. However, if the 
	graph is the only source of references to the arrow elements and they 
	are not trivially recomputed, then my cache should #holdStrongly."
	| tempCache |
	tempCache _ WeakSet newFrom: {}.
	cache
		ifNotNil: [tempCache init: cache size.
			cache addAll: tempCache].
	infinitary _ true! !


!EmptyGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 05:32'!
add: anArrow 
	"My intension is to be empty."
	^ self
		add: anArrow
		ifFail: [self error: 'This graph is intended to be empty.']! !


!ObjectGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:27'!
add: aWrapperArrow 
	"This graph should only contain WrapperArrows. This may be confusing 
	if one wants to add a wrapper for a wrapper and so forth (which is  
	valid, but produces a confusing chain)."
	"TODO: This also could speculatively create arrows for the various  
	meaningful relations for the object wrapped."
	^ self add: aWrapperArrow
		ifFail: [self error: 'ObjectGraphs can only meaningfully contain wrapper arrows.'. aWrapperArrow]! !


!TopGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 05:35'!
add: anArrow ifFail: errorBlock
	"Do nothing. A cache is useless in this case."
	^ anArrow! !

!TopGraph methodsFor: 'testing' stamp: 'btr 10/12/2001 05:34'!
intensionallyIncludes: anArrow 
	"I contain everything."
	^ true! !

