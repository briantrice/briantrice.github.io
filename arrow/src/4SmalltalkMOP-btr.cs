'From Squeak3.1alpha of 7 March 2001 [latest update: #4081] on 21 June 2001 at 7:30:46 pm'!
"Change Set:		4SmalltalkMOP-btr
Date:			18 June 2001
Author:			Brian T. Rice

Miscellaneous fixes and small enhancements to the SmalltalkMOP and its usage."!


!InductionGraph methodsFor: 'initialize' stamp: 'btr 6/18/2001 01:19'!
initialize
	"Set up an underlying monoid from my frame's reflective interface, and 
	set the kernel to wrap 0."
	super initialize.
	monoid _ self frame objects >> [:value | (value isKindOf: Integer) and: [value >= 0]].
	kernel _ Arrow wrapping: 0 in: monoid! !


!ObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 00:02'!
literals
	"Answer all the literals. However, PointerFinders also count due to a poor hack of #isLiteral to fix a bug in that class."
	^ self >> [:value | value isLiteral and: [(value class == PointerFinder) not]]! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 00:00'!
numbers
	"Return everything that is a number. This does not provide for distinguishment of types."
	^ self >> [:value | value isKindOf: Number]! !

