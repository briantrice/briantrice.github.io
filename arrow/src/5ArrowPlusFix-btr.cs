'From Squeak3.1alpha of 7 March 2001 [latest update: #4081] on 21 June 2001 at 7:30:48 pm'!
"Change Set:		5ArrowPlusFix-btr
Date:			18 June 2001
Author:			Brian T. Rice

Turns + among arrows and graphs into a commutative operator. Previously, arrow>>+ was not delegating to ArrowGraph>>addImmutable:."!


!Arrow methodsFor: 'adding' stamp: 'btr 6/18/2001 17:03'!
+ anArrow 
	"Polymorphic operator to construct graphs. This assumes no intension 
	other than user-driven composition."
	(anArrow isKindOf: Arrow)
		ifTrue: [^ self asGraph | anArrow asGraph].
	^ (anArrow isKindOf: ArrowGraph)
		ifTrue: [anArrow addImmutable: self]
		ifFalse: [self error: 'Arrows can only be composed with other arrows and graphs.']! !

