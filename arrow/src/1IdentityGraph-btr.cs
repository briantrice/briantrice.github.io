'From Squeak3.1alpha of 7 March 2001 [latest update: #4081] on 21 June 2001 at 7:30:37 pm'!
"Change Set:		1IdentityGraph-btr
Date:			18 June 2001
Author:			Brian T. Rice

Adds the Identity operation representative to Arrow-Relational Ops."!

MetaGraphGraph subclass: #IdentityGraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Arrow-Relational Ops'!

!IdentityGraph commentStamp: 'btr 6/18/2001 00:36' prior: 0!
This graph encapsulates Arrow>>#identity functionality.!


!IdentityGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 00:49'!
anyOne
	"Answer an identity over one of my graph's arrows."
	^ graph anyOne raiseIdentity! !

!IdentityGraph methodsFor: 'comparing' stamp: 'btr 6/18/2001 00:57'!
isSubGraphOf: anArrowGraph
	"Dispatch on my own type with a shortcut to check our arguments' inclusion."
	anArrowGraph class == self class
		ifTrue: [^ self graph isSubGraphOf: anArrowGraph graph].
	^ super isSubGraphOf: anArrowGraph! !

!IdentityGraph methodsFor: 'comparing' stamp: 'btr 6/18/2001 00:57'!
isSuperGraphOf: anArrowGraph 
	"Dispatch on my own type with a shortcut to check our arguments' 
	inclusion. "
	anArrowGraph class == self class
		ifTrue: [^ self graph isSuperGraphOf: anArrowGraph graph].
	^ super isSuperGraphOf: anArrowGraph! !

!IdentityGraph methodsFor: 'operations' stamp: 'btr 6/18/2001 01:00'!
applyTo: anArrow 
	"Answer the argument. I am the identity operation. Also ensure the 
	arrow is in my graph."
	^ (graph includes: anArrow)
		ifTrue: [anArrow]
		ifFalse: [nil]! !

!IdentityGraph methodsFor: 'operations' stamp: 'btr 6/18/2001 00:42'!
invert
	"Answer myself, since #applyTo: and #invertedApplyTo: are equivalent for identities."
	^ self! !

!IdentityGraph methodsFor: 'operations' stamp: 'btr 6/18/2001 01:02'!
invertedApplyTo: anArrow 
	"Answer the argument. I am the identity operation. Also ensure the  
	arrow is in my graph."
	^ (graph includes: anArrow)
		ifTrue: [anArrow]! !

!IdentityGraph methodsFor: 'testing' stamp: 'btr 6/18/2001 00:44'!
intensionallyIncludes: anArrow
	"The arrow must be an identity over one of my graph's elements."
	^ (graph includes: anArrow codomainElement)
		and: [anArrow isIdentity]! !


!IdentityGraph reorganize!
('accessing' anyOne)
('comparing' isSubGraphOf: isSuperGraphOf:)
('operations' applyTo: invert invertedApplyTo:)
('testing' intensionallyIncludes:)
!

