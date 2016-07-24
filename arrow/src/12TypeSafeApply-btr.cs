'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 23 September 2001 at 10:12:54 pm'!
"Change Set:		12TypeSafeApply-btr
Date:			17 September 2001
Author:			Brian T. Rice

Ensures that each and every method for graph application and inverse application always returns another graph, even singletons, bottom, and top types for particular cases."!


!ClassInstanceGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:49'!
applyTo: anArrow 
	"Answers a graph of wrappers of all the instances of the argument's  
	wrapped behavior."
	anArrow object isBehavior
		ifFalse: [^ ArrowGraph bottom].
	^ objectGraph
		>> [:value | value class = anArrow object]
		addAll: (anArrow object allInstances
				collect: [:each | Arrow wrapping: each in: objectGraph])! !


!ClassSubclassGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:49'!
applyTo: anArrow 
	"Answers all wrapped subclasses of the argument's wrapped behavior."
	anArrow object isBehavior
		ifFalse: [^ ArrowGraph bottom].
	^ objectGraph
		>> [:value | value superclass == anArrow object]
		addAll: (anArrow object subclasses
				collect: [:each | Arrow wrapping: each in: objectGraph])! !

!ClassSubclassGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:00'!
invertedApplyTo: anArrow 
	"Answer the superclass of the argument's wrapped behavior."
	(anArrow object isKindOf: Behavior)
		ifFalse: [^ ArrowGraph bottom].
	^ objectGraph
		>> [:value | anArrow object superclass == value]
		add: (Arrow wrapping: anArrow object superclass in: objectGraph)! !


!EmptyGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:50'!
applyTo: anArrow 
	"I am empty."
	^ ArrowGraph bottom! !

!EmptyGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:00'!
invertedApplyTo: anArrow 
	"I am empty."
	^ ArrowGraph bottom! !


!GraphInclusionGraph methodsFor: 'accessing' stamp: 'btr 9/19/2001 20:31'!
baseArrowFor: anArrow 
	"Return the arrow in the base graph which the argument represents the 
	set-inclusion of for me. The abstract case is not handled."
	(self includes: anArrow)
		ifFalse: [^ ArrowGraph bottom].
	^ self graph add: anArrow codomainElement! !

!GraphInclusionGraph methodsFor: 'operations' stamp: 'btr 9/20/2001 16:14'!
applyToGraph: anArrowGraph 
	"As in #invertedApplyToGraph:, error-handling is not proper here."
	((anArrowGraph isKindOf: SingletonGraph)
			and: [anArrowGraph singleton == self apexNode])
		ifTrue: [^ self graph].
	^ ArrowGraph bottom! !

!GraphInclusionGraph methodsFor: 'operations' stamp: 'btr 9/20/2001 16:15'!
invertedApplyToGraph: anArrowGraph 
	"Perform a simple check before answering a graph of the apex node,  
	but the error case does not consider possible lazy concurrency of  
	evaluation. The more lenient case would produce an error in parallel  
	and allow any proper element of the argument to lead to the right  
	answer."
	(anArrowGraph isSubGraphOf: graph)
		ifFalse: [^ ArrowGraph bottom].
	^ SingletonGraph of: self apexNode! !


!HeadGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:01'!
invertedApplyTo: anArrow 
	"First check to make sure the argument is valid as a reified head arrow. 
	If so, return the arrow it would have been the result of."
	"TODO: Abstract arrows rely on this being a lazy promise, which it is  
	not."
	^ (self intensionallyIncludes: anArrow)
		ifTrue: [anArrow domainElement]
		ifFalse: [ArrowGraph bottom]! !


!IdentityGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:54'!
applyTo: anArrow 
	"Answer the argument. I am the identity operation. Also ensure the  
	arrow is in my graph."
	^ (graph includes: anArrow)
		ifTrue: [SingletonGraph of: anArrow]
		ifFalse: [ArrowGraph bottom]! !

!IdentityGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:02'!
invertedApplyTo: anArrow 
	"Answer the argument. I am the identity operation. Also ensure the  
	arrow is in my graph."
	^ (graph includes: anArrow)
		ifTrue: [SingletonGraph of: anArrow]
		ifFalse: [ArrowGraph bottom]! !


!InductionGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:51'!
applyTo: anArrow 
	"Successors should be unique per Arrow."
	(monoid includes: anArrow)
		ifFalse: [^ ArrowGraph bottom].
	anArrow isWrapper not
		ifTrue: [^ super * anArrow].
	^ (Arrow wrapping: anArrow object + 1 in: monoid) asGraph! !

!InductionGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:02'!
invertedApplyTo: anArrow 
	"Predecessors should be unique per Arrow, and respect successor usage.  
	NOTE: either anArrow is the result of 'monoid anyOne', in which case it 
	is treated as a variable, or it is the result of a successor construction,  
	which numbers converted to my elements produce on their own."
	((monoid includes: anArrow)
			and: [anArrow ~~ kernel])
		ifFalse: [^ ArrowGraph bottom].
	cache
		do: [:each | each codomainElement == anArrow
				ifTrue: [^ each domainElement]].
	^ (cache add: monoid anyOne -> anArrow) domainElement asGraph! !


!InversionGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:51'!
applyTo: anArrow 
	"Abstract arrows have to be handled by the generic lazy case, looking 
	for arrows leading from it to other arrows. Concrete arrows of any 
	kind, however, are treated in the simplest way possible."
	anArrow class = Arrow
		ifTrue: [^ super applyTo: anArrow].
	^ SingletonGraph of: anArrow inv! !


!MonoidGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:55'!
applyTo: anArrow 
	"All arrows in a monoid are identities over the kernel arrow."
	^ anArrow == kernel
		ifTrue: [SingletonGraph of: kernel]
		ifFalse: [ArrowGraph bottom]! !


!SingletonGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:52'!
applyTo: anArrow 
	"My singleton is the only arrow to check against."
	anArrow == singleton domainElement
		ifTrue: [^ SingletonGraph of: singleton codomainElement].
	^ ArrowGraph bottom! !

!SingletonGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:03'!
invertedApplyTo: anArrow 
	"My singleton is the only arrow to check against."
	^ anArrow == singleton codomainElement
		ifTrue: [SingletonGraph of: singleton domainElement]
		ifFalse: [ArrowGraph bottom]! !


!TailGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:04'!
invertedApplyTo: anArrow 
	"First check to make sure the argument is valid as a reified head  
	reference. If so, return the arrow it would have been the result of."
	^ (self intensionallyIncludes: anArrow)
		ifTrue: [anArrow domainElement]
		ifFalse: [ArrowGraph bottom]! !


!TopGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 15:52'!
applyTo: anArrow 
	"I answer everything."
	^ ArrowGraph top! !

!TopGraph methodsFor: 'operations' stamp: 'btr 9/16/2001 16:04'!
invertedApplyTo: anArrow 
	"I answer everything."
	^ ArrowGraph top! !

