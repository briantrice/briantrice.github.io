'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 9 February 2002 at 12:15:57 pm'!
"Change Set:		20MOPFeatures-btr
Date:			12 October 2001
Author:			Brian T. Rice

The MOP was extended with various features to make it more useful. Also duplicate checking was removed in ObjectGraph>>add:."!


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


!FilteredObjectGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:48'!
addFor: anObject 
	"Convenient method. Transparently returns a wrapping."
	^ self
		add: (Arrow wrapping: anObject in: self objectGraph)! !

!FilteredObjectGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:48'!
addForAll: aCollection 
	"Convenient method."
	^ aCollection do: [:each | self add: (Arrow wrapping: each in: self objectGraph)]! !


!MonoidGraph methodsFor: 'comparing' stamp: 'btr 10/13/2001 13:02'!
hash
	"Supposedly this should allow for accurate comparing via =."
	^ kernel hash! !

!MonoidGraph methodsFor: 'initialize' stamp: 'btr 10/21/2001 12:47'!
kernel: anArrow 
	"Protects the kernel arrow from modification after it is initialized."
	kernel
		ifNotNil: [^ self class of: anArrow].
	kernel _ anArrow! !


!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 11:42'!
arrowFor: anObject 
	"Answer an arrow wrapping the given object for me; avoids duplicates. 
	Note that wrapper arrows add: themselves to their objectGraphs 
	implicitly."
	^ WrapperArrow for: anObject in: self! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 12:44'!
arrowsFor: aCollection 
	"Add all arrows for each element of the collection, failing silently."
	aCollection
		do: [:each | self
				add: (self arrowFor: each)
				ifFail: []]! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 08:21'!
booleans
	"Return a graph of the Smalltalk boolean objects."
	^ self
		>> [:each | each isKindOf: Boolean]
		+ (self arrowFor: true)
		+ (self arrowFor: false)! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 12:38'!
characterMappings
	"Answers the map of integers to Smalltalk characters."
	^ (PluggableObjectGraph
		over: self
		for: [:each | (Character value: each domainElement object)
				== each codomainElement object])
		domain: self
				>> [:each | [each isKindOf: Integer]
						and: [each > 0]]
		codomain: self characters! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 12:33'!
characters
	"Answers the set of Smalltalk characters."
	^ self
		>> [:each | Character characterTable includes: each]
		addForAll: (Character allCharacters); yourself! !

!ObjectGraph methodsFor: 'accessing' stamp: 'btr 10/21/2001 10:47'!
instancesOf: aBehavior
	^ self >> [:each | self instanceOf * (self arrowFor: each) == aBehavior]! !

!ObjectGraph methodsFor: 'adding' stamp: 'btr 10/21/2001 11:48'!
addFor: anObject 
	"Convenient method. Transparently returns a wrapping and adds it to 
	the cache."
	^ WrapperArrow for: anObject in: self! !

!ObjectGraph methodsFor: 'adding' stamp: 'btr 10/12/2001 06:37'!
addForAll: aCollection 
	"Convenient method."
	^ aCollection do: [:each | cache add: (Arrow wrapping: each in: self)]! !

!ObjectGraph methodsFor: 'comparing' stamp: 'btr 10/13/2001 12:55'!
= anArrowGraph 
	"ObjectGraphs are only equivalent to other objectGraphs with the same 
	kernel."
	^ anArrowGraph class == self class
		and: [kernel == anArrowGraph kernel]! !

!ObjectGraph methodsFor: 'initialize' stamp: 'btr 10/21/2001 12:49'!
initialize
	super initialize.
	self beIntensional.
	"Don't enumerate through objectGraphs."
	infinitary _ true.
	subclassOf _ ClassSubclassGraph over: self.
	slotOf _ ObjectInstVarGraph over: self.
	instanceOf _ ClassInstanceGraph over: self.
	closureTo _ ClosureResultGraph over: self! !


!PluggableObjectGraph methodsFor: 'operations' stamp: 'btr 10/12/2001 04:54'!
applyTo: anArrow 
	"If selector or block is defined, apply it to the given arrow's object and  
	answer its wrapped result."
	| a |
	selector
		ifNil: [block
				ifNil: [^ nil]
				ifNotNil: [a _ block value: anArrow object.
					^ objectGraph
						>> [:value | a = value]
						add: (Arrow wrapping: a in: objectGraph)]].
	(self treatsAsDomainElement: anArrow)
		ifFalse: [^ ArrowGraph bottom].
	a _ anArrow object perform: selector withArguments: args.
	^ objectGraph
		>> [:value | a = value]
		add: (Arrow wrapping: a in: objectGraph)! !

!PluggableObjectGraph methodsFor: 'operations' stamp: 'btr 10/12/2001 04:55'!
invertedApplyTo: anArrow 
	"If selector or block is defined, answer a graph which lazily filters  
	results."
	selector
		ifNil: [block
				ifNil: [^ nil]
				ifNotNil: [^ objectGraph
						>> [:value | (block value: value)
								= anArrow object]]].
	(self treatsAsDomainElement: anArrow)
		ifFalse: [^ ArrowGraph bottom].
	^ objectGraph
		>> [:value | (value perform: selector withArguments: args)
				= anArrow object]! !

ObjectGraph class removeSelector: #of:!
