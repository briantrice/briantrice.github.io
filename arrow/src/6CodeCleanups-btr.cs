'From Squeak3.1alpha of 7 March 2001 [latest update: #4081] on 21 June 2001 at 7:30:50 pm'!
"Change Set:		6CodeCleanups-btr
Date:			21 June 2001
Author:			Brian T. Rice

These changes don't affect semantics, but make the code more readible and make use of higher-level library protocols."!


!ClassInstanceGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:03'!
applyTo: anArrow 
	"Answers a graph of wrappers of all the instances of the argument's  
	wrapped behavior."
	anArrow object isBehavior
		ifFalse: [^ nil].
	^ objectGraph >> [:value | value class = anArrow object]
			addAll: (anArrow object allInstances
				collect: [:each | Arrow wrapping: each in: objectGraph])! !

!ClassInstanceGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:03'!
invertedApplyTo: anArrow 
	"Answer a wrapper for the class of the argument's wrapped object."
	^ objectGraph >> [:value | value == anArrow object class]
			add: (Arrow wrapping: anArrow object class in: objectGraph)! !


!ClassSubclassGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:02'!
applyTo: anArrow 
	"Answers all wrapped subclasses of the argument's wrapped behavior."
	anArrow object isBehavior
		ifFalse: [^ nil].
	^ objectGraph
		>> [:value | value superclass == anArrow object]
		addAll: (anArrow object subclasses
				collect: [:each | Arrow wrapping: each in: objectGraph])! !

!ClassSubclassGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:04'!
invertedApplyTo: anArrow 
	"Answer the superclass of the argument's wrapped behavior."
	(anArrow object isKindOf: Behavior)
		ifFalse: [^ nil].
	^ objectGraph
		>> [:value | anArrow object superclass == value] add: (Arrow wrapping: anArrow object superclass in: objectGraph)! !


!ClosureResultGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:04'!
applyTo: anArrow 
	"Since this graph only represents completed closures, getting the result is 
	straightforward."
	^ objectGraph
		>> [:value | value = anArrow object value] add: (Arrow wrapping: anArrow object value in: objectGraph)! !

!ClosureResultGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 00:54'!
invertedApplyTo: anArrow 
	"This is undecidable, since the class of closures returning a given value  
	is not well-defined. Instead I answer something that lazily filters for  
	such."
	^ objectGraph >> [:value | value value = anArrow]! !


!FilteredObjectGraph methodsFor: 'initialize' stamp: 'btr 6/21/2001 00:57'!
cache: aWrapperCollection 
	"Perform a few checks to ensure the cache's consistency."
	testBlock
		ifNil: [self error: 'Don''t set this graph''s cache before giving it a block to test against.'].
	objectGraph
		ifNil: [self error: 'Don''t set this graph''s cache before giving it an objectGraph to use.'].
	cache _ aWrapperCollection
		removeAllSuchThat: [:each | ((testBlock value: each object)
				and: [each objectGraph == objectGraph]) not]! !


!MetaGraph methodsFor: 'accessing' stamp: 'btr 6/21/2001 00:40'!
metaArrowFor: anArrow 
	"Returns the first (and only?) arrow whose head references the  
	argument."
	(graph includes: anArrow)
		ifFalse: [^ nil].
	^ cache
		detect: [:eachArrow | eachArrow codomainElement == anArrow]
				ifNone: [self add: self apexNode -> anArrow]! !


!ObjectInstVarGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:05'!
applyTo: anArrow 
	"This answers a graph of wrapper arrows for all the objects used in the  
	argument's slots."
	^ objectGraph >> [:value | anArrow object pointsTo: value]
			addAll: (anArrow object class allInstVarNames
				collect: [:each | Arrow
						wrapping: (self instVarNamed: each)
						in: objectGraph])! !

!ObjectInstVarGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:06'!
invertedApplyTo: anArrow 
	"This collects all objects having slots pointing to the argument slot,  
	answering a graph of arrow-wrappers for them."
	^ objectGraph >> [:value | value pointsTo: anArrow object]
			addAll: ((Smalltalk pointersTo: anArrow except: #()) asSet
				collect: [:each | Arrow wrapping: each in: objectGraph])! !


!PluggableObjectGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:09'!
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
		ifFalse: [^ nil].
	a _ anArrow object perform: selector withArguments: args.
	^ objectGraph
		>> [:value | a = value] add: (Arrow wrapping: a in: objectGraph)! !

!PluggableObjectGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:10'!
invertedApplyTo: anArrow 
	"If selector or block is defined, answer a graph which lazily filters  
	results."
	selector
		ifNil: [block
				ifNil: [^ nil]
				ifNotNil: [^ objectGraph >> [:value | (block value: value)
								= anArrow object]]].
	(self treatsAsDomainElement: anArrow)
		ifFalse: [^ nil].
	^ objectGraph
		>> [:value | (value perform: selector withArguments: args)
				= anArrow object]! !


!WrapperArrow methodsFor: 'accessing' stamp: 'btr 6/21/2001 00:24'!
object: anObject graph: anObjectGraph 
	"This is a protected mutator. It initializes, but also can return a new  
	instance of the appropriate ObjectGraph membership if possible. Also  
	ensures that only one wrapper exists for every (object, objectGraph) 
	pair. "
	(anObjectGraph isKindOf: ObjectGraph)
		ifFalse: [^ self error: 'Wrapper arrows can only meaningfully be owned by ObjectGraphs.'].
	graph
		ifNil: [graph _ anObjectGraph]
		ifNotNil: [graph = anObjectGraph
				ifFalse: [^ Arrow wrapping: anObject in: anObjectGraph]].
	"At this point, 'graph' must be initialized."
	object
		ifNotNil: [^ Arrow wrapping: anObject in: graph].
	object _ anObject.
	^ graph cache
		detect: [:each | each object = object]
		ifNone: [self addToGraph: graph]! !

