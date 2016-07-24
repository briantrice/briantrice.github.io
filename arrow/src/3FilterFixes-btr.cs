'From Squeak3.1alpha of 7 March 2001 [latest update: #4081] on 21 June 2001 at 7:30:43 pm'!
"Change Set:		3FilterFixes-btr
Date:			18 June 2001
Author:			Brian T. Rice

Added #operand to publically access the direct argument to a given filter. Fixes a bug when using filters over FilteredObjectGraphs. Modified the code to work with the right abstraction. Also added #>> support to all ArrowGraphs, which changes were lost in putting together v0.1 Arrow."!


!FilteredGraph commentStamp: 'btr 6/18/2001 13:44' prior: 0!
This graph type represents lazy results of queries on arrows. See also <FilteredObjectGraph>.

Filters are created from any graph and their testBlocks are composed via the >> selector.

Structure:
	<testBlock>	Determines membership by the call 'testBlock value: anArrow'.
		Example: If all arrows whose heads point to foo are desired, I can use:
			testBlock _ [:value | value head == foo]
			so that the mechanism is basically a lazy filter or set comprehension.!
]style[(72 19 372)f1,f1LFilteredObjectGraph Comment;,f1!


!FilteredObjectGraph commentStamp: 'btr 6/18/2001 13:36' prior: 0!
This graph type represents lazy results of queries on wrapped objects. This class has a strange place in the Arrow class hierarchy because it is a subgraph of its ObjectGraph while at the same time should not inherit from it and is not a proper MetaObjectGraph since it does not hold an operation over WrapperArrows' objects. See also <FilteredGraph>. However, note that FilteredObjectGraph differs from FilteredGraph in requiring that it's arrows be wrappers within the objectGraph originally filtered, and not any intermediate filteredObjectGraph.

Filters are created from any graph and their testBlocks are composed via the >> selector.

Structure:
	objectGraph	<ObjectGraph> or <FilteredObjectGraph> which is the operand of this
			filter. The accessor method always returns the original ObjectGraph in the
			filter chain.
	testBlock	Determines membership by the call 'testBlock value: anArrow object'.
		Example: If all objects of class OrderedCollection are desired, I can use:
			testBlock _ [:object | object class == OrderedCollection]
			so that the mechanism is basically a lazy filter or set comprehension.!
]style[(336 13 771)f1,f1LFilteredGraph Comment;,f1!


!ArrowGraph methodsFor: 'operations' stamp: 'btr 6/21/2001 01:16'!
>> aBlockContext 
	"Sugaring."
	^ self filteredBy: aBlockContext! !


!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 6/21/2001 01:17'!
filteredBy: aBlockContext 
	"Provide for filtering of my arrows by a test block taking the arrow as  
	argument. In this generic case, no cache or set of example values is  
	given."
	^ FilteredGraph
		newFrom: {}
		of: self
		for: aBlockContext! !

!ArrowGraphMain methodsFor: 'operations' stamp: 'btr 6/21/2001 01:17'!
filteredBy: aBlockContext caching: anArrowGraph 
	"Provide for filtering of my arrows by a test block taking the arrow as argument and an example graph of arrows satisfying the test."
	^ FilteredGraph
		newFrom: anArrowGraph asSet
		of: self
		for: aBlockContext! !


!FilteredGraph class methodsFor: 'instance creation' stamp: 'btr 6/21/2001 00:48'!
of: anArrowGraph for: aBlockContext 
	"Implicitly use the argument graph's cache to initialize the new instance's cache."
	^ self new graph: anArrowGraph;
		 block: aBlockContext;
		 cache: anArrowGraph asSet! !


!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 01:25'!
arrowFor: anObject 
	"This performs the intended check for membership by using objectGraph  
	properly."
	^ self
		add: (Arrow wrapping: anObject in: self objectGraph)! !

!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 00:10'!
cache
	"Accessing the cache directly bypasses this graph's checking against 
	testBlock."
	self shouldNotImplement! !

!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 13:28'!
graph
	"Polymorphism with FilteredGraph>>#graph. However, #objectGraph looks for the original objectGraph in the filter chain."
	^ objectGraph! !

!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 01:14'!
kernel
	"This allows FilteredObjectGraphs to be used for ObjectGraphs transparently. objectGraph is accessed indirectly because of the way successive filter applications work."
	^ self objectGraph kernel! !

!FilteredObjectGraph methodsFor: 'accessing' stamp: 'btr 6/18/2001 01:37'!
operand
	"Ensure that other filteredObjectGraphs know what my direct argument 
	is, since #objectGraph bypasses all FilteredObjectGraphs for the original 
	ObjectGraph."
	^ objectGraph! !

!FilteredObjectGraph methodsFor: 'adding' stamp: 'btr 6/18/2001 13:33'!
add: aWrapperArrow
	"Perform the usual method, but also ensure that my operand(s) and the original objectGraph get this added to their caches. The latter should not fail if the former doesn't."
	super add: aWrapperArrow.
	^ self operand add: aWrapperArrow! !

!FilteredObjectGraph methodsFor: 'initialize' stamp: 'btr 6/18/2001 01:43'!
over: anObjectGraph 
	"Protected mutator. This also is polymorphic in allowing for composition  
	of filters (of FilteredObjectGraphs)."
	objectGraph
		ifNotNil: [^ self class
				newFrom: cache
				over: anObjectGraph
				for: testBlock].
	anObjectGraph class == FilteredObjectGraph
		ifTrue: [objectGraph _ anObjectGraph.
			testBlock _ [:value | (testBlock value: value)
						and: [anObjectGraph
								includes: (Arrow wrapping: value in: self objectGraph)]].
			^ self].
	anObjectGraph class == ObjectGraph
		ifFalse: [self error: 'I cannot take that as an argument of my filter.'].
	objectGraph _ anObjectGraph! !

!FilteredObjectGraph methodsFor: 'set operations' stamp: 'btr 6/18/2001 13:20'!
& anArrowGraph 
	"My intersection with my objectGraph returns myself."
	anArrowGraph = objectGraph
		ifTrue: [^ self].
	((anArrowGraph isKindOf: FilteredObjectGraph)
			and: [anArrowGraph objectGraph == self objectGraph])
		ifTrue: ["Taking the intersection of two filteredObjectGraphs which are  
			over the same objectGraph is handled to take the intersection of 
			the testBlock and caches."
			^ FilteredObjectGraph
				newFrom: (cache intersect: anArrowGraph cache)
				over: (self operand intersect: anArrowGraph operand)
				for: [:value | (testBlock value: value)
						and: [anArrowGraph block value: value]]].
	^ super & anArrowGraph! !

!FilteredObjectGraph methodsFor: 'set operations' stamp: 'btr 6/18/2001 13:20'!
| anArrowGraph 
	"My union with my objectGraph returns the objectGraph."
	anArrowGraph = objectGraph
		ifTrue: [^ anArrowGraph].
	((anArrowGraph isKindOf: FilteredObjectGraph)
			and: [anArrowGraph objectGraph == self objectGraph])
		ifTrue: ["Taking the union of two filteredObjectGraphs which are  
			over the same objectGraph is handled to take the union of  
			the testBlock and caches."
			^ FilteredObjectGraph
				newFrom: (cache union: anArrowGraph cache)
				over: (self operand union: anArrowGraph operand)
				for: [:value | (testBlock value: value)
						or: [anArrowGraph block value: value]]].
	^ super | anArrowGraph! !

!FilteredObjectGraph methodsFor: 'testing' stamp: 'btr 6/18/2001 01:25'!
intensionallyIncludes: anArrow 
	"testBlock determines membership. Of course I check that the arrow is  
	a wrapper in the first place, and also that it belongs to the same  
	objectGraph as mine."
	^ (anArrow isWrapper
			and: [anArrow objectGraph = self objectGraph])
		and: [testBlock value: anArrow object]! !

!FilteredObjectGraph methodsFor: 'testing' stamp: 'btr 6/18/2001 13:42'!
treatsAsCodomainElement: anArrow 
	"Since I filter objectGraphs, which are monoids, this test just delegates to 
	them."
	^ self objectGraph treatsAsCodomainElement: anArrow! !

!FilteredObjectGraph methodsFor: 'testing' stamp: 'btr 6/18/2001 13:42'!
treatsAsDomainElement: anArrow 
	"Since I filter objectGraphs, which are monoids, this test just delegates to  
	them."
	^ self objectGraph treatsAsDomainElement: anArrow! !

AssertionGraph removeSelector: #intensionallyIncludes:!
