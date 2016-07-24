Object subclass: #AbstractObject	instanceVariableNames: 'uiName '	classVariableNames: ''	poolDictionaries: ''	category: 'Abstract-Arrows'!!AbstractObject commentStamp: '<historical>' prior: 0!This class provides the default behavior for all parts of the arrow system.  As yet, the only use for it is to provide a system-wide naming protocol for a base-level user-interface (textual sugaring, really).!!AbstractObject methodsFor: 'accessing' stamp: 'btr 5/18/1999 19:38'!uiName
	"Answers the object's given name."
	^ uiName! !!AbstractObject methodsFor: 'accessing' stamp: 'btr 5/18/1999 19:38'!uiName: aString
	"Sets the object's given name to the argument. Type-checking is deliberately omitted, to allow for pretty-printing and generalization of user interface to implmentation via other abstract objects."
	uiName _ aString! !AbstractObject subclass: #Arrow	instanceVariableNames: 'head tail '	classVariableNames: ''	poolDictionaries: ''	category: 'Abstract-Arrows'!!Arrow commentStamp: '<historical>' prior: 0!Arrows reference exactly two objects that must be other arrows.  Arrows are instantiated (as extensional entities) for user-choice purposes only.  This includes calculations and user-interface choices.
In the actual implementation, however, references are abstracted as Reference objects, which are containers for the actual references.  This allows self-reference by arrows and reference on a graph that contains the particular arrow without causing the VM to go into an infinite recursion.!!Arrow methodsFor: 'accessing' stamp: 'btr 6/8/1999 01:27'!at: anInteger
	"Answers the reference indexed by the argument. This supports the generalization to multi-arrows."
	anInteger == 0 ifTrue: [^ tail foo].
	anInteger == 1 ifTrue: [^ head foo].
	self halt: 'Arrows only have slots indexed 0 and 1.'! !!Arrow methodsFor: 'accessing' stamp: 'btr 6/8/1999 01:32'!at: anInteger put: anArrow
	"Changes the indexed reference to the argument. Type-checking performed by head: and tail:."
	anInteger == 0
		ifTrue: [self tail: anArrow]
		ifFalse: [anInteger == 1
				ifTrue: [self head: anArrow]
				ifFalse: [self halt: 'Arrows only have slots indexed 0 and 1.']]! !!Arrow methodsFor: 'accessing' stamp: 'btr 6/8/1999 01:32'!head
	"Answers the second reference."
	^ head foo! !!Arrow methodsFor: 'accessing' stamp: 'btr 6/8/1999 01:36'!head: anArrow
	"Changes the second reference to the argument."
	|newReference|
	(anArrow isKindOf: Arrow)
		ifFalse: [self error: 'Arrows may only reference other arrows.'].
	newReference _ Reference new.
	newReference store: anArrow.
	head _ newReference! !!Arrow methodsFor: 'accessing' stamp: 'btr 6/8/1999 01:40'!head: anArrow1 tail: anArrow2 
	"Sets both references."
	self head: anArrow1.
	self tail: anArrow2! !!Arrow methodsFor: 'accessing' stamp: 'btr 5/17/1999 05:58'!head: anArrow1 tail: anArrow2 uiName: aString
	"Set the receiver's references and given name."
	self head: anArrow1.
	self tail: anArrow2.
	self uiName: aString
! !!Arrow methodsFor: 'accessing' stamp: 'btr 6/9/1999 16:51'!tail
	"Answers the first reference."
	^ tail foo! !!Arrow methodsFor: 'accessing' stamp: 'btr 6/8/1999 01:38'!tail: anArrow
	"Changes the first reference to the argument."
	|newReference|
	(anArrow isKindOf: Arrow)
		ifFalse: [self error: 'Arrows may only reference other arrows.'].
	newReference _ Reference new.
	newReference store: anArrow.
	tail _ newReference! !!Arrow methodsFor: 'operations' stamp: 'btr 6/8/1999 01:45'!compose: anArrow 
	"Answers the arrow composition of two arrows, the receiver being the second arrow in the sequence and the argument the first."
	(anArrow isKindOf: Arrow)
		ifFalse: [^ self].
	self tail == anArrow head ifTrue: [^ Arrow new head: (self head) tail: (anArrow tail)]! !!Arrow methodsFor: 'operations' stamp: 'btr 6/8/1999 01:45'!invert
	"Answers a new arrow with transposed references."
	^ Arrow new head: (self tail) tail: (self head)! !!Arrow methodsFor: 'initialize' stamp: 'btr 5/17/1999 05:59'!initialize
	"Initialize the receiver to empty references, referencing the ArrowWorld constant NulArrow."
	self head: nil tail: nil uiName: (String new)
! !!Arrow methodsFor: 'testing' stamp: 'btr 10/17/1999 23:11'!= anArrow
	"Tests for extensional equality. That is, equality via comparing the object's variables. Translates to Arrow system equality within square and relativised square frames.  Intensional equality is purely reflexive."
	(anArrow isKindOf: Arrow)
		ifFalse: [^false].
	self head = anArrow head ifTrue: [^self tail = anArrow tail].
	^false! !!Arrow methodsFor: 'testing' stamp: 'btr 6/8/1999 01:42'!ifReferences: anArrow
	"Answers whether or not the receiver points to the argument."
	^ (self head == anArrow) | (self tail == anArrow)! !!Arrow methodsFor: 'testing' stamp: 'btr 6/2/1999 12:00'!isCompositionOf: anArrow1 and: anArrow2 
	"Syntactic sugar."
	^ self isCompositionOf: anArrow1 with: anArrow2! !!Arrow methodsFor: 'testing' stamp: 'btr 5/17/1999 06:05'!isCompositionOf: anArrow1 with: anArrow2 
	"Answers whether the receiver is a valid composition result of the arguments in either order."
	^ self == (anArrow1 compose: anArrow2) | self == (anArrow2 compose: anArrow1)! !!Arrow methodsFor: 'testing' stamp: 'btr 6/8/1999 01:44'!isIdentity
	"Answers whether the receiver's references are identical. Identity arrows represent nul information transitions."
	^ (self head == self tail)! !!Arrow methodsFor: 'testing' stamp: 'btr 6/8/1999 01:45'!isInverseOf: anArrow
	"Answers whether the receiver is a valid inverse of the argument."
	^ self head == (anArrow tail) & (self tail == (anArrow head)).! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!Arrow class	instanceVariableNames: ''!!Arrow class methodsFor: 'instance creation' stamp: 'btr 10/17/1999 23:22'!new
	"Create a new arrow and initialize it."
	^ super new initialize.! !Arrow subclass: #ArrowGraph	instanceVariableNames: 'infinitary intension cache '	classVariableNames: ''	poolDictionaries: ''	category: 'Abstract-Arrows'!!ArrowGraph commentStamp: '<historical>' prior: 0!Arrows will always be referenced as members of graphs. Graphs are sets of arrows that are intensionally (and lazily) implemented, so that they may contain an infinite number of arrows. The implementation as for access will be to first search the underlying finite set of chosen arrows. If the arrow is not found there, then the graph 'declaration' is consulted (whether implemented by graphs or SmallTalk objects).
Each graph represents a (possibly reflective) relation, providing the fundamental constraint construct for specification.  Graphs are relative, in that they only represent the resulting relation computed by a declarative inference scheme.  In this way, ArrowGraphs are an implementational hack, in that they encode the information of the arrows they replace.!!ArrowGraph methodsFor: 'accessing' stamp: 'btr 5/18/1999 19:34'!cache
	^ cache! !!ArrowGraph methodsFor: 'accessing' stamp: 'btr 5/19/1999 08:14'!cache: aSet
	"Sets the cache to a known set.  Type-checking is not implemented yet."
	cache _ aSet! !!ArrowGraph methodsFor: 'accessing' stamp: 'btr 5/18/1999 18:42'!intension
	"Returns the intensional meaning of the receiver."
	^ intension! !!ArrowGraph methodsFor: 'accessing' stamp: 'btr 5/18/1999 18:42'!intension: anArrow
	"Sets the graph's intensional meaning to anArrow."
	intension _ anArrow! !!ArrowGraph methodsFor: 'accessing' stamp: 'btr 5/17/1999 06:09'!uiName
	"Answers the given name of the graph."
	^ uiName! !!ArrowGraph methodsFor: 'accessing' stamp: 'btr 5/17/1999 06:11'!uiName: aString
	"Sets the given name of the graph. Type-checking is deliberately omitted to allow for pretty-printing and generalizing user interface to include implementation via arrows."
	uiName _ aString! !!ArrowGraph methodsFor: 'adding' stamp: 'btr 10/18/1999 00:05'!add: anArrow 
	"Modifies the cache of the graph to include the argument as well.  The argument must satisfy the receiver's intension. Provide the proof incrementally and intensionally."
	anArrow isNil ifTrue: [self error: 'ArrowGraphs cannot meaningfully contain nil as an element'].
	(anArrow isKindOf: Arrow)
		ifFalse: [self error: 'ArrowGraphs can only contain arrows'].
	"Implement here the intension type check."
	cache ifNil: [cache _ Set new].
	cache add: anArrow! !!ArrowGraph methodsFor: 'comparing' stamp: 'btr 5/18/1999 19:33'!isMetaGraphOf: anArrowGraph 
	"Answers whether the receiver contains arrows which reference any arrows from the argument."

	(cache isEmpty) | (anArrowGraph cache isEmpty) ifTrue: [^ false].
	cache size ~~ (anArrowGraph cache size) ifTrue: [^ false].
	cache do: [:element | element == nil
			ifFalse: [
				[(anArrowGraph includes: [element tail])
					| (anArrowGraph includes: [element head]) ifTrue: [^ true]]
					value: element]].
	^ false! !!ArrowGraph methodsFor: 'comparing' stamp: 'btr 10/18/1999 10:14'!isSubGraphOf: anArrowGraph
	"Answers whether the argument contains all of the receiver's arrows. The implementation so far is only extensional."
	self extensional ifTrue: [(cache size > anArrowGraph cache size)
		ifTrue: [^false]].
	(cache isEmpty) | (anArrowGraph cache isEmpty) ifTrue: [^ true].
	(cache do: [:eachArrow | (anArrowGraph includes: eachArrow)
		ifFalse: [^false]]).
	^ true! !!ArrowGraph methodsFor: 'comparing' stamp: 'btr 5/14/99 21:41'!isSuperGraphOf: anArrowGraph
	^ anArrowGraph isSubGraphOf: self! !!ArrowGraph methodsFor: 'initialize' stamp: 'btr 10/17/1999 23:45'!initialize
	"Sets up the receiver for default behavior."
	cache _ Set new. "Probably obsolete, but not unsafe."
	self uiName: 'New Arrow Graph'.
	self setIntensional! !!ArrowGraph methodsFor: 'operations' stamp: 'btr 7/5/2000 01:21'!applyTo: anArrow 	"A method for applying a graph as a function to an arrow. This amounts to returning a graph of all arrows referenced by the heads of those arrows whose tails reference the argument. This implementation is the most generic case of graph-application. Extensional only for now."	| tempGraph |	tempGraph _ ArrowGraph new.	self cache do: [:eachArrow | eachArrow tail == anArrow ifTrue: [tempGraph add: eachArrow head]].	^ tempGraph! !!ArrowGraph methodsFor: 'operations' stamp: 'btr 5/17/1999 06:14'!compose: anArrowGraph
	"Answer a graph of arrows which are those resulting from all possible combinations of arrows of the receiver and argument. not implemented yet."
	|newArrowGraph|
	newArrowGraph _ (ArrowGraph new: tally).
	^ newArrowGraph! !!ArrowGraph methodsFor: 'operations' stamp: 'btr 7/5/2000 01:23'!invert	"Answers a new graph with arrows in the opposite configuration of the receiver. The implementation is completely non-lazy and non-intensional so far."	| anArrowGraph |	anArrowGraph _ self deepCopy.	cache do: [:eachArrow | eachArrow invert].	^ anArrowGraph! !!ArrowGraph methodsFor: 'operations' stamp: 'btr 7/5/2000 01:23'!metaGraph	"The intensional case should be handled by 'ArrowWorld>>metaGraphOf:'. Perhaps a better implementation would use 'Set>>collect:' ."	| newGraph |	newGraph _ ArrowGraph new initialize uiName: 'The meta-graph of ' , uiName.	self finite ifTrue: [cache do: [:eachArrow | newGraph add: (Arrow new head: self tail: eachArrow)]].	^ newGraph! !!ArrowGraph methodsFor: 'private' stamp: 'btr 5/18/1999 18:57'!setExtensional
	"Allows the system to completely implement the receiver. The inner set is treated as the actual graph.  This is useful when the intension is incomputable, as is the case for user-level choices."
	infinitary _ false! !!ArrowGraph methodsFor: 'private' stamp: 'btr 5/17/1999 06:30'!setIntensional
	"Forces evaluation to be strictly lazy, treating the inner set as a cache. This should be used whenever graph definition is intensional, even if the extension of the graph is finite, since the algorithm is only used in case the cache doesn't contain the queried arrow."
	infinitary _ true! !!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/18/1999 10:14'!extensional
	^self finite! !!ArrowGraph methodsFor: 'testing' stamp: 'btr 5/17/1999 06:32'!finite
	"Answers whether the internal cache should be treated as the graph itself."
	^ infinitary not! !!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/17/1999 23:57'!includes: anArrow
	"Answers whether the argument is part of the receiver."
	"This method over-rides the set method to allow for intensional description of graphs and their arrows, which will permit infinitary graphs.  The intensional search should be implemented after the cache is searched."
	(cache includes: anArrow)
		ifFalse: ["Check intension and return false if necessary."].
	^true! !!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/17/1999 23:54'!includesReferencesTo: anArrow 
	"Answers whether the receiver contains arrows which reference the argument."
	"This method over-rides the set method to allow for intensional description of graphs and their arrows, which will permit infinitary graphs.  The intensional search should be implemented after the internal cache is searched."
	(cache isEmpty) ifTrue: [^ false].
	cache do: [:eachArrow | (eachArrow ifReferences: anArrow) ifTrue: [^true]].
	^ false! !!ArrowGraph methodsFor: 'testing' stamp: 'btr 5/17/1999 06:34'!infinitary
	"Answers if evaluation should be strictly lazy."
	^ infinitary! !!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/18/1999 10:14'!intensional
	^self infinitary! !!ArrowGraph methodsFor: 'testing' stamp: 'btr 5/17/1999 06:35'!isDegenerate
	"Answers whether the graph 'speaks about' its own arrows. If true, then the graph cannot represent a first-order relation."
	^ self isMetaGraphOf: self! !!ArrowGraph methodsFor: 'testing' stamp: 'btr 10/17/1999 23:48'!isFirstOrder
	"Answers whether the receiver constitutes a valid first-order logical relation.  It also addresses whether the graph's arrows refer to the graph itself."
	^ self isNonDegenerate & (self includesReferencesTo: self)! !!ArrowGraph methodsFor: 'testing' stamp: 'btr 5/17/1999 06:37'!isNonDegenerate
	"Answers whether the receiver is valid as a first-order relation."
	^ self isDegenerate not! !ArrowGraph subclass: #ArrowWorld	instanceVariableNames: ''	classVariableNames: 'Cars Cdrs NulArrow UserGraph '	poolDictionaries: ''	category: 'Abstract-Arrows'!!ArrowWorld commentStamp: '<historical>' prior: 0!Provides the base model of functionality for arrows and graphs.  Notice that it inherits all of the features of ArrowGraph.  Every new object that is not the result of system deduction is automatically part of the UserChoiceGraph, the most primitive ontology.  Every arrow is automatically part of the SelfGraph, including a node representing Self itself in the set-theoretic sense.!!ArrowWorld methodsFor: 'accessing' stamp: 'btr 7/5/2000 00:36'!cars	"Return the graph of CARs for the system's arrows."	^Cars! !!ArrowWorld methodsFor: 'accessing' stamp: 'btr 7/5/2000 00:36'!cdrs	"Return the graph of CARs for the system's arrows."	^ Cdrs! !!ArrowWorld methodsFor: 'adding' stamp: 'btr 5/27/1999 15:49'!newNodeNamed: aString
	"Answers a new node for the world and names it."
	^ NulArrow deepCopy uiName: aString! !!ArrowWorld methodsFor: 'adding' stamp: 'btr 5/27/1999 23:21'!quantifier: anArrow
	^ ArrowGraph new intension: anArrow! !!ArrowWorld methodsFor: 'initialize-release' stamp: 'btr 7/6/2000 20:34'!initialize	| newArrow |	super initialize.	uiName _ 'New Arrow World'.	newArrow _ Arrow new.	newArrow		head: newArrow		tail: newArrow		uiName: 'Root Node'.	self add: newArrow.	UserGraph _ ArrowGraph new.	Cars _ CarGraph new.	Cdrs _ CdrGraph new! !!ArrowWorld methodsFor: 'initialize-release' stamp: 'btr 6/2/1999 12:03'!newNode
	"Answers a new node for the world."
	^ NulArrow deepCopy uiName: 'New Node'! !!ArrowWorld methodsFor: 'testing' stamp: 'btr 5/17/1999 06:47'!isNode: anArrow
	"Answers whether the argument is a node."
	^ anArrow head == NulArrow & anArrow isIdentity! !ArrowGraph subclass: #CarGraph	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Abstract-Arrows'!!CarGraph commentStamp: 'btr 7/6/2000 22:57' prior: 0!This class provides ArrowWorlds with graphs encapsulating the ability to create arrows representing the structure of the CAR (or tail) reference of any arrow in the system.!!CarGraph methodsFor: 'operations' stamp: 'btr 7/6/2000 19:51'!applyTo: anArrow 	"Reifies the CAR of the arrow as another arrow. Notice that the answer 	provided is not unique by necessity."	| newArrow |	newArrow _ Arrow new head: anArrow head tail: anArrow.	self cache do: [:eachArrow | eachArrow = self ifTrue: [^ eachArrow]].	self add: newArrow.	^ newArrow! !ArrowGraph subclass: #CdrGraph	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Abstract-Arrows'!!CdrGraph commentStamp: 'btr 7/6/2000 22:58' prior: 0!This class provides ArrowWorlds with graphs encapsulating the ability to create arrows representing the structure of the CDR (or head) reference of any arrow in the system.!!CdrGraph methodsFor: 'operations' stamp: 'btr 7/6/2000 19:48'!applyTo: anArrow 	"Reifies the CDR of the arrow as another arrow. Notice that the answer  	provided is not unique by necessity."	| newArrow |	newArrow _ Arrow new head: anArrow tail tail: anArrow.	self cache do: [:eachArrow | eachArrow = self ifTrue: [^eachArrow]].	self add: newArrow.	^ newArrow! !Object subclass: #Reference	instanceVariableNames: 'foo '	classVariableNames: ''	poolDictionaries: ''	category: 'Abstract-Arrows'!!Reference methodsFor: 'accessing' stamp: 'btr 6/8/1999 01:24'!foo
	^ foo! !!Reference methodsFor: 'accessing' stamp: 'btr 6/8/1999 01:26'!foo: anArrow
	(anArrow isKindOf: Arrow)
		ifTrue: [foo _ anArrow].! !!Reference methodsFor: 'accessing' stamp: 'btr 6/8/1999 01:27'!store: anArrow
	(anArrow isKindOf: Arrow)
		ifTrue: [foo _ anArrow].! !