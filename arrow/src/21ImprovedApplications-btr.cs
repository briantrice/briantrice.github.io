'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 9 February 2002 at 12:15:59 pm'!
"Change Set:		21ImprovedApplications-btr
Date:			12 October 2001
Author:			Brian T. Rice

Some improvement was made on the robustness of graph application results."!


!GraphApplicationGraph commentStamp: 'btr 10/9/2001 23:45' prior: 0!
This is graph type holds the results of graph applications. It is responsible for ensuring the laziness of the operation by never allocating arrows until they are absolutely necessary. It should keep its results consistent with updates to the original graph. Instances of this graph type represent the result of single applications.

The apex node holds my argument list. The tail is the operator and the head is the argument.!


!GraphApplicationGraph methodsFor: 'accessing' stamp: 'btr 10/9/2001 22:03'!
anyResultForArrow: anArrow
	"Forces evaluation of one result arrow."
	^ (self resultForArrow: anArrow) anyOne! !

!GraphApplicationGraph methodsFor: 'accessing' stamp: 'btr 10/12/2001 06:23'!
resultForArrow: anArrow 
	"Apply my operator to the argument. Remember that this still always  
	returns a graph."
	| temp |
	(self argument includes: anArrow)
		ifFalse: [^ ArrowGraph bottom].
	temp _ self operator applyTo: anArrow.
	self addAll: temp cache.
	^ temp! !

!GraphApplicationGraph methodsFor: 'comparing' stamp: 'btr 10/9/2001 23:44'!
isSubGraphOf: anArrowGraph
	anArrowGraph class == self class
		ifTrue: [^ anArrowGraph operator == self operator
			and: [self argument isSubGraphOf: anArrowGraph argument]].
	^ super isSubGraphOf: anArrowGraph! !

