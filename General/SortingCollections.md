# Sorting collections

A common requirement in software engineering is to sort collection. This page will give some informations about collection sorting in Pharo.

- [Sorting API](#sorting-api)
  * [Sort a collection](#sort-a-collection)
  * [Keep a collection sorted](#keep-a-collection-sorted)
- [Sort functions](#sort-functions)
- [Sorting via a block](#sorting-via-a-block)

## Sorting API

Collections in Pharo comes with an API to sort them. In this section we will present some if it and explain their difference.

The default sorting implemented on Pharo's collection is a merge sort. Mergesort is a worst-case O(N log N) sorting algorithm that usually does only half as many comparisons as heapsort or quicksort.

### Sort a collection 

The two main methods to sort a collection are `#sort:` and `#sorted:`. Those two methods are taking a block or a sort function as parameter (those are explained later in this page) and will return a collection sorted based on the parameter.
The difference between the two methods is that `#sort:` will sort the receiver when the `#sorted:` method will sort a copy of the receiver.

```Smalltalk
#(1 2 4 7 3 6 4) sort: #yourself ascending. "#(1 2 3 4 4 6 7) <= This result is the receiver"
#(1 2 4 7 3 6 4) sorted: #yourself ascending. "#(1 2 3 4 4 6 7) <= This result is a copy of the receiver"
```

Those two methods also have an equivalent without argument, `#sort` and `#sorted` that will sort the collection using the `#<=` comparison operator.

The API described here will sort a collection at one point but new elements added to the collection will not be sorted. If you wish to keep a collection sorted, you should use `SortedCollection` as explained in the next section.

### Keep a collection sorted

In case you want to keep a collection sorted, you should use a `SortedCollection`. This collection is configured with a sort block or sort function and will sort all new element added to the collection.

You can transform a collection into sorted collection using `#asSortedCollection:` or `#asSortedCollection`. 

```Smalltalk
(#(1 2 4 7 3 6 4) asSortedCollection: #yourself ascending)
	add: 2;
	yourself "a SortedCollection(1 2 2 3 4 4 6 7)"
```

You can also instantiate yourself the sorted collection:

```Smalltalk
(SortedCollection sortBlock: #yourself ascending)
	addAll: #(1 2 4 7 3 6 4);
	yourself "a SortedCollection(1 2 3 4 4 6 7)"
```


## Sort functions

The first way to sort a collection is to use a sort function. A sort function is an object configuring how to sort a collection. They can be created in defferent ways and can be composed. 

You can create a sort function using a property and a direction:

```Smalltalk
#('longstring' 'test' 'test2') sorted: #size ascending. "#('test' 'test2' 'longstring')"
#('longstring' 'test' 'test2') sorted: [ :string | string size ] descending "#('longstring' 'test2' 'test')"
```

```Smalltalk
#(#(1 2) #(2 3) #(0 0)) sorted: [:sequence | sequence inject: 0 into: [:sum :each | sum + each] ] descending. "#(#(2 3) #(1 2) #(0 0))"
```

You can use 2 arg blocks, for those cases where the function isn't expressible with objects that respond to < and =. The only catch, is that such a function has to return not true and false, but instead a collation order, values of -1 (for before), 0 (the same) or 1 (to follow). For example:

```Smalltalk
| oddBlock |
oddBlock := [ :a :b | a odd = b odd ifTrue: [ 0 ] ifFalse: [ a odd ifTrue: [ -1 ] ifFalse: [ 1 ] ] ].
#(1 5 1 3 2 7 9 4 6) asSortedCollection: oddBlock descending	"a SortedCollection(6 2 4 3 1 7 9 5 1)"
```

Using #undefinedFirst and #undefinedLast it is possible to deal with nil values, moving them first or last. For Example:

```Smalltalk
#(a nil z b) sorted: #value ascending undefinedFirst. "#(nil #a #b #z)"
#(a nil z b) sorted: #value ascending undefinedLast. "#(#a #b #z nil)"
```

Finally, you can use chained sorted function in case you have elements of the same priority in the first sort function.

In this next example, we sost by size and for elements of same size we sort by alphabetical order:

```Smalltalk
#('test' 'toto1' 'test2' 'toto') sorted: #size ascending, #yourself ascending "#('test' 'toto' 'test2' 'toto1')"
```

## Sorting via a block

The second way to sort a collection is to use a sort block. A sort block takes two elements of the collection as paramter and should return a boolean defining if the first one shoould be before the second one.

```Smalltalk
#('longstring' 'test' 'test2') sorted: [ :string1 :string2 | string1 size < string2 size ]. "#('test' 'test2' 'longstring')"
```
