# Interesting things to know for a beginners

This page will expose different things that might be interesting to know for beginners. Tools, API, shortcuts...

The goal is to ease the use of Pharo for beginners.

## API

Pharo API is wide. In this section we will explain divers methods existing in Pharo that can often be useful from our experience.

**String class>>#streamContents:**

String concatenation can be costly because it will create a new string instance and copy the content of the two strings at each concatenation. To make it more efficient, Pharo includes a method to create a String via a stream:

```Smalltalk
'This is a ', 'strong concatenation ' , 'that is not really' , ' ' , ' effective'.

String streamContents: [ :aStream | aStream << 'This is a '<< 'strong concatenation ' << 'that is ' << ' ' << ' effective' ]
```

> TODO

## Valuables (Blocks and Symbols)
Pharo's Blocks and Symbols have an interesting property: they are polymorphic through `#value:` and `#cull:` methods.

For example, the two code snippets below are equivalent:

```Smalltalk
#class value: Object new
```

```
[ :o | o class ] value: Object new
```

They both return the class of an object.

It implies that for methods taking a block with one argument as parameter, this block can be replaced by a symbol. The effect is that the unary method having its selector equals to the symbol will be executed with the object as receiver.

The difference between `#value:` and `#cull:` is that `#value:` requires a parameter when `#cull:` can work with or without a parameter.

The common usage of this feature is via iterators. For example, if you want to collect the result of `name` method of all objects in a collection you can either do:

```
objectCollection collect: [ :o | o name ].
```

or

```
objectCollecton collect: #name
```

The second way is shorter and often more readable.

## Tools

> TODO

### Dependency Analyzer

> TODO

## Flags

> TODO

## Useful pragmas

> TODO
