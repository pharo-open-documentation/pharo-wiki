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

> TODO

## Tools

> TODO

### Dependency Analyzer

> TODO

## Flags

> TODO

## Useful pragmas

> TODO
