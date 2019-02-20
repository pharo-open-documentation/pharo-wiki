# Numbers
This page provides some hints for using Pharo's numbers. For more detailled information, check [Pharo by Example book](http://books.pharo.org/pharo-by-example).


## Numbers are not primitive
As written in [Pharo by Example book](http://books.pharo.org/pharo-by-example),

> Remarkably, numbers in Smalltalk are not primitive data values but true objects. Of course numbers are implemented efficiently in the virtual machine, but the Number hierarchy is as perfectly accessible and extensible as any other portion of the Smalltalk class hierarchy. - [Pharo by Example](http://books.pharo.org/pharo-by-example), Chapter 8 Section 2

Which is an interesting feature because it means that extension methods can be added to create objects holding a quantity. This is what is done for `Duration`s:

```
5 seconds. "0:00:00:05"
```

`#seconds` method creates a new instance of `Duration` representing the number second required.

## Scaled decimals
Scaled decimals allow one to model rationals. That is to say numbers with a fixed number of decimals. They are handy when dealing with money for example. It is possible to create a ScaledDecimal from its litteral form using the `s` notation: `1.45s2` which will store 1.45 with 2 digits precision in decimals.

## Special numbers
- Infinity: `Float infinity`
- Negative infinity: `Float negativeInfinity`
- Not a number: `Float nan`
- Pi, 2\*Pi, 3\*Pi: `Float pi. Float twoPi. Float threePi`
- Euler number: `Float e`

## Parse number
The class-side methods `#readFrom:` and `#readFrom:base:` of `Number` class allow one to parse a number from a `String`. Here are some examples:

```
Number readFrom: '10'. "10"
Number readFrom: '10' base: 2. "2"
Number readFrom: '1.0'. "1.0"
```

A specific subclass of `Number` can be used to ensure that the
