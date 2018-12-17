# Traits

## Description

Traits are pure units of behavior that can be composed to form classes or other traits. The trait composition mechanism is an alternative to multiple or mixin inheritance in which the composer has full control over the trait composition. The result enables more reuse than single inheritance without introducing the drawbacks of multiple or mixin inheritance.

Since Pharo 7 Traits are modular and not tight the the Kernel. So it would be possible to have multiple implementations.

## Add a new Trait

Creation of a new Trait is close to the creation of a new class. It is done in a programatic way:

```Smalltalk
Trait named: #TNameOfMyTrait
	uses: {}
	package: 'MyPackage'
```

This will create a new Trait called `TNameOfMyTrait` stored in `MyPackage`

### Add a new method

> TODO

### Explicit requirement

> TODO

### Add a new variable

> TODO

## Use a Trait

> TODO

### Reject some methods

> TODO

### Trait using Traits

> TODO
