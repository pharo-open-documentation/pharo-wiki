# Coding conventions

Pharo is full of implicit coding conventions. 

This page aims to document all those conventions to help newcomers to write Pharo code following the community's idioms.

**/!\ This document is a work in progress**

## Tests conventions

Pharo as some conventions when it comes to tests. This section will cover them.

**Package name**

First, it is usual to store tests rather in a separated package than in the package of the tested classes. The name of this package should be `NameOfOriginalPackage-Tests`.

For example, if one wants to add tests to `Material-Design-Lite-Core`, the tests should be in a package called `Material-Design-Lite-Core-Tests`.

**Class name**

Tests classes (subclasses of `TestCase`) should end with the suffix `Test` (and not `Tests` because a test class is a test case).

In the case of unit tests, the name of the class should be `MyClassTest`. For example, if one wants to add unit tests of `MDLButton`, the test class will be named `MDLButtonTest`.

**Method name**

Test method names in Pharo should always be prefixed with `test`. For example, `testListRectToDropEvent`.

In case of a unit test on a method of the tested class, the name should be prefixed by `test` followed by the name of the method to test.

For example:
- The unit test for `#click` will be named `#testClick`
- The unit test for `#clickElement:` will be named `#testClickElement`
- The unit test for `#clickElement:withShiftPressed:` will be named `#testClickElementWithShiftPressed`

By doing that, you should see a new icon next to your actual method name. This icon allows one to launch the unit test of the method via a click.

## Protocols conventions

> TODO
