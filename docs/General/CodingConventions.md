# Coding conventions

Pharo is full of implicit coding conventions. 

This page aims to document all those conventions to help newcomers write Pharo code as per the community's idioms.

## Tests conventions

Pharo has some conventions when it comes to tests. This section covers those.

**Package name**

It is preferred to store tests in a separate package rather than in the package of the tested classes.
The name of this package should be `NameOfOriginalPackage-Tests`.

For example, if one wants to add tests to `Material-Design-Lite-Core`, the tests should be in a package called `Material-Design-Lite-Core-Tests`.

**Class name**

Tests classes (subclasses of `TestCase`) should end with the suffix `Test` (and not `Tests` because a test class is a test case).

In the case of unit tests, the name of the class should be `MyClassTest`. For example, if one wants to add unit tests of `MDLButton`, the test class will be named `MDLButtonTest`.

**Method name**

Test method names in Pharo need to be prefixed with `test`. This is the way the test framework recognizes them as test methods. 

In case of a unit test on a method of the tested class, the name should be prefixed by `test` followed by the name of the method to test.

For example:
- The unit test for `#click` will be named `#testClick`
- The unit test for `#clickElement:` will be named `#testClickElement`
- The unit test for `#clickElement:withShiftPressed:` will be named `#testClickElementWithShiftPressed`

By doing that, a new icon next to your actual method name will appear in the system browser.
This icon allows one to launch the unit test of the method via a click.
