# Cursor

The `Cursor` class in Pharo manages the display of the cursor on the screen.

It is possible to change the way it looks and we will explain how here.

To select the cursor you want you can check the class side of the `Cursor` class, in "constants" protocol.

The common way to change the look of the cursor is to change it for the execution of a block and then set it back to the original cursor. To do that you can use the `showWhile:` method.

```Smalltalk
Cursor wait showWhile: [ 3 second wait ]
```

It might be needed to have full control on the cursor without using a block. To do that Morph implements methods to help with the management of cursors via the hand morph. In the following code, `self` can be any object (`#currentHand` is defined on Object).

```Smalltalk
self currentHand showTemporaryCursor: Cursor wait. 
self flag: #doSomethingHere.
self currentHand showTemporaryCursor: nil. "Reset the cursor to the previous one"
```
