# Cursor

The `Cursor` class in Pharo manage the display of the cursor on the screen.

It is possible to change the way it looks and we will explain how here.

To select the cursor you want you can check the class side of the `Cursor` class, in the "constants" protocol.

The first way to change the look of the cursor is to change it for the execution of a block. To do that you can use the `showWhile:` method.

```Smalltalk
Cursor wait showWhile: [ 3 second wait ]
```

The inconveniant with this method is that we do not always have a block and we can need to enable the new cursor in a method and disable it in another method. To do that Morph implements methods to help with the management of cursors via the hand morph.

```Smalltalk
self currentHand showTemporaryCursor: Cursor wait. 
self flag: #doSomethingHere.
self currentHand showTemporaryCursor: nil. "Reset the cursor to the previous one"
```
