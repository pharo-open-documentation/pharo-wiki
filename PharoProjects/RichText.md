# Rich text in Pharo

`Text` object models rich text. That is to say text that can be stylised with **bold**, *italics*, colors, different fonts and a few other things. The class `Text` is one of the really ancient classes in smalltalk.

Here is short example showing how it works basically:

```Smalltalk
text := 'Hello World!' asText.
text
	addAttribute: TextEmphasis bold from: 1 to: 5;
	addAttribute: (TextColor color: Color red) from: 7 to: 11;
	addAttribute: (TextColor color: Color green) from: 12 to: 12.
```

If you inspect `text`, you will see that `Hello` is bold, `World` is red, `!` is green.

## TextEmphasis
The class `TextEmphasis` defines a set of simple (no font change) changes you can do. See the class side of `TextEmphasis` for the supported styles of emphasis.

It i possible to overlap the different emphasis styles:

```Smalltalk
text := 'Hello World!' asText.
text
    addAttribute: TextEmphasis italic from: 7 to: 11;
    addAttribute: TextEmphasis struckOut from: 7 to: 11.
text
	addAttribute: TextEmphasis italic from: 7 to: 11;
	addAttribute: TextEmphasis struckOut from: 7 to: 11.
```

## Colors
Colored text is done as shown above. Examine the class `Color` for other colors.

## Different fonts
Loading and managing fonts is an issue in itself.

You can see which fonts you have available in your image using 'StrikeFont actualFamilyNames'.

Assume you have a font named: `Bitmap Source Sans Pro` (was the case on Macbook Pro, Pharo 8, fall 2019). 

You need to turn the font into a text attribute:

```Smalltalk
largeAttribute := TextFontReference
		toFont:
			(StrikeFont
				familyName: 'Bitmap Source Sans Pro'
				pointSize: 20).
'My larger text' asText addAttribute: largeAttribute from: 4 to: 10.
```
This should give you 'My larger text' with 'larger' being in font size 20 and in the 'Bitmap Source Sans Pro' font.

## Clickable text.
It is possible to make text do stuff when clicked. This is obviously a huge area. One simple example is:

```Smalltalk
textAction := TextAction new 
	actOnClickBlock: [ 
		Transcript nextPutAll: 'action clicked';cr;endEntry
		].

'My action text' asText addAttribute: textAction from: 4 to: 10.
```
