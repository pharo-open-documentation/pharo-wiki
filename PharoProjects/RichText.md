# Rich text in Pharo

`Text` object models rich text. That is to say text that can be stylised.

Here is short example showing how it works basically:

```Smalltalk
text := 'Hello World!' asText.
text
	addAttribute: TextEmphasis bold from: 1 to: 5;
	addAttribute: (TextColor color: Color red) from: 7 to: 11;
	addAttribute: (TextColor color: Color green) from: 12 to: 12.
```

If you inspect `text`, you will see that `Hello` is bold, `World` is red, `!` is green.
