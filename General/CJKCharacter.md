# Using CJK (Chinese, Japanese, and Korean) characters

Pharo's default font settings for code (Source Code Pro) and UI (Source Sans Pro) are unable to handle [CJK characters](https://en.wikipedia.org/wiki/CJK_characters), this page explains how to display them properly in Pharo.

![Preview string for chinese characters under Source Code Pro](CJKCharacter_Screenshot_1.png)

Operating systems may come with fonts that support those characters but they are usually not monospaced which is not good for displaying code. If one need to display CJK characters in Pharo, it is recommended to install a monospace font which also covers CJK characters. Some of them are [`Noto Sans Mono CJK`](https://www.google.com/get/noto/) , `Microsoft YaHei Mono`, [`WenQuanYi Zen Hei Mono`](http://wenq.org).

Note one must enable the option `Update fonts at startup` first and restart Pharo to make the font selection dialog below show all available fonts:
1. Select menu `Pharo` -> `Settings` to show the `Settings Browser`.
2. Enable `Appearance` -> `Use Free type...` -> `Update fonts at startup`, this can be disabled later after one changes the fonts.

![Preview string for chinese characters under Microsoft YaHei Mono](CJKCharacter_Screenshot_2.png)

Note that on Windows 10, one may need to manually copy font files to `C:\Windows\Fonts\`.

Here is a code sample:

```Smalltalk
StandardFonts 
	defaultFont: (LogicalFont familyName: 'Noto Sans CJK SC' pointSize: 9);
	codeFont: (LogicalFont familyName: 'Microsoft YaHei Mono' pointSize: 10);
	listFont: (LogicalFont familyName: 'Noto Sans CJK SC' pointSize: 9);
	menuFont: (LogicalFont familyName: 'Noto Sans CJK SC' pointSize: 9);
	buttonFont: (LogicalFont familyName: 'Noto Sans CJK SC' pointSize: 9);
	windowTitleFont: (LogicalFont familyName: 'Noto Sans CJK SC' pointSize: 10);
	balloonFont: (LogicalFont familyName: 'Noto Sans CJK SC' pointSize: 8);
	haloFont: (LogicalFont familyName: 'Noto Sans CJK SC' pointSize: 8).
```

## Related links:

https://www.samadhiweb.com/blog/2019.02.22.windows.multilingual.html  
http://forum.world.st/Japanese-Fonts-td4928056.html  
http://forum.world.st/im-using-pharo-7-with-linux-env-but-cannot-input-korean-td5095342.html
