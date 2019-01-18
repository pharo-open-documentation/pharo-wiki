# Progress bar

This document will cover different ways in Pharo to display a progress bar when executing code.

## Progress bar on iterators

The easiest way to create a progress bar is to use the method `do:displayingProgress:`. This method is present on Collections and will add a progress bar updating during the iteration.

```Smalltalk
(1 to: 100) do: [ :each | each logCr. 100 milliSecond wait. ] displayingProgress:  [ :each | 'Iterating step ' , each asString ]
```

You can also customize the refresh time if you want better performances:

```Smalltalk
(1 to: 100) do: [ :each | each logCr. 100 milliSecond wait. ] displayingProgress:  [ :each | 'Iterating step ' , each asString ] every: 2000
```

This code will update the progress bar every 2 seconds.

## Use Jobs

An other way to display a progress bar is to use Jobs.

```Smalltalk
[:job | job title: 'Let us get started'.
	1 to: 10 do: [:each |
		job
			progress: (0.1 * each);
			title: 'Youpi ', each printString.
		(Delay forMilliseconds: 100) wait.
		] ]  asJob run
```

Or using UIManager:

```Smalltalk
UIManager default
	displayProgress: 'Let us get started'
	from: 1
	to: 10
	during: [ :bar | 
		1 to: 10 do: [:each |
			bar
				value: each;
				title: 'Youpi ', each printString.
	  (Delay forMilliseconds: 100) wait.
	]  ]
```

While using this method be careful of performances when you have a lot a really fast steps.

Here is a way to manage this:

```Smalltalk
UIManager default
	displayProgress: 'Let us get started'
	from: 1
	to: 40
	during: [ :bar | | lastUpdate | 
		lastUpdate := 0.
		1 to: 40 do: [:each |
			((Time millisecondsSince: lastUpdate) >= 500) ifTrue: [ 
				bar
					value: each;
					title: 'Youpi ', each printString.
				lastUpdate := Time millisecondClockValue
				 ].
	  (Delay forMilliseconds: 100) wait.
	]  ]
```

This will update the progress bar every 500 milliseconds instead of updating the bar each time we call #value: or #title:
