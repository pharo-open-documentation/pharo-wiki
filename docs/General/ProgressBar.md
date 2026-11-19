# Progress bar

This document covers different ways to display a progress bar during code execution.

## Progress bar on iterators

The easiest way to create a progress bar is to use the method `do:displayingProgress:`. This method is available on Collections and adds a progress bar automatically updated during iterations.

```Smalltalk
(1 to: 100) do: [ :each | each logCr. 100 milliSecond wait. ] displayingProgress:  [ :each | 'Iterating step ' , each asString ]
```

Refresh time can also be customized if better performances are needed. For example, the following code will update the progress bar every 2 seconds:

```Smalltalk
(1 to: 100) do: [ :each | each logCr. 100 milliSecond wait. ] displayingProgress:  [ :each | 'Iterating step ' , each asString ] every: 2000
```

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

Be careful while using this method. It might cause performance issues when time between two steps is short.

However, the following code shows a way to avoid these issues.

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

The trick is to leave at least 500 milliseconds between two updates instead of updating the bar each time we call `value:` or `title:`.
