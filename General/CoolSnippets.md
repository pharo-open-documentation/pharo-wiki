# Cool Snippets

This files contains snippets of code that can be useful sometimes.

## Download a file with a progress bar

```Smalltalk
| outputFileName |
outputFileName := 'C:/Users/MyUser/myFile.png'.
outputFileName asFileReference ensureDelete.
	[ :bar | 
	bar title: 'Download: ' , url asString , ' to ' , outputFileName.
	[ ZnClient new
		url: url;
		signalProgress: true;
		downloadTo: outputFileName ]
		on: HTTPProgress
		do: [ :progress | 
			progress ifNotEmpty: [ bar current: progress percentage ].
			progress resume ] ] asJob run
```

## Bench and profile a project from the tests

```Smalltalk

	packageSelectionBlock := [ :e | e name beginsWith: 'Spec' ].
	testSuite := TestSuite new.
	
	((RPackageOrganizer default packages select: packageSelectionBlock) flatCollect: #classes) select: [ :e | e inheritsFrom: TestCase ] thenDo: [ :e | e addToSuiteFromSelectors: testSuite ].

	"Bench the test suite"	
	[ testSuite run ] bench.

	"Profile the test suite"
	TimeProfiler spyOn: [ testSuite run ]

```
