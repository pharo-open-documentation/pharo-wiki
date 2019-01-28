# Cool Snippets

This files contains snippets of code that can be useful sometimes.

## Download a file with a progress bar

```Smalltalk
| outputFileName url |
outputFileName := './pharoLogo.png'.
outputFileName asFileReference ensureDelete.
url := 'http://files.pharo.org/media/logo/logo-flat.png' asZnUrl.
[ :bar | 
  bar title: 'Download: ' , url asString , ' to ' , outputFileName.
  [ ZnClient new
    url: url;
    signalProgress: true;
    downloadTo: outputFileName ]
    on: HTTPProgress
    do: [ :progress | 
      progress isEmpty ifFalse: [ bar current: progress percentage ].
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

## Browse all available icons
The following code snippet opens an inspector in which the 'Icons' tab allows to browse all icons available in the image.

```
Smalltalk ui icons inspect
```

To get a specific icon, use `#iconNamed:` method as follow:

```
Smalltalk ui icons iconNamed: #arrowUp
```
