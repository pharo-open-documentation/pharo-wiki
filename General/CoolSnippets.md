# Cool Snippets

This files contains snippets of code that can be useful sometimes.

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