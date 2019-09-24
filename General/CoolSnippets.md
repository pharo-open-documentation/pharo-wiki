# Cool Snippets

This file contains snippets of code that can be useful sometimes.

- [Download a file with a progress bar](#download-a-file-with-a-progress-bar)
- [Bench and profile a project from the tests](#bench-and-profile-a-project-from-the-tests)
- [Automatic transformation of Pharo's methods source code](#automatic-transformation-of-pharo-s-methods-source-code)
- [Browse all available icons](#browse-all-available-icons)
- [Rename programatically methods](#rename-programatically-methods)
- [Get all senders/implementors of a selector](#get-all-sendersimplementors-of-a-selector)
- [Find dependencies on a package](#find-dependencies-on-a-package)
- [Embed an image (picture) into Pharo](#embed-an-image-picture-into-pharo)

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

## Automatic transformation of Pharo's methods source code
During software maintenance, being able to transform automatically source code can be handy. It can be done easily in Pharo using the built-in code rewriting engine.

For example, let's say that we want to replace the occurences of `#symbolToReplace` symbol with occurences of `#replacedSymbol` in the following method:

```Smalltalk
ExampleCodeTransformation>>#methodToTransform
	1 + 1 = 2
		ifTrue: [ ^ #symbolToReplace ].

	^ #symbolToReplace , #symbolNotToReplace
```

It can be done easily using the following code snippet:

```Smalltalk
compiledMethod := ExampleCodeTransformation >> #methodToTransform.

ast := compiledMethod parseTree.

"Here we select only nodes holding the target symbol and we replace it with a node representing `#replacedSymbol`."
ast allChildren
	select: [ :n | n class = RBLiteralValueNode and: [ n value = #symbolToReplace ] ]
	thenDo: [ :node |
		rewriter := RBParseTreeRewriter new
			replaceTree: node
			withTree: (RBLiteralValueNode value: #replacedSymbol);
			yourself.
		(rewriter executeTree: ast)
			ifTrue: [ node replaceWith: rewriter tree ] ].
		
"Serialize the new AST as Pharo source code."
rewrittenSourceCode := (BIConfigurableFormatter format: ast).

"Recompile the method with transformed source code."
ExampleCodeTransformation compile: rewrittenSourceCode
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

## Rename programatically methods

In this section we'll present a snippet of code to rename all the methods of a class containing a substring to replace it by another substring. 

```Smalltalk
"Class in which we want to rename the methods"
class := EpApplyPreviewerTest.
"Substring to replace"
from := 'With'.
"Substring to use"
to := 'Without'.

class methods
	select: [ :method | method selector includesSubstring: from ]
	thenDo: [ :method | 
		| permutationMap |
		"We want to keep the arguments in the same order"
		permutationMap := method numArgs = 0 ifTrue: [ #() ] ifFalse: [ (1 to: method numArgs) asArray ].
		
		(RBRenameMethodRefactoring
			renameMethod: method selector
			in: class
			to: (method selector copyReplaceAll: from with: to)
			permutation: permutationMap) execute ]
```

> Be careful, this will also rename the senders of those methods and if you have two methods of the same name in the image, it might badly rename some. Use this only on methods with unique names.

## Get all senders/implementors of a selector
The selector of a method is kind of the equivalent of the signature of a method or function in other programming language.
However, since Pharo is dynamically typed, this selector is only the name of the method, without the parameters.
For example, the selector of the method below is `#name:`

```
Example>>name: aString
	name := aString
```

For a given `CompiledMethod` in the system, its selector is accessible via `#selector` message.

```
(Object>>#yourself) selector. "#yourself"
```

### Senders
To get all the senders of a selector accross the image, simply call `#senders` on the selector:

```
#yourself senders
```

### Implementors
To get all `CompiledMethod`s implementing a method having a selector, simply call `#implementors` on the selector:

```
#yourself implementors
```

## Find dependencies on a package

In Pharo it is easy to find the dependencies of one package through the `Dependency Analyzer`, but there is not tool to browse the dependencies *on* a single package. 

It is possible to do it programatically via this snippet:

```Smalltalk
report := DADependencyChecker new computeImageDependencies. "This might take some time but it will run in background"
report knownDependantsOf: 'Epicea' "Replace Epicia by the name of your package."
```

## Embed an image (picture) into Pharo
If you want to use an image into Pharo, you will need to import it
```Smalltalk
ImageReadWriter formFromFileNamed: 'test.png'
```
and probably to store it into the image for further reuse. It is achieved by encoding the image into a base 64 String. Then, the String can be stored in a method.
```Smalltalk
(Base64MimeConverter mimeEncode: 'test.png' asFileReference binaryReadStream) contents
```
Let's say we stored the image base64 String in `Foo>>#image`. To materialize a Form from this image, you can do:
```Smalltalk
Form fromBinaryStream: Foo image base64Decoded asByteArray readStream
```
Here is a shortcut available since Pharo 8.0:
```Smalltalk
Form fromBase64String: Foo image
```
