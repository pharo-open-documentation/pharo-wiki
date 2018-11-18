# JSON support in Pharo
Currently, Pharo provides two main frameworks to handle the [JSON format](https://en.wikipedia.org/wiki/JSON).

This page briefly present these two frameworks and expose their differences to help users to choose the that fit their needs.

## STONJSON
STONJSON is the built-in JSON parser available in default Pharo images. It is part of the STON package and its development takes place in Pharo's [github repository](https://github.com/pharo-project/pharo).

### Parse JSON
From `String`:
```Smalltalk
STONJSON fromString: '{ "foo" : 42.0 }' "a Dictionary('foo'->42.0 )"
```

From `Stream`:
```Smalltalk
readStream := '{ "foo" : 42.0 }' readStream.
STONJSON fromStream: readStream "a Dictionary('foo'->42.0 )"
```

### Generate JSON:
To `String`:
```Smalltalk
jsonObject := Dictionary new
		at: 'foo' put: 42.0;
		yourself.
STONJSON toString: jsonObject "'{""foo"":42.0}'"
```

On `Stream`:
```Smalltalk
jsonObject := Dictionary new
		at: 'foo' put: 42.0;
		yourself.
String streamContents: [ :writeStream  |
	STONJSON put: jsonObject onStream: writeStream ] "'{""foo"":42.0}'"
```

To pretty print JSON, either use `STONJSON>>#toStringPretty:` or `STONJSON>>#put:onStreamPretty:`.

## NeoJSON
Explain briefly with cool snippets and refer to Sven's doc.
