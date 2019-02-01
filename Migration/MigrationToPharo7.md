# Migration from Pharo 6.1 to Pharo 7.0 guideline

## Pharo 7 file streams guideline

Since the version 5, Pharo provides a new file streams API that makes the old one based on classes 
like `FileStream` or `MultiByteBinaryOrTextStream` deprecated. Pharo 7 makes the next important 
steps and removes usages of the old API from the kernel.

### What you should remember

- use file references as entry points to file streams
- do not use `FileStream` class
- `'file.txt' asFileReference readStream` and similar methods now return an instance of `ZnCharacterReadStream`
instead of `MultiByteFileStream`
- `'file.txt' asFileReference writeStream` and similar methods now return an instance of `ZnCharacterWriteStream` 
instead of `MultiByteFileStream`
- the new API has more clear separation between binary and text files

### Code conversion

In this section, we will present the most common examples of file stream usages. For simplicity, errors are not handled.

#### Force creation of a new file and write a UTF-8 text

##### obsolete code
```smalltalk
FileStream forceNewFileNamed: '1.txt' do: [ :stream | stream nextPutAll: 'a ≠ b' ].
```

##### new code
```smalltalk
'1.txt' asFileReference ensureDelete; 
	writeStreamDo: [ :stream | stream nextPutAll: 'a ≠ b' ].
```

#### Read UTF-8 text from an existing file

##### obsolete code
```smalltalk
FileStream readOnlyFileNamed: '1.txt' do: [ :stream | 
	stream upToEnd ].
```

##### new code
```smalltalk
'1.txt' asFileReference readStreamDo: [ :stream | 
	stream upToEnd ].
```

#### Get all content of existing UTF-8 file

##### obsolete code
```smalltalk
(FileStream readOnlyFileNamed: '1.txt') contentsOfEntireFile.
  ```

##### new code
```smalltalk
'1.txt' asFileReference readStream upToEnd.
```

#### Force creation of a new file and write binary data into it

##### obsolete code
```smalltalk
(FileStream forceNewFileNamed: '1.bin') 
	binary;
	nextPutAll: #[1 2 3].
```

##### new code
```smalltalk
'1.bin' asFileReference ensureDelete; 
	binaryWriteStreamDo: [ :stream | stream nextPutAll: #[1 2 3] ].
```

#### Read binary data from an existing file

##### obsolete code
```smalltalk
(FileStream readOnlyFileNamed: '1.bin') binary; contentsOfEntireFile.
```

##### new code
```smalltalk
'1.bin' asFileReference binaryReadStream upToEnd.
```

#### Force creation of a new file with a different encoding

##### obsolete code
```smalltalk
FileStream forceNewFileNamed: '2.txt' do: [ :stream | 
	stream converter: (TextConverter newForEncoding: 'cp-1250').
	stream nextPutAll: 'Příliš žluťoučký kůň úpěl ďábelské ódy.' ].
```

##### new code
```smalltalk
('2.txt' asFileReference) ensureDelete;
	writeStreamEncoded: 'cp-1250' do: [ :stream |
		stream nextPutAll: 'Příliš žluťoučký kůň úpěl ďábelské ódy.' ].
```

#### Read encoded text from an existing file

##### obsolete code
```smalltalk
FileStream readOnlyFileNamed: '2.txt' do: [ :stream | 
	stream converter: (TextConverter newForEncoding: 'cp-1250').
	stream upToEnd ].

```

##### new code
```smalltalk
('2.txt' asFileReference)
	readStreamEncoded: 'cp-1250' do: [ :stream |
		stream upToEnd ].
```

#### Write a UTF-8 text to STDOUT

##### obsolete code
```smalltalk
FileStream stdout nextPutAll: 'a ≠ b'; lf.
```

##### new code
```smalltalk
(ZnCharacterWriteStream on: Stdio stdout)
	nextPutAll: 'a ≠ b'; lf;
	flush.
```

#### Write CP-1250 encoded text to STDOUT

##### obsolete code
```smalltalk
FileStream stdout 
	converter: (TextConverter newForEncoding: 'cp-1250');
	nextPutAll: 'Příliš žluťoučký kůň úpěl ďábelské ódy.'; lf.
```

##### new code
```smalltalk
(ZnCharacterWriteStream on: Stdio stdout encoding: 'cp1250')
	nextPutAll: 'Příliš žluťoučký kůň úpěl ďábelské ódy.'; lf;
	flush.
```

#### Read a UTF-8 text from STDIN

**CAUTION:** Following code will stop your VM until an input on STDIN will be provided!

##### obsolete code
```smalltalk
FileStream stdin upTo: Character lf.
```

##### new code
```smalltalk
(ZnCharacterReadStream on: Stdio stdin) upTo: Character lf.
```

#### Write binary data to STDOUT

##### obsolete code
```smalltalk
FileStream stdout 
	binary
	nextPutAll: #[80 104 97 114 111 10 ].
```

##### new code
```smalltalk
Stdio stdout 
	nextPutAll: #[80 104 97 114 111 10 ].
```

#### Read binary data from STDIN

**CAUTION:** Following code will stop your VM until an input on STDIN will be provided!

##### obsolete code
```smalltalk
FileStream stdin binary upTo: 10.
```

##### new code
```smalltalk
Stdio stdin upTo: 10.
```

### Positionable streams

The message `#position:` always works on the binary level, not on the character level.

```smalltalk
'1.txt' asFileReference readStreamDo: [ :stream | 
	stream position: 4.
	stream upToEnd ].
```

This will lead to an error (ZnInvalidUTF8: Illegal leading byte for utf-8 encoding) in case of 
the file created above because we set the position into the middle of a UTF-8 encoded character.
To be safe, you need to read the file from the beginning.

```smalltalk
'1.txt' asFileReference readStreamDo: [ :stream |
	3 timesRepeat: [ stream next ].
	stream upToEnd.].
```

#### Buffering

The `MultiByteFileStream` was buffered. If you create a stream using the expression

```smalltalk
'file.txt' asFileReference readStream.
```

then the `ZnCharacterReadStream` is not created directly on top of the stream but on top of a buffered stream 
that uses the file stream internally. 

If you will create a `ZnCharacterReadStream` directly on the file stream, then the characters from the file are read 
one by one which may be about **ten times slower**!

```smalltalk
ZnCharacterReadStream on: (File openForReadFileNamed: 'file.txt').
```

