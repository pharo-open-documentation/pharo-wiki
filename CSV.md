# CSV support in Pharo
Currently, Pharo provides one main framework to handle the [CSV format](https://fr.wikipedia.org/wiki/Comma-separated_values): NeoCSV.

## NeoCSV
NeoJSON is actually maintained by Sven Van Caekenberghe on [github](https://github.com/svenvc/NeoCSV).
This section shows some quick examples but there is a great [documentation made by Sven](https://github.com/svenvc/docs/blob/master/neo/neo-csv-paper.md) and a chapter in [Enterprise Pharo (chapter 7)](http://books.pharo.org/enterprise-pharo/).

### Install
To install NeoCSV, simply execute the following code snippet in a playground:
```Smalltalk
Metacello new
    repository: 'github://svenvc/NeoCSV/repository';
    baseline: 'NeoCSV';
    load
```

### Parse CSV
- From `String`:
```Smalltalk
str := 'id, name
0, Julien
1, Cyril
2, Guillaume
'.
str readStreamDo: [ :s |
	(NeoCSVReader on: s)
		skipHeader;
		addIntegerField;
		addField;
		upToEnd ]
```

- From `Stream`:
```Smalltalk
stream := 'id, name
0, Julien
1, Cyril
2, Guillaume
' readStream.

(NeoCSVReader on: stream)
	skipHeader;
	addIntegerField;
	addField;
	upToEnd
```

- Read from CSV file:
```Smalltalk
'/path/to/file.csv' asFileReference
    readStreamDo: [ :readStream |
	    (NeoCSVReader on: readStream)
		    skipHeader;
		    addIntegerField;
		    addField;
		    upToEnd ]
```

### Generate CSV
Let `data` be defined as:
```Smalltalk
data := #(
(0 'Julien')
(1 'Cyril')
(2 'Guillaume')).
```

- To generate a CSV `String`:
```Smalltalk
String streamContents: [ :writeStream |
	(NeoCSVWriter on: writeStream)
		nextPut: #(id name);
		nextPutAll: data ]
```

- To generate CSV on a `Stream`:
```Smalltalk
(NeoCSVWriter on: writeStream)
	nextPut: #(id name);
	nextPutAll: data
```

- To write a CSV file:
```Smalltalk
'/path/to/file.csv' asFileReference
    writeStreamDo: [ :writeStream |
        (NeoCSVWriter on: writeStream)
	        nextPut: #(id name);
	        nextPutAll: data ]
```
