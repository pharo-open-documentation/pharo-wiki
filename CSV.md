# CSV support in Pharo
Currently, Pharo provides two main frameworks to handle the [CSV format](https://fr.wikipedia.org/wiki/Comma-separated_values).

## NeoCSV
NeoJSON is actually maintained by Sven Van Caekenberghe on [github](https://github.com/svenvc/NeoCSV).
This section shows some quick examples but there is a great [documentation made by Sven](https://github.com/svenvc/docs/blob/master/neo/neo-csv-paper.md) and a chapter in [Enterprise Pharo (chapter 8)](http://books.pharo.org/enterprise-pharo/).

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
```
```

- From `Stream`:
```
```

- Read from CSV file:
```
```

### Generate CSV
- To generate a CSV `String`:
```
```

- To generate CSV on a `Stream`:
```
```

- To write a CSV file:
```
```
