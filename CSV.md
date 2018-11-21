# CSV support in Pharo

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

### Generate CSV
