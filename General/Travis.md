Your repository should have two extra hidden files.

.travis.yml

```
language: smalltalk
sudo: false

os:
  - linux
  - osx

smalltalk:
  - Pharo64-8.0
  - Pharo64-7.0
```

.smalltalk.ston where you specify the baseline of your project and the packages containing the tests

```
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'EnlumineurFormatter',
      #directory : 'src',
      #platforms : [ #pharo ]
    }
  ],
  #testing : {
    #coverage : {
      #packages : [ 'EnlumineurFormatter-Tests' ]
    }
  }
}
```
