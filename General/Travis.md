Your repository should have two extra hidden files: .travis.yml and .smalltalk.ston.
In addition you may want to have a .coveralls configuration to support automatic coverage computation.

# .travis.yml

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

# .smalltalk.ston
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

If you have a coveralls account you can add a .coveralls.yml file with the coveralls token of your project.


```
service_name: travis-pro
repo_token: twxvbgLWXzoj3syZg3eaYfBZxCKEbensg
```
