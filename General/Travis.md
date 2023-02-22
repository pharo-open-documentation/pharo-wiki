# Setting up Travis Continuous Integration

> IMPORTANT NOTE: Since Travis is becoming a pay to use service, it is possible that this documentation will not be updated anymore.
> [A page on how to use Github Actions](General/GithubActions.md) in order to have a continuous integration is already available on this wiki.

Your repository should have two extra hidden files: `.travis.yml` and `.smalltalk.ston`.
In addition, you may want to have a `.coveralls` configuration to support automatic coverage computation.

# `.travis.yml`
Here is a basic `.travis.yml` that configure travis to create a matrix to run your projects tests on both linux and osx plateforms and for both Pharo 8.0 and Pharo 7.0 64 bits.

```yaml
language: smalltalk
sudo: false

os:
  - linux
  - osx

smalltalk:
  - Pharo64-8.0
  - Pharo64-7.0
```

# `.smalltalk.ston`
`.smalltalk.ston` is the file where you specify the baseline to use for your project and the packages containing the tests (used to compute coverage).

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

# `.coveralls.yml`
If one have a coveralls account, one can add a `.coveralls.yml` file with the coveralls token of their project.


```yaml
service_name: travis-pro
repo_token: twxvbgLWXzoj3syZg3eaYfBZxCKEbensg
```
