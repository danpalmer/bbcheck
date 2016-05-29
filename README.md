### bbcheck

[![Travis](https://travis-ci.org/danpalmer/bbcheck.svg)](https://travis-ci.org/danpalmer/bbcheck)

Provides an API that takes an address query and returns a list of broadband providers and their speed ranges available.

Configuration is done through files (although the files can include references to environment variables), the root configuration file must be specified in the `CONFIG_FILE` environment variable.

```
CONFIG_FILE=`pwd`/service.cfg bbcheck
```

`stack` is used for building and testing in the standard way.
