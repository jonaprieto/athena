# Athena [![Build Status](https://travis-ci.org/jonaprieto/athena.svg?branch=master)](https://travis-ci.org/jonaprieto/athena) [![DOI](https://zenodo.org/badge/85713337.svg)](https://zenodo.org/badge/latestdoi/85713337)

**Athena** is a [Haskell][haskell] program that translates proofs given by
[Metis][metis] ATP in [TSTP][tstp] format to [Agda][agda] code.

![diagram]

## Quick Start

##### Requirements

Athena was tested with:

* GHC v8.0.2
* [Agda][agda] v2.5.2+

To install Metis ATP, we recommend use a client of TPTP World:
* [OnlineATPs][online-atps]

But you would probably want to install locally [Metis][metis] ATP.

Lastly, Athena needs these Agda libraries:

* [agda-prop][agda-prop]
* [agda-metis][agda-metis]

##### Installing

After satisfies all requirements, we can install Athena running:

```
$ git clone https://github.com/jonaprieto/athena.git
$ cd athena
$ cabal install
```

##### Testing

```bash
$ make basic
```

Many errors could show if you don't install the Agda libraries above listed.
Make you sure the Agda file to manage the libraries looks similar to this one:

```
$ cat ~/.agda/libraries
/Users/jonaprieto/agda-stdlib/standard-library.agda-lib
/Users/jonaprieto/agda-prop/agda-prop.agda-lib
/Users/jonaprieto/agda-metis/agda-metis.agda-lib
/Users/jonaprieto/athena/test/test.agda-lib
```

[diagram]: https://raw.githubusercontent.com/jonaprieto/athena/master/slides/diagram.png
[haskell]: http://www.haskell.org
[tstp]:    http://www.cs.miami.edu/~tptp/TPTP/QuickGuide/
[metis]:   http://github.com/gilith/metis
[agda]:    http://github.com/agda/agda
[agda-prop]: http://github.com/jonaprieto/agda-prop
[agda-metis]: http://github.com/jonaprieto/agda-metis
[online-atps]: http://github.com/jonaprieto/online-atps
