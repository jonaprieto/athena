# Athena [![Build Status](https://travis-ci.org/jonaprieto/athena.svg?branch=master)](https://travis-ci.org/jonaprieto/athena) [![DOI](https://zenodo.org/badge/85713337.svg)](https://zenodo.org/badge/latestdoi/85713337)

**Athena** is a [Haskell][haskell] program that translates proofs given by [Metis][metis] ATP in [TSTP][tstp] format to [Agda][agda] code.

<div style="text-align:center"><img src ="https://raw.githubusercontent.com/jonaprieto/athena/master/slides/diagram.png" /></div>

### Requisites

Agda libraries:

* [agda-prop](https://github.com/jonaprieto/agda-prop)
* [agda-metis](https://github.com/jonaprieto/agda-metis)

### Install

```
$ git clone https://github.com/jonaprieto/athena.git
$ cd athena
$ cabal install
```

[diagram]: https://raw.githubusercontent.com/jonaprieto/athena/master/slides/diagram.png
[haskell]: http://www.haskell.org
[tstp]:    http://www.cs.miami.edu/~tptp/TPTP/QuickGuide/
[metis]:   http://github.com/gilith/metis
[agda]:    http://github.com/agda/agda
