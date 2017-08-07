# Athena [![Build Status](https://travis-ci.org/jonaprieto/athena.svg?branch=master)](https://travis-ci.org/jonaprieto/athena) [![DOI](https://zenodo.org/badge/85713337.svg)](https://zenodo.org/badge/latestdoi/85713337)

**Athena** is a [Haskell][haskell] program that translates [TSTP][tstp] proofs given by
the [Metis][metis] prover to Agda code.

![diagram]

## Quick Start

For a more detailed explanation about how Athena is about, we invite you
to see our last
[talk](https://github.com/jonaprieto/athena/raw/master/slides/Jonathan-Proof-Reconstruction.pdf).


### Installing

Athena was tested with:

* GHC v8.0.2
* [Agda][agda] v2.5.2

To run Athena, you need to install Metis prover. If you
have some problem for installing it, we recommend you our client for TPTP World:
* [OnlineATPs][online-atps]

```
$ git clone https://github.com/jonaprieto/athena.git
$ cd athena
$ make install
```

[diagram]: https://raw.githubusercontent.com/jonaprieto/athena/master/slides/diagram.png
[haskell]: http://www.haskell.org
[tstp]:    http://www.cs.miami.edu/~tptp/TPTP/QuickGuide/
[metis]:   http://github.com/gilith/metis
[agda]:    http://github.com/agda/agda
[agda-prop]: http://github.com/jonaprieto/agda-prop
[agda-metis]: http://github.com/jonaprieto/agda-metis
[online-atps]: http://github.com/jonaprieto/online-atps
