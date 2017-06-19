# Athena [![Build Status](https://travis-ci.org/jonaprieto/athena.svg?branch=master)](https://travis-ci.org/jonaprieto/athena) [![DOI](https://zenodo.org/badge/85713337.svg)](https://zenodo.org/badge/latestdoi/85713337)

**Athena** is a [Haskell][haskell] program that translates proofs given by
[Metis][metis] ATP in [TSTP][tstp] format to [Agda][agda] code.

![diagram]

## Quick Start

For a more detailed explanation about how Athena is about, we invite you
to see our last [talk](https://github.com/jonaprieto/athena/raw/master/slides/Jonathan-Proof-Reconstruction.pdf)


##### Installing

Athena was tested with:

* GHC v8.0.2
* [Agda][agda] v2.5.2

To run Athena, you will to install Metis ATP locally. If you
have some problem install it, we recommend to use our client for TPTP World:
* [OnlineATPs][online-atps]

Now, installing Athena is very easy, just run this in your terminal:

```
$ git clone https://github.com/jonaprieto/athena.git
$ cd athena
$ make install
```

While running, you should see generation of TPTP problems, its solutions, and
reconstruction of these problems. Some of these problems were type-checked with Agda.


[diagram]: https://raw.githubusercontent.com/jonaprieto/athena/master/slides/diagram.png
[haskell]: http://www.haskell.org
[tstp]:    http://www.cs.miami.edu/~tptp/TPTP/QuickGuide/
[metis]:   http://github.com/gilith/metis
[agda]:    http://github.com/agda/agda
[agda-prop]: http://github.com/jonaprieto/agda-prop
[agda-metis]: http://github.com/jonaprieto/agda-metis
[online-atps]: http://github.com/jonaprieto/online-atps
