# Athena [![Build Status](https://travis-ci.org/jonaprieto/athena.svg?branch=master)](https://travis-ci.org/jonaprieto/athena) [![DOI](https://zenodo.org/badge/85713337.svg)](https://zenodo.org/badge/latestdoi/85713337)

![diagram]

**Athena** is a [Haskell][haskell] program in a early stage that
translates [TSTP][tstp] proofs for problems in classical
propositional logic generated by [Metis][metis] to [Agda][agda] code.

See our last [talk](https://github.com/jonaprieto/athena/raw/master/slides/Jonathan-Proof-Reconstruction.pdf) for a complete description
of this tool.

### Installing

Clone this repository:

```
$ git clone https://github.com/jonaprieto/athena.git
$ cd athena
$ make check
```

Athena was tested with:

* GHC v8.0.2 and GHC v8.2.1
* [Agda][agda] v2.5.3
* [Metis][metis] v2.3 (release 20170810)

If you don't have installed the prover, you could use many ATP by
only use one program: [OnlineATPs][online-atps].

[diagram]: https://raw.githubusercontent.com/jonaprieto/athena/master/slides/diagram.png
[haskell]: http://www.haskell.org
[tstp]:    http://www.cs.miami.edu/~tptp/TPTP/QuickGuide/
[metis]:   http://github.com/gilith/metis
[agda]:    http://github.com/agda/agda
[agda-prop]: http://github.com/jonaprieto/agda-prop
[agda-metis]: http://github.com/jonaprieto/agda-metis
[online-atps]: http://github.com/jonaprieto/online-atps
