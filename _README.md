# Athena [![Build Status](https://travis-ci.org/jonaprieto/athena.svg?branch=master)](https://travis-ci.org/jonaprieto/athena) [![DOI](https://zenodo.org/badge/85713337.svg)](https://zenodo.org/badge/latestdoi/85713337)

<img align="center" src="https://tex.s2cms.ru/svg/%0A%5Cdefinecolor%7Bdarkgreen%7D%7Brgb%7D%7B0.0%2C%200.5%2C%200.0%7D%0A%5Cdefinecolor%7Bdarkred%7D%7Brgb%7D%7B0.5%2C%200.0%2C%200.13%7D%0A%5Cusetikzlibrary%7Bpositioning%7D%0A%5Cusetikzlibrary%7Barrows%7D%0A%5Cusetikzlibrary%7Bcalc%7D%0A%5Cusetikzlibrary%7Bshapes%7D%0A%5Ctikzstyle%7Bline%7D%20%3D%20%5Bdraw%2C%20-latex'%5D%0A%5Cbegin%7Btikzpicture%7D%5B%0A%20%20%20auto%0A%20%25%20%2C%20scale%3D0.15%0A%20%2C%20font%3D%5Csmall%0A%20%2C%20base%2F.style%20%3D%0A%20%20%20%20%20%20%7B%20align%3Dcenter%0A%20%20%20%20%20%20%7D%0A%20%2C%20file%2F.style%20%3D%0A%20%20%20%20%20%20%7B%20base%0A%20%20%20%20%20%20%2C%20minimum%20height%3D3em%0A%20%20%20%20%20%20%2C%20shape%3Drectangle%0A%20%20%20%20%20%20%25%20%2C%20rounded%20corners%0A%25%20%20%20%20%20%20%2C%20fill%3Dblue!2%0A%20%20%20%20%20%20%2C%20inner%20sep%3D3pt%0A%20%20%20%20%20%20%2C%20outer%20sep%3D1pt%0A%20%20%20%20%20%20%2C%20draw%25%3Dblue!20%0A%20%20%20%20%20%20%7D%0A%20%20%2C%20program%2F.style%20%3D%0A%20%20%20%20%20%20%7B%20base%0A%20%20%20%20%20%20%25%20%2C%20fill%3Dgray!1%0A%20%20%20%20%20%20%25%20%2C%20rounded%20corners%0A%20%20%20%20%20%20%2C%20inner%20sep%3D3pt%0A%20%20%20%20%20%20%2C%20outer%20sep%3D1pt%0A%20%20%20%20%20%20%7D%0A%20%20%2C%20library%2F.style%20%3D%0A%20%20%20%20%20%20%7B%20shape%3Drectangle%0A%20%20%20%20%20%20%25%20%2C%20rounded%20corners%0A%20%20%20%20%20%25%20%2C%20fill%3Dblue!2%0A%20%20%20%20%20%20%2C%20draw%25%3Dblue!20%0A%20%20%20%20%20%20%2C%20inner%20sep%3D3pt%0A%20%20%20%20%20%20%2C%20outer%20sep%3D1pt%0A%20%20%20%20%20%20%7D%0A%20%20%2C%20Rhoumbus%2F.style%20%3D%0A%20%20%20%20%20%20%7B%20base%0A%20%20%20%20%20%20%2C%20aspect%3D2%0A%20%20%20%20%20%20%25%20%2C%20fill%3Dblue!2%0A%20%20%20%20%20%20%2C%20draw%25%3Dblue!20%0A%20%20%20%20%20%20%2C%20diamond%0A%20%20%20%20%20%20%2C%20align%3Dcenter%0A%20%20%20%20%20%20%2C%20inner%20xsep%3D1pt%2C%0A%20%20%20%20%20%20%2C%20inner%20ysep%3D1.5pt%0A%20%20%20%20%20%20%2C%20outer%20sep%20%3D%202pt%0A%20%20%20%20%20%20%7D%0A%20%5D%0A%5Cnode%5Bfile%5D%0A(tptp)%20at%20(0%2C0)%20%7B1.~%5Ctexttt%7BTPTP%7D%20file%20%5C%5C%20(CPL%20problem)%7D%3B%0A%5Cnode%5B%20Rhoumbus%0A%20%20%20%20%20%2C%20right%20of%3Dtptp%0A%20%20%20%20%20%2C%20node%20distance%3D3.8cm%0A%20%20%20%20%20%5D%0A(metis)%20%7B%5Ctexttt%7BMetis%7D%20%5C%5C%20(prover)%20%7D%3B%0A%5Cnode%5B%20below%20of%3D%20metis%0A%20%20%20%20%20%2C%20node%20distance%3D1.6cm%0A%20%20%20%20%20%5D%0A(nothm)%20%7B%7D%3B%0A%5Cnode%5B%20file%0A%20%20%20%20%20%2C%20right%20of%3Dmetis%0A%20%20%20%20%20%2C%20node%20distance%3D4cm%0A%20%20%20%20%20%5D%0A(tstp)%20%7B2.~%5Ctexttt%7BTSTP%7D%20file%5C%5C(derivation)%7D%3B%0A%5Cnode%5B%20program%0A%20%20%20%20%20%2C%20below%20of%3Dtstp%0A%20%20%20%20%20%2C%20node%20distance%3D2.5cm%0A%20%20%20%20%20%5D%0A(athena)%20%7B%5Ctexttt%7BAthena%7D%20tool%5C%5C(translator)%0A%7D%3B%0A%5Cnode%5B%20library%0A%20%20%20%20%20%2C%20right%20of%3Dathena%0A%20%20%20%20%20%2C%20align%3Dcenter%0A%20%20%20%20%20%2C%20node%20distance%3D2.5cm%0A%20%20%20%20%20%5D%0A(agdaprop)%0A%7B%5Ctexttt%7Bagda-metis%7D%5C%5C%0A%5Ctexttt%7Bagda-prop%7D%5C%5C%0A(libraries)%7D%3B%0A%5Cnode%5B%20file%0A%20%20%20%20%20%2C%20left%20of%20%3Dathena%0A%20%20%20%20%20%2C%20node%20distance%3D4cm%0A%20%20%20%20%20%5D%0A(agdaproof)%20%7B3.~%5Ctexttt%7BAgda%7D%20file%5C%5C(proof-term)%7D%3B%0A%5Cnode%5B%20Rhoumbus%0A%20%20%20%20%20%2C%20below%20of%3Dagdaproof%0A%20%20%20%20%20%2C%20node%20distance%3D2cm%0A%20%20%20%20%20%5D%0A(agda)%20%7B%5Ctexttt%7BAgda%7D%5C%5C(type-checker)%7D%3B%0A%5Cnode%5B%20library%0A%20%20%20%20%20%2C%20align%3Dcenter%0A%20%20%20%20%20%2C%20left%20of%3Dagda%0A%20%20%20%20%20%2C%20node%20distance%3D3.8cm%0A%20%20%20%20%20%5D%0A(libraries)%0A%7B%0A%5Ctexttt%7Bagda-metis%7D%5C%5C%0A%5Ctexttt%7Bagda-prop%7D%5C%5C%0A%5Ctexttt%7Bagda-stdlib%7D%5C%5C%0A(imports)%0A%7D%3B%0A%5Cnode%5B%20file%0A%20%20%20%20%20%2C%20below%20of%3D%20agda%0A%20%20%20%20%20%2C%20node%20distance%3D2.1cm%0A%20%20%20%20%20%5D%0A(agdai)%20%7B4.1.%20Interface%5C%5C%20%5Ctexttt%7BAgda%7D%20file%7D%3B%0A%5Cnode%5B%20file%0A%20%20%20%20%20%2C%20right%20of%20%3Dagda%0A%20%20%20%20%20%2C%20node%20distance%3D4.3cm%0A%20%20%20%20%20%5D%0A(invalid)%20%7B4.2.%20Invalid%5C%5C%20%5Ctexttt%7BAgda%7D%20file%7D%3B%0A%5Cpath%20%5Bline%2C%20thick%5D%20(tptp)%20%20%20%20%20%20--%20(metis)%3B%0A%5Cpath%20%5Bline%2C%20thick%5D%20(metis)%20%20%20%20%20--%20node%20%7B%5Ccolor%7Bdarkgreen%7Dtheorem%7D%20(tstp)%3B%0A%5Cdraw%20%5B-o%2C%20%20%20thick%5D%20(metis)%20%20%20%20%20--%20node%20%7B%5Ccolor%7Bdarkred%7Dno%20theorem%7D%20(nothm)%3B%0A%5Cpath%20%5Bline%2C%20thick%5D%20(tstp)%20%20%20%20%20%20--%20(athena)%3B%0A%5Cpath%20%5Bline%2C%20thick%5D%20(athena)%20%20%20%20--%20(agdaproof)%3B%0A%5Cpath%20%5Bline%2C%20thick%5D%20(agdaprop)%20%20--%20(athena)%3B%0A%5Cpath%20%5Bline%2C%20thick%5D%20(agdaproof)%20--%20(agda)%3B%0A%5Cpath%20%5Bline%2C%20thick%5D%20(libraries)%20--%20(agda)%3B%0A%5Cpath%20%5Bline%2C%20thick%5D%20(agda)%20%20%20%20%20%20--%20node%20%7B%5Ccolor%7Bdarkgreen%7Dsuccess%7D%20(agdai)%3B%0A%5Cpath%20%5Bline%2C%20thick%5D%20(agda)%20%20%20%20%20%20--%20node%20%7B%5Ccolor%7Bdarkred%7Dfailure%7D%20(invalid)%3B%0A%5Cend%7Btikzpicture%7D%0A" alt="
\definecolor{darkgreen}{rgb}{0.0, 0.5, 0.0}
\definecolor{darkred}{rgb}{0.5, 0.0, 0.13}
\usetikzlibrary{positioning}
\usetikzlibrary{arrows}
\usetikzlibrary{calc}
\usetikzlibrary{shapes}
\tikzstyle{line} = [draw, -latex']
\begin{tikzpicture}[
   auto
 % , scale=0.15
 , font=\small
 , base/.style =
      { align=center
      }
 , file/.style =
      { base
      , minimum height=3em
      , shape=rectangle
      % , rounded corners
%      , fill=blue!2
      , inner sep=3pt
      , outer sep=1pt
      , draw%=blue!20
      }
  , program/.style =
      { base
      % , fill=gray!1
      % , rounded corners
      , inner sep=3pt
      , outer sep=1pt
      }
  , library/.style =
      { shape=rectangle
      % , rounded corners
     % , fill=blue!2
      , draw%=blue!20
      , inner sep=3pt
      , outer sep=1pt
      }
  , Rhoumbus/.style =
      { base
      , aspect=2
      % , fill=blue!2
      , draw%=blue!20
      , diamond
      , align=center
      , inner xsep=1pt,
      , inner ysep=1.5pt
      , outer sep = 2pt
      }
 ]
\node[file]
(tptp) at (0,0) {1.~\texttt{TPTP} file \\ (CPL problem)};
\node[ Rhoumbus
     , right of=tptp
     , node distance=3.8cm
     ]
(metis) {\texttt{Metis} \\ (prover) };
\node[ below of= metis
     , node distance=1.6cm
     ]
(nothm) {};
\node[ file
     , right of=metis
     , node distance=4cm
     ]
(tstp) {2.~\texttt{TSTP} file\\(derivation)};
\node[ program
     , below of=tstp
     , node distance=2.5cm
     ]
(athena) {\texttt{Athena} tool\\(translator)
};
\node[ library
     , right of=athena
     , align=center
     , node distance=2.5cm
     ]
(agdaprop)
{\texttt{agda-metis}\\
\texttt{agda-prop}\\
(libraries)};
\node[ file
     , left of =athena
     , node distance=4cm
     ]
(agdaproof) {3.~\texttt{Agda} file\\(proof-term)};
\node[ Rhoumbus
     , below of=agdaproof
     , node distance=2cm
     ]
(agda) {\texttt{Agda}\\(type-checker)};
\node[ library
     , align=center
     , left of=agda
     , node distance=3.8cm
     ]
(libraries)
{
\texttt{agda-metis}\\
\texttt{agda-prop}\\
\texttt{agda-stdlib}\\
(imports)
};
\node[ file
     , below of= agda
     , node distance=2.1cm
     ]
(agdai) {4.1. Interface\\ \texttt{Agda} file};
\node[ file
     , right of =agda
     , node distance=4.3cm
     ]
(invalid) {4.2. Invalid\\ \texttt{Agda} file};
\path [line, thick] (tptp)      -- (metis);
\path [line, thick] (metis)     -- node {\color{darkgreen}theorem} (tstp);
\draw [-o,   thick] (metis)     -- node {\color{darkred}no theorem} (nothm);
\path [line, thick] (tstp)      -- (athena);
\path [line, thick] (athena)    -- (agdaproof);
\path [line, thick] (agdaprop)  -- (athena);
\path [line, thick] (agdaproof) -- (agda);
\path [line, thick] (libraries) -- (agda);
\path [line, thick] (agda)      -- node {\color{darkgreen}success} (agdai);
\path [line, thick] (agda)      -- node {\color{darkred}failure} (invalid);
\end{tikzpicture}
" />

**Athena** is a [Haskell][haskell] program that translates
proposition proofs generated by [Metis][Metis] to [Agda][Agda] code.


## Installing the Athena Tool

This tool is written in Haskell and it was tested with

  - [GHC v8.2.1](https://www.haskell.org/ghc/download_ghc_8_2_1.html)

To install Athena, the package manager `cabal` is required as well.
Athena was tested with

  - [cabal 1.24.0](https://hackage.haskell.org/package/cabal-install-1.24.0.0)

Let us download the Athena repository running the following command:

```bash
  $ git clone https://github.com/jonaprieto/athena.git
  $ cd athena
```

To install Athena run the following command:

```bash
  $ make install
```

To install the Agda libraries, `agda-prop`, `agda-metis`, and
the Agda standard library, run the following command:

```bash
  $ make install-libraries
  $ ls lib/
  agda-metis  agda-prop  agda-stdlib
```

## Installing the Metis Prover

Athena supports reconstruction for the prover

-  [Metis v2.3 (release 20171021)](https://github.com/gilith/metis)

As an alternative to install the prover from the Metis sources,
we have provided a Haskell client
to use this prover but also other provers with

- [Online-ATPs v0.1.1][online-atps]

To install this tool run the following command:

```bash
  $ make online-atps
  $ online-atps --version
  Online-atps version 0.1.1
```

## Example

## TPTP problem

Let us consider the following
theorem Problem No. 13 in Disjunction Section
in [Prop-Pack][problems]:

<img align="center" src="https://tex.s2cms.ru/svg/%0A(p%20%5CRightarrow%20q)%20%5Cwedge%20(q%20%5CRightarrow%20p)%5C%20%5Cvdash%5C%20(p%20%5Cvee%20q)%20%5CRightarrow%20(p%20%5Cwedge%20q)%0A" alt="
(p \Rightarrow q) \wedge (q \Rightarrow p)\ \vdash\ (p \vee q) \Rightarrow (p \wedge q)
" />

This problem can be encode in [TPTP][tptp] syntax
(file `problem.tptp`) as follows:

```bash
  $ cat problem.tptp
  fof(premise, axiom, (p => q) & (q => p)).
  fof(goal, conjecture, (p | q) => (p & q)).
```

### Metis derivation

To obtain the Metis derivation of the TPTP problem showed above,
make sure your Metis version is supported by running the following
command. Recall we support the version 2.3 (release 20171021).

```bash
  $ metis --version
  metis 2.3 (release 20171021)
```

To generate the TSTP derivation of `problem.tptp`
run the following command:

```bash
  $ metis --show proof problem.tptp > problem.tstp
  $ cat problem.tstp
  ...
  fof(premise, axiom, ((p => q) & (q => p))).
  fof(goal, conjecture, ((p | q) => (p & q))).
  fof(subgoal_0, plain, ((p | q) => p), inference(strip, [], [goal])).
  fof(subgoal_1, plain, (((p | q) & p) => q), inference(strip, [], [goal])).
  fof(negate_0_0, plain, (~ ((p | q) => p)),
      inference(negate, [], [subgoal_0])).
  ...
```

If we are using the Online-ATPs tool run the following command:

```
  $ online-atps --atp=metis problem.tptp  > problem.tstp
```

Using our customized TSTP syntax, the above Metis derivation looks like:

```
  fof(premise, axiom, (p \supset q) \wedge (q \supset p)).
  fof(goal, conjecture, (p \vee q) \supset (p \wedge q)).
  fof(s₀, (p \vee q) \supset p, inf(strip, goal)).
  fof(s_{1}, ((p \vee q) \wedge p) \supset q, inf(strip, goal)).
  fof(neg₁, ¬ ((p \vee q) \supset p), inf(negate, s₀)).
  fof(n₀₀, (¬ p \vee q) \wedge (¬ q \vee p), inf(canonicalize, premise)).
  fof(n₀₁, ¬ q \vee p, inf(conjunct, n₀₀)).
  fof(n₀₂, ¬ p \wedge (p \vee q), inf(canonicalize, neg₁)).
  fof(n₀₃, p \vee q, inf(conjunct, n₀₂)).
  fof(n₀₄, ¬ p, inf(conjunct, n₀₂)).
  fof(n₀₅, q, inf(simplify,[n₀₃, n₀₄])).
  cnf(r₀₀, ¬ q \vee p, inf(canonicalize, n₀₁)).
  cnf(r₀₁, q, inf(canonicalize, n₀₅)).
  cnf(r₀₂, p, inf(resolve, q, [r₀₁, r₀₀])).
  cnf(r₀₃, ¬ p, inf(canonicalize, n₀₄)).
  cnf(r₀₄, ⊥, inf(resolve, p, [r₀₂, r₀₃])).
  fof(neg₁, ¬ (((p \vee q) \wedge p) \supset q), inf(negate, s_{1})).
  fof(n₁₀, ¬ q \wedge p \wedge (p \vee q), inf(canonicalize, neg₁)).
  fof(n₁₁, (¬ p \vee q) \wedge (¬ q \vee p), inf(canonicalize, premise)).
  fof(n₁₂, ¬ p \vee q, inf(conjunct, n₁₁)).
  fof(n₁₃, ⊥, inf(simplify,[n₁₀, n₁₂])).
  cnf(r₁₀, ⊥, inf(canonicalize, n₁₃)).
```

### Generating the Agda proof-term

To obtain the Agda proof-term of the Metis derivation run
the following command:

```bash
  $ athena problem.tstp
```

The correspondent Agda file will be created in the same directory that contains `problem.tstp` using the same name but the extension of Agda, that is, `.agda`.


```agda
  $ cat problem.agda
  ------------------------------------------------------------------------------
  -- Athena version 0.1-f54e580.
  -- TSTP file: problem.tstp.
  ------------------------------------------------------------------------------

  module problem where

  ------------------------------------------------------------------------------

  open import ATP.Metis 2 public
  open import Data.PropFormula 2 public

  ------------------------------------------------------------------------------

  -- Variables.

  p : PropFormula
  p = Var (# 0)

  q : PropFormula
  q = Var (# 1)

  -- Axiom.

  a₁ : PropFormula
  a₁ = ((p ⊃ q) ∧ (q ⊃ p))

  -- Premise.

  Γ : Ctxt
  Γ = [ a₁ ]

  -- Conjecture.

  goal : PropFormula
  goal = ((p ∨ q) ⊃ (p ∧ q))

  -- Subgoals.

  subgoal₀ : PropFormula
  subgoal₀ = ((p ∨ q) ⊃ p)

  subgoal₁ : PropFormula
  subgoal₁ = (((p ∨ q) ∧ p) ⊃ q)

  ------------------------------------------------------------------------------
  -- Proof of subgoal₀.
  ------------------------------------------------------------------------------

  proof₀ : Γ ⊢ subgoal₀
  proof₀ =
    (RAA
      (resolve-thm ⊥ p
        (resolve-thm p q
          (simplify-thm q
            (conjunct-thm (p ∨ q)
              (canonicalize-thm ((¬ p) ∧ (p ∨ q))
                (assume {Γ = Γ} (¬ subgoal₀))))
            (conjunct-thm (¬ p)
              (canonicalize-thm ((¬ p) ∧ (p ∨ q))
                (assume {Γ = Γ} (¬ subgoal₀)))))
          (conjunct-thm ((¬ q) ∨ p)
            (canonicalize-thm (((¬ p) ∨ q) ∧ ((¬ q) ∨ p))
              (weaken (¬ subgoal₀)
                (assume {Γ = ∅} a₁)))))
        (conjunct-thm (¬ p)
          (canonicalize-thm ((¬ p) ∧ (p ∨ q))
            (assume {Γ = Γ} (¬ subgoal₀))))))

  ------------------------------------------------------------------------------
  -- Proof of subgoal₁.
  ------------------------------------------------------------------------------

  proof₁ : Γ ⊢ subgoal₁
  proof₁ =
    (RAA
      (simplify-thm ⊥
        (canonicalize-thm ((¬ q) ∧ (p ∧ (p ∨ q)))
          (assume {Γ = Γ} (¬ subgoal₁)))
        (conjunct-thm ((¬ p) ∨ q)
          (canonicalize-thm (((¬ p) ∨ q) ∧ ((¬ q) ∨ p))
            (weaken (¬ subgoal₁)
              (assume {Γ = ∅} a₁))))))

  ------------------------------------------------------------------------------
  -- Proof of the goal.
  ------------------------------------------------------------------------------

  proof : Γ ⊢ goal
  proof =
    ⊃-elim
      strip-thm
      (∧-intro proof₀ proof₁)
```

### Type-checking the proof

Now, we are ready to verify the Metis derivation by type-checking
with Agda the reconstructed proof showed above. Make sure the Agda
version is 2.5.3.

```
  $ agda --version
  Agda version 2.5.3
  $ agda problem.agda
```

As we can see in the Agda code showed above, the term `proof`,
the proof-term of the Metis derivation is referring to the proof-
terms `proof₀` and `proof₁`. Recall, Metis stripes the goal
into subgoals to prove it. Therefore, these terms are the proof-terms
for the refutations of the subgoals s_{1} and s₂. We show in the
following sections the respective natural deduction trees for these
refutations.


### Refutation tree for the subgoal s₀

For this subgoal, its respective TSTP derivation is
the following:

```
  fof(premise, axiom, (p ⇒ q) ∧ (q ⇒ p)).
  fof(goal, conjecture, (p ∨ q) ⇒ (p ∧ q))).
  fof(s₀, (p ∨ q) ⇒ p, inf(strip, goal)).
  ...
  fof(neg₀, ¬ ((p ∨ q) ⇒ p), inf(negate, s₀)).
  fof(n₀₀, (¬ p ∨ q) ∧ (¬ q ∨ p), inf(canonicalize, premise)).
  fof(n₀₁, ¬ q ∨ p, inf(conjunct, n₀₀)).
  fof(n₀₂, ¬ p ∧ (p ∨ q), inf(canonicalize, neg₀)).
  fof(n₀₃, p ∨ q, inf(conjunct, n₀₂)).
  fof(n₀₄, ¬ p, inf(conjunct, n₀₂)).
  fof(n₀₅, q, inf(simplify,[n₀₃, n₀₄])).
  cnf(r₀₀, ¬ q ∨ p, inf(canonicalize, n₀₁)).
  cnf(r₀₁, q, inf(canonicalize, n₀₅)).
  cnf(r₀₂, p, inf(resolve, q, [r₀₁, r₀₀])).
  cnf(r₀₃, ¬ p, inf(canonicalize, n₀₄)).
  cnf(r₀₄, ⊥, inf(resolve, p, [r₀₂, r₀₃])).
  ...
```

The refutation tree is the following:

<img align="center" src="https://tex.s2cms.ru/svg/%0A%5Cbegin%7Bprooftree%7D%0A%5CAxiomC%7B%24%5Cmathcal%7BD%7D_1%24%7D%0A%25%5CAxiomC%7B%24%5Cmathcal%7BD%7D_3%24%7D%0A%5CAxiomC%7B%7D%0A%5CRightLabel%7Bassume%20%24%5Cneg%20s_{0}%24%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_{0}%20%5Cvdash%20%5Cneg%20s_{0}%24%7D%0A%25%20%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_{0}%20%5Cvdash%20(p%20%E2%88%A8%20q)%20%5Csupset%20p%24%7D%0A%25%5CLeftLabel%7B%24(%5Cmathcal%7BD%7D_3)%24%5Chspace%7B1.5cm%7D%7D%0A%5CRightLabel%7Bcanonicalize%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_{0}%20%5Cvdash%20%5Cneg%20p%20%E2%88%A7%20(p%20%E2%88%A8%20q)%24%7D%0A%5CRightLabel%7Bconjunct%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_{0}%20%5Cvdash%20%5Cneg%20p%24%7D%0A%5CRightLabel%7Bresolve%20with%20%24%5Cell%20%3D%20p%24%7D%0A%5CLeftLabel%7B%24(%5Cmathcal%7BR%7D_%7B1%7D)%24%5Chspace%7B2mm%7D%7D%0A%5CBinaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_{0}%20%5Cvdash%20%5Cbot%24%7D%0A%5CRightLabel%7BRAA.%7D%0A%5CUnaryInfC%7B%24%5CGamma%20%5Cvdash%20s_{0}%24%7D%0A%5Cend%7Bprooftree%7D%0A" alt="
$$\begin{prooftree}
\AxiomC{$\mathcal{D}_1$}
%\AxiomC{$\mathcal{D}_3$}
\AxiomC{}
\RightLabel{assume $\neg s_{0}$}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg s_{0}$}
% \UnaryInfC{$\Gamma, \neg s_{0} \vdash (p \vee q) \supset p$}
%\LeftLabel{$(\mathcal{D}_3)$\hspace{1.5cm}}
\RightLabel{canonicalize}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg p \wedge (p \vee q)$}
\RightLabel{conjunct}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg p$}
\RightLabel{resolve with $\ell = p$}
\LeftLabel{$(\mathcal{R}_{1})$\hspace{2mm}}
\BinaryInfC{$\Gamma, \neg s_{0} \vdash \bot$}
\RightLabel{RAA.}
\UnaryInfC{$\Gamma \vdash s_{0}$}
\end{prooftree}$$
" />


The refutation tree is the following:

<img src="https://tex.s2cms.ru/svg/%5Cbegin%7Bprooftree%7D%0A%5CAxiomC%7B%24%5Cmathcal%7BD%7D_1%24%7D%0A%25%5CAxiomC%7B%24%5Cmathcal%7BD%7D_3%24%7D%0A%5CAxiomC%7B%7D%0A%5CRightLabel%7Bassume%20%24%5Cneg%20s_%7B0%7D%24%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cneg%20s_%7B0%7D%24%7D%0A%25%20%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20(p%20%5Cvee%20q)%20%5Csupset%20p%24%7D%0A%25%5CLeftLabel%7B%24(%5Cmathcal%7BD%7D_3)%24%5Chspace%7B1.5cm%7D%7D%0A%5CRightLabel%7Bcanonicalize%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cneg%20p%20%5Cwedge%20(p%20%5Cvee%20q)%24%7D%0A%5CRightLabel%7Bconjunct%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cneg%20p%24%7D%0A%5CRightLabel%7Bresolve%20with%20%24%E2%84%93%20%3D%20p%24%7D%0A%5CLeftLabel%7B%24(%5Cmathcal%7BR%7D_%7B1%7D)%24%5Chspace%7B2mm%7D%7D%0A%5CBinaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cbot%24%7D%0A%5CRightLabel%7BRAA.%7D%0A%5CUnaryInfC%7B%24%5CGamma%20%5Cvdash%20s_%7B0%7D%24%7D%0A%5Cend%7Bprooftree%7D" alt="\begin{prooftree}
\AxiomC{$\mathcal{D}_1$}
%\AxiomC{$\mathcal{D}_3$}
\AxiomC{}
\RightLabel{assume $\neg s_{0}$}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg s_{0}$}
% \UnaryInfC{$\Gamma, \neg s_{0} \vdash (p \vee q) \supset p$}
%\LeftLabel{$(\mathcal{D}_3)$\hspace{1.5cm}}
\RightLabel{canonicalize}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg p \wedge (p \vee q)$}
\RightLabel{conjunct}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg p$}
\RightLabel{resolve with $ℓ = p$}
\LeftLabel{$(\mathcal{R}_{1})$\hspace{2mm}}
\BinaryInfC{$\Gamma, \neg s_{0} \vdash \bot$}
\RightLabel{RAA.}
\UnaryInfC{$\Gamma \vdash s_{0}$}
\end{prooftree}" />


<img src="https://tex.s2cms.ru/svg/%0A%5Cbegin%7Bprooftree%7D%0A%5CAxiomC%7B%24%5Cmathcal%7BD%7D_2%24%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cneg%20q%20%5Cvee%20p%24%7D%0A%5CAxiomC%7B%24%5Cmathcal%7BD%7D_3%24%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20p%20%5Cvee%20q%24%7D%0A%25%0A%5CAxiomC%7B%24%5Cmathcal%7BD%7D_4%24%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cneg%20p%24%7D%0A%25%0A%25%5CLeftLabel%7B%24(%5Cmathcal%7BD%7D_4)%24%5Chspace%7B1.5cm%7D%7D%0A%5CRightLabel%7Bsimplify%7D%0A%5CBinaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20q%24%7D%0A%5CLeftLabel%7B%24(%5Cmathcal%7BD%7D_1)%24%5Chspace%7B2mm%7D%7D%0A%5CRightLabel%7Bresolve~with%20%24%E2%84%93%20%3D%20q%24%7D%0A%5CBinaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20p%24%7D%0A%5Cend%7Bprooftree%7D%0A" alt="
\begin{prooftree}
\AxiomC{$\mathcal{D}_2$}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg q \vee p$}
\AxiomC{$\mathcal{D}_3$}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash p \vee q$}
%
\AxiomC{$\mathcal{D}_4$}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg p$}
%
%\LeftLabel{$(\mathcal{D}_4)$\hspace{1.5cm}}
\RightLabel{simplify}
\BinaryInfC{$\Gamma, \neg s_{0} \vdash q$}
\LeftLabel{$(\mathcal{D}_1)$\hspace{2mm}}
\RightLabel{resolve~with $ℓ = q$}
\BinaryInfC{$\Gamma, \neg s_{0} \vdash p$}
\end{prooftree}
" />

<img src="https://tex.s2cms.ru/svg/%0A%5Cbegin%7Bprooftree%7D%0A%20%20%5CAxiomC%7B%7D%0A%20%20%5CRightLabel%7Baxiom%20premise%7D%0A%20%20%5CUnaryInfC%7B%24%5CGamma%20%5Cvdash%20(p%20%5Csupset%20q)%20%5Cwedge%20(q%20%5Csupset%20p)%24%7D%0A%20%20%5CRightLabel%7Bweaken%7D%0A%20%20%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20(p%20%5Csupset%20q)%20%5Cwedge%20(q%20%5Csupset%20p)%24%7D%0A%20%20%5CLeftLabel%7B%24(%5Cmathcal%7BD%7D_2)%24%5Chspace%7B2mm%7D%7D%0A%20%20%5CRightLabel%7Bcanonicalize%7D%0A%20%20%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20(%5Cneg%20p%20%5Cvee%20q)%20%5Cwedge%20(%5Cneg%20q%20%5Cvee%20p)%24%7D%0A%20%20%5CRightLabel%7Bconjunct%7D%0A%20%20%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cneg%20q%20%5Cvee%20p%24%7D%0A%5Cend%7Bprooftree%7D%0A" alt="
\begin{prooftree}
  \AxiomC{}
  \RightLabel{axiom premise}
  \UnaryInfC{$\Gamma \vdash (p \supset q) \wedge (q \supset p)$}
  \RightLabel{weaken}
  \UnaryInfC{$\Gamma, \neg s_{0} \vdash (p \supset q) \wedge (q \supset p)$}
  \LeftLabel{$(\mathcal{D}_2)$\hspace{2mm}}
  \RightLabel{canonicalize}
  \UnaryInfC{$\Gamma, \neg s_{0} \vdash (\neg p \vee q) \wedge (\neg q \vee p)$}
  \RightLabel{conjunct}
  \UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg q \vee p$}
\end{prooftree}
" />

<img src="https://tex.s2cms.ru/svg/%0A%5Cbegin%7Bprooftree%7D%0A%5CAxiomC%7B%7D%0A%5CRightLabel%7Bassume%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cneg%20s_%7B0%7D%24%7D%0A%5CLeftLabel%7B%24(%5Cmathcal%7BD%7D_3)%24%5Chspace%7B2mm%7D%7D%0A%5CRightLabel%7Bcanonicalize%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cneg%20p%20%5Cwedge%20(p%20%5Cvee%20q)%24%7D%0A%5CRightLabel%7Bconjunct%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20p%20%5Cvee%20q%24%7D%0A%5Cend%7Bprooftree%7D%0A" alt="
\begin{prooftree}
\AxiomC{}
\RightLabel{assume}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg s_{0}$}
\LeftLabel{$(\mathcal{D}_3)$\hspace{2mm}}
\RightLabel{canonicalize}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg p \wedge (p \vee q)$}
\RightLabel{conjunct}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash p \vee q$}
\end{prooftree}
" />

<img src="https://tex.s2cms.ru/svg/%0A%5Cbegin%7Bprooftree%7D%0A%5CAxiomC%7B%7D%0A%5CRightLabel%7Bassume%20%24%5Cneg%20s_%7B0%7D%24%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cneg%20s_%7B0%7D%24%7D%0A%25%20%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20(p%20%5Cvee%20q)%20%5Csupset%20p%24%7D%0A%5CLeftLabel%7B%24(%5Cmathcal%7BD%7D_4)%24%5Chspace%7B2mm%7D%7D%0A%5CRightLabel%7Bcanonicalize%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cneg%20p%20%5Cwedge%20(p%20%5Cvee%20q)%24%7D%0A%5CRightLabel%7Bconjunct%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B0%7D%20%5Cvdash%20%5Cneg%20p%24%7D%0A%5Cend%7Bprooftree%7D%0A" alt="
\begin{prooftree}
\AxiomC{}
\RightLabel{assume $\neg s_{0}$}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg s_{0}$}
% \UnaryInfC{$\Gamma, \neg s_{0} \vdash (p \vee q) \supset p$}
\LeftLabel{$(\mathcal{D}_4)$\hspace{2mm}}
\RightLabel{canonicalize}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg p \wedge (p \vee q)$}
\RightLabel{conjunct}
\UnaryInfC{$\Gamma, \neg s_{0} \vdash \neg p$}
\end{prooftree}
" />

### Refutation tree for the subgoal s₁

For this subgoal, its respective TSTP derivation is
the following:

```
  fof(premise, axiom, (p ⇒ q) ∧ (q ⇒ p)).
  ...
  fof(s₁, ((p ∨ q) ∧ p) ⇒ q, inf(strip, goal)).
  ...
  fof(neg₁, ¬ (((p ∨ q) ∧ p) ⇒ q), inf(negate, s₁)).
  fof(n₁₀, ¬ q ∧ p ∧ (p ∨ q), inf(canonicalize, neg₁)).
  fof(n₁₁, (¬ p ∨ q) ∧ (¬ q ∨ p), inf(canonicalize, premise)).
  fof(n₁₂, ¬ p ∨ q, inf(conjunct, n₁₁)).
  fof(n₁₃, ⊥, inf(simplify,[n₁₀, n₁₂])).
  cnf(r₁₀, ⊥, inf(canonicalize, n₁₃)).
```

The refutation tree is the following:

<img src="https://tex.s2cms.ru/svg/%0A%5Cbegin%7Bprooftree%7D%0A%5CAxiomC%7B%7D%0A%5CRightLabel%7Bassume%20(%24%5Cneg%20s_%7B1%7D)%24%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%5Cneg%20s_%7B1%7D%20%5Cvdash%20%5Cneg%20s_%7B1%7D%24%7D%0A%5CRightLabel%7Bcanonicalize%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B1%7D%E2%8A%A2%20%5Cneg%20q%20%5Cwedge%20p%20%5Cwedge%20(p%20%5Cvee%20q)%24%7D%0A%5CAxiomC%7B%7D%0A%5CRightLabel%7Baxiom%20premise%7D%0A%5CUnaryInfC%7B%24%5CGamma%20%5Cvdash%20(p%20%5Csupset%20q)%20%5Cwedge%20(q%20%5Csupset%20p)%24%7D%0A%5CRightLabel%7Bweaken%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B1%7D%20%5Cvdash%20(p%20%5Csupset%20q)%20%5Cwedge%20(q%20%5Csupset%20p)%24%7D%0A%5CRightLabel%7Bcanonicalize%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B1%7D%20%5Cvdash%20(%5Cneg%20p%20%5Cvee%20q)%20%5Cwedge%20(%5Cneg%20q%20%5Cvee%20p)%24%7D%0A%5CRightLabel%7Bconjunct%7D%0A%5CUnaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B1%7D%20%5Cvdash%20%5Cneg%20p%20%5Cvee%20q%24%7D%0A%5CLeftLabel%7B%24(%5Cmathcal%7BR%7D_%7B2%7D)%24%5Chspace%7B1mm%7D%7D%0A%5CRightLabel%7Bsimplify%7D%0A%5CBinaryInfC%7B%24%5CGamma%2C%20%5Cneg%20s_%7B1%7D%20%5Cvdash%20%5Cbot%24%7D%0A%5CRightLabel%7BRAA.%7D%0A%5CUnaryInfC%7B%24%5CGamma%20%5Cvdash%20s_%7B1%7D%24%7D%0A%5Cend%7Bprooftree%7D" alt="
\begin{prooftree}
\AxiomC{}
\RightLabel{assume ($\neg s_{1})$}
\UnaryInfC{$\Gamma,\neg s_{1} \vdash \neg s_{1}$}
\RightLabel{canonicalize}
\UnaryInfC{$\Gamma, \neg s_{1}⊢ \neg q \wedge p \wedge (p \vee q)$}
\AxiomC{}
\RightLabel{axiom premise}
\UnaryInfC{$\Gamma \vdash (p \supset q) \wedge (q \supset p)$}
\RightLabel{weaken}
\UnaryInfC{$\Gamma, \neg s_{1} \vdash (p \supset q) \wedge (q \supset p)$}
\RightLabel{canonicalize}
\UnaryInfC{$\Gamma, \neg s_{1} \vdash (\neg p \vee q) \wedge (\neg q \vee p)$}
\RightLabel{conjunct}
\UnaryInfC{$\Gamma, \neg s_{1} \vdash \neg p \vee q$}
\LeftLabel{$(\mathcal{R}_{2})$\hspace{1mm}}
\RightLabel{simplify}
\BinaryInfC{$\Gamma, \neg s_{1} \vdash \bot$}
\RightLabel{RAA.}
\UnaryInfC{$\Gamma \vdash s_{1}$}
\end{prooftree}" />


### The proof of the goal



<img src="https://tex.s2cms.ru/svg/%5Cbegin%7Bprooftree%7D%0A%5CAxiomC%7B%7D%0A%5CRightLabel%7Bstrip%7D%0A%5CUnaryInfC%7B%24%5CGamma%20%5Cvdash%20(s_%7B0%7D%20%5Cwedge%20s_%7B1%7D)%20%5Csupset%20%5Ctext%7Bgoal%7D%24%7D%0A%5CAxiomC%7B%24%5Cmathcal%7BR%7D_%7B1%7D%24%7D%0A%5CUnaryInfC%7B%24%5CGamma%20%5Cvdash%20s_%7B0%7D%24%7D%0A%5CAxiomC%7B%24%5Cmathcal%7BR%7D_2%24%7D%0A%5CUnaryInfC%7B%24%5CGamma%20%5Cvdash%20s_%7B1%7D%24%7D%0A%5CRightLabel%7B%24%5Cwedge%24-intro%7D%0A%5CBinaryInfC%7B%24%5CGamma%20%5Cvdash%20s_%7B0%7D%20%5Cwedge%20s_%7B1%7D%24%7D%0A%5CRightLabel%7B%24%5Csupset%24-elim%7D%0A%5CBinaryInfC%7B%24%5CGamma%20%5Cvdash%20%5Ctext%7Bgoal%7D%24%7D%0A%5Cend%7Bprooftree%7D" alt="\begin{prooftree}
\AxiomC{}
\RightLabel{strip}
\UnaryInfC{$\Gamma \vdash (s_{0} \wedge s_{1}) \supset \text{goal}$}
\AxiomC{$\mathcal{R}_{1}$}
\UnaryInfC{$\Gamma \vdash s_{0}$}
\AxiomC{$\mathcal{R}_2$}
\UnaryInfC{$\Gamma \vdash s_{1}$}
\RightLabel{$\wedge$-intro}
\BinaryInfC{$\Gamma \vdash s_{0} \wedge s_{1}$}
\RightLabel{$\supset$-elim}
\BinaryInfC{$\Gamma \vdash \text{goal}$}
\end{prooftree}" />

**Remark** : Metis does not print out the above proof where the
explicit use of subgoals is necessary to prove the goal. In our Agda
file we did.


## Testing

We have including a test-suite of at least eighty theorems
from [Prop-Pack TPTP Collection][problems]. To reconstruct all these derivations run the following command:

```
$ make problems
$ make reconstruct
```

To type-check the derivations:

```
$ make check
```

If anything didn't work as you expected, please report it
opening an [issue]. Thanks!


[haskell]: http://www.haskell.org
[issue]: http://github.com/jonaprieto/athena/issues/new
[tptp]:    http://www.cs.miami.edu/~tptp/TPTP/SyntaxBNF.html
[tstp]:    http://www.cs.miami.edu/~tptp/TPTP/QuickGuide/
[Metis]:   http://github.com/gilith/metis
[Agda]:    http://github.com/agda/agda
[agda-prop]: http://github.com/jonaprieto/agda-prop
[agda-metis]: http://github.com/jonaprieto/agda-metis
[agda-stdlib]: http://github.com/agda/agda-stdlib
[problems]: http://github.com/jonaprieto/prop-pack
[online-atps]: http://github.com/jonaprieto/online-atps
