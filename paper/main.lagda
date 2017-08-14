%if style == newcode

\begin{code}

module paper where

\end{code}

%endif

\documentclass[runningheads,a4paper]{llncs}

\setcounter{tocdepth}{3}
\usepackage{graphicx}
\usepackage{array}
\usepackage{bussproofs}
\newenvironment{bprooftree}
  {\leavevmode\hbox\bgroup}
  {\DisplayProof\egroup}

% --------------------------------------------------------------------------
% Tikz Configuration
% --------------------------------------------------------------------------
\usepackage{tikz}
\usetikzlibrary{positioning}
\usetikzlibrary{arrows}
\usetikzlibrary{automata}
\usetikzlibrary{calc}
\usepackage{rotating}

\tikzset{
    hyperlink node/.style={
        alias=sourcenode,
        append after command={
            let     \p1 = (sourcenode.north west),
                \p2=(sourcenode.south east),
                \n1={\x2-\x1},
                \n2={\y1-\y2} in
            node [inner sep=0pt, outer sep=0pt,anchor=north west,at=(\p1)]
            {\hyperlink{#1}{\XeTeXLinkBox{\phantom{\rule{\n1}{\n2}}}}}
                    %xelatex needs \XeTeXLinkBox, won't create a link unless it
                    %finds text --- rules don't work without \XeTeXLinkBox.
                    %Still builds correctly with pdflatex and lualatex
        }
    }
}

\tikzstyle{vertex}=[auto=left,circle,fill=black!25,minimum size=5pt,inner sep=0pt]

%\usepackage[utf8]{inputenc}
\usepackage{ucs}
\usepackage{autofe}
\usepackage{afterpage}
\usepackage{comment}
\usepackage{url}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amsmath}
\usepackage{stmaryrd} %new symbol font for tcs
\usepackage{color}


% \usepackage{fontspec}
% \usepackage{mathtools}
% \usepackage{unicode-math}
% \setmonofont[ExternalLocation=fonts/
% , BoldFont=DejaVuSansMono-Bold.ttf
% , BoldItalicFont=DejaVuSansMono-BoldOblique.ttf
% , ItalicFont=DejaVuSansMono-Oblique.ttf
% ]{DejaVuSansMono.ttf}
% \setmathfont[ExternalLocation=fonts/
%   ]{DejaVuMathTeXGyre.ttf}
% \newfontfamily\mathfont{fonts/DejaVuMathTeXGyre.ttf}
% \usepackage{minted}
% \setminted[code]{
% , fontsize  = \footnotesize
% , frame     = none
% % , framerule = 0.4pt
% % , framesep  = 0pt
% , style     = cagda
% }

\usepackage{verbatim}
\usepackage{relsize}
\usepackage{fancyvrb}
% \DefineShortVerb{\+}
\DefineVerbatimEnvironment
  {code}{Verbatim}
  { fontsize=\relsize{-2}
  , fontfamily=freemodo
  , frame =single
  , framesep=5mm
  , showspaces=true
  , obeytabs=true
  } % {}

\setcounter{secnumdepth}{5}

%include lhs2TeX.fmt

%include agda.fmt

%include lib.fmt

%format . = "."
%format $$ = "\!\!"

\renewcommand*\rmdefault{cmr}

\usepackage{url}
% \urldef{\mailsc}\path|{jprieto9, asr}@eafit.edu.co|
\newcommand{\keywords}[1]{\par\addvspace\baselineskip
\noindent\keywordname\enspace\ignorespaces#1}

\begin{document}

\mainmatter  % start of an individual contribution

% first the title is needed
\title{Towards Proof-Reconstruction of Problems
in Classical Propositional Logic in Agda}

% a short form should be given in case it is too long for the running head
\titlerunning{Proof-Reconstruction in Classical
Propositional Logic}

\author{Jonathan Prieto-Cubides%
\and Andr\'es Sicard-Ram\'irez}
%
\authorrunning{Proof-Reconstruction in Classical Propositional Logic}

\institute{EAFIT University, \emph{School of Sciences and Engineering},\\
Medell\'in, Colombia\\
\verb${jprieto9, asr}@eafit.edu.co$}

% \toctitle{Lecture Notes in Computer Science}
% \tocauthor{Authors' Instructions}
\maketitle


\begin{abstract}
...
\keywords{Proof reconstruction, Agda, Automatic theorem prover,
  Classical Propositional Logic, Metis }
\end{abstract}

\section{Introduction}
Proof reconstruction is a hard labor since it depends on the integration of two
complex system. On one hand, we have the automatic theorem provers
(henceforth ATP) and their specification logic. These tools are usually
classified in at least one of the following categories.
A SAT solver (e.g. \verb!zChaff!~\cite{Moskewicz2001} and \verb!MiniSat!
~\cite{Een2004}) to prove unsatisfiability of CNF formulas, a QBF solver
(e.g. \verb!GhostQ!~\cite{Klieber2014} and \verb!DepQBF!~\cite{Lonsing2017})
to prove satisfiability and invalidity of quantified Boolean formulas, a SMT
solver (e.g \verb!CVC4!~\cite{Barrett2011}, \verb!veriT!~\cite{bouton2009}, and
\verb!Z3!~\cite{DeMoura2008}) to prove unsatisfiability of formulas from
first-order logic with theories, and a prover for validity of formulas from
first-order logic with equality (e.g. \verb!E!~\cite{Schulz:AICOM-2002},
\verb!leanCoP!~\cite{Otten2008}, \verb!Metis!~\cite{hurd2003first}, \verb!SPASS!
~\cite{Weidenbach2009} and \verb!Vampire!~\cite{Riazanov1999}), high-order logic
(e.g. \verb!Leo-II!~\cite{Benzmuller2008} and \verb!Satallax!~\cite{Brown2012})
or intuitionistic logic (e.g. \verb!ileanCoP!~\cite{Otten2008},
\verb!JProver!~\cite{Schmitt2001}, and \verb!Gandalf!~\cite{Tammet1997}),
among others.

On the other hand, we have the proof checkers, interactive theorem provers
(henceforth ITP) or proof assistants (e.g. \verb!Agda!~\cite{agdateam},
\verb!Coq!~\cite{coqteam}, \verb!Isabelle!~\cite{paulson1994isabelle}, and
\verb!HOL4!~\cite{norrish2007hol}). The ITP tools provide us the logic framework
to check and validate the reply of the ATPs, since they allow us to define the
formal language for the problems like operators, logic variables, axioms, and
theorems.

A proof reconstruction tool provides such an integration in one direction, the
bridge between ATPs to ITPs. This is mostly a translation of the reply delivered
by the prover into the formalism of the proof assistant.
Because the formalism of the source (the proof generated by the ATP) is not
necessarily the same logic in the target, the reconstruction turns out in a
``reverse engineering'' task. Then, reconstructing a proof involves a deep
understanding of the algorithms in the ATP and the specification logic in the ITP.

What we need from the ATP tools is a proof object in a consistent format to work
with, that is, a full script describing step-by-step with exhaustive details and
without ambiguities, the table of the derivation to get the actual proof.
For problems in classical propositional logic (henceforth CPL), from a list of
at least forty\footnote{ATPs available from the web service \texttt{SystemOnTPTP}
of the TPTP World.} ATPs, just a few provers were able to deliver proofs (e.g.
\verb!CVC4!~\cite{Barrett2011}, \verb!SPASS!, and \verb!Waldmeister!
~\cite{hillenbrand1997}) and a little bit less replied with a proof in a file
format like TSTP~\cite{sutcliffe2004tstp} (e.g. \verb!E!, \verb!Metis!,
\verb!Vampire!, and \verb!Z3!), LFSC~\cite{Stump2008}, or the SMT-LIB
~\cite{Bohme2011} format.

Many approaches have been proposed and some tools have been implemented for
proof reconstruction in the last decades. These programs are relevant not only
because it helps to spread their usage but they also increase the confidence of
their users about their algorithms and their correctness (see, for example, bugs
in ATPs~\cite{Keller2013}, \cite{Bohme2011}, \cite{Fleury2014} and
\cite{Kanso2012}). We mention some tools in the following.

\subsection*{Related Work.}\label{Related Work}

\verb!Sledgehammer! is a tool for \verb!Isabelle! proof assistant
~\cite{paulson1994isabelle} that provides a full integration of automatic
theorem provers including ATPs (see, for example, \cite{meng2006automation},
\cite{blanchette2013extending} and \cite{Fleury2014}) and SMT solvers (see,
for example, \cite{hurlin07practical}, \cite{bohme2010},
\cite{blanchette2013extending}, and \cite{Fleury2014}) with \verb!Isabelle/HOL!
\cite{nipkow2002isabelle}, the specialization of \verb!Isabelle! for
higher-order logic. Sultana, Benzm{\"{u}}ller, and Paulson~\cite{Een2004}
integrates \verb!Leo-II! and \verb!Satallax!, two theorem provers for high-order
logic with \verb!Isabelle/HOL! proposing a modular proof reconstruction work flow.

\verb!SMTCoq!~\cite{armand2011,Ekici2017} is a tool for the \verb!Coq! proof
assistant \cite{coqteam} which provides a certified checker for proof witness
coming from the SMT solver \verb!veriT! \cite{bouton2009} and adds a new tactic
named verit, that calls \verb!veriT! on any \verb!Coq! goal.
In \cite{bezem2002automated}, given a fixed but arbitrary first-order signature,
the authors transform a proof produced by the first-order automatic theorem
prover \verb!Bliksem! \cite{deNivelle2003} in a \verb!Coq! proof term.

Hurd~\cite{Hurd1999} integrates the first-order resolution prover \verb!Gandalf!
with \verb!HOL!~\cite{norrish2007hol}, a high-order theorem prover, following a
LCF model implementing the tactic \verb!GANDALF_TAC!. The SMT solver \verb!CVC4!
was integrated with \verb!HOL Light!, a version of \verb!HOL! but with a simpler
logic core. \verb!PRocH!~ tool by Kaliszyk and Urban~\cite{kaliszyk2013}
reconstruct proofs from different ATPs to \verb!HOL Light!, replaying the
detailed inference steps from the ATPs with internal inference methods
implemented in the ITP.

% Taken from:
% Färber, M., & Kaliszyk, C. (2015). Metis-based Paramodulation Tactic for HOL
% Light. In GCAI 2015. Global Conference on Artificial Intelligence Metis-based
%(Vol. 36, pp. 127–136).
% HOL(y)Hammer [KU15] is an automated deduction framework for HOL4 and HOL Light.
% Given a conjecture, it attempts to find suitable premises, then calls external ATPs such as E[Sch13], Vampire [KV13], or Z3 [dMB08], and attempts to reconstruct the proof using the premises used by the ATP. To reconstruct proofs, it uses tactics such as MESON, simplification, and a few other decision procedures, however, these are sometimes not powerful enough to reconstruct proofs found by the external ATPs.

\verb!Waldmeister! is an automatic theorem prover for unit equational logic
\cite{hillenbrand1997}. Foster and Struth~\cite{foster2011integrating} integrate
\verb!Waldmeister! into \verb!Agda!~\cite{agdateam}. This integration requires a
proof reconstruction step but authors' approach is restricted to pure equational
logic --also called identity theory~\cite{humberstone2011}-- that is,
first-order logic with equality but no other predicate symbols and no functions
symbols~\cite{appel1959}.

Kanso and Setzer~\cite{kanso2016light} integrate \verb!Z3! in \verb!Agda!,
and for producing propositional justification of a propositional theorems,
they integrate the propositional fragment of the \verb!E! prover in
\cite{Kanso2012}. They catalogue the two integration as
\emph{Oracle + Reflection} and \emph{Oracle + Justification}, respectively.
Their integration with the \verb!E! prover is the most related work
found with our proof reconstruction tool. Beside we shared
some of the same obstacles and some achievements in the proof reconstruction,
we found crucial differences in the treatment of the proofs beyond that we
choose a different prover, we describe all details later on.

In this paper, we describe the integration of \verb!Metis! prover with the proof
assistant \verb!Agda!. We structure the paper as follows. In section \ref{sec2},
we briefly introduce the \verb!Metis! prover. In section \ref{secproofrecon},
we present our approach to reconstruct proofs deliver by \verb!Metis! in
\verb!Agda!. In section 4, we present a complete example of a proof
reconstructed with our tool for a CPL problem. In section \ref{secconclusion},
we discuss some limitations and conclusions, for ending up with the future work.

\section{Metis: Language and Proof Terms}\label{sec2}
\verb!Metis! is an automatic theorem prover  written in Standard ML for
first-order logic with equality developed by John Hurd~\cite{hurd2003first}.
It has been integrated to \verb!Isabelle/HOL! as a macro-step reconstruction
tool justifying proof steps (usually CNF goals) replied from other ATPs like
\verb!CVC4!, \verb!Vampire!, or \verb!Z3!. In \cite{Farber2015}, \verb!Metis!
was also used to provide a tactic for \verb!HOL Light! that could challenge
others tactics like \verb!MESON! or the \verb!leanCoP! tactic to reconstruct
proofs as well~\cite{Farber2016}.

%and is used in our proof reconstruction approach as an external program.
We ported a subset of \verb!Metis!'s inference rules to \verb!Agda!, the
propositional fragmented, to allow us justify step-by-step the proofs delivered
in \verb!TSTP! format.

\subsection{Input and Output Language}
\textit{Input.}~The \verb!TPTP! language  --which includes the first-order
form (FOF) and clause normal form formats~\cite{sutcliffe2009} -- is de facto input standard language to encode problems for many ATPs. The \verb!TPTP! syntax describes
a well-defined grammar to handle annotated formulas with the form
\begin{code}
language(name, role, formula).
\end{code}
We will only consider two languages, FOF or CNF. The \verb!name!
serves to identify the formula within the problem. The formula assume one
role, it could be an axiom, definition, hypothesis, or conjecture.
The problem  $\vdash \neg (p \wedge \neg p) \vee (q \vee \neg q)$ can be
written in \verb!TPTP! as follows.

\begin{code}
fof(goal, conjecture, ~((p & ~ p) | (q & ~ q))).
\end{code}

\textit{Output.}~\verb!TSTP! language is de facto output standard language~\cite{sutcliffe2004tstp}. A TSTP derivation is a directed acyclic graph, a proof tree, where each leaf is a formula from the TPTP input. A node is a formula inferred from the parent formulas. The root is the final derived formula.
Such a derivation is a list of annotated formulas with the form
\begin{code}
language(name, role, formula, source [,useful info]).
\end{code}
where \verb!source! typically is an inference record
\begin{code}
inference(rule, useful info, parents).
\end{code}


\begin{figure}
\begin{code}
fof(a, axiom, p) .
fof(goal, conjecture, p) .
fof(subgoal_0, plain, p, inference (strip, [], [goal])) .
fof(negate_0_0, plain, ∼ p, inference (negate, [], [subgoal_0])) .
fof(normalize_0_0, plain, ∼ p, inference (canonicalize, [], [negate_0_0])) .
fof(normalize_0_1, plain, p, inference (canonicalize, [], [a])) .
fof(normalize_0_2, plain, $false, inference (simplify, [],
  [normalize_0_0, normalize_0_1]))
cnf(refute_0_0, plain, $false, inference (canonicalize, [], [normalize_0_2])) .
\end{code}
\caption{\verb!Metis!'s TSTP derivation for the problem $p\vdash p$.}
\label{metis-proof-tstp}
\end{figure}

\subsection{Proof Terms}

The proof-objects delivered in the \verb!Metis!
s proofs encode natural deduction proofs. Its deduction system
uses six simple inference rules and it proves conjectures by refutation.

\begin{figure}
\[
% \scalebox{0.9}{
\begin{bprooftree}
  \AxiomC{}
  \RightLabel{axiom}
  \UnaryInfC{$C$}
\end{bprooftree}
\qquad
\begin{bprooftree}
  \AxiomC{}
  \RightLabel{assume $L$}
  \UnaryInfC{$L \vee \neg L$}
\end{bprooftree}
\qquad
\begin{bprooftree}
  \AxiomC{}
  \RightLabel{refl $t$}
  \UnaryInfC{$t = t$}
\end{bprooftree}
\qquad
\begin{bprooftree}
  \AxiomC{$C$}
  \RightLabel{subst $\sigma$}
  \UnaryInfC{$\sigma\,C$}
\end{bprooftree}
\qquad
% }
\]
\[
% \scalebox{0.9}{
\begin{bprooftree}
  \AxiomC{}
  \RightLabel{equality $L$ $p$ $t$}
  \UnaryInfC{$\neg (L[p] = t) \vee \neg L \vee L[ p \mapsto t]$}
\end{bprooftree}
\qquad
\begin{bprooftree}
  \AxiomC{$L \vee C$}
  \AxiomC{$\neg L \vee D$}
  \RightLabel{resolve $L$}
  \BinaryInfC{$C \vee D$}
\end{bprooftree}
% }
\]
\caption{Inferences rules of the \verb!Metis! prover.}
\label{metis-inferences}
\end{figure}

These proofs are directed acyclic graphs (henceforth DAG), refutations
trees. Each node stands for an application of an inference rule and the leaves
in the tree represent formulas in the given problem. Each node is labeled with
an axiom or an inference rule name (e.g. \verb!resolve!). Each edge links a
premise with one conclusion. All proof graphs have in their root the conclusion
⊥ since \verb!Metis! uses refutation in each deduction step.

To prove $p \vdash p$, \verb!Metis! generates the following proof-object.

\begin{figure}
\centering
\scalebox{1.1}{
\begin{bprooftree}
\footnotesize\centering\tt
\AxiomC{}
\RightLabel{assume}
\UnaryInfC{$\neg p$}
\RightLabel{strip}
\UnaryInfC{$\neg p$}
\AxiomC{}
\RightLabel{axiom}
\UnaryInfC{$p$}
\RightLabel{canonicalize}
\UnaryInfC{$p$}
\RightLabel{simplify}
\BinaryInfC{$\bot$}
\RightLabel{canonicalize}
\UnaryInfC{$\bot$}
\end{bprooftree}
}
\caption{The natural deduction proof for $p \vdash p$ from the derivation in Fib.~\ref{metis-proof-tstp}}
\label{metis-example}
\end{figure}

\subsection{Proof Rules}
Using \verb!Metis! to prove CPL problems, we found that their TSTP derivations
showed six inference rules, \verb!canonicalize!, \verb!conjunct!, \verb!negate!,
\verb!simplify!, \verb!strip! and \verb!resolve!.

\textit{Splitting}. A list of subgoals is generated by \verb!Metis! to split the proof of the goal into smaller proofs. These subgoals appear in the \verb!TSTP! derivation with the \verb!strip! inference rule.

\begin{code}
fof(goal, conjecture, p & r & q).
fof(subgoal_0, plain, p, inference(strip, [], [goal])).
fof(subgoal_1, plain, p => r, inference(strip, [], [goal])).
fof(subgoal_2, plain, (p & r) => q, inference(strip, [], [goal])).
\end{code}

To split the goal, we apply apply a theorem that by pattern matching
over the input formula, the following theorems are applied.


\textit{Normalization.} The process to normalize a propositional
formula includes transformation of the formula in one of its normal form,
and posteriori, the application of simplification for conjuctions and disjunctions. This normalization is introduced by the \verb!canonicalize!
inference rule.

The normal forms saw in the \verb!TSTP! derivations were conjunctive
normal form (CNF), negative normal form (NNF) and disjunctive normal form.

\[ \scalebox{0.9}{
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi$}
  \UnaryInfC{$\Gamma \vdash \texttt{cnf}~\varphi$}
\end{bprooftree}
\qquad
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi$}
  \UnaryInfC{$\Gamma \vdash \texttt{nnf}~\varphi$}
\end{bprooftree}
\qquad
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi$}
  \UnaryInfC{$\Gamma \vdash \texttt{dnf}~\varphi$}
\end{bprooftree}
}\]

Recursively over the structure of the formula, we perform the
simplification that applies the following theorems assuming commutativity
for disjunctions and conjunctions.

\[
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash P \wedge \bot$}
  \UnaryInfC{$\Gamma \vdash \bot$}
\end{bprooftree}
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash P \wedge \top$}
  \UnaryInfC{$\Gamma \vdash P$}
\end{bprooftree}
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash P \wedge \neg P$}
  \UnaryInfC{$\Gamma \vdash \bot$}
\end{bprooftree}
\]

\[\scalebox{0.9}{
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash P \vee \bot$}
  \UnaryInfC{$\Gamma \vdash P$}
\end{bprooftree}
\qquad
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash P \vee \top$}
  \UnaryInfC{$\Gamma \vdash P$}
\end{bprooftree}
\qquad
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash P \vee \neg P$}
  \UnaryInfC{$\Gamma \vdash \top$}
\end{bprooftree}
}
\]

\textit{Resolution.} The \verb!resolve! inference rule.

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi_1 \vee\cdots\varphi_{i-1}\vee \neg \psi\vee\varphi_{i+1}\vee \cdots\vee \varphi_n $}
\RightLabel{\texttt{resolve} $\psi$}
\UnaryInfC{$\Gamma \vdash \varphi_1 \vee\cdots\vee \varphi_{i-1}\vee\varphi_{i+1}\vee\cdots\vee \varphi_n $}
\end{bprooftree}
}\]

\textit{Clausification.} Application of the distributive laws for both
disjunctions and conjunctions.

\textit{Split a conjunct.} The \verb!conjunct! inference takes a conjunction
and extracts one of its conjuncts. This rules is defined as follows.

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi_1 \wedge \cdots\wedge\varphi_i \wedge\cdots\wedge \varphi_n$}
\AxiomC{$\varphi_i \equiv \psi$}
\RightLabel{\footnotesize\tt conjunct $\psi$}
\BinaryInfC{$\Gamma\vdash\varphi_i$}
\end{bprooftree}
}
\]\par
From the \verb!TSTP! derivation, we infer the formula $\psi$ in the above theorem since it is not given as a argument inside the field of the inference as the following example shows.
\begin{code}
fof(normalize_1, plain, p & q, inference(canonicalize, [], [axiom1])).
fof(normalize_2, plain, p, inference(conjunct, [], [normalize_1])).
\end{code}

\textit{Negate.} Since \verb!Metis!'s proofs are refutations, for each subgoal
given by splitting the goal, in their proof, the negation of such a subgoal is
assumed. This inference rule named \verb!negate! always derived from a node
deduced by the \verb!strip! rule.

\begin{code}
fof(subgoal_0, plain, p, inference(strip, [], [goal])).
fof(negate_0_0, plain, ~ p, inference(negate, [], [subgoal_0])).
\end{code}

\textit{Simplification.} The \verb!simplify! rule could
reduce a list of formulas into an empty clause by transversing the list while
while applying different theorems. These theorems could been the same list of theorems used by \verb!canonicalize! when it simplifies disjunctions and conjunctions, but it also could apply the resolution theorem of the \verb!resolve! rule.

\section{Proof Reconstruction in Agda}
\label{secproofrecon}
As a proof checker, we choose the proof-assistant \verb!Agda!.

Such TPTP proofs produced by ATPs on the type-annotated input are the
starting point for the HOL proof reconstruction.

\subsection{LCF-Style Theorem Proving}

A propositional formula is a string of indivisible propostional atoms,
the logic constants $\{\top, \bot\}$ and logical connectives $\{\wedge, \vee, \Rightarrow, \Leftrightarrow, \neg\}$. In \verb!Agda!, we define the formula
as an inductive type using the keyword \texttt{data} and we include every
connective, the atom, and the logic constants as constructors.
We could restrict formulas to use less connectives but we aim to get
natural deduction proofs with enough expressiveness.

Then, our propositional formulas are represented using the \verb!Prop! data type. %an extension of the syntax defintion used in \cite{Altenkirch2015}.

\begin{code}
data Prop : Set where
  Var              : Fin n → Prop
  ⊤                : Prop
  ⊥                : Prop
  _∧_ _∨_ _⇒_ _⇔_  : (φ ψ : Prop) → Prop
  ¬_               : (φ : Prop)   → Prop
\end{code}

On the other hand, we represent theorems in CPL using an abstract data type to implement a natural deduction calculus.

\begin{code}
data _⊢_ : (Γ : Ctxt)(φ : Prop) → Set where
\end{code}

The \emph{sequents} $\Gamma \vdash \phi$ represent theorems,
where $\Gamma$ is a set of premises and $\phi$ is the
sequent's conclusion. Strictly speaking, the type for the set of premises
$\Gamma$ is \verb!Ctxt!, a type synonymous for a list of formulas \verb!Prop!.
This list's implementation is from the \verb!Agda! standard library.


Then, the theorem data type has the following constructors, the propositional logic deduction rules for our formal system.

\begin{figure}
\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{}
\RightLabel{\footnotesize\tt assume $\varphi$}
\UnaryInfC{$\Gamma , \varphi \vdash \varphi$}
\end{bprooftree}
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi$}
\RightLabel{\footnotesize\tt weaken $\psi$}
\UnaryInfC{$\Gamma , \psi \vdash \varphi$}
\end{bprooftree}
\begin{bprooftree}
\AxiomC{}
\RightLabel{\footnotesize\tt $\top$-intro}
\UnaryInfC{$\Gamma \vdash \top$}
\end{bprooftree}
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \bot$}
\RightLabel{\footnotesize\tt $\bot$-elim $\varphi$}
\UnaryInfC{$\Gamma \vdash \varphi$}
\end{bprooftree}
}\]

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma , \varphi \vdash \bot$}
\RightLabel{\footnotesize\tt $\neg$-intro}
\UnaryInfC{$\Gamma \vdash \neg \varphi$}
\end{bprooftree}
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \neg \varphi$}
\AxiomC{$\Gamma \vdash \varphi$}
\RightLabel{\footnotesize\tt $\neg$-elim}
\BinaryInfC{$\Gamma \vdash \bot$}
\end{bprooftree}
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi$}
\AxiomC{$\Gamma \vdash \psi$}
\RightLabel{\footnotesize\tt $\wedge$-intro}
\BinaryInfC{$\Gamma \vdash \varphi \wedge \psi$}
\end{bprooftree}
}\]

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi$}
\AxiomC{$\Gamma \vdash \psi$}
\RightLabel{\footnotesize\tt $\wedge$-proj$_1$}
\BinaryInfC{$\Gamma\vdash \varphi$}
\end{bprooftree}
\qquad
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi$}
\AxiomC{$\Gamma \vdash \psi$}
\RightLabel{\footnotesize\tt $\wedge$-proj$_2$}
\BinaryInfC{$\Gamma\vdash \psi$}
\end{bprooftree}
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi$}
\RightLabel{\footnotesize\tt $\vee$-intro$_1$ $\psi$}
\UnaryInfC{$\Gamma \vdash \varphi \vee \psi$}
\end{bprooftree}
}\]

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \psi$}
\RightLabel{\footnotesize\tt $\vee$-intro$_2$ $\varphi$}
\UnaryInfC{$\Gamma \varphi \vee \psi$}
\end{bprooftree}
\begin{bprooftree}
\AxiomC{$\Gamma ,\varphi \vdash \gamma $}
\AxiomC{$\Gamma , \psi  \vdash \gamma$}
\RightLabel{\footnotesize\tt $\vee$-elim}
\BinaryInfC{$\Gamma , \varphi \vee \psi \vdash \gamma$}
\end{bprooftree}
\begin{bprooftree}
\AxiomC{$\Gamma , \varphi \vdash \psi$}
\RightLabel{\footnotesize\tt $\Rightarrow$-intro}
\UnaryInfC{$\Gamma \vdash \varphi \Rightarrow \psi$}
\end{bprooftree}
}\]

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi \Rightarrow \psi$}
\AxiomC{$\Gamma \vdash \varphi$}
\RightLabel{\footnotesize\tt $\Rightarrow$-elim}
\BinaryInfC{$\Gamma \vdash \psi$}
\end{bprooftree}
\begin{bprooftree}
\AxiomC{$\Gamma , \varphi \vdash \psi$}
\AxiomC{$\Gamma , \psi \vdash \varphi$}
\RightLabel{\footnotesize\tt $\Leftrightarrow$-intro}
\BinaryInfC{$\Gamma \vdash \varphi \Leftrightarrow \psi$}
\end{bprooftree}
}\]


\[\scalebox{0.9}{
% \qquad
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi$}
\AxiomC{$\Gamma \vdash \varphi \Leftrightarrow \psi$}
\RightLabel{\footnotesize\tt $\Leftrightarrow$-elim$_1$}
\BinaryInfC{$\Gamma \vdash \psi$}
\end{bprooftree}
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi$}
\AxiomC{$\Gamma \vdash \varphi \Leftrightarrow \psi$}
\RightLabel{\footnotesize\tt $\Leftrightarrow$-elim$_2$}
\BinaryInfC{$\Gamma \vdash \psi$}
\end{bprooftree}
}\]
\caption{...}
\label{theorem-constructors}
\end{figure}

\subsection{The Translation Method}

\subsection{Reconstruction Work-flow}
Explain in a diagram like we did in the slides for the AIM ...

\subsection{Emulation of Inference Rules in Agda}

\textit{Splitting a goal}


\begin{figure}
\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi_1 \wedge \varphi_2$}
\UnaryInfC{$\Gamma \vdash \texttt{unshunt}~(\texttt{split}~\varphi_1) \wedge \texttt{unshunt}~(\varphi_1 \Rightarrow \texttt{split}~\varphi_2)$}
\end{bprooftree}
}\]

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi_1 \vee \varphi_2$}
\UnaryInfC{$\Gamma \vdash \texttt{unshunt}~(\neg \varphi_1 \Rightarrow \texttt{split}~\varphi_2)$}
\end{bprooftree}
}\]

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi_1 \Rightarrow \varphi_2$}
\UnaryInfC{$\Gamma \vdash \texttt{unshunt}~(\varphi_1 \Rightarrow \texttt{split}~\varphi_2)$}
\end{bprooftree}\qquad
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \neg \neg \varphi$}
\UnaryInfC{$\Gamma \vdash \texttt{unshunt}~(\texttt{split}~\varphi)$}
\end{bprooftree}
}\]

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$ \Gamma \vdash \varphi_1 \Leftrightarrow \varphi_2$}
\UnaryInfC{$\Gamma\vdash\texttt{unshunt}~(\varphi_1 \Rightarrow \texttt{split}~\varphi_2) \wedge \texttt{unshunt}~(\varphi_2 \Rightarrow \texttt{split}~\varphi_1)$}
\end{bprooftree}
}\]

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \neg \top$}
\UnaryInfC{$\Gamma \vdash \bot$}
\end{bprooftree}\qquad
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \neg \bot$}
\UnaryInfC{$\Gamma \vdash \top$}
\end{bprooftree}\qquad
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \neg (\varphi_1 \wedge \varphi_2)$}
\UnaryInfC{$\Gamma \vdash \texttt{unshunt} (\varphi_1 \Rightarrow \texttt{split}~ \neg \varphi_2)$}
\end{bprooftree}
}\]

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \neg (\varphi_1 \vee \varphi_2)$}
\UnaryInfC{$\Gamma \vdash \texttt{unshunt}~(\texttt{split}~\neg \varphi_1) \wedge  \texttt{unshunt}~(\neg \varphi_1 \Rightarrow \texttt{split}~\neg \varphi_2)$}
\end{bprooftree}
}\]

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \neg (\varphi_1 \Rightarrow \varphi_2)$}
\UnaryInfC{$\Gamma \vdash \texttt{unshunt}~(\texttt{split}~\varphi_1) \wedge \texttt{unshunt}~(\varphi_1 \Rightarrow \texttt{split}~\neg \varphi_2)$}
\end{bprooftree}\qquad
}\]

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \neg (\varphi_1 \Leftrightarrow \varphi_2)$}
\UnaryInfC{$\Gamma \vdash \texttt{unshunt}~(\varphi_1 \Rightarrow \texttt{split}~\neg \varphi_2) \wedge \texttt{unshunt}~(\neg \varphi_2 \Rightarrow \texttt{split}~\varphi_1)$}
\end{bprooftree}
}\]
\caption{...}
\label{split}
\end{figure}

In the theorems above appear two recursive functions \texttt{unshunt} and \texttt{split}. They have the following definitions.

\begin{code}
unshunt : Prop → Prop
unshunt (φ₁ ⇒ (φ₂ ⇒ φ₃)) = unshunt ((φ₁ ∧ φ₂) ⇒ φ₃)
unshunt (φ₁ ⇒ (φ₂ ∧ φ₃)) = unshunt (φ₁ ⇒ φ₂)) ∧ (unshunt(φ₁ ⇒ φ₃)
unshunt other            = φ
\end{code}

\begin{code}
split : Prop → Prop
split (φ₁ ∧ φ₂)     = unshunt (split φ₁) ∧ unshunt (φ₁ ⇒ split φ₂)
split (φ₁ ∨ φ₂)     = unshunt (¬ φ₁ ⇒ (split φ₂))
split (φ₁ ⇒ φ₂)     = unshunt (φ₁ ⇒ (split φ₂))
split (φ₁ ⇔ φ₂)     = unshunt (φ₁ ⇒ (split φ₂)) ∧ unshunt (φ₂ ⇒ (split φ₁))
split (¬ ⊤)         = unshunt (φ₁ ⇒ (split (¬ φ₂)))
split (¬ ⊥)         = unshunt (split (¬ φ₁)) ∧ unshunt (¬ φ₁ ⇒ split (¬ φ₂))
split (¬ (φ₁ ∧ φ₂)) = unshunt (split φ₁) ∧ unshunt (φ₁ ⇒ split (¬ φ₂))
split (¬ (φ₁ ∨ φ₂)) = unshunt (φ₁ ⇒ split (¬ φ₂)) ∧ unshunt ((¬ φ₂) ⇒ split φ₁)
split (¬ (φ₁ ⇒ φ₂)) = unshunt (split φ₁)
split (¬ (φ₁ ⇔ φ₂)) = ⊤
split (¬ (¬ φ₁))    = ⊥
split φ₁            = φ
\end{code}

...
\subsection{Examples}
...


\section{Conclusions}
\label{secconclusion}
\verb!simplify! and \verb!canonicalize! coverage.
Proof-reconstruction can be done in Agda from the Metis' proofs.
...

\subsubsection*{Future Work.}
First-Order Logic support.

\subsubsection*{Acknowledgments.}
We thank EAFIT University of Medell\'in, Colombia for funding support. This is
part of first author's Master thesis in Applied Mathematics, written under the
supervision of Andr\'es Sicard-Ram\'irez for the Logic and Computation Research
Group at the EAFIT University.

We thank Joe Leslie-Hurd for supporting comments about \verb!Metis!.
We also acknowledge the work done by Alejandro G\'omez-Londo\~no~
\cite{Gomez-Londono2015} that deserves as the basis code for our parsing module
for TSTP files.
And last but not least, we gratefully acknowledge Andreas Abel and Chalmers
University of Gothenburg, Sweden for inviting us to be part of the Agda
Implementors’ Meeting XXV where we presented part of this paper.

\bibliographystyle{splncs03}
\bibliography{ref}
\addcontentsline{toc}{section}{References}

\end{document}