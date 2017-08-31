% TODO: align the turnstiles.

%if style == newcode

\begin{code}

module paper where

\end{code}

%endif

\documentclass[runningheads,a4paper]{llncs}

\setcounter{tocdepth}{5}
\usepackage{graphicx}
\usepackage{array}
\usepackage{bussproofs}
\newenvironment{bprooftree}
  {\leavevmode\hbox\bgroup}
  {\DisplayProof\egroup}
\def\extraVskip{3pt}
\def\labelSpacing{3pt}
\def\defaultHypSeparation{\hskip.2in}
\def\EnableBpAbbreviations{true}

% --------------------------------------------------------------------------
% Tikz Configuration
% --------------------------------------------------------------------------

\usepackage{tikz}
\usetikzlibrary{positioning}
\usetikzlibrary{arrows}
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

\tikzstyle{vertex}=[
    auto=left
  , circle
  , fill=black!25
  , minimum size=5pt
  , inner sep=0pt
  ]

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
\usepackage{fancybox}

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
% % , framerule = 0.4pt

\usepackage{verbatim}
\usepackage{relsize}
\usepackage{fancyvrb}
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

\institute{Universidad EAFIT, \emph{School of Sciences and Engineering},\\
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

% ============================================================================

\section{Introduction}
\label{sec:introduction}

Proof reconstruction is a hard labor since it depends on the integration
of two complex system. On one hand, we have the automatic theorem provers
(henceforth ATP) and their specification logic. These tools are usually
classified in at least one of the following categories. A SAT solver
(e.g. \verb!zChaff!~\cite{Moskewicz2001} and \verb!MiniSat!~\cite{Een2004})
to prove unsatisfiability of CNF formulas, a QBF solver (e.g. \verb!
GhostQ!~\cite{Klieber2014} and \verb!DepQBF!~\cite{Lonsing2017}) to prove
satisfiability and invalidity of quantified Boolean formulas, a SMT
solver (e.g \verb!CVC4!~\cite{Barrett2011}, \verb!veriT!~\cite{bouton2009},
and \verb!Z3!~\cite{DeMoura2008}) to prove unsatisfiability of formulas
from first-order logic with theories, and a prover for validity of
formulas from first-order logic with equality
(e.g. \verb!E!~\cite{Schulz:AICOM-2002}, \verb!leanCoP!~\cite{Otten2008},
\verb!Metis!~\cite{hurd2003first}, \verb!SPASS! ~\cite{Weidenbach2009} and
\verb!Vampire!~\cite{Riazanov1999}), high-order logic (e.g. \verb!Leo-II!
\cite{Benzmuller2008} and \verb!Satallax!~\cite{Brown2012}) or intuitonistic
logic (e.g. \verb!ileanCoP!~\cite{Otten2008}, \verb!JProver!
\cite{Schmitt2001}, and \verb!Gandalf!~\cite{Tammet1997}), among others.
On the other hand, we have the proof checkers, interactive theorem provers
(henceforth ITP) or proof assistants (e.g. \verb!Agda!~\cite{agdateam},
\verb!Coq!~\cite{coqteam}, \verb!Isabelle!~\cite{paulson1994isabelle}, and
\verb!HOL4!~\cite{norrish2007hol}).
The ITP tools provide us the logic framework to check and validate the
reply of the ATPs, since they allow us to define the formal language for
the problems like operators, logic variables, axioms, and theorems.

A proof reconstruction tool provides such an integration in one
direction, the bridge between ATPs to ITPs. This is mostly a translation
of the reply delivered by the prover into the formalism of the proof
assistant. Because the formalism of the source (the proof generated by
the ATP) is not necessarily the same logic in the target, the
reconstruction turns out in a ``reverse engineering'' task. Then,
reconstructing a proof involves a deep understanding of the algorithms in
the ATP and the specification logic in the ITP.

What we need from the ATP tools is a proof object in a consistent format
to work with, that is, a full script describing step-by-step with
exhaustive details and without ambiguities, the table of the derivation
to get the actual proof. For problems in classical propositional logic (
henceforth CPL), from a list of at least forty\footnote{ATPs available
from the web service \texttt{SystemOnTPTP} of the TPTP World.} ATPs, just
a few provers were able to deliver proofs (e.g. \verb!CVC4!~
\cite{Barrett2011}, \verb!SPASS!, and \verb!Waldmeister!~\cite{hillenbrand1997})
and a little bit less replied with a proof in a
file format like TSTP~\cite{sutcliffe2004tstp} (e.g. \verb!E!, \verb!Metis!,
\verb!Vampire!, and \verb!Z3!), LFSC~\cite{Stump2008}, or the
SMT-LIB~\cite{Bohme2011} format.

Many approaches have been proposed and some tools have been implemented
for proof reconstruction in the last decades. These programs are relevant
not only because it helps to spread their usage but they also increase
the confidence of their users about their algorithms and their
correctness (see, for example, bugs in ATPs~\cite{Keller2013},
\cite{Bohme2011}, \cite{Fleury2014} and \cite{Kanso2012}). We mention
some tools in the following.

\subsection*{Related Work.}
\label{ssec:related-work}

\verb!Sledgehammer! is a tool for \verb!Isabelle! proof assistant
~\cite{paulson1994isabelle} that provides a full integration of automatic
theorem provers including ATPs
(see, for example, \cite{meng2006automation}, \cite{blanchette2013extending}
and \cite{Fleury2014}) and SMT solvers (see, for example,
\cite{hurlin07practical}, \cite{bohme2010},
\cite{blanchette2013extending}, and \cite{Fleury2014}) with
\verb!Isabelle/HOL! \cite{nipkow2002isabelle}, the specialization of
\verb!Isabelle! for higher-order logic. Sultana, Benzm{\"{u}}ller, and
Paulson~\cite{Een2004} integrates \verb!Leo-II! and \verb!Satallax!, two
theorem provers for high-order logic with \verb!Isabelle/HOL! proposing a
modular proof reconstruction work flow.

\verb!SMTCoq!~\cite{armand2011,Ekici2017} is a tool for the \verb!Coq!
proof assistant \cite{coqteam} which provides a certified checker for
proof witness coming from the SMT solver \verb!veriT! \cite{bouton2009}
and adds a new tactic named verit, that calls \verb!veriT! on any
\verb!Coq! goal. In \cite{bezem2002automated}, given a fixed but arbitrary
first-order signature, the authors transform a proof produced by the
first-order automatic theorem prover \verb!Bliksem! \cite{deNivelle2003}
in a \verb!Coq! proof term.

Hurd~\cite{Hurd1999} integrates the first-order resolution prover
\verb!Gandalf! with \verb!HOL!~\cite{norrish2007hol}, a high-order theorem
prover, following a LCF model implementing the tactic \verb!GANDALF_TAC!.
For \verb!HOL Light!, a version of \verb!HOL! but with a simpler logic core,
the SMT solver \verb!CVC4! was integrated and Kaliszyk and
Urban~\cite{kaliszyk2013} reconstruct proofs from different ATPs with their
\verb!PRocH! tool, replaying the detailed inference steps from the ATPs with
internal inference methods implemented in \verb!HOL Light!.

% Taken from: % Färber, M., & Kaliszyk, C. (2015). Metis-based
% Paramodulation Tactic for HOL

% Light. In GCAI 2015. Global Conference on Artificial Intelligence
% Metis-based

%(Vol. 36, pp. 127–136).

% HOL(y)Hammer [KU15] is an automated deduction framework for HOL4 and
% HOL Light.

% Given a conjecture, it attempts to find suitable premises, then calls
% external ATPs such as E[Sch13], Vampire [KV13], or Z3 [dMB08], and
% attempts to reconstruct the proof using the premises used by the ATP.
% To reconstruct proofs, it uses tactics such as MESON, simplification,
% and a
% few other decision procedures, however, these are sometimes not p
% owerful
% enough to reconstruct proofs found by the external ATPs.

\verb!Waldmeister! is an automatic theorem prover for unit equational
logic \cite{hillenbrand1997}.
Foster and Struth~\cite{foster2011integrating} integrate \verb!Waldmeister! into
\verb!Agda!~\cite{agdateam}. This integration requires a proof
reconstruction step but authors' approach is restricted to pure
equational logic --also called identity theory~\cite{humberstone2011}--
that is, first-order logic with equality but no other predicate symbols
and no functions symbols~\cite{appel1959}.

Kanso and Setzer~\cite{kanso2016light} integrate \verb!Z3! in \verb!Agda!
but they also integrate the propositional fragment of the \verb!E! prover in
\cite{Kanso2012}. They cataloged these two integration as
\emph{Oracle and Reflection} and \emph{Oracle and Justification}, respectively.
Their integration with the \verb!E! prover is the most related work with our
apporach found at the moment while we write this document. Besides the use
of the same ITP, crucial differences easily arise like decidability of
propositional logic using semantics that we do not take into account for the
reason we describe later on in section~\ref{sec:conclusions}.

In this paper, we describe the integration of \verb!Metis! prover with the
proof assistant \verb!Agda!. We structure the paper as follows. In section
\ref{sec:metis-language-and-proof-terms} we briefly introduce the
\verb!Metis! prover. In section
\ref{sec:proof-reconstruction}, we present our approach to reconstruct
proofs deliver by \verb!Metis! in \verb!Agda!. In section \ref{sec:example},
we present a complete example of a proof reconstructed with our tool for a CPL
problem. In section~\ref{sec:conclusions}, we discuss some limitations,
advantages and disadvantages with other similar proof reconstruction tools and
conclusions for ending up with the future work.

% ===========================================================================

\section{Metis: Language and Proof Terms}
\label{sec:metis-language-and-proof-terms}

\verb!Metis! is an automatic theorem prover written in Standard ML for
first-order logic with equality developed by John Hurd~\cite{hurd2003first}.
As we show later this prover is suitable for proof reconstruction task since
it provides well-documented proof-objects with enough information to justify
the deduction steps.

\verb!Metis! has been integrated to \verb!Isabelle/HOL! as a macro-step reconstruction
tool for justifying proof steps (usually CNF goals) replied from other ATPs like
\verb!CVC4!, \verb!Vampire!, or \verb!Z3!.
In \cite{Farber2015}, \verb!Metis! was used to provide a tactic for
\verb!HOL Light!, this tactic challenges others like \verb!MESON! tactic or the
\verb!leanCoP! tactic that reconstructs proofs delivered by
\verb!leanCoP!~\cite{Farber2016}.

In this paper, we ported a subset of \verb!Metis!' inference rules to \verb!Agda!,
the propositional fragmented, to allow us justify step-by-step the proofs
delivered in \verb!TSTP! format generated by \verb!Metis!.

% ---------------------------------------------------------------------------

\subsection{Input and Output Language}
\label{ssec:input-and-output-language}

\textit{Input.}~The \verb!TPTP! language  --which includes the first-order
form (denoted by \verb!fof!) and clause normal form (denoted by \verb!cnf!)
formats~\cite{sutcliffe2009} -- is de
facto input standard language to encode problems for many ATPs.
The \verb!TPTP! syntax describes a well-defined grammar to handle annotated
formulas with the following form

\begin{code}
language(name, role, formula).
\end{code}

The \verb!language! can be \verb!fof! or \verb!cnf!. The \verb!name!
serves to identify the formula within the problem. Each formula assumes one
\verb!role!, this could be an \verb!axiom!, \verb!conjecture!,
\verb!definition!, or a \verb!hypothesis!. The formulas include Boolean
connectives (\verb!&!, \verb!|!, \verb!=>!, \verb!<=>!, $\tt\sim$) and logic
constants \verb!$true! and \verb!$false!.

For instance, we express the problem
$p\, \vdash \neg (p \wedge \neg p) \vee (q \vee \neg q)$
in \verb!TPTP! syntax as follows.

\begin{code}
fof(h, axiom, p).
fof(goal, conjecture, ~ ((p & ~ p) | (q & ~ q))).
\end{code}

\textit{Output.}~\verb!TSTP! language is de facto output standard language
for derivations of ATPs~\cite{sutcliffe2004tstp}.
A TSTP derivation is a directed acyclic graph, a proof tree, where each leaf
is a formula from the TPTP input. A node is a formula inferred from the parent
formulas. The root is the final derived formula. Such a derivation is a list of
annotated formulas with the form

\begin{code}
language(name, role, formula, source [,useful info]).
\end{code}

The \verb!source! field is an inference record with the following pattern
\begin{code}
inference(rule, useful info, parents).
\end{code}

The \verb!rule! stands for the inference name, the other fields are
auxiliary arguments or useful information to apply the inference, and
the list of parents nodes.
For instance, in Fig.~\ref{fig:metis-proof-tstp}, \verb!strip! is the name of one
of the inference and it has no arguments and only one parent node, \verb!goal!.

\begin{figure}
\begin{code}
fof(a, axiom, p) .
fof(goal, conjecture, p) .
fof(subgoal_0, plain, p, inference (strip, [], [goal])) .
fof(negate_0_0, plain, ~ p, inference (negate, [], [subgoal_0])) .
fof(normalize_0_0, plain, ∼ p, inference (canonicalize, [], [negate_0_0])) .
fof(normalize_0_1, plain, p, inference (canonicalize, [], [a])) .
fof(normalize_0_2, plain, $false, inference (simplify, [],
  [normalize_0_0, normalize_0_1]))
cnf(refute_0_0, plain, $false, inference (canonicalize, [], [normalize_0_2])) .
\end{code}
\caption{A simple \verb!Metis!' TSTP derivation for the problem $p\vdash p$.}
\label{fig:metis-proof-tstp}
\end{figure}

% ---------------------------------------------------------------------------

\subsection{Proof Terms}
\label{ssec:proof-terms}

A proof-object delivered in a \verb!Metis!' proof encodes a natural
deduction proof. Its deduction system uses six simple inference rules
(see Fig.~\ref{fig:metis-inferences}) to attempt to prove conjectures
by refutation (see. e.g. Fig.~\ref{fig:metis-example}).

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
\caption{Inference rules of the \verb!Metis! prover.}
\label{fig:metis-inferences}
\end{figure}

These proofs are directed acyclic graphs, trees of refutations. Each node stands
for an application of an inference rule and the
leaves in the tree represent formulas in the given problem. Each node is
labeled with a name of the inference rule
(e.g. \verb!canonicalize! in Fig.~\ref{fig:metis-example}). Each
edge links a premise with one conclusion. The proof graphs have in their
root the conclusion $\bot$ since \verb!Metis! delivers proof by refutation.

\begin{figure}
\centering
\begin{bprooftree}
  \footnotesize\centering\tt
  \AxiomC{}
  \RightLabel{\footnotesize negate}
  \UnaryInfC{$\neg p$}
  \RightLabel{\footnotesize strip}
  \UnaryInfC{$\neg p$}
  \AxiomC{}
  \RightLabel{\footnotesize axiom}
  \UnaryInfC{$p$}
  \RightLabel{\footnotesize canonicalize}
  \UnaryInfC{$p$}
  \RightLabel{\footnotesize simplify}
  \BinaryInfC{$\bot$}
  \RightLabel{\footnotesize canonicalize}
  \UnaryInfC{$\bot$}
\end{bprooftree}
\caption{The \verb!Metis!' refutation tree for $p \vdash p$ from the
derivation in Fig.~\ref{fig:metis-proof-tstp}}
\label{fig:metis-example}
\end{figure}

% ---------------------------------------------------------------------------

\subsection{Proof Rules}
\label{ssec:proof-rules}

Despite the six simple rules in the \verb!Metis!' logic
kernel (see~Fig. \ref{fig:metis-inferences}), we found in \verb!TSTP!
derivations for CPL problems other inference rules.
They are \verb!canonicalize!, \verb!conjunct!, \verb!negate!, \verb!simplify!,
\verb!strip! and \verb!resolve!. We briefly review each of these rules
following no order except maybe by their frequency in the TSTP derivations.

% ...........................................................................

\textit{Splitting}.
To prove a goal, \verb!Metis! splits the goal into disjoint cases. This
process produces a list of new subgoals, the conjunction of these subgoals
implies the goal as we show later in subsection~\ref{ssec:emulating-inferences}.
Then, a proof of the goal becomes in smaller proofs, one
refutation for each subgoal. These subgoals are introduced in the \verb!TSTP!
derivation with the \verb!strip! inference rule.

\begin{code}
fof(goal, conjecture, p & r & q).
fof(subgoal_0, plain, p, inference(strip, [], [goal])).
fof(subgoal_1, plain, p => r, inference(strip, [], [goal])).
fof(subgoal_2, plain, (p & r) => q, inference(strip, [], [goal])).
\end{code}

% ...........................................................................

\textit{Clausification.} The \verb!clausify! rule transforms a
propositional formula in clausal normal form or CNF,
a conjunction of clauses. Where a clause is the disjunction of zero or
more literals and a literal is an atom (positive literal) or a negation of an
atom (negative literal). This conversion is not unique and \verb!Metis! performs
this transformation using its own criteria.

% ...........................................................................

\textit{Normalization.}
\verb!Metis! uses the \verb!canonicalize! rule to normalize a
propositional formula that comes from an axiom or a definition.
It often converts the formula in one of its normal form,
the conjunctive normal form,%as \verb!clausify! does
the negative normal form or the disjunctive normal form.
Posterior, this rule simplifies the formula with some definitions, applying
recursively on the structure's formula conjunctions and disjunctions simplifications
to remove tautologies
(see Fig.~\ref{fig:conjunctive-disjunctive-simplification} with a list
of these theorems. We had assumed the commutative property for conjunction and
disjunction connectives).

\begin{figure}
\[%\scalebox{0.9}{
  \begin{bprooftree}
    \AxiomC{$\varphi \wedge \bot$}
    \UnaryInfC{$\bot$}
  \end{bprooftree}
  \qquad
  \begin{bprooftree}
    \AxiomC{$\varphi \wedge \top$}
    \UnaryInfC{$\varphi$}
  \end{bprooftree}
  \qquad
  \begin{bprooftree}
    \AxiomC{$\varphi \wedge \neg \varphi$}
    \UnaryInfC{$\bot$}
  \end{bprooftree}
  \qquad
  \begin{bprooftree}
    \AxiomC{$\varphi \wedge \varphi$}
    \UnaryInfC{$\varphi$}
  \end{bprooftree}
%}
\]

\[%\scalebox{0.9}{
  \begin{bprooftree}
    \AxiomC{$\varphi \vee \bot$}
    \UnaryInfC{$\varphi$}
  \end{bprooftree}
  \qquad
  \begin{bprooftree}
    \AxiomC{$\varphi \vee \top$}
    \UnaryInfC{$\top$}
  \end{bprooftree}
  \qquad
  \begin{bprooftree}
    \AxiomC{$\varphi \vee \neg \varphi$}
    \UnaryInfC{$\top$}
  \end{bprooftree}
  \qquad
  \begin{bprooftree}
    \AxiomC{$\varphi \vee \varphi$}
    \UnaryInfC{$\varphi$}
  \end{bprooftree}
%}
\]
\caption{Some rules of \verb!canonicalize! inference.}
\label{fig:conjunctive-disjunctive-simplification}
\end{figure}

% ...........................................................................

\textit{Resolution.} The \verb!resolve! rule (see, for example,
Fig.~\ref{fig:resolve-inference}) is the resolution
theorem with two argument, the positive literal for resolution and two
formulas from their own derivations. The positive literal $p$ must occur in the first formula of the
input and the negative literal must occur in the second formula of the input
(see the resolve rule in \verb!Metis!' logic core in
Fig.~\ref{fig:metis-inferences}).

\begin{figure}
\begin{code}
cnf(refute_0_0, plain, p | q, inference(canonicalize, [], [normalize_0_0])).
cnf(refute_0_1, plain, ~ p, inference(canonicalize, [], [normalize_0_4])).
cnf(refute_0_2, plain, q,
  inference(resolve, [$cnf(p)], [refute_0_0, refute_0_1])).
\end{code}

\caption{Excerpt of an example of \verb!resolve! rule in
  a \verb!TSTP! derivation.}
\label{fig:resolve-inference}
\end{figure}

% ...........................................................................

\textit{Splitting a conjunct.}
The \verb!conjunct! rule extracts from a conjunction one of its conjuncts, it is
a generalization of the projection rules for the conjunction connective as
we can see in the following TSTP derivation.

\begin{code}
fof(normalize_1, plain, p & q & r & s, inference(canonicalize, [], [axiom])).
fof(normalize_2, plain, r, inference(conjunct, [], [normalize_1])).
\end{code}

\textit{Negate.}
Each subgoal proof is a refutation, thereby each proof assumes
the negation of its subgoal. The \verb!negate! rule
introduces the negation of a subgoal that results
after applies the \verb!strip! inference to the goal.

\begin{code}
fof(subgoal_0, plain, p, inference(strip, [], [goal])).
fof(negate_0_0, plain, ~ p, inference(negate, [], [subgoal_0])).
\end{code}

% ...........................................................................

\textit{Simplification.}
The \verb!simplify! rule is an $n$-ary rule that performs simplification
of definitions. This rule transverses a list of formulas by
applying different theorems (e.g.\emph{modus pones}, \emph{modus tollens},
or \emph{disjunctive syllogism}) including the list of theorems used by
\verb!canonicalize! in Fig.~\ref{fig:conjunctive-disjunctive-simplification}
and the resolution theorem of \verb!resolve! rule.
Moreover, many things happen inside this \verb!Metis!' procedure and
it is fairly complex to grasp it completely.

% ============================================================================

\section{Proof Reconstruction}
\label{sec:proof-reconstruction}

% In our proof reconstruction approach (see the overview diagram
% Fig.~\ref{fig:proof-reconstruction-overview}), we have three main tools present.


%   \verb!Haskell! program to generate the proof
% in \verb!Agda! code after parsing and analyzing the TSTP derivation delivered
% by \verb!Metis!. Then, we use \verb!Agda! to check the proof previously
% generated, based on the formalization of the classical propositional logic
% and the \verb!Metis!' inference rules described before in section
% \ref{sec:metis-language-and-proof-terms}.

\begin{figure}
\centering
\begin{tikzpicture}
\node[text width=2cm, align=center](problem) at (0,0)
  {CPL \\ Problem};

\node[right = 1.2cm of problem, text width=2cm, align=center]
  (tptp){\verb!TPTP! \\ Problem};

\node[right= 1.2cm of tptp, text width=2cm, align=center]
   (metis) {\verb!Metis! \\ Prover};

\node[right= 1.2cm of metis, text width=2cm, align=center, inner sep=10pt]
   (tstp) {\verb!TSTP! \\ Derivation};

\node[below= 0.5cm of tstp, text width=2cm, align=center, inner sep=10pt]
  (athena) {\verb!Haskell! \\ Traslator};

\node[left = 1.2cm of athena, text width=2cm, align=center]
   (agdafile) {\verb!Agda! \\ Proof};

\node[left = 1.2cm of agdafile, text width=2cm, align=center]
   (agda) {Proof Checker};

\node[below = 0.5cm of problem, text width=2cm, align=center]
   (verified) {Checked};

\node[below = 0.5cm of verified, text width=2cm, align=center]
   (failure) {Error};

% node[below] {send to}
\draw[->, thick] (problem) to
  % node[below] {\tiny encoding}
  (tptp);
\draw[->, thick] (tptp) to
  % node[below] {\tiny }
  (metis);
\draw[->, thick] (metis) to
  % node[below] {\tiny replies on}
  (tstp);
\draw[->, thick] (tstp) to
  % node[right] {\tiny parsing}
  (athena);
\draw[->, thick] (athena) to
  % node[below] {\tiny traslation}
  (agdafile);
\draw[->, thick] (agdafile) to
  % node[below] {\tiny type-checking}
  (agda);
\draw[->, thick] (agda) to (verified);
\draw[->, thick, gray] (agda) to (failure);
\end{tikzpicture}
\caption{Proof reconstruction overview.}
\label{fig:proof-reconstruction-overview}
\end{figure}

% ---------------------------------------------------------------------------

\subsection{Proof Cheker}
\label{ssec:agda}\

\verb!Agda! is an interactive system for constructing proofs and programs,
based on Martin-L\"{o}f's type theory and extended with records, parametrised
modules, among other features.
One of the main strengths of \verb!Agda! is its support for writing proofs,
which we shall call \verb!Agda!'s proof engine and it consists of: support for
inductively defined types, including inductive families, and function
definitions using pattern matching on such types, normalisation during type-
checking, commands for refining proof terms, coverage checker and termination
checker.
The inductive approach for representing classical propositional logic
is better because we benefit from \verb!Agda!'s proof engine and its Unicode
support that allows us writing proofs similar as we find in math text books.

% ---------------------------------------------------------------------------

\subsection{LCF-Style Theorem Proving}
\label{ssec:lcf-style-theormem-proving}

A propositional formula is a string of indivisible propositional atoms,
logical connectives $\{\wedge, \vee, \Rightarrow, \Leftrightarrow, \neg\}$
and logic constants $\{\top, \bot\}$.
In \verb!Agda!, we define the formula
as an inductive type using the keyword \texttt{data} and including every
connective as a constructor. We represent propositional formulas with
\verb!Prop! data type using the following definition.

%an extension of the syntax definition used in \cite{Altenkirch2015}.

\begin{code}
data Prop : Set where
  Var              : Fin n → Prop
  ⊤                : Prop
  ⊥                : Prop
  _∧_ _∨_ _⇒_ _⇔_  : (φ ψ : Prop) → Prop
  ¬_               : (φ : Prop)   → Prop
\end{code}

On the other hand, we represent theorems in CPL using an abstract data type
to implement a natural deduction calculus.

\begin{code}
data _⊢_ : (Γ : Ctxt)(φ : Prop) → Set where
\end{code}

The sequents  $\Gamma \vdash \phi$ represent theorems,
where $\Gamma$ is a set of premises and $\phi$ is the
sequent's conclusion. Strictly speaking, we define the set of premises
$\Gamma$ with \verb!List! data type
\footnote{We use the \verb!Agda! standard library's implementation
of the \verb!List! data type.}.

Our CPL system define all inference rules for intuitonistic propositional logic
(see Fig.~\ref{fig:theorem-constructors}) as constructors of the theorem
data type, and we postulate the principle of the excluded middle (PEM).

\begin{code}
postulate  PEM : ∀ {Γ} {φ} → Γ ⊢ φ ∨ ¬ φ
\end{code}

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
\AxiomC{$\Gamma \vdash \varphi \wedge \psi$}
\RightLabel{\footnotesize\tt $\wedge$-proj$_1$}
\UnaryInfC{$\Gamma\vdash \varphi$}
\end{bprooftree}
\qquad
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi \wedge \psi$}
\RightLabel{\footnotesize\tt $\wedge$-proj$_2$}
\UnaryInfC{$\Gamma\vdash \psi$}
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
\UnaryInfC{$\Gamma \vdash \varphi \vee \psi$}
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

\[
%\scalebox{0.9}{
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
%}
\]


\[
%\scalebox{0.9}{
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
%}
\]
\caption{Inference rules for CPL system.}
\label{fig:theorem-constructors}
\end{figure}

% ---------------------------------------------------------------------------

\subsection{The Translation Method}

% ---------------------------------------------------------------------------

\subsection{Reconstruction Work-flow}
Explain in a diagram like we did in the slides for the AIM ...

% ---------------------------------------------------------------------------

\subsection{Emulation of Inference Rules in Agda}
\label{ssec:emulating-inferences}


\textit{Normalization.}

\begin{figure}
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
\end{figure}

\begin{figure}
\[%\scalebox{0.9}{
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi \wedge \bot$}
  \UnaryInfC{$\Gamma \vdash \bot$}
\end{bprooftree}
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi \wedge \top$}
  \UnaryInfC{$\Gamma \vdash \varphi$}
\end{bprooftree}
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi \wedge \neg \varphi$}
  \UnaryInfC{$\Gamma \vdash \bot$}
\end{bprooftree}
%}
\]

\[%\scalebox{0.9}{
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi \vee \bot$}
  \UnaryInfC{$\Gamma \vdash \varphi$}
\end{bprooftree}
\qquad
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi \vee \top$}
  \UnaryInfC{$\Gamma \vdash \top$}
\end{bprooftree}
\qquad
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi \vee \neg \varphi$}
  \UnaryInfC{$\Gamma \vdash \top$}
\end{bprooftree}
%}
\]
\end{figure}

\textit{Resolution.}

\begin{figure}
\[%\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\delta_{\phi} := \Gamma \vdash \varphi$}
\AxiomC{$\delta_{\psi} := \Gamma \vdash \psi$}
\RightLabel{\texttt{resolve} $p$}
\BinaryInfC{$\Gamma \vdash$ \texttt{resolve} $\phi\ \psi\ p$}
\end{bprooftree}
%}
\]
\caption{Resolution in \verb!Metis!.}
\label{fig:resolution-rule}
\end{figure}

\textit{Splitting a conjunct.}
In the following theorem, we see the definition for the \verb!conjunct! rule.
It extracts the $\varphi_i$ conjunct from the conjunction given by $\varphi$.

\[\scalebox{0.9}{
\begin{bprooftree}
\AxiomC{$\Gamma \vdash \varphi := \varphi_1 \wedge \cdots\wedge\varphi_i
\wedge\cdots\wedge \varphi_n$}
\AxiomC{$\varphi_i \equiv \psi$}
\RightLabel{\footnotesize\tt conjunct $\psi$}
\BinaryInfC{$\Gamma\vdash\varphi_i$}
\end{bprooftree}
}
\]


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
\caption{Split a goal in an equivalent conjunction of subgoals.}
\label{split}
\end{figure}

In the theorems above appear two recursive functions \texttt{unshunt} and \texttt{split}. They have the following definitions.

\begin{code}
unshunt : Prop → Prop
unshunt (φ₁ ⇒ (φ₂ ⇒ φ₃)) = unshunt ((φ₁ ∧ φ₂) ⇒ φ₃)
unshunt (φ₁ ⇒ (φ₂ ∧ φ₃)) = unshunt (φ₁ ⇒ φ₂)) ∧ (unshunt(φ₁ ⇒ φ₃)
unshunt φ                = φ
\end{code}

\begin{code}
split : Prop → Prop
split (φ₁ ∧ φ₂)     = {!!}
split (φ₁ ∨ φ₂)     = {!!}
split (φ₁ ⇒ φ₂)     = {!!}
split (φ₁ ⇔ φ₂)     = {!!}
split (¬ ⊤)         = {!!}
split (¬ ⊥)         = {!!}
split (¬ (φ₁ ∧ φ₂)) = {!!}
split (¬ (φ₁ ∨ φ₂)) = {!!}
split (¬ (φ₁ ⇒ φ₂)) = {!!}
split (¬ (φ₁ ⇔ φ₂)) = {!!}
split (¬ (¬ φ))     = {!!}
split φ₁            = {!!}
\end{code}

...

\textit{Simplification.}

\begin{figure}
\[%\scalebox{0.9}{
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi \Leftrightarrow \psi$}
  \AxiomC{$\Gamma \vdash \psi$}
  \RightLabel{\footnotesize\tt $\Leftrightarrow$-elim}
  \BinaryInfC{$\Gamma \vdash \varphi$}
\end{bprooftree}
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi \Rightarrow \psi$}
  \AxiomC{$\Gamma \vdash \varphi$}
  \RightLabel{\footnotesize\tt $\Rightarrow$-elim}
  \BinaryInfC{$\Gamma \vdash \psi$}
\end{bprooftree}
\]
\[
\begin{bprooftree}
  \AxiomC{$\Gamma \vdash \varphi \vee \neg \psi$}
  \AxiomC{$\Gamma \vdash \neg \varphi$}
  \BinaryInfC{$\Gamma \vdash \psi$}
\end{bprooftree}
%}
\]

\caption{Additional simplification theorems.}
\label{fig:simplify-rule}
\end{figure}



% ============================================================================

\section{A Small Example}
\label{sec:example}
...

% ============================================================================

\section{Conclusions}
\label{sec:conclusions}

Amazing conclusions.
...

\subsubsection*{Future Work.}
\label{ssec:future-work}

First-Order Logic support.

\subsubsection*{Acknowledgments.}
We thank Universidad EAFIT of Medell\'in, Colombia for funding support. This is
part of first author's Master thesis in Applied Mathematics, written under the
supervision of Andr\'es Sicard-Ram\'irez for the Logic and Computation Research
Group at the Universidad EAFIT.

We thank Joe Leslie-Hurd for his support with the \verb!Metis! prover and
for fixing quickly the bugs.
We also acknowledge the work carry out by Alejandro G\'omez-Londo\~no~
\cite{Gomez-Londono2015} that was used as the basis code for our TSTP parsing
module.
And last but not least, we gratefully acknowledge Andreas Abel and Chalmers
University of Goteborg, Sweden for inviting us to be part of the Agda
Implementors’ Meeting XXV where we presented part of this paper.

\bibliographystyle{splncs03}
\bibliography{ref}
\addcontentsline{toc}{section}{References}

\end{document}
