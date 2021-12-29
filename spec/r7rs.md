<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Summary](#summary)
- [Contents](#contents)
    - [-](#-)
    - [Acknowledgments](#acknowledgments)
- [Overview of Scheme](#overview-of-scheme)
    - [Semantics](#semantics)
    - [Syntax](#syntax)
    - [Notation and terminology](#notation-and-terminology)
        - [Base and optional features](#base-and-optional-features)
        - [Error situations and unspecified behavior](#error-situations-and-unspecified-behavior)
        - [Entry format](#entry-format)
        - [Evaluation examples](#evaluation-examples)
        - [Naming conventions](#naming-conventions)
- [Lexical conventions](#lexical-conventions)
    - [Identifiers](#identifiers)
    - [Whitespace and comments](#whitespace-and-comments)
    - [Other notations](#other-notations)
    - [Datum labels](#datum-labels)
- [Basic concepts](#basic-concepts)
    - [Variables, syntactic keywords, and regions](#variables-syntactic-keywords-and-regions)
    - [Disjointness of types](#disjointness-of-types)
    - [External representations](#external-representations)
    - [Storage model](#storage-model)
    - [Proper tail recursion](#proper-tail-recursion)
- [Expressions](#expressions)
    - [Primitive expression types](#primitive-expression-types)
        - [Variable references](#variable-references)
        - [Literal expressions](#literal-expressions)
        - [Procedure calls](#procedure-calls)
        - [Procedures](#procedures)
        - [Conditionals](#conditionals)
        - [Assignments](#assignments)
        - [Inclusion](#inclusion)
    - [Derived expression types](#derived-expression-types)
        - [Conditionals](#conditionals-1)
        - [Binding constructs](#binding-constructs)
        - [Sequencing](#sequencing)
        - [Iteration](#iteration)
        - [Delayed evaluation](#delayed-evaluation)
        - [Dynamic bindings](#dynamic-bindings)
        - [Exception handling](#exception-handling)
        - [Quasiquotation](#quasiquotation)
        - [Case-lambda](#case-lambda)
    - [Macros](#macros)
        - [Binding constructs for syntactic keywords](#binding-constructs-for-syntactic-keywords)
        - [Pattern language](#pattern-language)
        - [Signaling errors in macro transformers](#signaling-errors-in-macro-transformers)
- [Program structure](#program-structure)
    - [Programs](#programs)
    - [Import declarations](#import-declarations)
    - [Variable definitions](#variable-definitions)
        - [Top level definitions](#top-level-definitions)
        - [Internal definitions](#internal-definitions)
        - [Multiple-value definitions](#multiple-value-definitions)
    - [Syntax definitions](#syntax-definitions)
    - [Record-type definitions](#record-type-definitions)
    - [Libraries](#libraries)
        - [Library Syntax](#library-syntax)
        - [Library example](#library-example)
    - [The REPL](#the-repl)
- [Standard procedures](#standard-procedures)
    - [Equivalence predicates](#equivalence-predicates)
    - [Numbers](#numbers)
        - [Numerical types](#numerical-types)
        - [Exactness](#exactness)
        - [Implementation restrictions](#implementation-restrictions)
        - [Implementation extensions](#implementation-extensions)
        - [Syntax of numerical constants](#syntax-of-numerical-constants)
        - [Numerical operations](#numerical-operations)
        - [Numerical input and output](#numerical-input-and-output)
    - [Booleans](#booleans)
    - [Pairs and lists](#pairs-and-lists)
    - [Symbols](#symbols)
    - [Characters](#characters)
    - [Strings](#strings)
    - [Vectors](#vectors)
    - [Bytevectors](#bytevectors)
    - [Control features](#control-features)
    - [Exceptions](#exceptions)
    - [Environments and evaluation](#environments-and-evaluation)
    - [Input and output](#input-and-output)
        - [Ports](#ports)
        - [Input](#input)
        - [Output](#output)
    - [System interface](#system-interface)
    - [Derived expression types](#derived-expression-types-1)
- [Standard Libraries](#standard-libraries)
- [Standard Feature Identifiers](#standard-feature-identifiers)
- [Language changes](#language-changes)
    - [-](#--1)
    - [Other language changes since R<sup>5</sup>RS](#other-language-changes-since-rsup5suprs)
    - [Incompatibilities with R<sup>6</sup>RS](#incompatibilities-with-rsup6suprs)
- [Additional material](#additional-material)
- [Example](#example)

<!-- markdown-toc end -->

# Summary

The report gives a defining description of the programming language
Scheme. Scheme is a statically scoped and properly tail recursive
dialect of the Lisp programming language <span class="citation"
cites="McCarthy"></span> invented by Guy Lewis Steele Jr. and Gerald
Jay Sussman. It was designed to have exceptionally clear and simple
semantics and few different ways to form expressions. A wide variety of
programming paradigms, including imperative, functional, and
object-oriented styles, find convenient expression in Scheme.

The introduction offers a brief history of the language and of the
report.

The first three chapters present the fundamental ideas of the language
and describe the notational conventions used for describing the language
and for writing programs in the language.

Chapters [\[expressionchapter\]](#expressionchapter)
and [\[programchapter\]](#programchapter) describe the syntax and
semantics of expressions, definitions, programs, and libraries.

Chapter [\[builtinchapter\]](#builtinchapter) describes Scheme’s
built-in procedures, which include all of the language’s data
manipulation and input/output primitives.

Chapter [\[formalchapter\]](#formalchapter) provides a formal syntax for
Scheme written in extended BNF, along with a formal denotational
semantics. An example of the use of the language follows the formal
syntax and semantics.

Appendix [\[stdlibraries\]](#stdlibraries) provides a list of the
standard libraries and the identifiers that they export.

Appendix [\[stdfeatures\]](#stdfeatures) provides a list of optional but
standardized implementation feature names.

The report concludes with a list of references and an alphabetic index.

*Note:* The editors of the R<sup>5</sup>RS and R<sup>6</sup>RS reports
are listed as authors of this report in recognition of the substantial
portions of this report that are copied directly from
R<sup>5</sup>RS and R<sup>6</sup>RS. There is no intended implication
that those editors, individually or collectively, support or do not
support this report.

# Contents

Programming languages should be designed not by piling feature on top of
feature, but by removing the weaknesses and restrictions that make
additional features appear necessary. Scheme demonstrates that a very
small number of rules for forming expressions, with no restrictions on
how they are composed, suffice to form a practical and efficient
programming language that is flexible enough to support most of the
major programming paradigms in use today.

Scheme was one of the first programming languages to incorporate
first-class procedures as in the lambda calculus, thereby proving the
usefulness of static scope rules and block structure in a dynamically
typed language. Scheme was the first major dialect of Lisp to
distinguish procedures from lambda expressions and symbols, to use a
single lexical environment for all variables, and to evaluate the
operator position of a procedure call in the same way as an operand
position. By relying entirely on procedure calls to express iteration,
Scheme emphasized the fact that tail-recursive procedure calls are
essentially GOTOs that pass arguments, thus allowing a programming style
that is both coherent and efficient. Scheme was the first widely used
programming language to embrace first-class escape procedures, from
which all previously known sequential control structures can be
synthesized. A subsequent version of Scheme introduced the concept of
exact and inexact numbers, an extension of Common Lisp’s generic
arithmetic. More recently, Scheme became the first programming language
to support hygienic macros, which permit the syntax of a
block-structured language to be extended in a consistent and reliable
manner.

### Background

The first description of Scheme was written in 1975 <span
class="citation" cites="Scheme75"></span>. A revised report <span
class="citation" cites="Scheme78"></span> appeared in 1978, which
described the evolution of the language as its MIT implementation was
upgraded to support an innovative compiler <span class="citation"
cites="Rabbit"></span>. Three distinct projects began in 1981 and 1982
to use variants of Scheme for courses at MIT, Yale, and Indiana
University <span class="citation"
cites="Rees82 MITScheme Scheme311"></span>. An introductory computer
science textbook using Scheme was published in 1984 <span
class="citation" cites="SICP"></span>.

As Scheme became more widespread, local dialects began to diverge until
students and researchers occasionally found it difficult to understand
code written at other sites. Fifteen representatives of the major
implementations of Scheme therefore met in October 1984 to work toward a
better and more widely accepted standard for Scheme. Their report, the
RRRS <span class="citation" cites="RRRS"></span>, was published at MIT
and Indiana University in the summer of 1985. Further revision took
place in the spring of 1986, resulting in the R<sup>3</sup>RS <span
class="citation" cites="R3RS"></span>. Work in the spring of 1988
resulted in R<sup>4</sup>RS <span class="citation" cites="R4RS"></span>,
which became the basis for the IEEE Standard for the Scheme Programming
Language in 1991 <span class="citation" cites="IEEEScheme"></span>. In
1998, several additions to the IEEE standard, including high-level
hygienic macros, multiple return values, and eval, were finalized as the
R<sup>5</sup>RS <span class="citation" cites="R5RS"></span>.

In the fall of 2006, work began on a more ambitious standard, including
many new improvements and stricter requirements made in the interest of
improved portability. The resulting standard, the R<sup>6</sup>RS, was
completed in August 2007 <span class="citation" cites="R6RS"></span>,
and was organized as a core language and set of mandatory standard
libraries. Several new implementations of Scheme conforming to it were
created. However, most existing R<sup>5</sup>RS implementations (even
excluding those which are essentially unmaintained) did not adopt
R<sup>6</sup>RS, or adopted only selected parts of it.

In consequence, the Scheme Steering Committee decided in August 2009 to
divide the standard into two separate but compatible languages — a
“small” language, suitable for educators, researchers, and users of
embedded languages, focused on R<sup>5</sup>RS compatibility, and a
“large” language focused on the practical needs of mainstream software
development, intended to become a replacement for R<sup>6</sup>RS. The
present report describes the “small” language of that effort: therefore
it cannot be considered in isolation as the successor to
R<sup>6</sup>RS.

We intend this report to belong to the entire Scheme community, and so
we grant permission to copy it in whole or in part without fee. In
particular, we encourage implementers of Scheme to use this report as a
starting point for manuals and other documentation, modifying it as
necessary.

### Acknowledgments

We would like to thank the members of the Steering Committee, William
Clinger, Marc Feeley, Chris Hanson, Jonathan Rees, and Olin Shivers, for
their support and guidance.

This report is very much a community effort, and we’d like to thank
everyone who provided comments and feedback, including the following
people: David Adler, Eli Barzilay, Taylan Ulrich Bayırlı/Kammer, Marco
Benelli, Pierpaolo Bernardi, Peter Bex, Per Bothner, John Boyle, Taylor
Campbell, Raffael Cavallaro, Ray Dillinger, Biep Durieux, Sztefan
Edwards, Helmut Eller, Justin Ethier, Jay Reynolds Freeman, Tony
Garnock-Jones, Alan Manuel Gloria, Steve Hafner, Sven Hartrumpf, Brian
Harvey, Moritz Heidkamp, Jean-Michel Hufflen, Aubrey Jaffer, Takashi
Kato, Shiro Kawai, Richard Kelsey, Oleg Kiselyov, Pjotr Kourzanov,
Jonathan Kraut, Daniel Krueger, Christian Stigen Larsen, Noah Lavine,
Stephen Leach, Larry D. Lee, Kun Liang, Thomas Lord, Vincent Stewart
Manis, Perry Metzger, Michael Montague, Mikael More, Vitaly Magerya,
Vincent Manis, Vassil Nikolov, Joseph Wayne Norton, Yuki Okumura, Daichi
Oohashi, Jeronimo Pellegrini, Jussi Piitulainen, Alex Queiroz, Jim Rees,
Grant Rettke, Andrew Robbins, Devon Schudy, Bakul Shah, Robert Smith,
Arthur Smyles, Michael Sperber, John David Stone, Jay Sulzberger,
Malcolm Tredinnick, Sam Tobin-Hochstadt, Andre van Tonder, Daniel
Villeneuve, Denis Washington, Alan Watson, Mark H. Weaver, Göran
Weinholt, David A. Wheeler, Andy Wingo, James Wise, Jörg F.
Wittenberger, Kevin A. Wortman, Sascha Ziemann.

In addition we would like to thank all the past editors, and the people
who helped them in turn: Hal Abelson, Norman Adams, David Bartley, Alan
Bawden, Michael Blair, Gary Brooks, George Carrette, Andy Cromarty,
Pavel Curtis, Jeff Dalton, Olivier Danvy, Ken Dickey, Bruce Duba, Robert
Findler, Andy Freeman, Richard Gabriel, Yekta Gürsel, Ken Haase, Robert
Halstead, Robert Hieb, Paul Hudak, Morry Katz, Eugene Kohlbecker, Chris
Lindblad, Jacob Matthews, Mark Meyer, Jim Miller, Don Oxley, Jim
Philbin, Kent Pitman, John Ramsdell, Guillermo Rozas, Mike Shaff,
Jonathan Shapiro, Guy Steele, Julie Sussman, Perry Wagle, Mitchel Wand,
Daniel Weise, Henry Wu, and Ozan Yigit. We thank Carol Fessenden, Daniel
Friedman, and Christopher Haynes for permission to use text from the
Scheme 311 version 4 reference manual. We thank Texas Instruments,
Inc. for permission to use text from the *TI Scheme Language Reference
Manual* <span class="citation" cites="TImanual85"></span>. We gladly
acknowledge the influence of manuals for MIT Scheme <span
class="citation" cites="MITScheme"></span>, T <span class="citation"
cites="Rees84"></span>, Scheme 84 <span class="citation"
cites="Scheme84"></span>, Common Lisp <span class="citation"
cites="CLtL"></span>, and Algol 60 <span class="citation"
cites="Naur63"></span>, as well as the following SRFIs: 0, 1, 4, 6, 9,
11, 13, 16, 30, 34, 39, 43, 46, 62, and 87, all of which are available
at http://srfi.schemers.org.

# Overview of Scheme

## Semantics

This section gives an overview of Scheme’s semantics. A detailed
informal semantics is the subject of
chapters [\[basicchapter\]](#basicchapter)
through [\[builtinchapter\]](#builtinchapter). For reference purposes,
section [\[formalsemanticssection\]](#formalsemanticssection) provides a
formal semantics of Scheme.

Scheme is a statically scoped programming language. Each use of a
variable is associated with a lexically apparent binding of that
variable.

Scheme is a dynamically typed language. Types are associated with values
(also called objects) rather than with variables. Statically typed
languages, by contrast, associate types with variables and expressions
as well as with values.

All objects created in the course of a Scheme computation, including
procedures and continuations, have unlimited extent. No Scheme object is
ever destroyed. The reason that implementations of Scheme do not
(usually!) run out of storage is that they are permitted to reclaim the
storage occupied by an object if they can prove that the object cannot
possibly matter to any future computation.

Implementations of Scheme are required to be properly tail-recursive.
This allows the execution of an iterative computation in constant space,
even if the iterative computation is described by a syntactically
recursive procedure. Thus with a properly tail-recursive implementation,
iteration can be expressed using the ordinary procedure-call mechanics,
so that special iteration constructs are useful only as syntactic sugar.
See section [\[proper tail recursion\]](#proper%20tail%20recursion).

Scheme procedures are objects in their own right. Procedures can be
created dynamically, stored in data structures, returned as results of
procedures, and so on.

One distinguishing feature of Scheme is that continuations, which in
most other languages only operate behind the scenes, also have
“first-class” status. Continuations are useful for implementing a wide
variety of advanced control constructs, including non-local exits,
backtracking, and coroutines. See
section [\[continuations\]](#continuations).

Arguments to Scheme procedures are always passed by value, which means
that the actual argument expressions are evaluated before the procedure
gains control, regardless of whether the procedure needs the result of
the evaluation.

Scheme’s model of arithmetic is designed to remain as independent as
possible of the particular ways in which numbers are represented within
a computer. In Scheme, every integer is a rational number, every
rational is a real, and every real is a complex number. Thus the
distinction between integer and real arithmetic, so important to many
programming languages, does not appear in Scheme. In its place is a
distinction between exact arithmetic, which corresponds to the
mathematical ideal, and inexact arithmetic on approximations. Exact
arithmetic is not limited to integers.

## Syntax

Scheme, like most dialects of Lisp, employs a fully parenthesized prefix
notation for programs and other data; the grammar of Scheme generates a
sublanguage of the language used for data. An important consequence of
this simple, uniform representation is that Scheme programs and data can
easily be treated uniformly by other Scheme programs. For example, the
eval procedure evaluates a Scheme program expressed as data.

The read procedure performs syntactic as well as lexical decomposition
of the data it reads. The read procedure parses its input as data
(section [\[datumsyntax\]](#datumsyntax)), not as program.

The formal syntax of Scheme is described in section [\[BNF\]](#BNF).

## Notation and terminology

### Base and optional features

Every identifier defined in this report appears in one or more of
several *libraries*. Identifiers defined in the *base library* are not
marked specially in the body of the report. This library includes the
core syntax of Scheme and generally useful procedures that manipulate
data. For example, the variable abs is bound to a procedure of one
argument that computes the absolute value of a number, and the
variable + is bound to a procedure that computes sums. The full list all
the standard libraries and the identifiers they export is given in
Appendix [\[stdlibraries\]](#stdlibraries).

All implementations of Scheme:

-   Must provide the base library and all the identifiers exported from
    it.

-   May provide or omit the other libraries given in this report, but
    each library must either be provided in its entirety, exporting no
    additional identifiers, or else omitted altogether.

-   May provide other libraries not described in this report.

-   May also extend the function of any identifier in this report,
    provided the extensions are not in conflict with the language
    reported here.

-   Must support portable code by providing a mode of operation in which
    the lexical syntax does not conflict with the lexical syntax
    described in this report.

### Error situations and unspecified behavior

When speaking of an error situation, this report uses the phrase “an
error is signaled” to indicate that implementations must detect and
report the error. An error is signaled by raising a non-continuable
exception, as if by the procedure raise as described in
section [\[exceptionsection\]](#exceptionsection). The object raised is
implementation-dependent and need not be distinct from objects
previously used for the same purpose. In addition to errors signaled in
situations described in this report, programmers can signal their own
errors and handle signaled errors.

The phrase “an error that satisfies *predicate* is signaled” means that
an error is signaled as above. Furthermore, if the object that is
signaled is passed to the specified predicate (such as file-error? or
read-error?), the predicate returns #t.

If such wording does not appear in the discussion of an error, then
implementations are not required to detect or report the error, though
they are encouraged to do so. Such a situation is sometimes, but not
always, referred to with the phrase “an error.” In such a situation, an
implementation may or may not signal an error; if it does signal an
error, the object that is signaled may or may not satisfy the predicates
error-object?, file-error?, or read-error?. Alternatively,
implementations may provide non-portable extensions.

For example, it is an error for a procedure to be passed an argument of
a type that the procedure is not explicitly specified to handle, even
though such domain errors are seldom mentioned in this report.
Implementations may signal an error, extend a procedure’s domain of
definition to include such arguments, or fail catastrophically.

This report uses the phrase “may report a violation of an implementation
restriction” to indicate circumstances under which an implementation is
permitted to report that it is unable to continue execution of a correct
program because of some restriction imposed by the implementation.
Implementation restrictions are discouraged, but implementations are
encouraged to report violations of implementation restrictions.

For example, an implementation may report a violation of an
implementation restriction if it does not have enough storage to run a
program, or if an arithmetic operation would produce an exact number
that is too large for the implementation to represent.

If the value of an expression is said to be “unspecified,” then the
expression must evaluate to some object without signaling an error, but
the value depends on the implementation; this report explicitly does not
say what value is returned.

Finally, the words and phrases “must,” “must not,” “shall,” “shall not,”
“should,” “should not,” “may,” “required,” “recommended,” and
“optional,” although not capitalized in this report, are to be
interpreted as described in RFC 2119 <span class="citation"
cites="rfc2119"></span>. They are used only with reference to
implementer or implementation behavior, not with reference to programmer
or program behavior.

### Entry format

Chapters [\[expressionchapter\]](#expressionchapter)
and [\[builtinchapter\]](#builtinchapter) are organized into entries.
Each entry describes one language feature or a group of related
features, where a feature is either a syntactic construct or a
procedure. An entry begins with one or more header lines of the form

*template*  *category*  

for identifiers in the base library, or

*template*  *name* library *category*  

where *name* is the short name of a library as defined in
Appendix [\[stdlibraries\]](#stdlibraries).

If *category* is “syntax,” the entry describes an expression type, and
the template gives the syntax of the expression type. Components of
expressions are designated by syntactic variables, which are written
using angle brackets, for example expression and variable. Syntactic
variables are intended to denote segments of program text; for example,
expression stands for any string of characters which is a syntactically
valid expression. The notation

<div class="tabbing">

…

</div>

indicates zero or more occurrences of a thing, and

<div class="tabbing">

thing<sub>2</sub> …

</div>

indicates one or more occurrences of a thing.

If *category* is “auxiliary syntax,” then the entry describes a syntax
binding that occurs only as part of specific surrounding expressions.
Any use as an independent syntactic construct or variable is an error.

If *category* is “procedure,” then the entry describes a procedure, and
the header line gives a template for a call to the procedure. Argument
names in the template are *italicized*. Thus the header line

(vector-ref *vector* *k*)  procedure  

indicates that the procedure bound to the `vector-ref` variable takes
two arguments, a vector *vector* and an exact non-negative integer *k*
(see below). The header lines

(make-vector *k*)  procedure  
(make-vector *k* *fill*)  procedure  

indicate that the `make-vector` procedure must be defined to take either
one or two arguments.

It is an error for a procedure to be presented with an argument that it
is not specified to handle. For succinctness, we follow the convention
that if an argument name is also the name of a type listed in
section [\[disjointness\]](#disjointness), then it is an error if that
argument is not of the named type. For example, the header line for
`vector-ref` given above dictates that the first argument to
`vector-ref` is a vector. The following naming conventions also imply
type restrictions:

|                                                                         |                                                    |
|:------------------------------------------------------------------------|:---------------------------------------------------|
| *a**l**i**s**t*                                                         | association list (list of pairs)                   |
| *b**o**o**l**e**a**n*                                                   | boolean value (#t or #f)                           |
| *b**y**t**e*                                                            | exact integer 0 ≤ *b**y**t**e* \< 256              |
| *b**y**t**e**v**e**c**t**o**r*                                          | bytevector                                         |
| *c**h**a**r*                                                            | character                                          |
| *e**n**d*                                                               | exact non-negative integer                         |
| *k*, *k*<sub>1</sub>, … *k*<sub>*j*</sub>, …                            | exact non-negative integer                         |
| *l**e**t**t**e**r*                                                      | alphabetic character                               |
| *l**i**s**t*, *l**i**s**t*<sub>1</sub>, … *l**i**s**t*<sub>*j*</sub>, … | list (see section [\[listsection\]](#listsection)) |
| *n*, *n*<sub>1</sub>, … *n*<sub>*j*</sub>, …                            | integer                                            |
| *obj*                                                                   | any object                                         |
| *p**a**i**r*                                                            | pair                                               |
| *p**o**r**t*                                                            | port                                               |
| *p**r**o**c*                                                            | procedure                                          |
| *q*, *q*<sub>1</sub>, … *q*<sub>*j*</sub>, …                            | rational number                                    |
| *s**t**a**r**t*                                                         | exact non-negative integer                         |
| *s**t**r**i**n**g*                                                      | string                                             |
| *s**y**m**b**o**l*                                                      | symbol                                             |
| *t**h**u**n**k*                                                         | zero-argument procedure                            |
| *v**e**c**t**o**r*                                                      | vector                                             |
| *x*, *x*<sub>1</sub>, … *x*<sub>*j*</sub>, …                            | real number                                        |
| *y*, *y*<sub>1</sub>, … *y*<sub>*j*</sub>, …                            | real number                                        |
| *z*, *z*<sub>1</sub>, … *z*<sub>*j*</sub>, …                            | complex number                                     |

The names *s**t**a**r**t* and *e**n**d* are used as indexes into
strings, vectors, and bytevectors. Their use implies the following:

-   It is an error if *start* is greater than *end*.

-   It is an error if *end* is greater than the length of the string,
    vector, or bytevector.

-   If *start* is omitted, it is assumed to be zero.

-   If *end* is omitted, it assumed to be the length of the string,
    vector, or bytevector.

-   The index *start* is always inclusive and the index *end* is always
    exclusive. As an example, consider a string. If *start* and *end*
    are the same, an empty substring is referred to, and if *start* is
    zero and *end* is the length of *string*, then the entire string is
    referred to.

### Evaluation examples

The symbol “⟶” used in program examples is read “evaluates to.” For
example,

      (* 5 8)      $\Longrightarrow$ 40

means that the expression `(* 5 8)` evaluates to the object `40`. Or,
more precisely: the expression given by the sequence of characters
“`(* 5 8)`” evaluates, in an environment containing the base library, to
an object that can be represented externally by the sequence of
characters “` 40`.” See section [\[externalreps\]](#externalreps) for a
discussion of external representations of objects.

### Naming conventions

By convention, `?` is the final character of the names of procedures
that always return a boolean value. Such procedures are called
*predicates*. Predicates are generally understood to be side-effect
free, except that they may raise an exception when passed the wrong type
of argument.

Similarly, `!` is the final character of the names of procedures that
store values into previously allocated locations (see
section [\[storagemodel\]](#storagemodel)). Such procedures are called
*mutation procedures*. The value returned by a mutation procedure is
unspecified.

By convention, “`->`” appears within the names of procedures that take
an object of one type and return an analogous object of another type.
For example, list->vector takes a list and returns a vector whose
elements are the same as those of the list.

A *command* is a procedure that does not return useful values to its
continuation.

A *thunk* is a procedure that does not accept arguments.

# Lexical conventions

This section gives an informal account of some of the lexical
conventions used in writing Scheme programs. For a formal syntax of
Scheme, see section [\[BNF\]](#BNF).

## Identifiers

An identifier is any sequence of letters, digits, and “extended
identifier characters” provided that it does not have a prefix which is
a valid number. However, the `.` token (a single period) used in the
list syntax is not an identifier.

All implementations of Scheme must support the following extended
identifier characters:

    !\ \$ \% \verb"&" * + - . / :\ < = > ? @ \verb"^" \verb"_" \verb"~" %

Alternatively, an identifier can be represented by a sequence of zero or
more characters enclosed within vertical lines (\|), analogous to string
literals. Any character, including whitespace characters, but excluding
the backslash and vertical line characters, can appear verbatim in such
an identifier. In addition, characters can be specified using either an
inline hex escape or the same escapes available in strings.

For example, the identifier `|H\x65;llo|` is the same identifier as
`Hello`, and in an implementation that supports the appropriate Unicode
character the identifier `|\x3BB;|` is the same as the identifier *λ*.
What is more, `|\t\t|` and `|\x9;\x9;|` are the same. Note that `||` is
a valid identifier that is different from any other identifier.

Here are some examples of identifiers:

    ...                      {+}
    +soup+                   <=?
    ->string                 a34kTMNs
    lambda                   list->vector
    q                        V17a
    |two words|              |two\backwhack{}x20;words|
    the-word-recursion-has-many-meanings%

See section [\[extendedalphas\]](#extendedalphas) for the formal syntax
of identifiers.

Identifiers have two uses within Scheme programs:

-   Any identifier can be used as a variable or as a syntactic keyword
    (see sections [\[variablesection\]](#variablesection)
    and [\[macrosection\]](#macrosection)).

-   When an identifier appears as a literal or within a literal (see
    section [\[quote\]](#quote)), it is being used to denote a *symbol*
    (see section [\[symbolsection\]](#symbolsection)).

In contrast with earlier revisions of the report <span class="citation"
cites="R5RS"></span>, the syntax distinguishes between upper and lower
case in identifiers and in characters specified using their names.
However, it does not distinguish between upper and lower case in
numbers, nor in inline hex escapes used in the syntax of identifiers,
characters, or strings. None of the identifiers defined in this report
contain upper-case characters, even when they appear to do so as a
result of the English-language convention of capitalizing the first word
of a sentence.

The following directives give explicit control over case folding.

#!fold-case  
#!no-fold-case

These directives can appear anywhere comments are permitted (see
section [\[wscommentsection\]](#wscommentsection)) but must be followed
by a delimiter. They are treated as comments, except that they affect
the reading of subsequent data from the same port. The #!fold-case
directive causes subsequent identifiers and character names to be
case-folded as if by string-foldcase (see
section [\[stringsection\]](#stringsection)). It has no effect on
character literals. The #!no-fold-case directive causes a return to the
default, non-folding behavior.

## Whitespace and comments

*Whitespace* characters include the space, tab, and newline characters.
(Implementations may provide additional whitespace characters such as
page break.) Whitespace is used for improved readability and as
necessary to separate tokens from each other, a token being an
indivisible lexical unit such as an identifier or number, but is
otherwise insignificant. Whitespace can occur between any two tokens,
but not within a token. Whitespace occurring inside a string or inside a
symbol delimited by vertical lines is significant.

The lexical syntax includes several comment forms. Comments are treated
exactly like whitespace.

A semicolon (`;`) indicates the start of a line comment. The comment
continues to the end of the line on which the semicolon appears.

Another way to indicate a comment is to prefix a datum
(cf. section [\[datumsyntax\]](#datumsyntax)) with `#;` and optional
whitespace. The comment consists of the comment prefix `#;`, the space,
and the datum together. This notation is useful for “commenting out”
sections of code.

Block comments are indicated with properly nested ` #|` and `|#` pairs.

    \#|
       The FACT procedure computes the factorial
       of a non-negative integer.
    |\#
    (define fact
      (lambda (n)
        (if (= n 0)
            \#;(= n 1)
            1        ;Base case: return 1
            (* n (fact (- n 1))))))%

## Other notations

For a description of the notations used for numbers, see
section [\[numbersection\]](#numbersection).

`. + -`  
These are used in numbers, and can also occur anywhere in an identifier.
A delimited plus or minus sign by itself is also an identifier. A
delimited period (not occurring within a number or identifier) is used
in the notation for pairs (section [\[listsection\]](#listsection)), and
to indicate a rest-parameter in a formal parameter list
(section [\[lambda\]](#lambda)). Note that a sequence of two or more
periods *is* an identifier.

`( )`  
Parentheses are used for grouping and to notate lists
(section [\[listsection\]](#listsection)).

`’`  
The apostrophe (single quote) character is used to indicate literal data
(section [\[quote\]](#quote)).

``  
The grave accent (backquote) character is used to indicate partly
constant data (section [\[quasiquote\]](#quasiquote)).

`, ,@`  
The character comma and the sequence comma at-sign are used in
conjunction with quasiquotation (section [\[quasiquote\]](#quasiquote)).

`"`  
The quotation mark character is used to delimit strings
(section [\[stringsection\]](#stringsection)).

`‘ `  
Backslash is used in the syntax for character constants
(section [\[charactersection\]](#charactersection)) and as an escape
character within string constants
(section [\[stringsection\]](#stringsection)) and identifiers
(section [\[extendedalphas\]](#extendedalphas)).

`[ ] { }`

  
Left and right square and curly brackets (braces) are reserved for
possible future extensions to the language.

`#`  
The number sign is used for a variety of purposes depending on the
character that immediately follows it:

#t #f  
These are the boolean constants
(section [\[booleansection\]](#booleansection)), along with the
alternatives `#true` and `#false`.

`#``‘ `  
This introduces a character constant
(section [\[charactersection\]](#charactersection)).

`#``(`  
This introduces a vector constant
(section [\[vectorsection\]](#vectorsection)). Vector constants are
terminated by `)` .

`#``u8(`  
This introduces a bytevector constant
(section [\[bytevectorsection\]](#bytevectorsection)). Bytevector
constants are terminated by `)` .

`#e #i #b #o #d #x`  
These are used in the notation for numbers
(section [\[numbernotations\]](#numbernotations)).

`#n= #n#`  
These are used for labeling and referencing other literal data
(section [\[labelsection\]](#labelsection)).

## Datum labels

#n=datum  lexical syntax  
#n#  lexical syntax  
The lexical syntax #n=datum reads the same as datum, but also results in
datum being labelled by n. It is an error if n is not a sequence of
digits.

The lexical syntax #n# serves as a reference to some object labelled by
#n=; the result is the same object as the #n= (see
section [\[equivalencesection\]](#equivalencesection)).

Together, these syntaxes permit the notation of structures with shared
or circular substructure.

    (let ((x (list 'a 'b 'c)))
      (set-cdr! (cddr x) x)
      x)                       \ev \#0=(a b c . \#0\#)

The scope of a datum label is the portion of the outermost datum in
which it appears that is to the right of the label. Consequently, a
reference #n# can occur only after a label #n=; it is an error to
attempt a forward reference. In addition, it is an error if the
reference appears as the labelled object itself (as in #n= #n#), because
the object labelled by #n= is not well defined in this case.

It is an error for a program or library to include circular references
except in literals. In particular, it is an error for quasiquote
(section [\[quasiquote\]](#quasiquote)) to contain them.

    \#1=(begin (display \#\backwhack{}x) \#1\#)
                           \ev \scherror%

# Basic concepts

## Variables, syntactic keywords, and regions

An identifier can name either a type of syntax or a location where a
value can be stored. An identifier that names a type of syntax is called
a *syntactic keyword* and is said to be *bound* to a transformer for
that syntax. An identifier that names a location is called a *variable*
and is said to be *bound* to that location. The set of all visible
bindings in effect at some point in a program is known as the
*environment* in effect at that point. The value stored in the location
to which a variable is bound is called the variable’s value. By abuse of
terminology, the variable is sometimes said to name the value or to be
bound to the value. This is not quite accurate, but confusion rarely
results from this practice.

Certain expression types are used to create new kinds of syntax and to
bind syntactic keywords to those new syntaxes, while other expression
types create new locations and bind variables to those locations. These
expression types are called *binding constructs*.

Those that bind syntactic keywords are listed in
section [\[macrosection\]](#macrosection). The most fundamental of the
variable binding constructs is the lambda expression, because all other
variable binding constructs (except top-level bindings) can be explained
in terms of lambda expressions. The other variable binding constructs
are let, let\*, letrec, letrec\*, let-values, let\*-values, and do
expressions (see sections [\[lambda\]](#lambda), [\[letrec\]](#letrec),
and [\[do\]](#do)).

Scheme is a language with block structure. To each place where an
identifier is bound in a program there corresponds a *region* of the
program text within which the binding is visible. The region is
determined by the particular binding construct that establishes the
binding; if the binding is established by a lambda expression, for
example, then its region is the entire lambda expression. Every mention
of an identifier refers to the binding of the identifier that
established the innermost of the regions containing the use. If there is
no binding of the identifier whose region contains the use, then the use
refers to the binding for the variable in the global environment, if any
(chapters [\[expressionchapter\]](#expressionchapter) and
[\[initialenv\]](#initialenv)); if there is no binding for the
identifier, it is said to be *unbound*.

## Disjointness of types

No object satisfies more than one of the following predicates:

    boolean?          bytevector?
    char?             eof-object?
    null?             number?
    pair?             port?
    procedure?        string?
    symbol?           vector?

and all predicates created by define-record-type.

These predicates define the types *boolean, bytevector, character*, the
empty list object, *eof-object, number, pair, port, procedure, string,
symbol, vector*, and all record types.

Although there is a separate boolean type, any Scheme value can be used
as a boolean value for the purpose of a conditional test. As explained
in section [\[booleansection\]](#booleansection), all values count as
true in such a test except for #f. This report uses the word “true” to
refer to any Scheme value except #f, and the word “false” to refer to
#f.

## External representations

An important concept in Scheme (and Lisp) is that of the *external
representation* of an object as a sequence of characters. For example,
an external representation of the integer 28 is the sequence of
characters “`28`”, and an external representation of a list consisting
of the integers 8 and 13 is the sequence of characters “`(8 13)`”.

The external representation of an object is not necessarily unique. The
integer 28 also has representations “`#e28.000`” and “`#x1c`”, and the
list in the previous paragraph also has the representations
“`( 08 13 )`” and “`(8 . (13 . ()))`” (see
section [\[listsection\]](#listsection)).

Many objects have standard external representations, but some, such as
procedures, do not have standard representations (although particular
implementations may define representations for them).

An external representation can be written in a program to obtain the
corresponding object (see quote, section [\[quote\]](#quote)).

External representations can also be used for input and output. The
procedure read (section [\[read\]](#read)) parses external
representations, and the procedure write (section [\[write\]](#write))
generates them. Together, they provide an elegant and powerful
input/output facility.

Note that the sequence of characters “`(+ 2 6)`” is *not* an external
representation of the integer 8, even though it *is* an expression
evaluating to the integer 8; rather, it is an external representation of
a three-element list, the elements of which are the symbol `+` and the
integers 2 and 6. Scheme’s syntax has the property that any sequence of
characters that is an expression is also the external representation of
some object. This can lead to confusion, since it is not always obvious
out of context whether a given sequence of characters is intended to
denote data or program, but it is also a source of power, since it
facilitates writing programs such as interpreters and compilers that
treat programs as data (or vice versa).

The syntax of external representations of various kinds of objects
accompanies the description of the primitives for manipulating the
objects in the appropriate sections of
chapter [\[initialenv\]](#initialenv).

## Storage model

Variables and objects such as pairs, strings, vectors, and bytevectors
implicitly denote locations or sequences of locations. A string, for
example, denotes as many locations as there are characters in the
string. A new value can be stored into one of these locations using the
`string-set!` procedure, but the string continues to denote the same
locations as before.

An object fetched from a location, by a variable reference or by a
procedure such as car, vector-ref, or string-ref, is equivalent in the
sense of `eqv?` (section [\[equivalencesection\]](#equivalencesection))
to the object last stored in the location before the fetch.

Every location is marked to show whether it is in use. No variable or
object ever refers to a location that is not in use.

Whenever this report speaks of storage being newly allocated for a
variable or object, what is meant is that an appropriate number of
locations are chosen from the set of locations that are not in use, and
the chosen locations are marked to indicate that they are now in use
before the variable or object is made to denote them. Notwithstanding
this, it is understood that the empty list cannot be newly allocated,
because it is a unique object. It is also understood that empty strings,
empty vectors, and empty bytevectors, which contain no locations, may or
may not be newly allocated.

Every object that denotes locations is either mutable or immutable.
Literal constants, the strings returned by `symbol->string`, and
possibly the environment returned by scheme-report-environment are
immutable objects. All objects created by the other procedures listed in
this report are mutable. It is an error to attempt to store a new value
into a location that is denoted by an immutable object.

These locations are to be understood as conceptual, not physical. Hence,
they do not necessarily correspond to memory addresses, and even if they
do, the memory address might not be constant.

*Rationale:* In many systems it is desirable for constants (i.e. the
values of literal expressions) to reside in read-only memory. Making it
an error to alter constants permits this implementation strategy, while
not requiring other systems to distinguish between mutable and immutable
objects.

## Proper tail recursion

Implementations of Scheme are required to be *properly tail-recursive*.
Procedure calls that occur in certain syntactic contexts defined below
are *tail calls*. A Scheme implementation is properly tail-recursive if
it supports an unbounded number of active tail calls. A call is *active*
if the called procedure might still return. Note that this includes
calls that might be returned from either by the current continuation or
by continuations captured earlier by call-with-current-continuation that
are later invoked. In the absence of captured continuations, calls could
return at most once and the active calls would be those that had not yet
returned. A formal definition of proper tail recursion can be found
in <span class="citation" cites="propertailrecursion"></span>.

*Rationale:*

Intuitively, no space is needed for an active tail call because the
continuation that is used in the tail call has the same semantics as the
continuation passed to the procedure containing the call. Although an
improper implementation might use a new continuation in the call, a
return to this new continuation would be followed immediately by a
return to the continuation passed to the procedure. A properly
tail-recursive implementation returns to that continuation directly.

Proper tail recursion was one of the central ideas in Steele and
Sussman’s original version of Scheme. Their first Scheme interpreter
implemented both functions and actors. Control flow was expressed using
actors, which differed from functions in that they passed their results
on to another actor instead of returning to a caller. In the terminology
of this section, each actor finished with a tail call to another actor.

Steele and Sussman later observed that in their interpreter the code for
dealing with actors was identical to that for functions and thus there
was no need to include both in the language.

A *tail call* is a procedure call that occurs in a *tail context*. Tail
contexts are defined inductively. Note that a tail context is always
determined with respect to a particular lambda expression.

-   The last expression within the body of a lambda expression, shown as
    tail expression below, occurs in a tail context. The same is true of
    all the bodies of case-lambda expressions. ̄ \| ̄ (lāmbda formals
    expression tail expression)

    (case-lambda (formals tail body))

-   If one of the following expressions is in a tail context, then the
    subexpressions shown as tail expression are in a tail context. These
    were derived from rules in the grammar given in
    chapter [\[formalchapter\]](#formalchapter) by replacing some
    occurrences of body with tail body, some occurrences of expression
    with tail expression, and some occurrences of sequence with
    tail sequence. Only those rules that contain tail contexts are shown
    here.

    ̄ \| ̄ (if expression tail expression tail expression) (if
    expression tail expression)

    (cond cond clause<sup>+</sup>) (cond cond clause (else
    tail sequence))

    (cāse expression <sup>+</sup>) (cāse expression (else
    tail sequence))

    (and expression tail expression) (or expression tail expression)

    (when test tail sequence) (unless test tail sequence)

    (let (binding spec) tail body) (let variable (binding spec)
    tail body) (let\* (binding spec) tail body) (letrec (binding spec)
    tail body) (letrec\* (binding spec) tail body) (let-values
    (mv binding spec) tail body) (let\*-values (mv binding spec)
    tail body)

    (let-syntax (syntax spec) tail body) (letrec-syntax (syntax spec)
    tail body)

    (begin tail sequence)

    (dō (̄iteration spec) (test tail sequence) )

    where

    cond clause ⟶ (test tail sequence) case clause ⟶ ((datum)
    tail sequence)

    tail body ⟶ definition tail sequence tail sequence ⟶ expression
    tail expression

-   If a cond or case expression is in a tail context, and has a clause
    of the form (expression<sub>1</sub> => expression<sub>2</sub>) then
    the (implied) call to the procedure that results from the evaluation
    of expression<sub>2</sub> is in a tail context.
    expression<sub>2</sub> itself is not in a tail context.

Certain procedures defined in this report are also required to perform
tail calls. The first argument passed to `apply` and to
`call-with-current-continuation`, and the second argument passed to
`call-with-values`, must be called via a tail call. Similarly, `eval`
must evaluate its first argument as if it were in tail position within
the `eval` procedure.

In the following example the only tail call is the call to f. None of
the calls to g or h are tail calls. The reference to x is in a tail
context, but it is not a call and thus is not a tail call.

    %
    (lambda ()
      (if (g)
          (let ((x (h)))
            x)
          (and (g) (f))))

*Note:* Implementations may recognize that some non-tail calls, such as
the call to h above, can be evaluated as though they were tail calls. In
the example above, the let expression could be compiled as a tail call
to h. (The possibility of h returning an unexpected number of values can
be ignored, because in that case the effect of the let is explicitly
unspecified and implementation-dependent.)

# Expressions

Expression types are categorized as *primitive* or *derived*. Primitive
expression types include variables and procedure calls. Derived
expression types are not semantically primitive, but can instead be
defined as macros. Suitable syntax definitions of some of the derived
expressions are given in section [\[derivedsection\]](#derivedsection).

The procedures force, promise?, make-promise, and make-parameter are
also described in this chapter because they are intimately associated
with the delay, delay-force, and parameterize expression types.

## Primitive expression types

### Variable references

-2ex

variable  syntax  
An expression consisting of a variable
(section [\[variablesection\]](#variablesection)) is a variable
reference. The value of the variable reference is the value stored in
the location to which the variable is bound. It is an error to reference
an unbound variable.

    (define x 28)
    x   \ev  28%

### Literal expressions

-2ex

(quote *datum*)  syntax  
`’`datum  syntax  
constant  syntax  
(quote datum) evaluates to datum. Datum can be any external
representation of a Scheme object (see
section [\[externalreps\]](#externalreps)). This notation is used to
include literal constants in Scheme code.

    %
    (quote a)                     \ev  a
    (quote \sharpsign(a b c))     \ev  \#(a b c)
    (quote (+ 1 2))               \ev  (+ 1 2)%

(quote datum) can be abbreviated as `’`datum. The two notations are
equivalent in all respects.

    'a                   \ev  a
    '\#(a b c)           \ev  \#(a b c)
    '()                  \ev  ()
    '(+ 1 2)             \ev  (+ 1 2)
    '(quote a)           \ev  (quote a)
    ''a                  \ev  (quote a)%

Numerical constants, string constants, character constants, vector
constants, bytevector constants, and boolean constants evaluate to
themselves; they need not be quoted.

    '145932    \ev  145932
    145932     \ev  145932
    '"abc"     \ev  "abc"
    "abc"      \ev  "abc"
    '\#\backwhack{}a   \ev  \#\backwhack{}a
    \#\backwhack{}a   \ev  \#\backwhack{}a
    '\#(a 10)  \ev  \#(a 10)
    \#(a 10)  \ev  \#(a 10)
    '\#u8(64 65)  \ev  \#u8(64 65)
    \#u8(64 65)  \ev  \#u8(64 65)
    '\schtrue  \ev  \schtrue
    \schtrue   \ev  \schtrue%

As noted in section [\[storagemodel\]](#storagemodel), it is an error to
attempt to alter a constant (i.e. the value of a literal expression)
using a mutation procedure like set-car! or string-set!.

### Procedure calls

-2ex

(operator operand<sub>1</sub> … )  syntax  
A procedure call is written by enclosing in parentheses an expression
for the procedure to be called followed by expressions for the arguments
to be passed to it. The operator and operand expressions are evaluated
(in an unspecified order) and the resulting procedure is passed the
resulting arguments.

    %
    (+ 3 4)                          \ev  7
    ((if \schfalse + *) 3 4)         \ev  12%

The procedures in this document are available as the values of variables
exported by the standard libraries. For example, the addition and
multiplication procedures in the above examples are the values of the
variables + and \* in the base library. New procedures are created by
evaluating lambda expressions (see section [\[lambda\]](#lambda)).

Procedure calls can return any number of values (see `values` in
section [\[proceduresection\]](#proceduresection)). Most of the
procedures defined in this report return one value or, for procedures
such as apply, pass on the values returned by a call to one of their
arguments. Exceptions are noted in the individual descriptions.

*Note:* In contrast to other dialects of Lisp, the order of evaluation
is unspecified, and the operator expression and the operand expressions
are always evaluated with the same evaluation rules.

*Note:* Although the order of evaluation is otherwise unspecified, the
effect of any concurrent evaluation of the operator and operand
expressions is constrained to be consistent with some sequential order
of evaluation. The order of evaluation may be chosen differently for
each procedure call.

*Note:* In many dialects of Lisp, the empty list, ` ()`, is a legitimate
expression evaluating to itself. In Scheme, it is an error.

### Procedures

-2ex

(lambda *formals body*)  syntax  
*Syntax:* Formals is a formal arguments list as described below, and
body is a sequence of zero or more definitions followed by one or more
expressions.

*Semantics:* A lambda expression evaluates to a procedure. The
environment in effect when the lambda expression was evaluated is
remembered as part of the procedure. When the procedure is later called
with some actual arguments, the environment in which the lambda
expression was evaluated will be extended by binding the variables in
the formal argument list to fresh locations, and the corresponding
actual argument values will be stored in those locations. (A *fresh*
location is one that is distinct from every previously existing
location.) Next, the expressions in the body of the lambda expression
(which, if it contains definitions, represents a letrec\* form — see
section [\[letrecstar\]](#letrecstar)) will be evaluated sequentially in
the extended environment. The results of the last expression in the body
will be returned as the results of the procedure call.

    (lambda (x) (+ x x))      \ev  {\em{}a procedure}
    ((lambda (x) (+ x x)) 4)  \ev  8

    (define reverse-subtract
      (lambda (x y) (- y x)))
    (reverse-subtract 7 10)         \ev  3

    (define add4
      (let ((x 4))
        (lambda (y) (+ x y))))
    (add4 6)                        \ev  10%

Formals have one of the following forms:

-   `(variable_1 \ldots\,)`: The procedure takes a fixed number of
    arguments; when the procedure is called, the arguments will be
    stored in fresh locations that are bound to the corresponding
    variables.

-   variable: The procedure takes any number of arguments; when the
    procedure is called, the sequence of actual arguments is converted
    into a newly allocated list, and the list is stored in a fresh
    location that is bound to variable.

-   `(variable_1 \ldots\, variable_{n} . variable_{n+1})`: If a
    space-delimited period precedes the last variable, then the
    procedure takes *n* or more arguments, where *n* is the number of
    formal arguments before the period (it is an error if there is not
    at least one). The value stored in the binding of the last variable
    will be a newly allocated list of the actual arguments left over
    after all the other actual arguments have been matched up against
    the other formal arguments.

It is an error for a variable to appear more than once in formals.

    ((lambda x x) 3 4 5 6)          \ev  (3 4 5 6)
    ((lambda (x y . z) z)
     3 4 5 6)                       \ev  (5 6)%

Each procedure created as the result of evaluating a lambda expression
is (conceptually) tagged with a storage location, in order to make
`eqv?` and `eq?` work on procedures (see
section [\[equivalencesection\]](#equivalencesection)).

### Conditionals

-2ex

(if *test consequent alternate*)  syntax  
(if *test consequent*)  syntax  
*Syntax:* Test, consequent, and alternate are expressions.

*Semantics:* An if expression is evaluated as follows: first, test is
evaluated. If it yields a true value (see
section [\[booleansection\]](#booleansection)), then consequent is
evaluated and its values are returned. Otherwise alternate is evaluated
and its values are returned. If test yields a false value and no
alternate is specified, then the result of the expression is
unspecified.

    (if (> 3 2) 'yes 'no)           \ev  yes
    (if (> 2 3) 'yes 'no)           \ev  no
    (if (> 3 2)
        (- 3 2)
        (+ 3 2))                    \ev  1%

### Assignments

-2ex

(set! *variable expression*)  syntax  
*Semantics:* Expression is evaluated, and the resulting value is stored
in the location to which variable is bound. It is an error if variable
is not bound either in some region enclosing the set! expression or else
globally. The result of the set! expression is unspecified.

    (define x 2)
    (+ x 1)                 \ev  3
    (set! x 4)              \ev  \unspecified
    (+ x 1)                 \ev  5%

### Inclusion

-2ex

(include *string<sub>1</sub> string<sub>2</sub> … *)  syntax  
(include-ci *string<sub>1</sub> string<sub>2</sub> … *)  syntax  
*Semantics:* Both `include` and `include-ci` take one or more filenames
expressed as string literals, apply an implementation-specific algorithm
to find corresponding files, read the contents of the files in the
specified order as if by repeated applications of read, and effectively
replace the include or include-ci expression with a begin expression
containing what was read from the files. The difference between the two
is that `include-ci` reads each file as if it began with the #!fold-case
directive, while `include` does not.

*Note:* Implementations are encouraged to search for files in the
directory which contains the including file, and to provide a way for
users to specify other directories to search.

## Derived expression types

The constructs in this section are hygienic, as discussed in
section [\[macrosection\]](#macrosection). For reference purposes,
section [\[derivedsection\]](#derivedsection) gives syntax definitions
that will convert most of the constructs described in this section into
the primitive constructs described in the previous section.

### Conditionals

-2ex

(cond *clause<sub>1</sub> clause<sub>2</sub> … *)  syntax  
else  auxiliary syntax  
=>  auxiliary syntax  
*Syntax:* Clauses take one of two forms, either

    (\hyper{test} \hyperi{expression} \dotsfoo)%

where test is any expression, or

    (\hyper{test} => \hyper{expression})%

The last clause can be an “else clause,” which has the form

    (else \hyperi{expression} \hyperii{expression} \dotsfoo)\rm.%

*Semantics:* A cond expression is evaluated by evaluating the test
expressions of successive clauses in order until one of them evaluates
to a true value (see section [\[booleansection\]](#booleansection)).
When a test evaluates to a true value, the remaining expressions in its
clause are evaluated in order, and the results of the last expression in
the clause are returned as the results of the entire cond expression.

If the selected clause contains only the test and no expressions, then
the value of the test is returned as the result. If the selected clause
uses the `=>` alternate form, then the expression is evaluated. It is an
error if its value is not a procedure that accepts one argument. This
procedure is then called on the value of the test and the values
returned by this procedure are returned by the cond expression.

If all tests evaluate to #f, and there is no else clause, then the
result of the conditional expression is unspecified; if there is an else
clause, then its expressions are evaluated in order, and the values of
the last one are returned.

    (cond ((> 3 2) 'greater)
          ((< 3 2) 'less))         \ev  greater%

    (cond ((> 3 3) 'greater)
          ((< 3 3) 'less)
          (else 'equal))            \ev  equal%

    (cond ((assv 'b '((a 1) (b 2))) => cadr)
          (else \schfalse{}))         \ev  2%

(case *key clause<sub>1</sub> clause<sub>2</sub> … *)  syntax  
*Syntax:* Key can be any expression. Each clause has the form

    ((\hyperi{datum} \dotsfoo) \hyperi{expression} \hyperii{expression} \dotsfoo)\rm,%

where each datum is an external representation of some object. It is an
error if any of the datums are the same anywhere in the expression.
Alternatively, a clause can be of the form

    ((\hyperi{datum} \dotsfoo) => \hyper{expression})%

The last clause can be an “else clause,” which has one of the forms

    (else \hyperi{expression} \hyperii{expression} \dotsfoo)

or

    (else => \hyper{expression})\rm.%

*Semantics:* A case expression is evaluated as follows. Key is evaluated
and its result is compared against each datum. If the result of
evaluating key is the same (in the sense of eqv?; see
section [\[eqv?\]](#eqv?)) to a datum, then the expressions in the
corresponding clause are evaluated in order and the results of the last
expression in the clause are returned as the results of the case
expression.

If the result of evaluating key is different from every datum, then if
there is an else clause, its expressions are evaluated and the results
of the last are the results of the case expression; otherwise the result
of the case expression is unspecified.

If the selected clause or else clause uses the `=>` alternate form, then
the expression is evaluated. It is an error if its value is not a
procedure accepting one argument. This procedure is then called on the
value of the key and the values returned by this procedure are returned
by the case expression.

    (case (* 2 3)
      ((2 3 5 7) 'prime)
      ((1 4 6 8 9) 'composite))     \ev  composite
    (case (car '(c d))
      ((a) 'a)
      ((b) 'b))                     \ev  \unspecified
    (case (car '(c d))
      ((a e i o u) 'vowel)
      ((w y) 'semivowel)
      (else => (lambda (x) x)))     \ev  c%

(and *test<sub>1</sub> … *)  syntax  
*Semantics:* The test expressions are evaluated from left to right, and
if any expression evaluates to #f (see
section [\[booleansection\]](#booleansection)), then #f is returned. Any
remaining expressions are not evaluated. If all the expressions evaluate
to true values, the values of the last expression are returned. If there
are no expressions, then #t is returned.

    (and (= 2 2) (> 2 1))           \ev  \schtrue
    (and (= 2 2) (< 2 1))           \ev  \schfalse
    (and 1 2 'c '(f g))             \ev  (f g)
    (and)                           \ev  \schtrue%

(or *test<sub>1</sub> … *)  syntax  
*Semantics:* The test expressions are evaluated from left to right, and
the value of the first expression that evaluates to a true value (see
section [\[booleansection\]](#booleansection)) is returned. Any
remaining expressions are not evaluated. If all expressions evaluate to
#f or if there are no expressions, then #f is returned.

    (or (= 2 2) (> 2 1))            \ev  \schtrue
    (or (= 2 2) (< 2 1))            \ev  \schtrue
    (or \schfalse \schfalse \schfalse) \ev  \schfalse
    (or (memq 'b '(a b c))
        (/ 3 0))                    \ev  (b c)%

(when *test expression<sub>1</sub> expression<sub>2</sub> … *)  syntax  
*Syntax:* The test is an expression.

*Semantics:* The test is evaluated, and if it evaluates to a true value,
the expressions are evaluated in order. The result of the when
expression is unspecified.

    (when (= 1 1.0)
      (display "1")
      (display "2"))  \ev  \unspecified
     \>{\em and prints}  12%

(unless *test expression<sub>1</sub> expression<sub>2</sub> … *)
 syntax  
*Syntax:* The test is an expression.

*Semantics:* The test is evaluated, and if it evaluates to #f, the
expressions are evaluated in order. The result of the unless expression
is unspecified.

    (unless (= 1 1.0)
      (display "1")
      (display "2"))  \ev  \unspecified
     \>{\em and prints nothing}%

(cond-expand *ce-clause<sub>1</sub> ce-clause<sub>2</sub> … *)  syntax  
*Syntax:* The `cond-expand` expression type provides a way to statically
expand different expressions depending on the implementation. A
ce-clause takes the following form:

`(feature requirement expression \ldots\,)`

The last clause can be an “else clause,” which has the form

`(else expression \ldots\,)`

A feature requirement takes one of the following forms:

-   `feature identifier`

-   `(library library name)`

-   `(and feature requirement \ldots\,)`

-   `(or feature requirement \ldots\,)`

-   `(not feature requirement)`

*Semantics:* Each implementation maintains a list of feature identifiers
which are present, as well as a list of libraries which can be imported.
The value of a feature requirement is determined by replacing each
feature identifier and `(library library name)` on the implementation’s
lists with #t, and all other feature identifiers and library names with
#f, then evaluating the resulting expression as a Scheme boolean
expression under the normal interpretation of and, or, and not.

A `cond-expand` is then expanded by evaluating the feature requirements
of successive ce-clauses in order until one of them returns #t. When a
true clause is found, the corresponding expressions are expanded to a
begin, and the remaining clauses are ignored. If none of the
feature requirements evaluate to #t, then if there is an else clause,
its expressions are included. Otherwise, the behavior of the
`cond-expand` is unspecified. Unlike cond, cond-expand does not depend
on the value of any variables.

The exact features provided are implementation-defined, but for
portability a core set of features is given in
appendix [\[stdfeatures\]](#stdfeatures).

### Binding constructs

The binding constructs let, let\*, letrec, letrec\*, let-values, and
let\*-values give Scheme a block structure, like Algol 60. The syntax of
the first four constructs is identical, but they differ in the regions
they establish for their variable bindings. In a let expression, the
initial values are computed before any of the variables become bound; in
a let\* expression, the bindings and evaluations are performed
sequentially; while in letrec and letrec\* expressions, all the bindings
are in effect while their initial values are being computed, thus
allowing mutually recursive definitions. The let-values and let\*-values
constructs are analogous to let and let\* respectively, but are designed
to handle multiple-valued expressions, binding different identifiers to
the returned values.

(let *bindings body*)  syntax  
*Syntax:* Bindings has the form

    ((\hyperi{variable} \hyperi{init}) \dotsfoo)\rm,%

where each init is an expression, and body is a sequence of zero or more
definitions followed by a sequence of one or more expressions as
described in section [\[lambda\]](#lambda). It is an error for a
variable to appear more than once in the list of variables being bound.

*Semantics:* The inits are evaluated in the current environment (in some
unspecified order), the variables are bound to fresh locations holding
the results, the body is evaluated in the extended environment, and the
values of the last expression of body are returned. Each binding of a
variable has body as its region.

    (let ((x 2) (y 3))
      (* x y))                      \ev  6

    (let ((x 2) (y 3))
      (let ((x 7)
            (z (+ x y)))
        (* z x)))                   \ev  35%

See also “named let,” section [\[namedlet\]](#namedlet).

(let\* *bindings body*)  syntax  

*Syntax:* Bindings has the form

    ((\hyperi{variable} \hyperi{init}) \dotsfoo)\rm,%

and body is a sequence of zero or more definitions followed by one or
more expressions as described in section [\[lambda\]](#lambda).

*Semantics:* The let\* binding construct is similar to let, but the
bindings are performed sequentially from left to right, and the region
of a binding indicated by (variable init) is that part of the let\*
expression to the right of the binding. Thus the second binding is done
in an environment in which the first binding is visible, and so on. The
variables need not be distinct.

    (let ((x 2) (y 3))
      (let* ((x 7)
             (z (+ x y)))
        (* z x)))             \ev  70%

(letrec *bindings body*)  syntax  
*Syntax:* Bindings has the form

    ((\hyperi{variable} \hyperi{init}) \dotsfoo)\rm,%

and body is a sequence of zero or more definitions followed by one or
more expressions as described in section [\[lambda\]](#lambda). It is an
error for a variable to appear more than once in the list of variables
being bound.

*Semantics:* The variables are bound to fresh locations holding
unspecified values, the inits are evaluated in the resulting environment
(in some unspecified order), each variable is assigned to the result of
the corresponding init, the body is evaluated in the resulting
environment, and the values of the last expression in body are returned.
Each binding of a variable has the entire letrec expression as its
region, making it possible to define mutually recursive procedures.

    (letrec ((even?
              (lambda (n)
                (if (zero? n)
                    \schtrue
                    (odd? (- n 1)))))
             (odd?
              (lambda (n)
                (if (zero? n)
                    \schfalse
                    (even? (- n 1))))))
      (even? 88))
            \ev  \schtrue%

One restriction on letrec is very important: if it is not possible to
evaluate each init without assigning or referring to the value of any
variable, it is an error. The restriction is necessary because letrec is
defined in terms of a procedure call where a lambda expression binds the
variables to the values of the inits. In the most common uses of letrec,
all the inits are lambda expressions and the restriction is satisfied
automatically.

(letrec\* *bindings body*)  syntax  
*Syntax:* Bindings has the form

    ((\hyperi{variable} \hyperi{init}) \dotsfoo)\rm,%

and body is a sequence of zero or more definitions followed by one or
more expressions as described in section [\[lambda\]](#lambda). It is an
error for a variable to appear more than once in the list of variables
being bound.

*Semantics:* The variables are bound to fresh locations, each variable
is assigned in left-to-right order to the result of evaluating the
corresponding init (interleaving evaluations and assignments), the body
is evaluated in the resulting environment, and the values of the last
expression in body are returned. Despite the left-to-right evaluation
and assignment order, each binding of a variable has the entire letrec\*
expression as its region, making it possible to define mutually
recursive procedures.

If it is not possible to evaluate each init without assigning or
referring to the value of the corresponding variable or the variable of
any of the bindings that follow it in bindings, it is an error. Another
restriction is that it is an error to invoke the continuation of an init
more than once.

    ;; Returns the arithmetic, geometric, and
    ;; harmonic means of a nested list of numbers
    (define (means ton)
      (letrec*
         ((mean
            (lambda (f g)
              (f (/ (sum g ton) n))))
          (sum
            (lambda (g ton)
              (if (null? ton)
                (+)
                (if (number? ton)
                    (g ton)
                    (+ (sum g (car ton))
                       (sum g (cdr ton)))))))
          (n (sum (lambda (x) 1) ton)))
        (values (mean values values)
                (mean exp log)
                (mean / /))))

Evaluating (means ’(3 (1 4))) returns three values: 8/3,
2.28942848510666 (approximately), and 36/19.

(let-values *mv binding spec body*)  syntax  
*Syntax:* Mv binding spec has the form

    ((\hyperi{formals} \hyperi{init}) \dotsfoo)\rm,%

where each init is an expression, and body is zero or more definitions
followed by a sequence of one or more expressions as described in
section [\[lambda\]](#lambda). It is an error for a variable to appear
more than once in the set of formals.

*Semantics:* The inits are evaluated in the current environment (in some
unspecified order) as if by invoking call-with-values, and the variables
occurring in the formals are bound to fresh locations holding the values
returned by the inits, where the formals are matched to the return
values in the same way that the formals in a lambda expression are
matched to the arguments in a procedure call. Then, the body is
evaluated in the extended environment, and the values of the last
expression of body are returned. Each binding of a variable has body as
its region.

It is an error if the formals do not match the number of values returned
by the corresponding init.

    (let-values (((root rem) (exact-integer-sqrt 32)))
      (* root rem))                \ev  35%

(let\*-values *mv binding spec body*)  syntax  

*Syntax:* Mv binding spec has the form

    ((\hyper{formals} \hyper{init}) \dotsfoo)\rm,%

and body is a sequence of zero or more definitions followed by one or
more expressions as described in section [\[lambda\]](#lambda). In each
formals, it is an error if any variable appears more than once.

*Semantics:* The let\*-values construct is similar to let-values, but
the inits are evaluated and bindings created sequentially from left to
right, with the region of the bindings of each formals including the
inits to its right as well as body. Thus the second init is evaluated in
an environment in which the first set of bindings is visible and
initialized, and so on.

    (let ((a 'a) (b 'b) (x 'x) (y 'y))
      (let*-values (((a b) (values x y))
                    ((x y) (values a b)))
        (list a b x y)))     \ev (x y x y)%

### Sequencing

-2ex

Both of Scheme’s sequencing constructs are named begin, but the two have
slightly different forms and uses:

(begin *expression or definition … *)  syntax  
This form of begin can appear as part of a body, or at the outermost
level of a program, or at the REPL, or directly nested in a begin that
is itself of this form. It causes the contained expressions and
definitions to be evaluated exactly as if the enclosing begin construct
were not present.

*Rationale:* This form is commonly used in the output of macros (see
section [\[macrosection\]](#macrosection)) which need to generate
multiple definitions and splice them into the context in which they are
expanded.

(begin *expression<sub>1</sub> expression<sub>2</sub> … *)  syntax  
This form of begin can be used as an ordinary expression. The
expressions are evaluated sequentially from left to right, and the
values of the last expression are returned. This expression type is used
to sequence side effects such as assignments or input and output.

    (define x 0)

    (and (= x 0)
         (begin (set! x 5)
                (+ x 1)))              \ev  6

    (begin (display "4 plus 1 equals ")
           (display (+ 4 1)))      \ev  \unspecified
     \>{\em and prints}  4 plus 1 equals 5%

Note that there is a third form of begin used as a library declaration:
see section [\[librarydeclarations\]](#librarydeclarations).

### Iteration

(do ((variable<sub>1</sub> init<sub>1</sub> step<sub>1</sub>)  syntax  
` \ldots\,) (test expression \ldots\,) command \ldots\,)`

*Syntax:* All of init, step, test, and command are expressions.

*Semantics:* A do expression is an iteration construct. It specifies a
set of variables to be bound, how they are to be initialized at the
start, and how they are to be updated on each iteration. When a
termination condition is met, the loop exits after evaluating the
expressions.

A do expression is evaluated as follows: The init expressions are
evaluated (in some unspecified order), the variables are bound to fresh
locations, the results of the init expressions are stored in the
bindings of the variables, and then the iteration phase begins.

Each iteration begins by evaluating test; if the result is false (see
section [\[booleansection\]](#booleansection)), then the command
expressions are evaluated in order for effect, the step expressions are
evaluated in some unspecified order, the variables are bound to fresh
locations, the results of the steps are stored in the bindings of the
variables, and the next iteration begins.

If test evaluates to a true value, then the expressions are evaluated
from left to right and the values of the last expression are returned.
If no expressions are present, then the value of the do expression is
unspecified.

The region of the binding of a variable consists of the entire do
expression except for the inits. It is an error for a variable to appear
more than once in the list of do variables.

A step can be omitted, in which case the effect is the same as if
(variable init variable) had been written instead of (variable init).

    (do ((vec (make-vector 5))
         (i 0 (+ i 1)))
        ((= i 5) vec)
      (vector-set! vec i i))          \ev  \#(0 1 2 3 4)

    (let ((x '(1 3 5 7 9)))
      (do ((x x (cdr x))
           (sum 0 (+ sum (car x))))
          ((null? x) sum)))             \ev  25%

(let *variable bindings body*)  syntax  
*Semantics:* “Named let” is a variant on the syntax of `let` which
provides a more general looping construct than do and can also be used
to express recursion. It has the same syntax and semantics as ordinary
let except that variable is bound within body to a procedure whose
formal arguments are the bound variables and whose body is body. Thus
the execution of body can be repeated by invoking the procedure named by
variable.

    (let loop ((numbers '(3 -2 1 6 -5))
               (nonneg '())
               (neg '()))
      (cond ((null? numbers) (list nonneg neg))
            ((>= (car numbers) 0)
             (loop (cdr numbers)
                   (cons (car numbers) nonneg)
                   neg))
            ((< (car numbers) 0)
             (loop (cdr numbers)
                   nonneg
                   (cons (car numbers) neg))))) %
      \lev  ((6 1 3) (-5 -2))%

### Delayed evaluation

-2ex

(delay *expression*)  lazy library syntax  
*Semantics:* The delay construct is used together with the procedure
`force` to implement *lazy evaluation* or *call by need*.
`(delay expression)` returns an object called a *promise* which at some
point in the future can be asked (by the force procedure) to evaluate
expression, and deliver the resulting value.

The effect of expression returning multiple values is unspecified.

(delay-force *expression*)  lazy library syntax  
*Semantics:* The expression (delay-force *expression*) is conceptually
similar to (delay (force *expression*)), with the difference that
forcing the result of delay-force will in effect result in a tail call
to (force *expression*), while forcing the result of (delay (force
*expression*)) might not. Thus iterative lazy algorithms that might
result in a long series of chains of delay and force can be rewritten
using delay-force to prevent consuming unbounded space during
evaluation.

(force *promise*)  lazy library procedure  
The force procedure forces the value of a *promise* created by `delay`,
`delay-force`, or `make-promise`. If no value has been computed for the
promise, then a value is computed and returned. The value of the promise
must be cached (or “memoized”) so that if it is forced a second time,
the previously computed value is returned. Consequently, a delayed
expression is evaluated using the parameter values and exception handler
of the call to force which first requested its value. If *promise* is
not a promise, it may be returned unchanged.

    (force (delay (+ 1 2)))   \ev  3
    (let ((p (delay (+ 1 2))))
      (list (force p) (force p)))  
                                   \ev  (3 3)

    (define integers
      (letrec ((next
                (lambda (n)
                  (delay (cons n (next (+ n 1)))))))
        (next 0)))
    (define head
      (lambda (stream) (car (force stream))))
    (define tail
      (lambda (stream) (cdr (force stream))))

    (head (tail (tail integers)))  
                                   \ev  2%

The following example is a mechanical transformation of a lazy
stream-filtering algorithm into Scheme. Each call to a constructor is
wrapped in delay, and each argument passed to a deconstructor is wrapped
in force. The use of (delay-force ...) instead of (delay (force ...))
around the body of the procedure ensures that an ever-growing sequence
of pending promises does not exhaust available storage, because force
will in effect force such sequences iteratively.

    (define (stream-filter p? s)
      (delay-force
       (if (null? (force s)) 
           (delay '())
           (let ((h (car (force s)))
                 (t (cdr (force s))))
             (if (p? h)
                 (delay (cons h (stream-filter p? t)))
                 (stream-filter p? t))))))

    (head (tail (tail (stream-filter odd? integers))))
                                   \ev 5%

The following examples are not intended to illustrate good programming
style, as delay, force, and delay-force are mainly intended for programs
written in the functional style. However, they do illustrate the
property that only one value is computed for a promise, no matter how
many times it is forced.

    (define count 0)
    (define p
      (delay (begin (set! count (+ count 1))
                    (if (> count x)
                        count
                        (force p)))))
    (define x 5)
    p                     \ev  {\it{}a promise}
    (force p)             \ev  6
    p                     \ev  {\it{}a promise, still}
    (begin (set! x 10)
           (force p))     \ev  6%

Various extensions to this semantics of delay, force and delay-force are
supported in some implementations:

-   Calling force on an object that is not a promise may simply return
    the object.

-   It may be the case that there is no means by which a promise can be
    operationally distinguished from its forced value. That is,
    expressions like the following may evaluate to either #t or to #f,
    depending on the implementation:

        (eqv? (delay 1) 1)          \ev  \unspecified
        (pair? (delay (cons 1 2)))  \ev  \unspecified%

-   Implementations may implement “implicit forcing,” where the value of
    a promise is forced by procedures that operate only on arguments of
    a certain type, like cdr and \*. However, procedures that operate
    uniformly on their arguments, like list, must not force them.

        (+ (delay (* 3 7)) 13)  \ev  \unspecified
        (car
          (list (delay (* 3 7)) 13))    \ev {\it{}a promise}%

(promise? **obj**)  lazy library procedure  
The promise? procedure returns #t if its argument is a promise, and #f
otherwise. Note that promises are not necessarily disjoint from other
Scheme types such as procedures.

(make-promise **obj**)  lazy library procedure  
The make-promise procedure returns a promise which, when forced, will
return *obj*. It is similar to delay, but does not delay its argument:
it is a procedure rather than syntax. If *obj* is already a promise, it
is returned.

### Dynamic bindings

-2ex

The *dynamic extent* of a procedure call is the time between when it is
initiated and when it returns. In Scheme, call-with-current-continuation
(section [\[continuations\]](#continuations)) allows reentering a
dynamic extent after its procedure call has returned. Thus, the dynamic
extent of a call might not be a single, continuous time period.

This sections introduces *parameter objects*, which can be bound to new
values for the duration of a dynamic extent. The set of all parameter
bindings at a given time is called the *dynamic environment*.

(make-parameter *init*)  procedure  
(make-parameter *init converter*)  procedure  
Returns a newly allocated parameter object, which is a procedure that
accepts zero arguments and returns the value associated with the
parameter object. Initially, this value is the value of (*converter*
*init*), or of *init* if the conversion procedure *converter* is not
specified. The associated value can be temporarily changed using
parameterize, which is described below.

The effect of passing arguments to a parameter object is
implementation-dependent.

(parameterize ((param<sub>1</sub> value<sub>1</sub>) … )  syntax  
` )`

*Syntax:* Both param<sub>1</sub> and value<sub>1</sub> are expressions.

It is an error if the value of any param expression is not a parameter
object.

*Semantics:* A parameterize expression is used to change the values
returned by specified parameter objects during the evaluation of the
body.

The param and value expressions are evaluated in an unspecified order.
The body is evaluated in a dynamic environment in which calls to the
parameters return the results of passing the corresponding values to the
conversion procedure specified when the parameters were created. Then
the previous values of the parameters are restored without passing them
to the conversion procedure. The results of the last expression in the
body are returned as the results of the entire parameterize expression.

*Note:* If the conversion procedure is not idempotent, the results of
(parameterize ((x (x))) ...), which appears to bind the parameter *x* to
its current value, might not be what the user expects.

If an implementation supports multiple threads of execution, then
parameterize must not change the associated values of any parameters in
any thread other than the current thread and threads created inside
body.

Parameter objects can be used to specify configurable settings for a
computation without the need to pass the value to every procedure in the
call chain explicitly.

    (define radix
      (make-parameter
       10
       (lambda (x)
         (if (and (exact-integer? x) (<= 2 x 16))
             x
             (error "invalid radix")))))

    (define (f n) (number->string n (radix)))

    (f 12)                                       \ev "12"
    (parameterize ((radix 2))
      (f 12))                                    \ev "1100"
    (f 12)                                       \ev "12"

    (radix 16)                                   \ev \unspecified

    (parameterize ((radix 0))
      (f 12))                                    \ev \scherror%

### Exception handling

-2ex

(guard (variable  syntax  
` cond clause_2 \ldots\,) )`  
*Syntax:* Each cond clause is as in the specification of cond.

*Semantics:* The body is evaluated with an exception handler that binds
the raised object (see `raise` in
section [\[exceptionsection\]](#exceptionsection)) to variable and,
within the scope of that binding, evaluates the clauses as if they were
the clauses of a cond expression. That implicit cond expression is
evaluated with the continuation and dynamic environment of the guard
expression. If every cond clause’s test evaluates to #f and there is no
else clause, then raise-continuable is invoked on the raised object
within the dynamic environment of the original call to raise or
raise-continuable, except that the current exception handler is that of
the guard expression.

See section [\[exceptionsection\]](#exceptionsection) for a more
complete discussion of exceptions.

    (guard (condition
             ((assq 'a condition) => cdr)
             ((assq 'b condition)))
      (raise (list (cons 'a 42))))
    \ev 42

    (guard (condition
             ((assq 'a condition) => cdr)
             ((assq 'b condition)))
      (raise (list (cons 'b 23))))
    \ev (b . 23)%

### Quasiquotation

-2ex

(quasiquote *qq template*)  syntax  
``qq template  syntax  
unquote  auxiliary syntax  
`‘ `  auxiliary syntax  
unquote-splicing  auxiliary syntax  
`‘ ``‘`  auxiliary syntax  
“Quasiquote” expressions are useful for constructing a list or vector
structure when some but not all of the desired structure is known in
advance. If no commas appear within the qq template, the result of
evaluating ``qq template is equivalent to the result of evaluating
`’`qq template. If a comma appears within the qq template, however, the
expression following the comma is evaluated (“unquoted”) and its result
is inserted into the structure instead of the comma and the expression.
If a comma appears followed without intervening whitespace by a
commercial at-sign (`‘`), then it is an error if the following
expression does not evaluate to a list; the opening and closing
parentheses of the list are then “stripped away” and the elements of the
list are inserted in place of the comma at-sign expression sequence. A
comma at-sign normally appears only within a list or vector qq template.

*Note:* In order to unquote an identifier beginning with @, it is
necessary to use either an explicit unquote or to put whitespace after
the comma, to avoid colliding with the comma at-sign sequence.

    `(list ,(+ 1 2) 4)  \ev  (list 3 4)
    (let ((name 'a)) `(list ,name ',name)) %
              \lev  (list a (quote a))
    `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) %
              \lev  (a 3 4 5 6 b)
    `(({\cf foo} ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) %
              \lev  ((foo 7) . cons)
    `\#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8) %
              \lev  \#(10 5 2 4 3 8)
    (let ((foo '(foo bar)) (@baz 'baz))
      `(list ,@foo , @baz))%
              \lev  (list foo bar baz)%

Quasiquote expressions can be nested. Substitutions are made only for
unquoted components appearing at the same nesting level as the outermost
quasiquote. The nesting level increases by one inside each successive
quasiquotation, and decreases by one inside each unquotation.

    `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) %
              \lev  (a `(b ,(+ 1 2) ,(foo 4 d) e) f)
    (let ((name1 'x)
          (name2 'y))
      `(a `(b ,,name1 ,',name2 d) e)) %
              \lev  (a `(b ,x ,'y d) e)%

A quasiquote expression may return either newly allocated, mutable
objects or literal structure for any structure that is constructed at
run time during the evaluation of the expression. Portions that do not
need to be rebuilt are always literal. Thus,

    (let ((a 3)) `((1 2) ,a ,4 ,'five 6))%

may be treated as equivalent to either of the following expressions:

    `((1 2) 3 4 five 6)

    (let ((a 3))
      (cons '(1 2)
            (cons a (cons 4 (cons 'five '(6))))))%

However, it is not equivalent to this expression:

    (let ((a 3)) (list (list 1 2) a 4 'five 6))%

The two notations ``qq template and `(quasiquote qq template)` are
identical in all respects. ,expression is identical to (unquote
expression), and ,@expression is identical to (unquote-splicing
expression). The `write` procedure may output either format.

    (quasiquote (list (unquote (+ 1 2)) 4)) %
              \lev  (list 3 4)
    '(quasiquote (list (unquote (+ 1 2)) 4)) %
              \lev  `(list ,(+ 1 2) 4)
         {\em{}i.e.,} (quasiquote (list (unquote (+ 1 2)) 4))%

It is an error if any of the identifiers quasiquote, unquote, or
unquote-splicing appear in positions within a qq template otherwise than
as described above.

### Case-lambda

-2ex

(case-lambda *clause … *)  case-lambda library syntax  
*Syntax:* Each clause is of the form (formals body), where formals and
body have the same syntax as in a lambda expression.

*Semantics:* A case-lambda expression evaluates to a procedure that
accepts a variable number of arguments and is lexically scoped in the
same manner as a procedure resulting from a lambda expression. When the
procedure is called, the first clause for which the arguments agree with
formals is selected, where agreement is specified as for the formals of
a lambda expression. The variables of formals are bound to fresh
locations, the values of the arguments are stored in those locations,
the body is evaluated in the extended environment, and the results of
body are returned as the results of the procedure call.

It is an error for the arguments not to agree with the formals of any
clause.

    (define range
      (case-lambda
       ((e) (range 0 e))
       ((b e) (do ((r '() (cons e r))
                   (e (- e 1) (- e 1)))
                  ((< e b) r)))))

    (range 3)    \ev (0 1 2)
    (range 3 5)  \ev (3 4)%

## Macros

Scheme programs can define and use new derived expression types, called
*macros*. Program-defined expression types have the syntax

    (\hyper{keyword} {\hyper{datum}} ...)%

where keyword is an identifier that uniquely determines the expression
type. This identifier is called the *syntactic keyword*, or simply
*keyword*, of the macro. The number of the datums, and their syntax,
depends on the expression type.

Each instance of a macro is called a *use* of the macro. The set of
rules that specifies how a use of a macro is transcribed into a more
primitive expression is called the *transformer* of the macro.

The macro definition facility consists of two parts:

-   A set of expressions used to establish that certain identifiers are
    macro keywords, associate them with macro transformers, and control
    the scope within which a macro is defined, and

-   a pattern language for specifying macro transformers.

The syntactic keyword of a macro can shadow variable bindings, and local
variable bindings can shadow syntactic bindings. Two mechanisms are
provided to prevent unintended conflicts:

-   If a macro transformer inserts a binding for an identifier (variable
    or keyword), the identifier will in effect be renamed throughout its
    scope to avoid conflicts with other identifiers. Note that a global
    variable definition may or may not introduce a binding; see
    section [\[defines\]](#defines).

-   If a macro transformer inserts a free reference to an identifier,
    the reference refers to the binding that was visible where the
    transformer was specified, regardless of any local bindings that
    surround the use of the macro.

In consequence, all macros defined using the pattern language are
“hygienic” and “referentially transparent” and thus preserve Scheme’s
lexical scoping. <span class="citation"
cites="Kohlbecker86 hygienic Bawden88 macrosthatwork syntacticabstraction"></span>

Implementations may provide macro facilities of other types.

### Binding constructs for syntactic keywords

The let-syntax and letrec-syntax binding constructs are analogous to let
and letrec, but they bind syntactic keywords to macro transformers
instead of binding variables to locations that contain values. Syntactic
keywords can also be bound globally or locally with define-syntax; see
section [\[define-syntax\]](#define-syntax).

(let-syntax *bindings body*)  syntax  
*Syntax:* Bindings has the form

    ((\hyper{keyword} \hyper{transformer spec}) \dotsfoo)%

Each keyword is an identifier, each transformer spec is an instance of
syntax-rules, and body is a sequence of zero or more definitions
followed by one or more expressions. It is an error for a keyword to
appear more than once in the list of keywords being bound.

*Semantics:* The body is expanded in the syntactic environment obtained
by extending the syntactic environment of the let-syntax expression with
macros whose keywords are the keywords, bound to the specified
transformers. Each binding of a keyword has body as its region.

    (let-syntax ((given-that (syntax-rules ()
                         ((given-that test stmt1 stmt2 ...)
                          (if test
                              (begin stmt1
                                     stmt2 ...))))))
      (let ((if \schtrue))
        (given-that if (set! if 'now))
        if))                           \ev  now

    (let ((x 'outer))
      (let-syntax ((m (syntax-rules () ((m) x))))
        (let ((x 'inner))
          (m))))                       \ev  outer%

(letrec-syntax *bindings body*)  syntax  
*Syntax:* Same as for let-syntax.

*Semantics:* The body is expanded in the syntactic environment obtained
by extending the syntactic environment of the letrec-syntax expression
with macros whose keywords are the keywords, bound to the specified
transformers. Each binding of a keyword has the transformer specs as
well as the body within its region, so the transformers can transcribe
expressions into uses of the macros introduced by the letrec-syntax
expression.

    (letrec-syntax
        ((my-or (syntax-rules ()
                  ((my-or) \schfalse)
                  ((my-or e) e)
                  ((my-or e1 e2 ...)
                   (let ((temp e1))
                     (if temp
                         temp
                         (my-or e2 ...)))))))
      (let ((x \schfalse)
            (y 7)
            (temp 8)
            (let odd?)
            (if even?))
        (my-or x
               (let temp)
               (if y)
               y)))        \ev  7%

### Pattern language

A transformer spec has one of the following forms:

(syntax-rules (pattern literal … )  syntax  
` \ldots\,) ` (syntax-rules ellipsis (pattern literal … )  syntax  
` \ldots\,)`  
\_  auxiliary syntax  
…   auxiliary syntax  
*Syntax:* It is an error if any of the pattern literals, or the ellipsis
in the second form, is not an identifier. It is also an error if
syntax rule is not of the form

    (\hyper{pattern} \hyper{template})%

The pattern in a syntax rule is a list pattern whose first element is an
identifier.

A pattern is either an identifier, a constant, or one of the following

    (\hyper{pattern} \ldots)
    (\hyper{pattern} \hyper{pattern} \ldots . \hyper{pattern})
    (\hyper{pattern} \ldots \hyper{pattern} \hyper{ellipsis} \hyper{pattern} \ldots)
    (\hyper{pattern} \ldots \hyper{pattern} \hyper{ellipsis} \hyper{pattern} \ldots
      . \hyper{pattern})
    \#(\hyper{pattern} \ldots)
    \#(\hyper{pattern} \ldots \hyper{pattern} \hyper{ellipsis} \hyper{pattern} \ldots)%

and a template is either an identifier, a constant, or one of the
following

    (\hyper{element} \ldots)
    (\hyper{element} \hyper{element} \ldots . \hyper{template})
    (\hyper{ellipsis} \hyper{template})
    \#(\hyper{element} \ldots)%

where an element is a template optionally followed by an ellipsis. An
ellipsis is the identifier specified in the second form of syntax-rules,
or the default identifier ... (three consecutive periods) otherwise.

*Semantics:* An instance of syntax-rules produces a new macro
transformer by specifying a sequence of hygienic rewrite rules. A use of
a macro whose keyword is associated with a transformer specified by
syntax-rules is matched against the patterns contained in the
syntax rules, beginning with the leftmost syntax rule. When a match is
found, the macro use is transcribed hygienically according to the
template.

An identifier appearing within a pattern can be an underscore (\_), a
literal identifier listed in the list of pattern literals, or the
ellipsis. All other identifiers appearing within a pattern are *pattern
variables*.

The keyword at the beginning of the pattern in a syntax rule is not
involved in the matching and is considered neither a pattern variable
nor a literal identifier.

Pattern variables match arbitrary input elements and are used to refer
to elements of the input in the template. It is an error for the same
pattern variable to appear more than once in a pattern.

Underscores also match arbitrary input elements but are not pattern
variables and so cannot be used to refer to those elements. If an
underscore appears in the pattern literals list, then that takes
precedence and underscores in the pattern match as literals. Multiple
underscores can appear in a pattern.

Identifiers that appear in (pattern literal … ) are interpreted as
literal identifiers to be matched against corresponding elements of the
input. An element in the input matches a literal identifier if and only
if it is an identifier and either both its occurrence in the macro
expression and its occurrence in the macro definition have the same
lexical binding, or the two identifiers are the same and both have no
lexical binding.

A subpattern followed by ellipsis can match zero or more elements of the
input, unless ellipsis appears in the pattern literals, in which case it
is matched as a literal.

More formally, an input expression *E* matches a pattern *P* if and only
if:

-   *P* is an underscore (\_).

-   *P* is a non-literal identifier; or

-   *P* is a literal identifier and *E* is an identifier with the same
    binding; or

-   *P* is a list (*P*<sub>1</sub> … *P*<sub>*n*</sub>) and *E* is a
    list of *n* elements that match *P*<sub>1</sub> through
    *P*<sub>*n*</sub>, respectively; or

-   *P* is an improper list (*P*<sub>1</sub> *P*<sub>2</sub> …
    *P*<sub>*n*</sub> . *P*<sub>*n* + 1</sub>) and *E* is a list or
    improper list of *n* or more elements that match *P*<sub>1</sub>
    through *P*<sub>*n*</sub>, respectively, and whose *n*th tail
    matches *P*<sub>*n* + 1</sub>; or

-   *P* is of the form (*P*<sub>1</sub> … *P*<sub>*k*</sub>
    *P*<sub>*e*</sub> ellipsis *P*<sub>*m* + 1</sub> … 
    *P*<sub>*n*</sub>) where *E* is a proper list of *n* elements, the
    first *k* of which match *P*<sub>1</sub> through *P*<sub>*k*</sub>,
    respectively, whose next *m* − *k* elements each match
    *P*<sub>*e*</sub>, whose remaining *n* − *m* elements match
    *P*<sub>*m* + 1</sub> through *P*<sub>*n*</sub>; or

-   *P* is of the form (*P*<sub>1</sub> … *P*<sub>*k*</sub>
    *P*<sub>*e*</sub> ellipsis *P*<sub>*m* + 1</sub> … 
    *P*<sub>*n*</sub> . *P*<sub>*x*</sub>) where *E* is a list or
    improper list of *n* elements, the first *k* of which match
    *P*<sub>1</sub> through *P*<sub>*k*</sub>, whose next *m* − *k*
    elements each match *P*<sub>*e*</sub>, whose remaining *n* − *m*
    elements match *P*<sub>*m* + 1</sub> through *P*<sub>*n*</sub>, and
    whose *n*th and final cdr matches *P*<sub>*x*</sub>; or

-   *P* is a vector of the form #(*P*<sub>1</sub> … *P*<sub>*n*</sub>)
    and *E* is a vector of *n* elements that match *P*<sub>1</sub>
    through *P*<sub>*n*</sub>; or

-   *P* is of the form #(*P*<sub>1</sub> … *P*<sub>*k*</sub>
    *P*<sub>*e*</sub> ellipsis *P*<sub>*m* + 1</sub>
    … *P*<sub>*n*</sub>) where *E* is a vector of *n* elements the first
    *k* of which match *P*<sub>1</sub> through *P*<sub>*k*</sub>, whose
    next *m* − *k* elements each match *P*<sub>*e*</sub>, and whose
    remaining *n* − *m* elements match *P*<sub>*m* + 1</sub> through
    *P*<sub>*n*</sub>; or

-   *P* is a constant and *E* is equal to *P* in the sense of the equal?
    procedure.

It is an error to use a macro keyword, within the scope of its binding,
in an expression that does not match any of the patterns.

When a macro use is transcribed according to the template of the
matching syntax rule, pattern variables that occur in the template are
replaced by the elements they match in the input. Pattern variables that
occur in subpatterns followed by one or more instances of the identifier
ellipsis are allowed only in subtemplates that are followed by as many
instances of ellipsis. They are replaced in the output by all of the
elements they match in the input, distributed as indicated. It is an
error if the output cannot be built up as specified.

Identifiers that appear in the template but are not pattern variables or
the identifier ellipsis are inserted into the output as literal
identifiers. If a literal identifier is inserted as a free identifier
then it refers to the binding of that identifier within whose scope the
instance of syntax-rules appears. If a literal identifier is inserted as
a bound identifier then it is in effect renamed to prevent inadvertent
captures of free identifiers.

A template of the form (ellipsis template) is identical to template,
except that ellipses within the template have no special meaning. That
is, any ellipses contained within template are treated as ordinary
identifiers. In particular, the template (ellipsis ellipsis) produces a
single ellipsis. This allows syntactic abstractions to expand into code
containing ellipses.

    (define-syntax be-like-begin
      (syntax-rules ()
        ((be-like-begin name)
         (define-syntax name
           (syntax-rules ()
             ((name expr (... ...))
              (begin expr (... ...))))))))

    (be-like-begin sequence)
    (sequence 1 2 3 4) \ev 4%

As an example, if `let` and `cond` are defined as in
section [\[derivedsection\]](#derivedsection) then they are hygienic (as
required) and the following is not an error.

    (let ((=> \schfalse))
      (cond (\schtrue => 'ok)))           \ev ok%

The macro transformer for cond recognizes => as a local variable, and
hence an expression, and not as the base identifier =>, which the macro
transformer treats as a syntactic keyword. Thus the example expands into

    (let ((=> \schfalse))
      (if \schtrue (begin => 'ok)))%

instead of

    (let ((=> \schfalse))
      (let ((temp \schtrue))
        (if temp ('ok temp))))%

which would result in an invalid procedure call.

### Signaling errors in macro transformers

(syntax-error message args … )  syntax  
syntax-error behaves similarly to error
([\[exceptionsection\]](#exceptionsection)) except that implementations
with an expansion pass separate from evaluation should signal an error
as soon as syntax-error is expanded. This can be used as a syntax-rules
template for a pattern that is an invalid use of the macro, which can
provide more descriptive error messages. message is a string literal,
and args arbitrary expressions providing additional information.
Applications cannot count on being able to catch syntax errors with
exception handlers or guards.

    (define-syntax simple-let
      (syntax-rules ()
        ((\_ (head ... ((x . y) val) . tail)
            body1 body2 ...)
         (syntax-error
          "expected an identifier but got"
          (x . y)))
        ((\_ ((name val) ...) body1 body2 ...)
         ((lambda (name ...) body1 body2 ...)
           val ...))))%

# Program structure

## Programs

A Scheme program consists of one or more import declarations followed by
a sequence of expressions and definitions. Import declarations specify
the libraries on which a program or library depends; a subset of the
identifiers exported by the libraries are made available to the program.
Expressions are described in
chapter [\[expressionchapter\]](#expressionchapter). Definitions are
either variable definitions, syntax definitions, or record-type
definitions, all of which are explained in this chapter. They are valid
in some, but not all, contexts where expressions are allowed,
specifically at the outermost level of a program and at the beginning of
a body.

At the outermost level of a program,
`(begin expression or definition_1 \ldots\,)` is equivalent to the
sequence of expressions and definitions in the `begin`. Similarly, in a
body, `(begin definition_1 \ldots\,)` is equivalent to the sequence
definition<sub>1</sub> … . Macros can expand into such begin forms. For
the formal definition, see [\[sequencing\]](#sequencing).

Import declarations and definitions cause bindings to be created in the
global environment or modify the value of existing global bindings. The
initial environment of a program is empty, so at least one import
declaration is needed to introduce initial bindings.

Expressions occurring at the outermost level of a program do not create
any bindings. They are executed in order when the program is invoked or
loaded, and typically perform some kind of initialization.

Programs and libraries are typically stored in files, although in some
implementations they can be entered interactively into a running Scheme
system. Other paradigms are possible. Implementations which store
libraries in files should document the mapping from the name of a
library to its location in the file system.

## Import declarations

An import declaration takes the following form:

    (import \hyper{import-set} \dotsfoo)

An import declaration provides a way to import identifiers exported by a
library. Each import set names a set of bindings from a library and
possibly specifies local names for the imported bindings. It takes one
of the following forms:

-   `library name`

-   `(only import set identifier \ldots\,)`

-   `(except import set identifier \ldots\,)`

-   `(prefix import set identifier)`

-   `(rename import set  (identifier_1 identifier_2) \ldots\,)`

In the first form, all of the identifiers in the named library’s export
clauses are imported with the same names (or the exported names if
exported with `rename`). The additional import set forms modify this set
as follows:

-   `only` produces a subset of the given import set including only the
    listed identifiers (after any renaming). It is an error if any of
    the listed identifiers are not found in the original set.

-   `except` produces a subset of the given import set, excluding the
    listed identifiers (after any renaming). It is an error if any of
    the listed identifiers are not found in the original set.

-   `rename` modifies the given import set, replacing each instance of
    identifier<sub>1</sub> with identifier<sub>2</sub>. It is an error
    if any of the listed identifier<sub>1</sub>s are not found in the
    original set.

-   `prefix` automatically renames all identifiers in the given
    import set, prefixing each with the specified identifier.

In a program or library declaration, it is an error to import the same
identifier more than once with different bindings, or to redefine or
mutate an imported binding with a definition or with set!, or to refer
to an identifier before it is imported. However, a REPL should permit
these actions.

## Variable definitions

A variable definition binds one or more identifiers and specifies an
initial value for each of them. The simplest kind of variable definition
takes one of the following forms:

-   `(define variable expression)`

-   `(define (variable formals) body)`

    Formals are either a sequence of zero or more variables, or a
    sequence of one or more variables followed by a space-delimited
    period and another variable (as in a lambda expression). This form
    is equivalent to

        (define \hyper{variable}
          (lambda (\hyper{formals}) \hyper{body}))\rm.%

-   `(define (variable . formal) body)`

    Formal is a single variable. This form is equivalent to

        (define \hyper{variable}
          (lambda \hyper{formal} \hyper{body}))\rm.%

### Top level definitions

At the outermost level of a program, a definition

    (define \hyper{variable} \hyper{expression})%

has essentially the same effect as the assignment expression

    (\ide{set!}\ \hyper{variable} \hyper{expression})%

if variable is bound to a non-syntax value. However, if variable is not
bound, or is a syntactic keyword, then the definition will bind variable
to a new location before performing the assignment, whereas it would be
an error to perform a set! on an unbound variable.

    (define add3
      (lambda (x) (+ x 3)))
    (add3 3)                            \ev  6
    (define first car)
    (first '(1 2))                      \ev  1%

### Internal definitions

Definitions can occur at the beginning of a body (that is, the body of a
`lambda`, `let`, `let*`, `letrec`, `letrec*`, `let-values`,
`let*-values`, `let-syntax`, `letrec-syntax`, `parameterize`, `guard`,
or `case-lambda`). Note that such a body might not be apparent until
after expansion of other syntax. Such definitions are known as *internal
definitions* as opposed to the global definitions described above. The
variables defined by internal definitions are local to the body. That
is, variable is bound rather than assigned, and the region of the
binding is the entire body. For example,

    (let ((x 5))
      (define foo (lambda (y) (bar x y)))
      (define bar (lambda (a b) (+ (* a b) a)))
      (foo (+ x 3)))                \ev  45%

An expanded body containing internal definitions can always be converted
into a completely equivalent letrec\* expression. For example, the let
expression in the above example is equivalent to

    (let ((x 5))
      (letrec* ((foo (lambda (y) (bar x y)))
                (bar (lambda (a b) (+ (* a b) a))))
        (foo (+ x 3))))%

Just as for the equivalent letrec\* expression, it is an error if it is
not possible to evaluate each expression of every internal definition in
a body without assigning or referring to the value of the corresponding
variable or the variable of any of the definitions that follow it in
body.

It is an error to define the same identifier more than once in the same
body.

Wherever an internal definition can occur,
`(begin definition_1 \ldots\,)` is equivalent to the sequence of
definitions that form the body of the `begin`.

### Multiple-value definitions

Another kind of definition is provided by define-values, which creates
multiple definitions from a single expression returning multiple values.
It is allowed wherever define is allowed.

(define-values *formals expression*)  syntax  

It is an error if a variable appears more than once in the set of
formals.

*Semantics:* Expression is evaluated, and the formals are bound to the
return values in the same way that the formals in a lambda expression
are matched to the arguments in a procedure call.

    (define-values (x y) (exact-integer-sqrt 17))
    (list x y) \ev (4 1)

    (let ()
      (define-values (x y) (values 1 2))
      (+ x y))     \ev 3%

## Syntax definitions

Syntax definitions have this form:

`(define-syntax keyword transformer spec)`

Keyword is an identifier, and the transformer spec is an instance of
`syntax-rules`. Like variable definitions, syntax definitions can appear
at the outermost level or nested within a `body`.

If the define-syntax occurs at the outermost level, then the global
syntactic environment is extended by binding the keyword to the
specified transformer, but previous expansions of any global binding for
keyword remain unchanged. Otherwise, it is an *internal syntax
definition*, and is local to the body in which it is defined. Any use of
a syntax keyword before its corresponding definition is an error. In
particular, a use that precedes an inner definition will not apply an
outer definition.

    (let ((x 1) (y 2))
      (define-syntax swap!
        (syntax-rules ()
          ((swap! a b)
           (let ((tmp a))
             (set! a b)
             (set! b tmp)))))
      (swap! x y)
      (list x y))                \ev (2 1)%

Macros can expand into definitions in any context that permits them.
However, it is an error for a definition to define an identifier whose
binding has to be known in order to determine the meaning of the
definition itself, or of any preceding definition that belongs to the
same group of internal definitions. Similarly, it is an error for an
internal definition to define an identifier whose binding has to be
known in order to determine the boundary between the internal
definitions and the expressions of the body it belongs to. For example,
the following are errors:

    (define define 3)

    (begin (define begin list))

    (let-syntax
        ((foo (syntax-rules ()
                ((foo (proc args ...) body ...)
                 (define proc
                   (lambda (args ...)
                     body ...))))))
      (let ((x 3))
        (foo (plus x y) (+ x y))
        (define foo x)
        (plus foo x)))%

## Record-type definitions

*Record-type definitions* are used to introduce new data types, called
*record types*. Like other definitions, they can appear either at the
outermost level or in a body. The values of a record type are called
*records* and are aggregations of zero or more *fields*, each of which
holds a single location. A predicate, a constructor, and field accessors
and mutators are defined for each record type.

(define-record-type name  syntax  
)

*Syntax:* name and pred are identifiers. The constructor is of the form

    (\hyper{constructor name} \hyper{field name} \dotsfoo)%

and each field is either of the form

    (\hyper{field name} \hyper{accessor name})%

or of the form

    (\hyper{field name} \hyper{accessor name} \hyper{modifier name})%

It is an error for the same identifier to occur more than once as a
field name. It is also an error for the same identifier to occur more
than once as an accessor or mutator name.

The define-record-type construct is generative: each use creates a new
record type that is distinct from all existing types, including Scheme’s
predefined types and other record types — even record types of the same
name or structure.

An instance of define-record-type is equivalent to the following
definitions:

-   name is bound to a representation of the record type itself. This
    may be a run-time object or a purely syntactic representation. The
    representation is not utilized in this report, but it serves as a
    means to identify the record type for use by further language
    extensions.

-   constructor name is bound to a procedure that takes as many
    arguments as there are field names in the (constructor name … )
    subexpression and returns a new record of type name. Fields whose
    names are listed with constructor name have the corresponding
    argument as their initial value. The initial values of all other
    fields are unspecified. It is an error for a field name to appear in
    constructor but not as a field name.

-   pred is bound to a predicate that returns #t when given a value
    returned by the procedure bound to constructor name and #f for
    everything else.

-   Each accessor name is bound to a procedure that takes a record of
    type name and returns the current value of the corresponding field.
    It is an error to pass an accessor a value which is not a record of
    the appropriate type.

-   Each modifier name is bound to a procedure that takes a record of
    type name and a value which becomes the new value of the
    corresponding field; an unspecified value is returned. It is an
    error to pass a modifier a first argument which is not a record of
    the appropriate type.

For instance, the following record-type definition

    (define-record-type <pare>
      (kons x y)
      pare?
      (x kar set-kar!)
      (y kdr))

defines kons to be a constructor, kar and kdr to be accessors, set-kar!
to be a modifier, and pare? to be a predicate for instances of \<pare>.

      (pare? (kons 1 2))        \ev \schtrue
      (pare? (cons 1 2))        \ev \schfalse
      (kar (kons 1 2))          \ev 1
      (kdr (kons 1 2))          \ev 2
      (let ((k (kons 1 2)))
        (set-kar! k 3)
        (kar k))                \ev 3

## Libraries

Libraries provide a way to organize Scheme programs into reusable parts
with explicitly defined interfaces to the rest of the program. This
section defines the notation and semantics for libraries.

### Library Syntax

A library definition takes the following form:

    (define-library \hyper{library name}
      \hyper{library declaration} \dotsfoo)

library name is a list whose members are identifiers and exact
non-negative integers. It is used to identify the library uniquely when
importing from other programs or libraries. Libraries whose first
identifier is scheme are reserved for use by this report and future
versions of this report. Libraries whose first identifier is srfi are
reserved for libraries implementing Scheme Requests for Implementation.
It is inadvisable, but not an error, for identifiers in library names to
contain any of the characters \| `‘ ` ? \* \< " : \> + \[ \] / or
control characters after escapes are expanded.

A library declaration is any of:

-   `(export export spec \ldots\,)`

-   `(import import set \ldots\,)`

-   `(begin command or definition \ldots\,)`

-   `(include filename_1 filename_2 \ldots\,)`

-   `(include-ci filename_1 filename_2 \ldots\,)`

-   `(include-library-declarations filename_1 filename_2 \ldots\,)`

-   `(cond-expand ce-clause_1 ce-clause_2 \ldots\,)`

An `export` declaration specifies a list of identifiers which can be
made visible to other libraries or programs. An export spec takes one of
the following forms:

-   identifier

-   `(rename identifier_1 identifier_2)`

In an export spec, an identifier names a single binding defined within
or imported into the library, where the external name for the export is
the same as the name of the binding within the library. A `rename` spec
exports the binding defined within or imported into the library and
named by identifier<sub>1</sub> in each `(identifier_1 identifier_2)`
pairing, using identifier<sub>2</sub> as the external name.

An `import` declaration provides a way to import the identifiers
exported by another library. It has the same syntax and semantics as an
import declaration used in a program or at the REPL (see
section [\[import\]](#import)).

The `begin`, `include`, and `include-ci` declarations are used to
specify the body of the library. They have the same syntax and semantics
as the corresponding expression types. This form of begin is analogous
to, but not the same as, the two types of begin defined in
section [\[sequencing\]](#sequencing).

The `include-library-declarations` declaration is similar to `include`
except that the contents of the file are spliced directly into the
current library definition. This can be used, for example, to share the
same `export` declaration among multiple libraries as a simple form of
library interface.

The `cond-expand` declaration has the same syntax and semantics as the
`cond-expand` expression type, except that it expands to spliced-in
library declarations rather than expressions enclosed in begin.

One possible implementation of libraries is as follows: After all
`cond-expand` library declarations are expanded, a new environment is
constructed for the library consisting of all imported bindings. The
expressions from all `begin`, `include` and `include-ci` library
declarations are expanded in that environment in the order in which they
occur in the library. Alternatively, `cond-expand` and `import`
declarations may be processed in left to right order interspersed with
the processing of other declarations, with the environment growing as
imported bindings are added to it by each `import` declaration.

When a library is loaded, its expressions are executed in textual order.
If a library’s definitions are referenced in the expanded form of a
program or library body, then that library must be loaded before the
expanded program or library body is evaluated. This rule applies
transitively. If a library is imported by more than one program or
library, it may possibly be loaded additional times.

Similarly, during the expansion of a library (foo), if any syntax
keywords imported from another library (bar) are needed to expand the
library, then the library (bar) must be expanded and its syntax
definitions evaluated before the expansion of (foo).

Regardless of the number of times that a library is loaded, each program
or library that imports bindings from a library must do so from a single
loading of that library, regardless of the number of import declarations
in which it appears. That is, (import (only (foo) a)) followed by
(import (only (foo) b)) has the same effect as (import (only (foo) a
b)).

### Library example

The following example shows how a program can be divided into libraries
plus a relatively small main program <span class="citation"
cites="life"></span>. If the main program is entered into a REPL, it is
not necessary to import the base library.

    (define-library (example grid)
      (export make rows cols ref each
              (rename put! set!))
      (import (scheme base))
      (begin
        ;; Create an NxM grid.
        (define (make n m)
          (let ((grid (make-vector n)))
            (do ((i 0 (+ i 1)))
                ((= i n) grid)
              (let ((v (make-vector m \sharpfalse{})))
                (vector-set! grid i v)))))
        (define (rows grid)
          (vector-length grid))
        (define (cols grid)
          (vector-length (vector-ref grid 0)))
        ;; Return \sharpfalse{} if out of range.
        (define (ref grid n m)
          (and (< -1 n (rows grid))
               (< -1 m (cols grid))
               (vector-ref (vector-ref grid n) m)))
        (define (put! grid n m v)
          (vector-set! (vector-ref grid n) m v))
        (define (each grid proc)
          (do ((j 0 (+ j 1)))
              ((= j (rows grid)))
            (do ((k 0 (+ k 1)))
                ((= k (cols grid)))
              (proc j k (ref grid j k)))))))

    (define-library (example life)
      (export life)
      (import (except (scheme base) set!)
              (scheme write)
              (example grid))
      (begin
        (define (life-count grid i j)
          (define (count i j)
            (if (ref grid i j) 1 0))
          (+ (count (- i 1) (- j 1))
             (count (- i 1) j)
             (count (- i 1) (+ j 1))
             (count i (- j 1))
             (count i (+ j 1))
             (count (+ i 1) (- j 1))
             (count (+ i 1) j)
             (count (+ i 1) (+ j 1))))
        (define (life-alive? grid i j)
          (case (life-count grid i j)
            ((3) \sharptrue{})
            ((2) (ref grid i j))
            (else \sharpfalse{})))
        (define (life-print grid)
          (display "\backwhack{}x1B;[1H\backwhack{}x1B;[J")  ; clear vt100
          (each grid
           (lambda (i j v)
             (display (if v "*" " "))
             (when (= j (- (cols grid) 1))
               (newline)))))
        (define (life grid iterations)
          (do ((i 0 (+ i 1))
               (grid0 grid grid1)
               (grid1 (make (rows grid) (cols grid))
                      grid0))
              ((= i iterations))
            (each grid0
             (lambda (j k v)
               (let ((a (life-alive? grid0 j k)))
                 (set! grid1 j k a))))
            (life-print grid1)))))

    ;; Main program.
    (import (scheme base)
            (only (example life) life)
            (rename (prefix (example grid) grid-)
                    (grid-make make-grid)))

    ;; Initialize a grid with a glider.
    (define grid (make-grid 24 24))
    (grid-set! grid 1 1 \sharptrue{})
    (grid-set! grid 2 2 \sharptrue{})
    (grid-set! grid 3 0 \sharptrue{})
    (grid-set! grid 3 1 \sharptrue{})
    (grid-set! grid 3 2 \sharptrue{})

    ;; Run for 80 iterations.
    (life grid 80)

## The REPL

Implementations may provide an interactive session called a *REPL*
(Read-Eval-Print Loop), where import declarations, expressions and
definitions can be entered and evaluated one at a time. For convenience
and ease of use, the global Scheme environment in a REPL must not be
empty, but must start out with at least the bindings provided by the
base library. This library includes the core syntax of Scheme and
generally useful procedures that manipulate data. For example, the
variable abs is bound to a procedure of one argument that computes the
absolute value of a number, and the variable + is bound to a procedure
that computes sums. The full list of (scheme base) bindings can be found
in Appendix [\[stdlibraries\]](#stdlibraries).

Implementations may provide an initial REPL environment which behaves as
if all possible variables are bound to locations, most of which contain
unspecified values. Top level REPL definitions in such an implementation
are truly equivalent to assignments, unless the identifier is defined as
a syntax keyword.

An implementation may provide a mode of operation in which the REPL
reads its input from a file. Such a file is not, in general, the same as
a program, because it can contain import declarations in places other
than the beginning.

# Standard procedures

This chapter describes Scheme’s built-in procedures.

The procedures force, promise?, and make-promise are intimately
associated with the expression types delay and delay-force, and are
described with them in section [\[force\]](#force). In the same way, the
procedure make-parameter is intimately associated with the expression
type parameterize, and is described with it in
section [\[make-parameter\]](#make-parameter).

A program can use a global variable definition to bind any variable. It
may subsequently alter any such binding by an assignment (see
section [\[assignment\]](#assignment)). These operations do not modify
the behavior of any procedure defined in this report or imported from a
library (see section [\[libraries\]](#libraries)). Altering any global
binding that has not been introduced by a definition has an unspecified
effect on the behavior of the procedures defined in this chapter.

When a procedure is said to return a *newly allocated* object, it means
that the locations in the object are fresh.

## Equivalence predicates

A *predicate* is a procedure that always returns a boolean value (#t or
#f). An *equivalence predicate* is the computational analogue of a
mathematical equivalence relation; it is symmetric, reflexive, and
transitive. Of the equivalence predicates described in this section,
eq? is the finest or most discriminating, equal? is the coarsest, and
eqv? is slightly less discriminating than eq?.

(eqv? **obj<sub>1</sub>* *obj<sub>2</sub>**)  procedure  
The eqv? procedure defines a useful equivalence relation on objects.
Briefly, it returns #t if *obj<sub>1</sub>* and *obj<sub>2</sub>* are
normally regarded as the same object. This relation is left slightly
open to interpretation, but the following partial specification of eqv?
holds for all implementations of Scheme.

The eqv? procedure returns #t if:

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are both #t or both #f.

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are both symbols and are the
    same symbol according to the symbol=? procedure
    (section [\[symbolsection\]](#symbolsection)).

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are both exact numbers and
    are numerically equal (in the sense of =).

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are both inexact numbers
    such that they are numerically equal (in the sense of =) and they
    yield the same results (in the sense of eqv?) when passed as
    arguments to any other procedure that can be defined as a finite
    composition of Scheme’s standard arithmetic procedures, provided it
    does not result in a NaN value.

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are both characters and are
    the same character according to the char=? procedure
    (section [\[charactersection\]](#charactersection)).

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are both the empty list.

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are pairs, vectors,
    bytevectors, records, or strings that denote the same location in
    the store (section [\[storagemodel\]](#storagemodel)).

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are procedures whose
    location tags are equal (section [\[lambda\]](#lambda)).

The eqv? procedure returns #f if:

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are of different types
    (section [\[disjointness\]](#disjointness)).

-   one of *obj<sub>1</sub>* and *obj<sub>2</sub>* is #t but the other
    is #f.

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are symbols but are not the
    same symbol according to the symbol=? procedure
    (section [\[symbolsection\]](#symbolsection)).

-   one of *obj<sub>1</sub>* and *obj<sub>2</sub>* is an exact number
    but the other is an inexact number.

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are both exact numbers and
    are numerically unequal (in the sense of =).

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are both inexact numbers
    such that either they are numerically unequal (in the sense of =),
    or they do not yield the same results (in the sense of eqv?) when
    passed as arguments to any other procedure that can be defined as a
    finite composition of Scheme’s standard arithmetic procedures,
    provided it does not result in a NaN value. As an exception, the
    behavior of eqv? is unspecified when both *obj<sub>1</sub>* and
    *obj<sub>2</sub>* are NaN.

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are characters for which the
    char=? procedure returns #f.

-   one of *obj<sub>1</sub>* and *obj<sub>2</sub>* is the empty list but
    the other is not.

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are pairs, vectors,
    bytevectors, records, or strings that denote distinct locations.

-   *obj<sub>1</sub>* and *obj<sub>2</sub>* are procedures that would
    behave differently (return different values or have different side
    effects) for some arguments.

<!-- -->

    (eqv? 'a 'a)                     \ev  \schtrue
    (eqv? 'a 'b)                     \ev  \schfalse
    (eqv? 2 2)                       \ev  \schtrue
    (eqv? 2 2.0)                     \ev  \schfalse
    (eqv? '() '())                   \ev  \schtrue
    (eqv? 100000000 100000000)       \ev  \schtrue
    (eqv? 0.0 +nan.0)                \ev  \schfalse
    (eqv? (cons 1 2) (cons 1 2))     \ev  \schfalse
    (eqv? (lambda () 1)
          (lambda () 2))             \ev  \schfalse
    (let ((p (lambda (x) x)))
      (eqv? p p))                    \ev  \schtrue
    (eqv? \#f 'nil)                  \ev  \schfalse%

The following examples illustrate cases in which the above rules do not
fully specify the behavior of eqv?. All that can be said about such
cases is that the value returned by eqv? must be a boolean.

    (eqv? "" "")             \ev  \unspecified
    (eqv? '\#() '\#())         \ev  \unspecified
    (eqv? (lambda (x) x)
          (lambda (x) x))    \ev  \unspecified
    (eqv? (lambda (x) x)
          (lambda (y) y))    \ev  \unspecified
    (eqv? 1.0e0 1.0f0)       \ev  \unspecified
    (eqv? +nan.0 +nan.0)     \ev  \unspecified%

Note that (eqv? 0.0 -0.0) will return #f if negative zero is
distinguished, and #t if negative zero is not distinguished.

The next set of examples shows the use of eqv? with procedures that have
local state. The gen-counter procedure must return a distinct procedure
every time, since each procedure has its own internal counter. The
gen-loser procedure, however, returns operationally equivalent
procedures each time, since the local state does not affect the value or
side effects of the procedures. However, eqv? may or may not detect this
equivalence.

    (define gen-counter
      (lambda ()
        (let ((n 0))
          (lambda () (set! n (+ n 1)) n))))
    (let ((g (gen-counter)))
      (eqv? g g))           \ev  \schtrue
    (eqv? (gen-counter) (gen-counter))
                            \ev  \schfalse
    (define gen-loser
      (lambda ()
        (let ((n 0))
          (lambda () (set! n (+ n 1)) 27))))
    (let ((g (gen-loser)))
      (eqv? g g))           \ev  \schtrue
    (eqv? (gen-loser) (gen-loser))
                            \ev  \unspecified

    (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
             (g (lambda () (if (eqv? f g) 'both 'g))))
      (eqv? f g))
                            \ev  \unspecified

    (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
             (g (lambda () (if (eqv? f g) 'g 'both))))
      (eqv? f g))
                            \ev  \schfalse%

Since it is an error to modify constant objects (those returned by
literal expressions), implementations may share structure between
constants where appropriate. Thus the value of eqv? on constants is
sometimes implementation-dependent.

    (eqv? '(a) '(a))                 \ev  \unspecified
    (eqv? "a" "a")                   \ev  \unspecified
    (eqv? '(b) (cdr '(a b)))     \ev  \unspecified
    (let ((x '(a)))
      (eqv? x x))                    \ev  \schtrue%

The above definition of eqv? allows implementations latitude in their
treatment of procedures and literals: implementations may either detect
or fail to detect that two procedures or two literals are equivalent to
each other, and can decide whether or not to merge representations of
equivalent objects by using the same pointer or bit pattern to represent
both.

*Note:* If inexact numbers are represented as IEEE binary floating-point
numbers, then an implementation of eqv? that simply compares equal-sized
inexact numbers for bitwise equality is correct by the above definition.

(eq? **obj<sub>1</sub>* *obj<sub>2</sub>**)  procedure  
The eq? procedure is similar to eqv? except that in some cases it is
capable of discerning distinctions finer than those detectable by eqv?.
It must always return #f when eqv? also would, but may return #f in some
cases where eqv? would return #t.

On symbols, booleans, the empty list, pairs, and records, and also on
non-empty strings, vectors, and bytevectors, eq? and eqv? are guaranteed
to have the same behavior. On procedures, eq? must return true if the
arguments’ location tags are equal. On numbers and characters, eq?’s
behavior is implementation-dependent, but it will always return either
true or false. On empty strings, empty vectors, and empty bytevectors,
eq? may also behave differently from eqv?.

    (eq? 'a 'a)                     \ev  \schtrue
    (eq? '(a) '(a))                 \ev  \unspecified
    (eq? (list 'a) (list 'a))       \ev  \schfalse
    (eq? "a" "a")                   \ev  \unspecified
    (eq? "" "")                     \ev  \unspecified
    (eq? '() '())                   \ev  \schtrue
    (eq? 2 2)                       \ev  \unspecified
    (eq? \#\backwhack{}A \#\backwhack{}A) \ev  \unspecified
    (eq? car car)                   \ev  \schtrue
    (let ((n (+ 2 3)))
      (eq? n n))      \ev  \unspecified
    (let ((x '(a)))
      (eq? x x))      \ev  \schtrue
    (let ((x '\#()))
      (eq? x x))      \ev  \schtrue
    (let ((p (lambda (x) x)))
      (eq? p p))      \ev  \schtrue%

*Rationale:* It will usually be possible to implement eq? much more
efficiently than eqv?, for example, as a simple pointer comparison
instead of as some more complicated operation. One reason is that it is
not always possible to compute eqv? of two numbers in constant time,
whereas eq? implemented as pointer comparison will always finish in
constant time.

(equal? **obj<sub>1</sub>* *obj<sub>2</sub>**)  procedure  
The equal? procedure, when applied to pairs, vectors, strings and
bytevectors, recursively compares them, returning #t when the unfoldings
of its arguments into (possibly infinite) trees are equal (in the sense
of equal?) as ordered trees, and #f otherwise. It returns the same as
eqv? when applied to booleans, symbols, numbers, characters, ports,
procedures, and the empty list. If two objects are eqv?, they must be
equal? as well. In all other cases, equal? may return either #t or #f.

Even if its arguments are circular data structures, equal? must always
terminate.

    (equal? 'a 'a)                  \ev  \schtrue
    (equal? '(a) '(a))              \ev  \schtrue
    (equal? '(a (b) c)
            '(a (b) c))             \ev  \schtrue
    (equal? "abc" "abc")            \ev  \schtrue
    (equal? 2 2)                    \ev  \schtrue
    (equal? (make-vector 5 'a)
            (make-vector 5 'a))     \ev  \schtrue
    (equal? '\#1=(a b . \#1\#)
            '\#2=(a b a b . \#2\#))    \ev  \schtrue
    (equal? (lambda (x) x)
            (lambda (y) y))  \ev  \unspecified%

*Note:* A rule of thumb is that objects are generally equal? if they
print the same.

## Numbers

It is important to distinguish between mathematical numbers, the Scheme
numbers that attempt to model them, the machine representations used to
implement the Scheme numbers, and notations used to write numbers. This
report uses the types *number*, *complex*, *real*, *rational*, and
*integer* to refer to both mathematical numbers and Scheme numbers.

### Numerical types

Mathematically, numbers are arranged into a tower of subtypes in which
each level is a subset of the level above it:

<div class="tabbing">

         n̄umber  
complex number  
real number  
rational number  
integer

</div>

For example, 3 is an integer. Therefore 3 is also a rational, a real,
and a complex number. The same is true of the Scheme numbers that model
3. For Scheme numbers, these types are defined by the predicates
`number?`, `complex?`, `real?`, `rational?`, and `integer?`.

There is no simple relationship between a number’s type and its
representation inside a computer. Although most implementations of
Scheme will offer at least two different representations of 3, these
different representations denote the same integer.

Scheme’s numerical operations treat numbers as abstract data, as
independent of their representation as possible. Although an
implementation of Scheme may use multiple internal representations of
numbers, this ought not to be apparent to a casual programmer writing
simple programs.

### Exactness

It is useful to distinguish between numbers that are represented exactly
and those that might not be. For example, indexes into data structures
must be known exactly, as must some polynomial coefficients in a
symbolic algebra system. On the other hand, the results of measurements
are inherently inexact, and irrational numbers may be approximated by
rational and therefore inexact approximations. In order to catch uses of
inexact numbers where exact numbers are required, Scheme explicitly
distinguishes exact from inexact numbers. This distinction is orthogonal
to the dimension of type.

A Scheme number is *exact* if it was written as an exact constant or was
derived from exact numbers using only exact operations. A number is
*inexact* if it was written as an inexact constant, if it was derived
using inexact ingredients, or if it was derived using inexact
operations. Thus inexactness is a contagious property of a number. In
particular, an *exact complex number* has an exact real part and an
exact imaginary part; all other complex numbers are *inexact complex
numbers*.

If two implementations produce exact results for a computation that did
not involve inexact intermediate results, the two ultimate results will
be mathematically equal. This is generally not true of computations
involving inexact numbers since approximate methods such as
floating-point arithmetic may be used, but it is the duty of each
implementation to make the result as close as practical to the
mathematically ideal result.

Rational operations such as + should always produce exact results when
given exact arguments. If the operation is unable to produce an exact
result, then it may either report the violation of an implementation
restriction or it may silently coerce its result to an inexact value.
However, (/ 3 4) must not return the mathematically incorrect value 0.
See section [\[restrictions\]](#restrictions).

Except for `exact`, the operations described in this section must
generally return inexact results when given any inexact arguments. An
operation may, however, return an exact result if it can prove that the
value of the result is unaffected by the inexactness of its arguments.
For example, multiplication of any number by an exact zero may produce
an exact zero result, even if the other argument is inexact.

Specifically, the expression (\* 0 +inf.0) may return 0, or +nan.0, or
report that inexact numbers are not supported, or report that
non-rational real numbers are not supported, or fail silently or noisily
in other implementation-specific ways.

### Implementation restrictions

Implementations of Scheme are not required to implement the whole tower
of subtypes given in section [\[numericaltypes\]](#numericaltypes), but
they must implement a coherent subset consistent with both the purposes
of the implementation and the spirit of the Scheme language. For
example, implementations in which all numbers are real, or in which
non-real numbers are always inexact, or in which exact numbers are
always integer, are still quite useful.

Implementations may also support only a limited range of numbers of any
type, subject to the requirements of this section. The supported range
for exact numbers of any type may be different from the supported range
for inexact numbers of that type. For example, an implementation that
uses IEEE binary double-precision floating-point numbers to represent
all its inexact real numbers may also support a practically unbounded
range of exact integers and rationals while limiting the range of
inexact reals (and therefore the range of inexact integers and
rationals) to the dynamic range of the IEEE binary double format.
Furthermore, the gaps between the representable inexact integers and
rationals are likely to be very large in such an implementation as the
limits of this range are approached.

An implementation of Scheme must support exact integers throughout the
range of numbers permitted as indexes of lists, vectors, bytevectors,
and strings or that result from computing the length of one of these.
The `length`, `vector-length`, `bytevector-length`, and `string-length`
procedures must return an exact integer, and it is an error to use
anything but an exact integer as an index. Furthermore, any integer
constant within the index range, if expressed by an exact integer
syntax, must be read as an exact integer, regardless of any
implementation restrictions that apply outside this range. Finally, the
procedures listed below will always return exact integer results
provided all their arguments are exact integers and the mathematically
expected results are representable as exact integers within the
implementation:

    -                     *
    +                     abs
    ceiling               denominator
    exact-integer-sqrt    expt
    floor                 floor/
    floor-quotient        floor-remainder
    gcd                   lcm
    max                   min
    modulo                numerator
    quotient              rationalize
    remainder             round
    square                truncate
    truncate/             truncate-quotient
    truncate-remainder

It is recommended, but not required, that implementations support exact
integers and exact rationals of practically unlimited size and
precision, and to implement the above procedures and the / procedure in
such a way that they always return exact results when given exact
arguments. If one of these procedures is unable to deliver an exact
result when given exact arguments, then it may either report a violation
of an implementation restriction or it may silently coerce its result to
an inexact number; such a coercion can cause an error later.
Nevertheless, implementations that do not provide exact rational numbers
should return inexact rational numbers rather than reporting an
implementation restriction.

An implementation may use floating-point and other approximate
representation strategies for inexact numbers. This report recommends,
but does not require, that implementations that use floating-point
representations follow the IEEE 754 standard, and that implementations
using other representations should match or exceed the precision
achievable using these floating-point standards <span class="citation"
cites="IEEE"></span>. In particular, the description of transcendental
functions in IEEE 754-2008 should be followed by such implementations,
particularly with respect to infinities and NaNs.

Although Scheme allows a variety of written notations for numbers, any
particular implementation may support only some of them. For example, an
implementation in which all numbers are real need not support the
rectangular and polar notations for complex numbers. If an
implementation encounters an exact numerical constant that it cannot
represent as an exact number, then it may either report a violation of
an implementation restriction or it may silently represent the constant
by an inexact number.

### Implementation extensions

Implementations may provide more than one representation of
floating-point numbers with differing precisions. In an implementation
which does so, an inexact result must be represented with at least as
much precision as is used to express any of the inexact arguments to
that operation. Although it is desirable for potentially inexact
operations such as sqrt to produce exact answers when applied to exact
arguments, if an exact number is operated upon so as to produce an
inexact result, then the most precise representation available must be
used. For example, the value of (sqrt 4) should be 2, but in an
implementation that provides both single and double precision floating
point numbers it may be the latter but must not be the former.

It is the programmer’s responsibility to avoid using inexact number
objects with magnitude or significand too large to be represented in the
implementation.

In addition, implementations may distinguish special numbers called
positive infinity, negative infinity, NaN, and negative zero.

Positive infinity is regarded as an inexact real (but not rational)
number that represents an indeterminate value greater than the numbers
represented by all rational numbers. Negative infinity is regarded as an
inexact real (but not rational) number that represents an indeterminate
value less than the numbers represented by all rational numbers.

Adding or multiplying an infinite value by any finite real value results
in an appropriately signed infinity; however, the sum of positive and
negative infinities is a NaN. Positive infinity is the reciprocal of
zero, and negative infinity is the reciprocal of negative zero. The
behavior of the transcendental functions is sensitive to infinity in
accordance with IEEE 754.

A NaN is regarded as an inexact real (but not rational) number so
indeterminate that it might represent any real value, including positive
or negative infinity, and might even be greater than positive infinity
or less than negative infinity. An implementation that does not support
non-real numbers may use NaN to represent non-real values like (sqrt
-1.0) and (asin 2.0).

A NaN always compares false to any number, including a NaN. An
arithmetic operation where one operand is NaN returns NaN, unless the
implementation can prove that the result would be the same if the NaN
were replaced by any rational number. Dividing zero by zero results in
NaN unless both zeros are exact.

Negative zero is an inexact real value written -0.0 and is distinct (in
the sense of eqv?) from 0.0. A Scheme implementation is not required to
distinguish negative zero. If it does, however, the behavior of the
transcendental functions is sensitive to the distinction in accordance
with IEEE 754. Specifically, in a Scheme implementing both complex
numbers and negative zero, the branch cut of the complex logarithm
function is such that (imag-part (log -1.0-0.0i)) is  − *π* rather than
*π*.

Furthermore, the negation of negative zero is ordinary zero and vice
versa. This implies that the sum of two or more negative zeros is
negative, and the result of subtracting (positive) zero from a negative
zero is likewise negative. However, numerical comparisons treat negative
zero as equal to zero.

Note that both the real and the imaginary parts of a complex number can
be infinities, NaNs, or negative zero.

### Syntax of numerical constants

The syntax of the written representations for numbers is described
formally in section [\[numbersyntax\]](#numbersyntax). Note that case is
not significant in numerical constants.

A number can be written in binary, octal, decimal, or hexadecimal by the
use of a radix prefix. The radix prefixes are #b (binary), #o (octal),
#d (decimal), and #x (hexadecimal). With no radix prefix, a number is
assumed to be expressed in decimal.

A numerical constant can be specified to be either exact or inexact by a
prefix. The prefixes are #e for exact, and #i for inexact. An exactness
prefix can appear before or after any radix prefix that is used. If the
written representation of a number has no exactness prefix, the constant
is inexact if it contains a decimal point or an exponent. Otherwise, it
is exact.

In systems with inexact numbers of varying precisions it can be useful
to specify the precision of a constant. For this purpose,
implementations may accept numerical constants written with an exponent
marker that indicates the desired precision of the inexact
representation. If so, the letter s, f, d, or l, meaning *short*,
*single*, *double*, or *long* precision, respectively, can be used in
place of e. The default precision has at least as much precision as
*double*, but implementations may allow this default to be set by the
user.

    3.14159265358979F0
           {\rm Round to single ---} 3.141593
    0.6L0
           {\rm Extend to long ---} .600000000000000%

The numbers positive infinity, negative infinity, and NaN are written
+inf.0, -inf.0 and +nan.0 respectively. NaN may also be written -nan.0.
The use of signs in the written representation does not necessarily
reflect the underlying sign of the NaN value, if any. Implementations
are not required to support these numbers, but if they do, they must do
so in general conformance with IEEE 754. However, implementations are
not required to support signaling NaNs, nor to provide a way to
distinguish between different NaNs.

There are two notations provided for non-real complex numbers: the
*rectangular notation* *a*+*b*i, where *a* is the real part and *b* is
the imaginary part; and the *polar notation* *r*@*θ*, where *r* is the
magnitude and *θ* is the phase (angle) in radians. These are related by
the equation *a* + *b**i* = *r*cos *θ* + (*r*sin*θ*)*i*. All of *a*,
*b*, *r*, and *θ* are real numbers.

### Numerical operations

The reader is referred to
section [\[typeconventions\]](#typeconventions) for a summary of the
naming conventions used to specify restrictions on the types of
arguments to numerical routines. The examples used in this section
assume that any numerical constant written using an exact notation is
indeed represented as an exact number. Some examples also assume that
certain numerical constants written using an inexact notation can be
represented without loss of accuracy; the inexact constants were chosen
so that this is likely to be true in implementations that use IEEE
binary doubles to represent inexact numbers.

(number? *obj*)  procedure  
(complex? *obj*)  procedure  
(real? *obj*)  procedure  
(rational? *obj*)  procedure  
(integer? *obj*)  procedure  
These numerical type predicates can be applied to any kind of argument,
including non-numbers. They return #t if the object is of the named
type, and otherwise they return #f. In general, if a type predicate is
true of a number then all higher type predicates are also true of that
number. Consequently, if a type predicate is false of a number, then all
lower type predicates are also false of that number.

If *z* is a complex number, then (real? *z*) is true if and only if
(zero? (imag-part *z*)) is true. If *x* is an inexact real number, then
(integer? *x*) is true if and only if (= *x* (round *x*)).

The numbers +inf.0, -inf.0, and +nan.0 are real but not rational.

    (complex? 3+4i)         \ev  \schtrue
    (complex? 3)            \ev  \schtrue
    (real? 3)               \ev  \schtrue
    (real? -2.5+0i)         \ev  \schtrue
    (real? -2.5+0.0i)       \ev  \schfalse
    (real? \#e1e10)          \ev  \schtrue
    (real? +inf.0)           \ev  \schtrue
    (real? +nan.0)           \ev  \schtrue
    (rational? -inf.0)       \ev  \schfalse
    (rational? 3.5)          \ev  \schtrue
    (rational? 6/10)        \ev  \schtrue
    (rational? 6/3)         \ev  \schtrue
    (integer? 3+0i)         \ev  \schtrue
    (integer? 3.0)          \ev  \schtrue
    (integer? 8/4)          \ev  \schtrue%

*Note:* The behavior of these type predicates on inexact numbers is
unreliable, since any inaccuracy might affect the result.

*Note:* In many implementations the `complex?` procedure will be the
same as `number?`, but unusual implementations may represent some
irrational numbers exactly or may extend the number system to support
some kind of non-complex numbers.

(exact? **z**)  procedure  
(inexact? **z**)  procedure  
These numerical predicates provide tests for the exactness of a
quantity. For any Scheme number, precisely one of these predicates is
true.

    (exact? 3.0)           \ev  \schfalse
    (exact? \#e3.0)         \ev  \schtrue
    (inexact? 3.)          \ev  \schtrue%

(exact-integer? **z**)  procedure  
Returns #t if *z* is both exact and an integer; otherwise returns #f.

    (exact-integer? 32) \ev \schtrue{}
    (exact-integer? 32.0) \ev \schfalse{}
    (exact-integer? 32/5) \ev \schfalse{}%

(finite? **z**)  inexact library procedure  
The finite? procedure returns #t on all real numbers except +inf.0,
-inf.0, and +nan.0, and on complex numbers if their real and imaginary
parts are both finite. Otherwise it returns #f.

    (finite? 3)         \ev  \schtrue
    (finite? +inf.0)       \ev  \schfalse
    (finite? 3.0+inf.0i)   \ev  \schfalse%

(infinite? **z**)  inexact library procedure  
The infinite? procedure returns #t on the real numbers +inf.0 and
-inf.0, and on complex numbers if their real or imaginary parts or both
are infinite. Otherwise it returns #f.

    (infinite? 3)         \ev  \schfalse
    (infinite? +inf.0)       \ev  \schtrue
    (infinite? +nan.0)       \ev  \schfalse
    (infinite? 3.0+inf.0i)   \ev  \schtrue%

(nan? **z**)  inexact library procedure  
The nan? procedure returns #t on +nan.0, and on complex numbers if their
real or imaginary parts or both are +nan.0. Otherwise it returns #f.

    (nan? +nan.0)          \ev  \schtrue
    (nan? 32)              \ev  \schfalse
    (nan? +nan.0+5.0i)     \ev  \schtrue
    (nan? 1+2i)            \ev  \schfalse%

(= **z*<sub>1</sub> *z*<sub>2</sub> *z*<sub>3</sub> … *)  procedure  
(\< **x*<sub>1</sub> *x*<sub>2</sub> *x*<sub>3</sub> … *)  procedure  
(> **x*<sub>1</sub> *x*<sub>2</sub> *x*<sub>3</sub> … *)  procedure  
(\<= **x*<sub>1</sub> *x*<sub>2</sub> *x*<sub>3</sub> … *)  procedure  
(>= **x*<sub>1</sub> *x*<sub>2</sub> *x*<sub>3</sub> … *)  procedure  
These procedures return #t if their arguments are (respectively): equal,
monotonically increasing, monotonically decreasing, monotonically
non-decreasing, or monotonically non-increasing, and #f otherwise. If
any of the arguments are +nan.0, all the predicates return #f. They do
not distinguish between inexact zero and inexact negative zero.

These predicates are required to be transitive.

*Note:* The implementation approach of converting all arguments to
inexact numbers if any argument is inexact is not transitive. For
example, let big be (expt 2 1000), and assume that big is exact and that
inexact numbers are represented by 64-bit IEEE binary floating point
numbers. Then (= (- big 1) (inexact big)) and (= (inexact big) (+ big
1)) would both be true with this approach, because of the limitations of
IEEE representations of large integers, whereas (= (- big 1) (+ big 1))
is false. Converting inexact values to exact numbers that are the same
(in the sense of =) to them will avoid this problem, though special care
must be taken with infinities.

*Note:* While it is not an error to compare inexact numbers using these
predicates, the results are unreliable because a small inaccuracy can
affect the result; this is especially true of `=` and `zero?`. When in
doubt, consult a numerical analyst.

(zero? **z**)  procedure  
(positive? **x**)  procedure  
(negative? **x**)  procedure  
(odd? **n**)  procedure  
(even? **n**)  procedure  
These numerical predicates test a number for a particular property,
returning #t or #f. See note above.

(max **x*<sub>1</sub> *x*<sub>2</sub> … *)  procedure  
(min **x*<sub>1</sub> *x*<sub>2</sub> … *)  procedure  
These procedures return the maximum or minimum of their arguments.

    (max 3 4)              \ev  4    ; exact
    (max 3.9 4)            \ev  4.0  ; inexact%

*Note:* If any argument is inexact, then the result will also be inexact
(unless the procedure can prove that the inaccuracy is not large enough
to affect the result, which is possible only in unusual
implementations). If min or max is used to compare numbers of mixed
exactness, and the numerical value of the result cannot be represented
as an inexact number without loss of accuracy, then the procedure may
report a violation of an implementation restriction.

(+ **z*<sub>1</sub> … *)  procedure  
(\* **z*<sub>1</sub> … *)  procedure  
These procedures return the sum or product of their arguments.

    (+ 3 4)                 \ev  7
    (+ 3)                   \ev  3
    (+)                     \ev  0
    (* 4)                   \ev  4
    (*)                     \ev  1%

(- **z**)  procedure  
(- **z*<sub>1</sub> *z*<sub>2</sub> … *)  procedure  
(/ **z**)  procedure  
(/ **z*<sub>1</sub> *z*<sub>2</sub> … *)  procedure  
With two or more arguments, these procedures return the difference or
quotient of their arguments, associating to the left. With one argument,
however, they return the additive or multiplicative inverse of their
argument.

It is an error if any argument of / other than the first is an exact
zero. If the first argument is an exact zero, an implementation may
return an exact zero unless one of the other arguments is a NaN.

    (- 3 4)                 \ev  -1
    (- 3 4 5)               \ev  -6
    (- 3)                   \ev  -3
    (/ 3 4 5)               \ev  3/20
    (/ 3)                   \ev  1/3%

(abs *x*)  procedure  
The abs procedure returns the absolute value of its argument.

    (abs -7)                \ev  7%

(floor/ **n*<sub>1</sub> *n*<sub>2</sub>*)  procedure  
(floor-quotient **n*<sub>1</sub> *n*<sub>2</sub>*)  procedure  
(floor-remainder **n*<sub>1</sub> *n*<sub>2</sub>*)  procedure  
(truncate/ **n*<sub>1</sub> *n*<sub>2</sub>*)  procedure  
(truncate-quotient **n*<sub>1</sub> *n*<sub>2</sub>*)  procedure  
(truncate-remainder **n*<sub>1</sub> *n*<sub>2</sub>*)  procedure  
These procedures implement number-theoretic (integer) division. It is an
error if *n*<sub>2</sub> is zero. The procedures ending in / return two
integers; the other procedures return an integer. All the procedures
compute a quotient *n*<sub>*q*</sub> and remainder *n*<sub>*r*</sub>
such that <span class="math inline">${\\hbox{$n_1$\\/}} =
{\\hbox{$n_2$\\/}} {\\hbox{$n_q$\\/}} + {\\hbox{$n_r$\\/}}$</span>. For
each of the division operators, there are three procedures defined as
follows:

    (\hyper{operator}/ \vri{n} \vrii{n})             \ev \vr{n_q} \vr{n_r}
    (\hyper{operator}-quotient \vri{n} \vrii{n})     \ev \vr{n_q}
    (\hyper{operator}-remainder \vri{n} \vrii{n})    \ev \vr{n_r}%

The remainder *n*<sub>*r*</sub> is determined by the choice of integer
*n*<sub>*q*</sub>: <span class="math inline">${\\hbox{$n_r$\\/}} =
{\\hbox{$n_1$\\/}} - {\\hbox{$n_2$\\/}} {\\hbox{$n_q$\\/}}$</span>. Each
set of operators uses a different choice of *n*<sub>*q*</sub>:

|          |                                                                                                                   |
|:---------|:------------------------------------------------------------------------------------------------------------------|
| floor    | <span class="math inline">${\\hbox{$n_q$\\/}} = \\lfloor{\\hbox{$n_1$\\/}} / {\\hbox{$n_2$\\/}}\\rfloor$</span>   |
| truncate | <span class="math inline">${\\hbox{$n_q$\\/}} = \\text{truncate}({\\hbox{$n_1$\\/}} / {\\hbox{$n_2$\\/}})$</span> |

For any of the operators, and for integers *n*<sub>1</sub> and
*n*<sub>2</sub> with *n*<sub>2</sub> not equal to 0,

         (= \vri{n} (+ (* \vrii{n} (\hyper{operator}-quotient \vri{n} \vrii{n}))
               (\hyper{operator}-remainder \vri{n} \vrii{n})))
                                     \ev  \schtrue%

provided all numbers involved in that computation are exact.

Examples:

    (floor/ 5 2)         \ev 2 1
    (floor/ -5 2)        \ev -3 1
    (floor/ 5 -2)        \ev -3 -1
    (floor/ -5 -2)       \ev 2 -1
    (truncate/ 5 2)      \ev 2 1
    (truncate/ -5 2)     \ev -2 -1
    (truncate/ 5 -2)     \ev -2 1
    (truncate/ -5 -2)    \ev 2 -1
    (truncate/ -5.0 -2)  \ev 2.0 -1.0%

(quotient **n*<sub>1</sub> *n*<sub>2</sub>*)  procedure  
(remainder **n*<sub>1</sub> *n*<sub>2</sub>*)  procedure  
(modulo **n*<sub>1</sub> *n*<sub>2</sub>*)  procedure  
The quotient and remainder procedures are equivalent to
truncate-quotient and truncate-remainder, respectively, and modulo is
equivalent to floor-remainder.

*Note:* These procedures are provided for backward compatibility with
earlier versions of this report.

(gcd **n*<sub>1</sub> … *)  procedure  
(lcm **n*<sub>1</sub> … *)  procedure  
These procedures return the greatest common divisor or least common
multiple of their arguments. The result is always non-negative.

    (gcd 32 -36)            \ev  4
    (gcd)                   \ev  0
    (lcm 32 -36)            \ev  288
    (lcm 32.0 -36)          \ev  288.0  ; inexact
    (lcm)                   \ev  1%

(numerator **q**)  procedure  
(denominator **q**)  procedure  
These procedures return the numerator or denominator of their argument;
the result is computed as if the argument was represented as a fraction
in lowest terms. The denominator is always positive. The denominator of
0 is defined to be 1.

    (numerator (/ 6 4))  \ev  3
    (denominator (/ 6 4))  \ev  2
    (denominator
      (inexact (/ 6 4))) \ev 2.0%

(floor *x*)  procedure  
(ceiling *x*)  procedure  
(truncate *x*)  procedure  
(round *x*)  procedure  
These procedures return integers. The floor procedure returns the
largest integer not larger than *x*. The ceiling procedure returns the
smallest integer not smaller than *x*, truncate returns the integer
closest to *x* whose absolute value is not larger than the absolute
value of *x*, and round returns the closest integer to *x*, rounding to
even when *x* is halfway between two integers.

*Rationale:* The round procedure rounds to even for consistency with the
default rounding mode specified by the IEEE 754 IEEE floating-point
standard.

*Note:* If the argument to one of these procedures is inexact, then the
result will also be inexact. If an exact value is needed, the result can
be passed to the exact procedure. If the argument is infinite or a NaN,
then it is returned.

    (floor -4.3)          \ev  -5.0
    (ceiling -4.3)        \ev  -4.0
    (truncate -4.3)       \ev  -4.0
    (round -4.3)          \ev  -4.0

    (floor 3.5)           \ev  3.0
    (ceiling 3.5)         \ev  4.0
    (truncate 3.5)        \ev  3.0
    (round 3.5)           \ev  4.0  ; inexact

    (round 7/2)           \ev  4    ; exact
    (round 7)             \ev  7%

(rationalize *x y*)  procedure  
The rationalize procedure returns the *simplest* rational number
differing from *x* by no more than *y*. A rational number
*r*<sub>1</sub> is *simpler* than another rational number
*r*<sub>2</sub> if *r*<sub>1</sub> = *p*<sub>1</sub>/*q*<sub>1</sub> and
*r*<sub>2</sub> = *p*<sub>2</sub>/*q*<sub>2</sub> (in lowest terms) and
\|*p*<sub>1</sub>\| ≤ \|*p*<sub>2</sub>\| and
\|*q*<sub>1</sub>\| ≤ \|*q*<sub>2</sub>\|. Thus 3/5 is simpler than 4/7.
Although not all rationals are comparable in this ordering (consider 2/7
and 3/5), any interval contains a rational number that is simpler than
every other rational number in that interval (the simpler 2/5 lies
between 2/7 and 3/5). Note that 0 = 0/1 is the simplest rational of all.

    (rationalize
      (exact .3) 1/10)  \ev 1/3    ; exact
    (rationalize .3 1/10)        \ev \#i1/3  ; inexact%

(exp **z**)  inexact library procedure  
(log **z**)  inexact library procedure  
(log **z*<sub>1</sub> *z*<sub>2</sub>*)  inexact library procedure  
(sin **z**)  inexact library procedure  
(cos **z**)  inexact library procedure  
(tan **z**)  inexact library procedure  
(asin **z**)  inexact library procedure  
(acos **z**)  inexact library procedure  
(atan **z**)  inexact library procedure  
(atan **y* *x**)  inexact library procedure  
These procedures compute the usual transcendental functions. The log
procedure computes the natural logarithm of *z* (not the base ten
logarithm) if a single argument is given, or the base-*z*<sub>2</sub>
logarithm of *z*<sub>1</sub> if two arguments are given. The asin, acos,
and atan procedures compute arcsine (sin<sup>−1</sup>), arc-cosine
(cos<sup>−1</sup>), and arctangent (tan<sup>−1</sup>), respectively. The
two-argument variant of atan computes `(angle (make-rectangular x y))`
(see below), even in implementations that don’t support complex numbers.

In general, the mathematical functions log, arcsine, arc-cosine, and
arctangent are multiply defined. The value of log *z* is defined to be
the one whose imaginary part lies in the range from  − *π* (inclusive if
-0.0 is distinguished, exclusive otherwise) to *π* (inclusive). The
value of log 0 is mathematically undefined. With log  defined this way,
the values of sin<sup>−1</sup>*z*, cos<sup>−1</sup>*z*, and
tan<sup>−1</sup>*z* are according to the following formulæ:
$$\\sin^{- 1}z = - i\\log\\left( iz + \\sqrt{1 - z^{2}} \\right)$$
cos<sup>−1</sup>*z* = *π*/2 − sin<sup>−1</sup>*z*
tan<sup>−1</sup>*z* = (log(1+*i**z*)−log(1−*i**z*))/(2*i*)

However, (log 0.0) returns -inf.0 (and (log -0.0) returns -inf.0+*π*i)
if the implementation supports infinities (and -0.0).

The range of (atan *y* *x*) is as in the following table. The asterisk
(\*) indicates that the entry applies to implementations that
distinguish minus zero.

<div class="center">

|     | *y* condition | *x* condition | range of result *r*                |
|:---:|:--------------|:--------------|:-----------------------------------|
|     | *y* = 0.0     | *x* \> 0.0    | 0.0                                |
| \*  | *y* =  + 0.0  | *x* \> 0.0    |  + 0.0                             |
| \*  | *y* =  − 0.0  | *x* \> 0.0    |  − 0.0                             |
|     | *y* \> 0.0    | *x* \> 0.0    | $0.0 \< r \< \\frac{\\pi}{2}$      |
|     | *y* \> 0.0    | *x* = 0.0     | $\\frac{\\pi}{2}$                  |
|     | *y* \> 0.0    | *x* \< 0.0    | $\\frac{\\pi}{2} \< r \< \\pi$     |
|     | *y* = 0.0     | *x* \< 0      | *π*                                |
| \*  | *y* =  + 0.0  | *x* \< 0.0    | *π*                                |
| \*  | *y* =  − 0.0  | *x* \< 0.0    |  − *π*                             |
|     | *y* \< 0.0    | *x* \< 0.0    | $- \\pi \< r \< - \\frac{\\pi}{2}$ |
|     | *y* \< 0.0    | *x* = 0.0     | $- \\frac{\\pi}{2}$                |
|     | *y* \< 0.0    | *x* \> 0.0    | $- \\frac{\\pi}{2} \< r \< 0.0$    |
|     | *y* = 0.0     | *x* = 0.0     | undefined                          |
| \*  | *y* =  + 0.0  | *x* =  + 0.0  |  + 0.0                             |
| \*  | *y* =  − 0.0  | *x* =  + 0.0  |  − 0.0                             |
| \*  | *y* =  + 0.0  | *x* =  − 0.0  | *π*                                |
| \*  | *y* =  − 0.0  | *x* =  − 0.0  |  − *π*                             |
| \*  | *y* =  + 0.0  | *x* = 0       | $\\frac{\\pi}{2}$                  |
| \*  | *y* =  − 0.0  | *x* = 0       | $- \\frac{\\pi}{2}$                |

</div>

The above specification follows <span class="citation"
cites="CLtL"></span>, which in turn cites <span class="citation"
cites="Penfield81"></span>; refer to these sources for more detailed
discussion of branch cuts, boundary conditions, and implementation of
these functions. When it is possible, these procedures produce a real
result from a real argument.

(square **z**)  procedure  
Returns the square of *z*. This is equivalent to (\* *z* *z*).

    (square 42)       \ev 1764
    (square 2.0)     \ev 4.0%

(sqrt **z**)  inexact library procedure  
Returns the principal square root of *z*. The result will have either a
positive real part, or a zero real part and a non-negative imaginary
part.

    (sqrt 9)  \ev 3
    (sqrt -1) \ev +i%

(exact-integer-sqrt *k*)  procedure  
Returns two non-negative exact integers *s* and *r* where <span
class="math inline">$\\hbox{\\it{}k\\/} = s^2 + r$</span> and <span
class="math inline">$\\hbox{\\it{}k\\/} \< (s+1)^2$</span>.

    (exact-integer-sqrt 4) \ev 2 0
    (exact-integer-sqrt 5) \ev 2 1%

(expt **z*<sub>1</sub> *z*<sub>2</sub>*)  procedure  
Returns *z*<sub>1</sub> raised to the power *z*<sub>2</sub>. For nonzero
*z*<sub>1</sub>, this is
*z*<sub>1</sub><sup>*z*<sub>2</sub></sup> = *e*<sup>*z*<sub>2</sub>log *z*<sub>1</sub></sup>
The value of 0<sup>*z*</sup> is 1 if (zero? z), 0 if (real-part z) is
positive, and an error otherwise. Similarly for 0.0<sup>*z*</sup>, with
inexact results.

(make-rectangular **x*<sub>1</sub> *x*<sub>2</sub>*)
 complex library procedure  
(make-polar **x*<sub>3</sub> *x*<sub>4</sub>*)
 complex library procedure  
(real-part **z**)  complex library procedure  
(imag-part **z**)  complex library procedure  
(magnitude **z**)  complex library procedure  
(angle **z**)  complex library procedure  
Let *x*<sub>1</sub>, *x*<sub>2</sub>, *x*<sub>3</sub>, and
*x*<sub>4</sub> be real numbers and *z* be a complex number such that
<span class="math display">$${\\hbox{$z$\\/}} = {\\hbox{$x_1$\\/}} +
{\\hbox{$x_2$\\/}}\\hbox{$i$} = {\\hbox{$x_3$\\/}} \\cdot e^{i
x_4}$$</span> Then all of

    (make-rectangular \vri{x} \vrii{x}) \ev \vr{z}
    (make-polar \vriii{x} \vriv{x})     \ev \vr{z}
    (real-part \vr{z})                  \ev \vri{x}
    (imag-part \vr{z})                  \ev \vrii{x}
    (magnitude \vr{z})                  \ev $|\vriii{x}|$
    (angle \vr{z})                      \ev $x_{angle}$%

are true, where  − *π* ≤ *x*<sub>*a**n**g**l**e*</sub> ≤ *π* with <span
class="math inline">$x\_{angle} = {\\hbox{$x_4$\\/}} + 2\\pi n$</span>
for some integer *n*.

The make-polar procedure may return an inexact complex number even if
its arguments are exact. The real-part and imag-part procedures may
return exact real numbers when applied to an inexact complex number if
the corresponding argument passed to make-rectangular was exact.

*Rationale:* The magnitude procedure is the same as `abs` for a real
argument, but abs is in the base library, whereas magnitude is in the
optional complex library.

(inexact **z**)  procedure  
(exact **z**)  procedure  
The procedure inexact returns an inexact representation of *z*. The
value returned is the inexact number that is numerically closest to the
argument. For inexact arguments, the result is the same as the argument.
For exact complex numbers, the result is a complex number whose real and
imaginary parts are the result of applying inexact to the real and
imaginary parts of the argument, respectively. If an exact argument has
no reasonably close inexact equivalent (in the sense of =), then a
violation of an implementation restriction may be reported.

The procedure exact returns an exact representation of *z*. The value
returned is the exact number that is numerically closest to the
argument. For exact arguments, the result is the same as the argument.
For inexact non-integral real arguments, the implementation may return a
rational approximation, or may report an implementation violation. For
inexact complex arguments, the result is a complex number whose real and
imaginary parts are the result of applying exact to the real and
imaginary parts of the argument, respectively. If an inexact argument
has no reasonably close exact equivalent, (in the sense of =), then a
violation of an implementation restriction may be reported.

These procedures implement the natural one-to-one correspondence between
exact and inexact integers throughout an implementation-dependent range.
See section [\[restrictions\]](#restrictions).

*Note:* These procedures were known in R<sup>5</sup>RS as exact->inexact
and inexact->exact, respectively, but they have always accepted
arguments of any exactness. The new names are clearer and shorter, as
well as being compatible with R<sup>6</sup>RS.

### Numerical input and output

(number->string *z*)  procedure  
(number->string *z radix*)  procedure  
It is an error if *r**a**d**i**x* is not one of 2, 8, 10, or 16.

The procedure numberstring takes a number and a radix and returns as a
string an external representation of the given number in the given radix
such that

    (let ((number \vr{number})
          (radix \vr{radix}))
      (eqv? number
            (string->number (number->string number
                                            radix)
                            radix)))%

is true. It is an error if no possible result makes this expression
true. If omitted, *r**a**d**i**x* defaults to 10.

If *z* is inexact, the radix is 10, and the above expression can be
satisfied by a result that contains a decimal point, then the result
contains a decimal point and is expressed using the minimum number of
digits (exclusive of exponent and trailing zeroes) needed to make the
above expression true <span class="citation"
cites="howtoprint howtoread"></span>; otherwise the format of the result
is unspecified.

The result returned by numberstring never contains an explicit radix
prefix.

*Note:* The error case can occur only when *z* is not a complex number
or is a complex number with a non-rational real or imaginary part.

*Rationale:* If *z* is an inexact number and the radix is 10, then the
above expression is normally satisfied by a result containing a decimal
point. The unspecified case allows for infinities, NaNs, and unusual
representations.

(string->number *string*)  procedure  
(string->number *string radix*)  procedure  
Returns a number of the maximally precise representation expressed by
the given *s**t**r**i**n**g*. It is an error if *r**a**d**i**x* is not
2, 8, 10, or 16.

If supplied, *r**a**d**i**x* is a default radix that will be overridden
if an explicit radix prefix is present in *s**t**r**i**n**g* (e.g.
`"#o177"`). If *r**a**d**i**x* is not supplied, then the default radix
is 10. If *s**t**r**i**n**g* is not a syntactically valid notation for a
number, or would result in a number that the implementation cannot
represent, then string->number returns #f. An error is never signaled
due to the content of *s**t**r**i**n**g*.

    (string->number "100")        \ev  100
    (string->number "100" 16)     \ev  256
    (string->number "1e2")        \ev  100.0%

*Note:* The domain of string->number may be restricted by
implementations in the following ways. If all numbers supported by an
implementation are real, then string->number is permitted to return #f
whenever *s**t**r**i**n**g* uses the polar or rectangular notations for
complex numbers. If all numbers are integers, then string->number may
return #f whenever the fractional notation is used. If all numbers are
exact, then string->number may return #f whenever an exponent marker or
explicit exactness prefix is used. If all inexact numbers are integers,
then string->number may return #f whenever a decimal point is used.

The rules used by a particular implementation for string->number must
also be applied to read and to the routine that reads programs, in order
to maintain consistency between internal numeric processing, I/O, and
the processing of programs. As a consequence, the
R<sup>5</sup>RS permission to return #f when *string* has an explicit
radix prefix has been withdrawn.

## Booleans

The standard boolean objects for true and false are written as #t and
#f. Alternatively, they can be written #true and #false, respectively.
What really matters, though, are the objects that the Scheme conditional
expressions (if, cond, and, or, when, unless, do) treat as true or
false. The phrase “a true value” (or sometimes just “true”) means any
object treated as true by the conditional expressions, and the phrase “a
false value” (or “false”) means any object treated as false by the
conditional expressions.

Of all the Scheme values, only #f counts as false in conditional
expressions. All other Scheme values, including #t, count as true.

*Note:* Unlike some other dialects of Lisp, Scheme distinguishes #f and
the empty list from each other and from the symbol `nil`.

Boolean constants evaluate to themselves, so they do not need to be
quoted in programs.

    \schtrue         \ev  \schtrue
    \schfalse        \ev  \schfalse
    '\schfalse       \ev  \schfalse%

(not *obj*)  procedure  
The not procedure returns #t if *obj* is false, and returns #f
otherwise.

    (not \schtrue)   \ev  \schfalse
    (not 3)          \ev  \schfalse
    (not (list 3))   \ev  \schfalse
    (not \schfalse)  \ev  \schtrue
    (not '())        \ev  \schfalse
    (not (list))     \ev  \schfalse
    (not 'nil)       \ev  \schfalse%

(boolean? *obj*)  procedure  
The boolean? predicate returns #t if *obj* is either #t or #f and
returns #f otherwise.

    (boolean? \schfalse)  \ev  \schtrue
    (boolean? 0)          \ev  \schfalse
    (boolean? '())        \ev  \schfalse%

(boolean=? **boolean<sub>1</sub>* *boolean<sub>2</sub>* *boolean<sub>3</sub>* … *)
 procedure  
Returns #t if all the arguments are #t or all are #f.

## Pairs and lists

A *pair* (sometimes called a *dotted pair*) is a record structure with
two fields called the car and cdr fields (for historical reasons). Pairs
are created by the procedure cons. The car and cdr fields are accessed
by the procedures car and cdr. The car and cdr fields are assigned by
the procedures set-car! and set-cdr!.

Pairs are used primarily to represent lists. A *list* can be defined
recursively as either the empty list or a pair whose cdr is a list. More
precisely, the set of lists is defined as the smallest set *X* such that

-   The empty list is in *X*.

-   If *list* is in *X*, then any pair whose cdr field contains *list*
    is also in *X*.

The objects in the car fields of successive pairs of a list are the
elements of the list. For example, a two-element list is a pair whose
car is the first element and whose cdr is a pair whose car is the second
element and whose cdr is the empty list. The length of a list is the
number of elements, which is the same as the number of pairs.

The empty list is a special object of its own type. It is not a pair, it
has no elements, and its length is zero.

*Note:* The above definitions imply that all lists have finite length
and are terminated by the empty list.

The most general notation (external representation) for Scheme pairs is
the “dotted” notation (*c<sub>1</sub>* . *c<sub>2</sub>*) where
*c<sub>1</sub>* is the value of the car field and *c<sub>2</sub>* is the
value of the cdr field. For example (4 . 5) is a pair whose car is 4 and
whose cdr is 5. Note that (4 . 5) is the external representation of a
pair, not an expression that evaluates to a pair.

A more streamlined notation can be used for lists: the elements of the
list are simply enclosed in parentheses and separated by spaces. The
empty list is written `()`. For example,

    (a b c d e)%

and

    (a . (b . (c . (d . (e . ())))))%

are equivalent notations for a list of symbols.

A chain of pairs not ending in the empty list is called an *improper
list*. Note that an improper list is not a list. The list and dotted
notations can be combined to represent improper lists:

    (a b c . d)%

is equivalent to

    (a . (b . (c . d)))%

Whether a given pair is a list depends upon what is stored in the cdr
field. When the `set-cdr!` procedure is used, an object can be a list
one moment and not the next:

    (define x (list 'a 'b 'c))
    (define y x)
    y                       \ev  (a b c)
    (list? y)               \ev  \schtrue
    (set-cdr! x 4)          \ev  \unspecified
    x                       \ev  (a . 4)
    (eqv? x y)              \ev  \schtrue
    y                       \ev  (a . 4)
    (list? y)               \ev  \schfalse
    (set-cdr! x x)          \ev  \unspecified
    (list? x)               \ev  \schfalse%

Within literal expressions and representations of objects read by the
`read` procedure, the forms `’`datum, ``datum, `,`datum, and `,@`datum
denote two-element lists whose first elements are the symbols `quote`,
`quasiquote`, `unquote`, and `unquote-splicing`, respectively. The
second element in each case is datum. This convention is supported so
that arbitrary Scheme programs can be represented as lists. That is,
according to Scheme’s grammar, every expression is also a datum (see
section [\[datum\]](#datum)). Among other things, this permits the use
of the read procedure to parse Scheme programs. See
section [\[externalreps\]](#externalreps).

(pair? *obj*)  procedure  
The pair? predicate returns #t if *obj* is a pair, and otherwise returns
#f.

    (pair? '(a . b))        \ev  \schtrue
    (pair? '(a b c))        \ev  \schtrue
    (pair? '())             \ev  \schfalse
    (pair? '\#(a b))         \ev  \schfalse%

(cons **obj<sub>1</sub>* *obj<sub>2</sub>**)  procedure  
Returns a newly allocated pair whose car is *obj<sub>1</sub>* and whose
cdr is *obj<sub>2</sub>*. The pair is guaranteed to be different (in the
sense of eqv?) from every existing object.

    (cons 'a '())           \ev  (a)
    (cons '(a) '(b c d))    \ev  ((a) b c d)
    (cons "a" '(b c))       \ev  ("a" b c)
    (cons 'a 3)             \ev  (a . 3)
    (cons '(a b) 'c)        \ev  ((a b) . c)%

(car *pair*)  procedure  
Returns the contents of the car field of *pair*. Note that it is an
error to take the car of the empty list.

    (car '(a b c))          \ev  a
    (car '((a) b c d))      \ev  (a)
    (car '(1 . 2))          \ev  1
    (car '())               \ev  \scherror%

(cdr *pair*)  procedure  
Returns the contents of the cdr field of *pair*. Note that it is an
error to take the cdr of the empty list.

    (cdr '((a) b c d))      \ev  (b c d)
    (cdr '(1 . 2))          \ev  2
    (cdr '())               \ev  \scherror%

(set-car! *pair obj*)  procedure  
Stores *obj* in the car field of *pair*.

    (define (f) (list 'not-a-constant-list))
    (define (g) '(constant-list))
    (set-car! (f) 3)             \ev  \unspecified
    (set-car! (g) 3)             \ev  \scherror%

(set-cdr! *pair obj*)  procedure  
Stores *obj* in the cdr field of *pair*.

`(cadr pair)` procedure

(caar *pair*)  procedure  
(cadr *pair*)  procedure  
(cdar *pair*)  procedure  
(cddr *pair*)  procedure  
These procedures are compositions of car and cdr as follows:

    (define (caar x) (car (car x)))
    (define (cadr x) (car (cdr x)))
    (define (cdar x) (cdr (car x)))
    (define (cddr x) (cdr (cdr x)))%

(caaar *pair*)  cxr library procedure  
(caadr *pair*)  cxr library procedure  
to 1 ⋮  to 1 ⋮  
(cdddar *pair*)  cxr library procedure  
(cddddr *pair*)  cxr library procedure  
These twenty-four procedures are further compositions of car and cdr on
the same principles. For example, caddr could be defined by

    (define caddr (lambda (x) (car (cdr (cdr x))))){\rm.}%

Arbitrary compositions up to four deep are provided.

(null? *obj*)  procedure  
Returns #t if *obj* is the empty list, otherwise returns #f.

(list? *obj*)  procedure  
Returns #t if *obj* is a list. Otherwise, it returns #f. By definition,
all lists have finite length and are terminated by the empty list.

            (list? '(a b c))     \ev  \schtrue
            (list? '())          \ev  \schtrue
            (list? '(a . b))     \ev  \schfalse
            (let ((x (list 'a)))
              (set-cdr! x x)
              (list? x))         \ev  \schfalse%

(make-list *k*)  procedure  
(make-list *k fill*)  procedure  
Returns a newly allocated list of *k* elements. If a second argument is
given, then each element is initialized to *fill*. Otherwise the initial
contents of each element is unspecified.

    (make-list 2 3)   \ev   (3 3)%

(list **obj* … *)  procedure  
Returns a newly allocated list of its arguments.

    (list 'a (+ 3 4) 'c)            \ev  (a 7 c)
    (list)                          \ev  ()%

(length *list*)  procedure  
Returns the length of *list*.

    (length '(a b c))               \ev  3
    (length '(a (b) (c d e)))       \ev  3
    (length '())                    \ev  0%

(append *list … *)  procedure  
The last argument, if there is one, can be of any type.

Returns a list consisting of the elements of the first *list* followed
by the elements of the other *list*s. If there are no arguments, the
empty list is returned. If there is exactly one argument, it is
returned. Otherwise the resulting list is always newly allocated, except
that it shares structure with the last argument. An improper list
results if the last argument is not a proper list.

    (append '(x) '(y))              \ev  (x y)
    (append '(a) '(b c d))          \ev  (a b c d)
    (append '(a (b)) '((c)))        \ev  (a (b) (c))%

    (append '(a b) '(c . d))        \ev  (a b c . d)
    (append '() 'a)                 \ev  a%

(reverse *list*)  procedure  
Returns a newly allocated list consisting of the elements of *list* in
reverse order.

    (reverse '(a b c))              \ev  (c b a)
    (reverse '(a (b c) d (e (f))))  \lev  ((e (f)) d (b c) a)%

(list-tail *list *k**)  procedure  
It is an error if *list* has fewer than *k* elements.

Returns the sublist of *list* obtained by omitting the first *k*
elements. The list-tail procedure could be defined by

    (define list-tail
      (lambda (x k)
        (if (zero? k)
            x
            (list-tail (cdr x) (- k 1)))))%

(list-ref *list *k**)  procedure  
The *list* argument can be circular, but it is an error if *list* has
*k* or fewer elements.

Returns the *k*th element of *list*. (This is the same as the car of
`(list-tail list k)`.)

    (list-ref '(a b c d) 2)                 \ev  c
    (list-ref '(a b c d)
              (exact (round 1.8))) \lev  c%

(list-set! *list k obj*)  procedure  
It is an error if *k* is not a valid index of *list*.

The list-set! procedure stores *obj* in element *k* of *list*.

    (let ((ls (list 'one 'two 'five!)))
      (list-set! ls 2 'three)
      ls)      \lev  (one two three)

    (list-set! '(0 1 2) 1 "oops")  \lev  \scherror  ; constant list%

(memq *obj list*)  procedure  
(memv *obj list*)  procedure  
(member *obj list*)  procedure  
(member *obj list compare*)  procedure  
These procedures return the first sublist of *list* whose car is *obj*,
where the sublists of *list* are the non-empty lists returned by
`(list-tail list k)` for *k* less than the length of *list*. If *obj*
does not occur in *list*, then #f (not the empty list) is returned. The
memq procedure uses eq? to compare *obj* with the elements of *list*,
while memv uses eqv? and member uses *compare*, if given, and equal?
otherwise.

    (memq 'a '(a b c))              \ev  (a b c)
    (memq 'b '(a b c))              \ev  (b c)
    (memq 'a '(b c d))              \ev  \schfalse
    (memq (list 'a) '(b (a) c))     \ev  \schfalse
    (member (list 'a)
            '(b (a) c))             \ev  ((a) c)
    (member "B"
            '("a" "b" "c")
            string-ci=?)            \ev  ("b" "c")
    (memq 101 '(100 101 102))       \ev  \unspecified
    (memv 101 '(100 101 102))       \ev  (101 102)%

(assq *obj alist*)  procedure  
(assv *obj alist*)  procedure  
(assoc *obj alist*)  procedure  
(assoc *obj alist compare*)  procedure  
It is an error if *alist* (for “association list”) is not a list of
pairs.

These procedures find the first pair in *alist* whose car field is
*obj*, and returns that pair. If no pair in *alist* has *obj* as its
car, then #f (not the empty list) is returned. The assq procedure uses
eq? to compare *obj* with the car fields of the pairs in *alist*, while
assv uses eqv? and assoc uses *compare* if given and equal? otherwise.

    (define e '((a 1) (b 2) (c 3)))
    (assq 'a e)     \ev  (a 1)
    (assq 'b e)     \ev  (b 2)
    (assq 'd e)     \ev  \schfalse
    (assq (list 'a) '(((a)) ((b)) ((c))))
                    \ev  \schfalse
    (assoc (list 'a) '(((a)) ((b)) ((c))))   
                               \ev  ((a))
    (assoc 2.0 '((1 1) (2 4) (3 9)) =)
                               \ev (2 4)
    (assq 5 '((2 3) (5 7) (11 13)))    
                               \ev  \unspecified
    (assv 5 '((2 3) (5 7) (11 13)))    
                               \ev  (5 7)%

*Rationale:* Although they are often used as predicates, memq, memv,
member, assq, assv, and assoc do not have question marks in their names
because they return potentially useful values rather than just #t or #f.

(list-copy *obj*)  procedure  
Returns a newly allocated copy of the given *obj* if it is a list. Only
the pairs themselves are copied; the cars of the result are the same (in
the sense of eqv?) as the cars of *list*. If *obj* is an improper list,
so is the result, and the final cdrs are the same in the sense of eqv?.
An *obj* which is not a list is returned unchanged. It is an error if
*obj* is a circular list.

    (define a '(1 8 2 8)) ; a may be immutable
    (define b (list-copy a))
    (set-car! b 3)        ; b is mutable
    b \ev (3 8 2 8)
    a \ev (1 8 2 8)%

## Symbols

Symbols are objects whose usefulness rests on the fact that two symbols
are identical (in the sense of eqv?) if and only if their names are
spelled the same way. For instance, they can be used the way enumerated
values are used in other languages.

The rules for writing a symbol are exactly the same as the rules for
writing an identifier; see sections [\[syntaxsection\]](#syntaxsection)
and [\[identifiersyntax\]](#identifiersyntax).

It is guaranteed that any symbol that has been returned as part of a
literal expression, or read using the read procedure, and subsequently
written out using the write procedure, will read back in as the
identical symbol (in the sense of eqv?).

*Note:* Some implementations have values known as “uninterned symbols,”
which defeat write/read invariance, and also violate the rule that two
symbols are the same if and only if their names are spelled the same.
This report does not specify the behavior of implementation-dependent
extensions.

(symbol? *obj*)  procedure  
Returns #t if *obj* is a symbol, otherwise returns #f.

    (symbol? 'foo)          \ev  \schtrue
    (symbol? (car '(a b)))  \ev  \schtrue
    (symbol? "bar")         \ev  \schfalse
    (symbol? 'nil)          \ev  \schtrue
    (symbol? '())           \ev  \schfalse
    (symbol? \schfalse)     \ev  \schfalse%

(symbol=? **symbol<sub>1</sub>* *symbol<sub>2</sub>* *symbol<sub>3</sub>* … *)
 procedure  
Returns #t if all the arguments all have the same names in the sense of
string=?.

*Note:* The definition above assumes that none of the arguments are
uninterned symbols.

(symbol->string *symbol*)  procedure  
Returns the name of *symbol* as a string, but without adding escapes. It
is an error to apply mutation procedures like `string-set!` to strings
returned by this procedure.

    (symbol->string 'flying-fish)     
                                      \ev  "flying-fish"
    (symbol->string 'Martin)          \ev  "Martin"
    (symbol->string
       (string->symbol "Malvina"))     
                                      \ev  "Malvina"%

(string->symbol *string*)  procedure  
Returns the symbol whose name is *string*. This procedure can create
symbols with names containing special characters that would require
escaping when written, but does not interpret escapes in its input.

    (string->symbol "mISSISSIppi")  \lev%
      mISSISSIppi
    (eqv? 'bitBlt (string->symbol "bitBlt"))     \lev  \schtrue
    (eqv? 'LollyPop
         (string->symbol
           (symbol->string 'LollyPop)))  \lev  \schtrue
    (string=? "K. Harper, M.D."
              (symbol->string
                (string->symbol "K. Harper, M.D.")))  \lev  \schtrue%

## Characters

Characters are objects that represent printed characters such as letters
and digits. All Scheme implementations must support at least the ASCII
character repertoire: that is, Unicode characters U+0000 through U+007F.
Implementations may support any other Unicode characters they see fit,
and may also support non-Unicode characters as well. Except as otherwise
specified, the result of applying any of the following procedures to a
non-Unicode character is implementation-dependent.

Characters are written using the notation `#``‘ `character or
`#``‘ `character name or `#``‘ `xhex scalar value.

The following character names must be supported by all implementations
with the given values. Implementations may add other names provided they
cannot be interpreted as hex scalar values preceded by x.

|     |                                                             |
|:----|:------------------------------------------------------------|
|     |                                                             |
|     | ; <span class="roman">U+0007</span>                         |
|     |                                                             |
|     | ; <span class="roman">U+0008</span>                         |
|     |                                                             |
|     | ; <span class="roman">U+007F</span>                         |
|     |                                                             |
|     | ; <span class="roman">U+001B</span>                         |
|     |                                                             |
|     | ; the linefeed character, <span class="roman">U+000A</span> |
|     |                                                             |
|     | ; the null character, <span class="roman">U+0000</span>     |
|     |                                                             |
|     | ; the return character, <span class="roman">U+000D</span>   |
|     |                                                             |
|     | ; the preferred way to write a space                        |
|     |                                                             |
|     | ; the tab character, <span class="roman">U+0009</span>      |

Here are some additional examples:

|     |                                             |
|:----|:--------------------------------------------|
|     |                                             |
|     | ; lower case letter                         |
|     |                                             |
|     | ; upper case letter                         |
|     |                                             |
|     | ; left parenthesis                          |
|     |                                             |
|     | ; the space character                       |
|     |                                             |
|     | ; *λ* (if character is supported)           |
|     |                                             |
|     | ; *ι* (if character and name are supported) |

Case is significant in `#``‘ `character, and in `#``‘ `⟨character name⟩,
but not in `#``‘ `xhex scalar value. If character in `#``‘ `character is
alphabetic, then any character immediately following character cannot be
one that can appear in an identifier. This rule resolves the ambiguous
case where, for example, the sequence of characters “`#‘ space`” could
be taken to be either a representation of the space character or a
representation of the character “`#‘ s`” followed by a representation of
the symbol “`pace`.”

Characters written in the `#``‘ ` notation are self-evaluating. That is,
they do not have to be quoted in programs.

Some of the procedures that operate on characters ignore the difference
between upper case and lower case. The procedures that ignore case have
“`-ci`” (for “case insensitive”) embedded in their names.

(char? *obj*)  procedure  
Returns #t if *obj* is a character, otherwise returns #f.

(char=? **c**h**a**r*<sub>1</sub> *c**h**a**r*<sub>2</sub> *c**h**a**r*<sub>3</sub> … *)
 procedure  
(char\<? **c**h**a**r*<sub>1</sub> *c**h**a**r*<sub>2</sub> *c**h**a**r*<sub>3</sub> … *)
 procedure  
(char>? **c**h**a**r*<sub>1</sub> *c**h**a**r*<sub>2</sub> *c**h**a**r*<sub>3</sub> … *)
 procedure  
(char\<=? **c**h**a**r*<sub>1</sub> *c**h**a**r*<sub>2</sub> *c**h**a**r*<sub>3</sub> … *)
 procedure  
(char>=? **c**h**a**r*<sub>1</sub> *c**h**a**r*<sub>2</sub> *c**h**a**r*<sub>3</sub> … *)
 procedure  
These procedures return #t if the results of passing their arguments to
charinteger are respectively equal, monotonically increasing,
monotonically decreasing, monotonically non-decreasing, or monotonically
non-increasing.

These predicates are required to be transitive.

(char-ci=? **c**h**a**r*<sub>1</sub> *c**h**a**r*<sub>2</sub> *c**h**a**r*<sub>3</sub> … *)
 char library procedure  
(char-ci\<? **c**h**a**r*<sub>1</sub> *c**h**a**r*<sub>2</sub> *c**h**a**r*<sub>3</sub> … *)
 char library procedure  
(char-ci>? **c**h**a**r*<sub>1</sub> *c**h**a**r*<sub>2</sub> *c**h**a**r*<sub>3</sub> … *)
 char library procedure  
(char-ci\<=? **c**h**a**r*<sub>1</sub> *c**h**a**r*<sub>2</sub> *c**h**a**r*<sub>3</sub> … *)
 char library procedure  
(char-ci>=? **c**h**a**r*<sub>1</sub> *c**h**a**r*<sub>2</sub> *c**h**a**r*<sub>3</sub> … *)
 char library procedure  
These procedures are similar to char=? et cetera, but they treat upper
case and lower case letters as the same. For example, (char-ci=? #`‘ `A
#`‘ `a) returns #t.

Specifically, these procedures behave as if char-foldcase were applied
to their arguments before they were compared.

(char-alphabetic? *char*)  char library procedure  
(char-numeric? *char*)  char library procedure  
(char-whitespace? *char*)  char library procedure  
(char-upper-case? *letter*)  char library procedure  
(char-lower-case? *letter*)  char library procedure  
These procedures return #t if their arguments are alphabetic, numeric,
whitespace, upper case, or lower case characters, respectively,
otherwise they return #f.

Specifically, they must return #t when applied to characters with the
Unicode properties Alphabetic, Numeric_Type=Decimal, White_Space,
Uppercase, and Lowercase respectively, and #f when applied to any other
Unicode characters. Note that many Unicode characters are alphabetic but
neither upper nor lower case.

(digit-value *char*)  char library procedure  
This procedure returns the numeric value (0 to 9) of its argument if it
is a numeric digit (that is, if char-numeric? returns #t), or #f on any
other character.

    (digit-value \#\backwhack{}3) \ev 3
    (digit-value \#\backwhack{}x0664) \ev 4
    (digit-value \#\backwhack{}x0AE6) \ev 0
    (digit-value \#\backwhack{}x0EA6) \ev \schfalse%

(char->integer *char*)  procedure  
(integer->char **n**)  procedure  
Given a Unicode character, charinteger returns an exact integer between
0 and `#xD7FF` or between `#xE000` and `#x10FFFF` which is equal to the
Unicode scalar value of that character. Given a non-Unicode character,
it returns an exact integer greater than `#x10FFFF`. This is true
independent of whether the implementation uses the Unicode
representation internally.

Given an exact integer that is the value returned by a character when
charinteger is applied to it, integerchar returns that character.

(char-upcase *char*)  char library procedure  
(char-downcase *char*)  char library procedure  
(char-foldcase *char*)  char library procedure  
The char-upcase procedure, given an argument that is the lowercase part
of a Unicode casing pair, returns the uppercase member of the pair,
provided that both characters are supported by the Scheme
implementation. Note that language-sensitive casing pairs are not used.
If the argument is not the lowercase member of such a pair, it is
returned.

The char-downcase procedure, given an argument that is the uppercase
part of a Unicode casing pair, returns the lowercase member of the pair,
provided that both characters are supported by the Scheme
implementation. Note that language-sensitive casing pairs are not used.
If the argument is not the uppercase member of such a pair, it is
returned.

The char-foldcase procedure applies the Unicode simple case-folding
algorithm to its argument and returns the result. Note that
language-sensitive folding is not used. If the character that results
from folding is not supported by the implementation, the argument is
returned. See UAX #44 <span class="citation" cites="uax44"></span> (part
of the Unicode Standard) for details.

Note that many Unicode lowercase characters do not have uppercase
equivalents.

## Strings

Strings are sequences of characters. Strings are written as sequences of
characters enclosed within quotation marks ("). Within a string literal,
various escape sequences represent characters other than themselves.
Escape sequences always start with a backslash (`‘ `):

-   `‘ `a : alarm, U+0007

-   `‘ `b : backspace, U+0008

-   `‘ `t : character tabulation, U+0009

-   `‘ `n : linefeed, U+000A

-   `‘ `r : return, U+000D

-   `‘ ``"` : double quote, U+0022

-   `‘ ``‘ ` : backslash, U+005C

-   `‘ `\| : vertical line, U+007C

-   `‘ `intraline whitespaceline ending intraline whitespace : nothing

-   `‘ `xhex scalar value; : specified character (note the terminating
    semi-colon).

The result is unspecified if any other character in a string occurs
after a backslash.

Except for a line ending, any character outside of an escape sequence
stands for itself in the string literal. A line ending which is preceded
by `‘ `intraline whitespace expands to nothing (along with any trailing
intraline whitespace), and can be used to indent strings for improved
legibility. Any other line ending has the same effect as inserting a
`‘ `n character into the string.

Examples:

    "The word \backwhack{}"recursion\backwhack{}" has many meanings."
    "Another example:\backwhack{}ntwo lines of text"
    "Here's text \backwhack{} 
       containing just one line"
    "\backwhack{}x03B1; is named GREEK SMALL LETTER ALPHA."%

The *length* of a string is the number of characters that it contains.
This number is an exact, non-negative integer that is fixed when the
string is created. The *valid indexes* of a string are the exact
non-negative integers less than the length of the string. The first
character of a string has index 0, the second has index 1, and so on.

Some of the procedures that operate on strings ignore the difference
between upper and lower case. The names of the versions that ignore case
end with “-ci” (for “case insensitive”).

Implementations may forbid certain characters from appearing in strings.
However, with the exception of `#‘ null`, ASCII characters must not be
forbidden. For example, an implementation might support the entire
Unicode repertoire, but only allow characters U+0001 to U+00FF (the
Latin-1 repertoire without `#‘ null`) in strings.

It is an error to pass such a forbidden character to make-string,
string, string-set!, or string-fill!, as part of the list passed to
liststring, or as part of the vector passed to vectorstring (see
section [\[vectortostring\]](#vectortostring)), or in UTF-8 encoded form
within a bytevector passed to utf8string (see
section [\[utf8tostring\]](#utf8tostring)). It is also an error for a
procedure passed to string-map (see section [\[stringmap\]](#stringmap))
to return a forbidden character, or for read-string (see
section [\[readstring\]](#readstring)) to attempt to read one.

(string? *obj*)  procedure  
Returns #t if *obj* is a string, otherwise returns #f.

(make-string **k**)  procedure  
(make-string **k* char*)  procedure  
The make-string procedure returns a newly allocated string of length
*k*. If *char* is given, then all the characters of the string are
initialized to *char*, otherwise the contents of the string are
unspecified.

(string *char … *)  procedure  
Returns a newly allocated string composed of the arguments. It is
analogous to list.

(string-length *string*)  procedure  
Returns the number of characters in the given *string*.

(string-ref *string *k**)  procedure  
It is an error if *k* is not a valid index of *string*.

The string-ref procedure returns character *k* of *string* using
zero-origin indexing. There is no requirement for this procedure to
execute in constant time.

(string-set! *string k char*)  procedure  
It is an error if *k* is not a valid index of *string*.

The string-set! procedure stores *char* in element *k* of *string*.
There is no requirement for this procedure to execute in constant time.

    (define (f) (make-string 3 \sharpsign\backwhack{}*))
    (define (g) "***")
    (string-set! (f) 0 \sharpsign\backwhack{}?)  \ev  \unspecified
    (string-set! (g) 0 \sharpsign\backwhack{}?)  \ev  \scherror
    (string-set! (symbol->string 'immutable)
                 0
                 \sharpsign\backwhack{}?)  \ev  \scherror%

(string=? **s**t**r**i**n**g*<sub>1</sub> *s**t**r**i**n**g*<sub>2</sub> *s**t**r**i**n**g*<sub>3</sub> … *)
 procedure  
Returns #t if all the strings are the same length and contain exactly
the same characters in the same positions, otherwise returns #f.

(string-ci=? **s**t**r**i**n**g*<sub>1</sub> *s**t**r**i**n**g*<sub>2</sub> *s**t**r**i**n**g*<sub>3</sub> … *)
 char library procedure  
Returns #t if, after case-folding, all the strings are the same length
and contain the same characters in the same positions, otherwise returns
#f. Specifically, these procedures behave as if string-foldcase were
applied to their arguments before comparing them.

(string\<? **s**t**r**i**n**g*<sub>1</sub> *s**t**r**i**n**g*<sub>2</sub> *s**t**r**i**n**g*<sub>3</sub> … *)
 procedure  
(string-ci\<? **s**t**r**i**n**g*<sub>1</sub> *s**t**r**i**n**g*<sub>2</sub> *s**t**r**i**n**g*<sub>3</sub> … *)
 char library procedure  
(string>? **s**t**r**i**n**g*<sub>1</sub> *s**t**r**i**n**g*<sub>2</sub> *s**t**r**i**n**g*<sub>3</sub> … *)
 procedure  
(string-ci>? **s**t**r**i**n**g*<sub>1</sub> *s**t**r**i**n**g*<sub>2</sub> *s**t**r**i**n**g*<sub>3</sub> … *)
 char library procedure  
(string\<=? **s**t**r**i**n**g*<sub>1</sub> *s**t**r**i**n**g*<sub>2</sub> *s**t**r**i**n**g*<sub>3</sub> … *)
 procedure  
(string-ci\<=? **s**t**r**i**n**g*<sub>1</sub> *s**t**r**i**n**g*<sub>2</sub> *s**t**r**i**n**g*<sub>3</sub> … *)
 char library procedure  
(string>=? **s**t**r**i**n**g*<sub>1</sub> *s**t**r**i**n**g*<sub>2</sub> *s**t**r**i**n**g*<sub>3</sub> … *)
 procedure  
(string-ci>=? **s**t**r**i**n**g*<sub>1</sub> *s**t**r**i**n**g*<sub>2</sub> *s**t**r**i**n**g*<sub>3</sub> … *)
 char library procedure  
These procedures return #t if their arguments are (respectively):
monotonically increasing, monotonically decreasing, monotonically
non-decreasing, or monotonically non-increasing.

These predicates are required to be transitive.

These procedures compare strings in an implementation-defined way. One
approach is to make them the lexicographic extensions to strings of the
corresponding orderings on characters. In that case, string\<? would be
the lexicographic ordering on strings induced by the ordering char\<? on
characters, and if the two strings differ in length but are the same up
to the length of the shorter string, the shorter string would be
considered to be lexicographically less than the longer string. However,
it is also permitted to use the natural ordering imposed by the
implementation’s internal representation of strings, or a more complex
locale-specific ordering.

In all cases, a pair of strings must satisfy exactly one of string\<?,
string=?, and string>?, and must satisfy string\<=? if and only if they
do not satisfy string>? and string>=? if and only if they do not satisfy
string\<?.

The “`-ci`” procedures behave as if they applied string-foldcase to
their arguments before invoking the corresponding procedures without
“`-ci`”.

(string-upcase *string*)  char library procedure  
(string-downcase *string*)  char library procedure  
(string-foldcase *string*)  char library procedure  
These procedures apply the Unicode full string uppercasing, lowercasing,
and case-folding algorithms to their arguments and return the result. In
certain cases, the result differs in length from the argument. If the
result is equal to the argument in the sense of string=?, the argument
may be returned. Note that language-sensitive mappings and foldings are
not used.

The Unicode Standard prescribes special treatment of the Greek letter
*Σ*, whose normal lower-case form is *σ* but which becomes ς at the end
of a word. See UAX #44 <span class="citation" cites="uax44"></span>
(part of the Unicode Standard) for details. However, implementations of
string-downcase are not required to provide this behavior, and may
choose to change *Σ* to *σ* in all cases.

(substring *string start end*)  procedure  
The substring procedure returns a newly allocated string formed from the
characters of *string* beginning with index *start* and ending with
index *end*. This is equivalent to calling string-copy with the same
arguments, but is provided for backward compatibility and stylistic
flexibility.

(string-append **string* … *)  procedure  
Returns a newly allocated string whose characters are the concatenation
of the characters in the given strings.

(string->list *string*)  procedure  
(string->list *string start*)  procedure  
(string->list *string start end*)  procedure  
(list->string *list*)  procedure  
It is an error if any element of *list* is not a character.

The stringlist procedure returns a newly allocated list of the
characters of *string* between *start* and *end*. liststring returns a
newly allocated string formed from the elements in the list *list*. In
both procedures, order is preserved. stringlist and liststring are
inverses so far as equal? is concerned.

(string-copy *string*)  procedure  
(string-copy *string start*)  procedure  
(string-copy *string start end*)  procedure  
Returns a newly allocated copy of the part of the given *string* between
*start* and *end*.

(string-copy! *to at from*)  procedure  
(string-copy! *to at from start*)  procedure  
(string-copy! *to at from start end*)  procedure  
It is an error if *at* is less than zero or greater than the length of
*to*. It is also an error if (- (string-length *to*) *at*) is less than
(- *end* *start*).

Copies the characters of string *from* between *start* and *end* to
string *to*, starting at *at*. The order in which characters are copied
is unspecified, except that if the source and destination overlap,
copying takes place as if the source is first copied into a temporary
string and then into the destination. This can be achieved without
allocating storage by making sure to copy in the correct direction in
such circumstances.

    (define a "12345")
    (define b (string-copy "abcde"))
    (string-copy! b 1 a 0 2)
    b \ev "a12de"%

(string-fill! *string fill*)  procedure  
(string-fill! *string fill start*)  procedure  
(string-fill! *string fill start end*)  procedure  
It is an error if *fill* is not a character.

The string-fill! procedure stores *fill* in the elements of *string*
between *start* and *end*.

## Vectors

Vectors are heterogeneous structures whose elements are indexed by
integers. A vector typically occupies less space than a list of the same
length, and the average time needed to access a randomly chosen element
is typically less for the vector than for the list.

The *length* of a vector is the number of elements that it contains.
This number is a non-negative integer that is fixed when the vector is
created. The *valid indexes* of a vector are the exact non-negative
integers less than the length of the vector. The first element in a
vector is indexed by zero, and the last element is indexed by one less
than the length of the vector.

Vectors are written using the notation `#(obj \ldots\,)`. For example, a
vector of length 3 containing the number zero in element 0, the list (2
2 2 2) in element 1, and the string "Anna" in element 2 can be written
as follows:

    \#(0 (2 2 2 2) "Anna")%

Vector constants are self-evaluating, so they do not need to be quoted
in programs.

(vector? *obj*)  procedure  
Returns #t if *obj* is a vector; otherwise returns #f.

(make-vector *k*)  procedure  
(make-vector *k fill*)  procedure  
Returns a newly allocated vector of *k* elements. If a second argument
is given, then each element is initialized to *fill*. Otherwise the
initial contents of each element is unspecified.

(vector *obj … *)  procedure  
Returns a newly allocated vector whose elements contain the given
arguments. It is analogous to list.

    (vector 'a 'b 'c)               \ev  \#(a b c)%

(vector-length *vector*)  procedure  
Returns the number of elements in *vector* as an exact integer.

(vector-ref *vector k*)  procedure  
It is an error if *k* is not a valid index of *vector*.

The vector-ref procedure returns the contents of element *k* of
*vector*.

    (vector-ref '\#(1 1 2 3 5 8 13 21)
                5)  \lev  8
    (vector-ref '\#(1 1 2 3 5 8 13 21)
                (exact
                 (round (* 2 (acos -1))))) \lev 13%

(vector-set! *vector k obj*)  procedure  
It is an error if *k* is not a valid index of *vector*.

The vector-set! procedure stores *obj* in element *k* of *vector*.

    (let ((vec (vector 0 '(2 2 2 2) "Anna")))
      (vector-set! vec 1 '("Sue" "Sue"))
      vec)      \lev  \#(0 ("Sue" "Sue") "Anna")

    (vector-set! '\#(0 1 2) 1 "doe")  \lev  \scherror  ; constant vector%

(vector->list *vector*)  procedure  
(vector->list *vector start*)  procedure  
(vector->list *vector start end*)  procedure  
(list->vector *list*)  procedure  
The vector->list procedure returns a newly allocated list of the objects
contained in the elements of *vector* between *start* and *end*. The
list->vector procedure returns a newly created vector initialized to the
elements of the list *list*.

In both procedures, order is preserved.

    (vector->list '\#(dah dah didah))  \lev  (dah dah didah)
    (vector->list '\#(dah dah didah) 1 2) \lev (dah)
    (list->vector '(dididit dah))   \lev  \#(dididit dah)%

(vector->string *vector*)  procedure  
(vector->string *vector start*)  procedure  
(vector->string *vector start end*)  procedure  
(string->vector *string*)  procedure  
(string->vector *string start*)  procedure  
(string->vector *string start end*)  procedure  
It is an error if any element of *vector* between *start* and *end* is
not a character.

The vector->string procedure returns a newly allocated string of the
objects contained in the elements of *vector* between *start* and *end*.
The string->vector procedure returns a newly created vector initialized
to the elements of the string *string* between *start* and *end*.

In both procedures, order is preserved.

    (string->vector "ABC")  \ev   \#(\#\backwhack{}A \#\backwhack{}B \#\backwhack{}C)
    (vector->string
      \#(\#\backwhack{}1 \#\backwhack{}2 \#\backwhack{}3) \ev "123"

(vector-copy *vector*)  procedure  
(vector-copy *vector start*)  procedure  
(vector-copy *vector start end*)  procedure  
Returns a newly allocated copy of the elements of the given *vector*
between *start* and *end*. The elements of the new vector are the same
(in the sense of eqv?) as the elements of the old.

    (define a \#(1 8 2 8)) ; a may be immutable
    (define b (vector-copy a))
    (vector-set! b 0 3)   ; b is mutable
    b \ev \#(3 8 2 8)
    (define c (vector-copy b 1 3))
    c \ev \#(8 2)%

(vector-copy! *to at from*)  procedure  
(vector-copy! *to at from start*)  procedure  
(vector-copy! *to at from start end*)  procedure  
It is an error if *at* is less than zero or greater than the length of
*to*. It is also an error if (- (vector-length *to*) *at*) is less than
(- *end* *start*).

Copies the elements of vector *from* between *start* and *end* to vector
*to*, starting at *at*. The order in which elements are copied is
unspecified, except that if the source and destination overlap, copying
takes place as if the source is first copied into a temporary vector and
then into the destination. This can be achieved without allocating
storage by making sure to copy in the correct direction in such
circumstances.

    (define a (vector 1 2 3 4 5))
    (define b (vector 10 20 30 40 50))
    (vector-copy! b 1 a 0 2)
    b \ev \#(10 1 2 40 50)%

(vector-append **vector* … *)  procedure  
Returns a newly allocated vector whose elements are the concatenation of
the elements of the given vectors.

    (vector-append \#(a b c) \#(d e f)) \lev \#(a b c d e f)%

(vector-fill! *vector fill*)  procedure  
(vector-fill! *vector fill start*)  procedure  
(vector-fill! *vector fill start end*)  procedure  
The vector-fill! procedure stores *fill* in the elements of *vector*
between *start* and *end*.

    (define a (vector 1 2 3 4 5))
    (vector-fill! a 'smash 2 4)
    a \lev \#(1 2 smash smash 5)%

## Bytevectors

*Bytevectors* represent blocks of binary data. They are fixed-length
sequences of bytes, where a *byte* is an exact integer in the range from
0 to 255 inclusive. A bytevector is typically more space-efficient than
a vector containing the same values.

The *length* of a bytevector is the number of elements that it contains.
This number is a non-negative integer that is fixed when the bytevector
is created. The *valid indexes* of a bytevector are the exact
non-negative integers less than the length of the bytevector, starting
at index zero as with vectors.

Bytevectors are written using the notation `#u8(byte \ldots\,)`. For
example, a bytevector of length 3 containing the byte 0 in element 0,
the byte 10 in element 1, and the byte 5 in element 2 can be written as
follows:

    \#u8(0 10 5)%

Bytevector constants are self-evaluating, so they do not need to be
quoted in programs.

(bytevector? *obj*)  procedure  
Returns #t if *obj* is a bytevector. Otherwise, #f is returned.

(make-bytevector *k*)  procedure  
(make-bytevector *k byte*)  procedure  
The make-bytevector procedure returns a newly allocated bytevector of
length *k*. If *byte* is given, then all elements of the bytevector are
initialized to *byte*, otherwise the contents of each element are
unspecified.

    (make-bytevector 2 12) \ev \#u8(12 12)%

(bytevector **byte* … *)  procedure  
Returns a newly allocated bytevector containing its arguments.

    (bytevector 1 3 5 1 3 5)        \ev  \#u8(1 3 5 1 3 5)
    (bytevector)                          \ev  \#u8()%

(bytevector-length *bytevector*)  procedure  
Returns the length of *bytevector* in bytes as an exact integer.

(bytevector-u8-ref *bytevector k*)  procedure  
It is an error if *k* is not a valid index of *bytevector*.

Returns the *k*th byte of *bytevector*.

    (bytevector-u8-ref '\#u8(1 1 2 3 5 8 13 21)
                5)  \lev  8%

(bytevector-u8-set! *bytevector k byte*)  procedure  
It is an error if *k* is not a valid index of *bytevector*.

Stores *byte* as the *k*th byte of *bytevector*.

    (let ((bv (bytevector 1 2 3 4)))
      (bytevector-u8-set! bv 1 3)
      bv) \lev \#u8(1 3 3 4)%

(bytevector-copy *bytevector*)  procedure  
(bytevector-copy *bytevector start*)  procedure  
(bytevector-copy *bytevector start end*)  procedure  
Returns a newly allocated bytevector containing the bytes in
*bytevector* between *start* and *end*.

    (define a \#u8(1 2 3 4 5))
    (bytevector-copy a 2 4)) \ev \#u8(3 4)%

(bytevector-copy! *to at from*)  procedure  
(bytevector-copy! *to at from start*)  procedure  
(bytevector-copy! *to at from start end*)  procedure  
It is an error if *at* is less than zero or greater than the length of
*to*. It is also an error if (- (bytevector-length *to*) *at*) is less
than (- *end* *start*).

Copies the bytes of bytevector *from* between *start* and *end* to
bytevector *to*, starting at *at*. The order in which bytes are copied
is unspecified, except that if the source and destination overlap,
copying takes place as if the source is first copied into a temporary
bytevector and then into the destination. This can be achieved without
allocating storage by making sure to copy in the correct direction in
such circumstances.

    (define a (bytevector 1 2 3 4 5))
    (define b (bytevector 10 20 30 40 50))
    (bytevector-copy! b 1 a 0 2)
    b \ev \#u8(10 1 2 40 50)%

*Note:* This procedure appears in R<sup>6</sup>RS, but places the source
before the destination, contrary to other such procedures in Scheme.

(bytevector-append **bytevector* … *)  procedure  
Returns a newly allocated bytevector whose elements are the
concatenation of the elements in the given bytevectors.

    (bytevector-append \#u8(0 1 2) \#u8(3 4 5)) \lev \#u8(0 1 2 3 4 5)%

(utf8->string *bytevector*)  procedure  
(utf8->string *bytevector start*)  procedure  
(utf8->string *bytevector start end*)  procedure  
(string->utf8 *string*)  procedure  
(string->utf8 *string start*)  procedure  
(string->utf8 *string start end*)  procedure  
It is an error for *bytevector* to contain invalid UTF-8 byte sequences.

These procedures translate between strings and bytevectors that encode
those strings using the UTF-8 encoding. The utf8string procedure decodes
the bytes of a bytevector between *start* and *end* and returns the
corresponding string; the stringutf8 procedure encodes the characters of
a string between *start* and *end* and returns the corresponding
bytevector.

    (utf8->string \#u8(\#x41)) \ev "A"
    (string->utf8 "$\lambda$") \ev \#u8(\#xCE \#xBB)%

## Control features

This section describes various primitive procedures which control the
flow of program execution in special ways. Procedures in this section
that invoke procedure arguments always do so in the same dynamic
environment as the call of the original procedure. The
procedure? predicate is also described here.

(procedure? *obj*)  procedure  
Returns #t if *obj* is a procedure, otherwise returns #f.

    (procedure? car)            \ev  \schtrue
    (procedure? 'car)           \ev  \schfalse
    (procedure? (lambda (x) (* x x)))   
                                \ev  \schtrue
    (procedure? '(lambda (x) (* x x)))  
                                \ev  \schfalse
    (call-with-current-continuation procedure?)
                                \ev  \schtrue%

(apply *proc *arg<sub>1</sub>* … args*)  procedure  
The apply procedure calls *proc* with the elements of the list (append
(list *arg<sub>1</sub>* … ) *args*) as the actual arguments.

    (apply + (list 3 4))              \ev  7

    (define compose
      (lambda (f g)
        (lambda args
          (f (apply g args)))))

    ((compose sqrt *) 12 75)              \ev  30%

(map *proc *list<sub>1</sub>* *list<sub>2</sub>* … *)  procedure  
It is an error if *proc* does not accept as many arguments as there are
*list*s and return a single value.

The map procedure applies *proc* element-wise to the elements of the
*list*s and returns a list of the results, in order. If more than one
*list* is given and not all lists have the same length, map terminates
when the shortest list runs out. The *list*s can be circular, but it is
an error if all of them are circular. It is an error for *proc* to
mutate any of the lists. The dynamic order in which *proc* is applied to
the elements of the *list*s is unspecified. If multiple returns occur
from map, the values returned by earlier returns are not mutated.

    (map cadr '((a b) (d e) (g h)))   \lev  (b e h)

    (map (lambda (n) (expt n n))
         '(1 2 3 4 5))                \lev  (1 4 27 256 3125)

    (map + '(1 2 3) '(4 5 6 7))         \ev  (5 7 9)

    (let ((count 0))
      (map (lambda (ignored)
             (set! count (+ count 1))
             count)
           '(a b)))                 \ev  (1 2) \var{or} (2 1)%

(string-map *proc *string<sub>1</sub>* *string<sub>2</sub>* … *)
 procedure  
It is an error if *proc* does not accept as many arguments as there are
*string*s and return a single character.

The string-map procedure applies *proc* element-wise to the elements of
the *string*s and returns a string of the results, in order. If more
than one *string* is given and not all strings have the same length,
string-map terminates when the shortest string runs out. The dynamic
order in which *proc* is applied to the elements of the *string*s is
unspecified. If multiple returns occur from string-map, the values
returned by earlier returns are not mutated.

    (string-map char-foldcase "AbdEgH") \lev  "abdegh"

    (string-map
     (lambda (c)
       (integer->char (+ 1 (char->integer c))))
     "HAL")                \lev  "IBM"

    (string-map
     (lambda (c k)
       ((if (eqv? k \sharpsign\backwhack{}u) char-upcase char-downcase)
        c))
     "studlycaps xxx"
     "ululululul")   \lev   "StUdLyCaPs"%

(vector-map *proc *vector<sub>1</sub>* *vector<sub>2</sub>* … *)
 procedure  
It is an error if *proc* does not accept as many arguments as there are
*vector*s and return a single value.

The vector-map procedure applies *proc* element-wise to the elements of
the *vector*s and returns a vector of the results, in order. If more
than one *vector* is given and not all vectors have the same length,
vector-map terminates when the shortest vector runs out. The dynamic
order in which *proc* is applied to the elements of the *vector*s is
unspecified. If multiple returns occur from vector-map, the values
returned by earlier returns are not mutated.

    (vector-map cadr '\#((a b) (d e) (g h)))   \lev  \#(b e h)

    (vector-map (lambda (n) (expt n n))
                '\#(1 2 3 4 5))                \lev  \#(1 4 27 256 3125)

    (vector-map + '\#(1 2 3) '\#(4 5 6 7))       \lev  \#(5 7 9)

    (let ((count 0))
      (vector-map
       (lambda (ignored)
         (set! count (+ count 1))
         count)
       '\#(a b)))                     \ev  \#(1 2) \var{or} \#(2 1)%

(for-each *proc *list<sub>1</sub>* *list<sub>2</sub>* … *)  procedure  
It is an error if *proc* does not accept as many arguments as there are
*list*s.

The arguments to for-each are like the arguments to map, but for-each
calls *proc* for its side effects rather than for its values. Unlike
map, for-each is guaranteed to call *proc* on the elements of the
*list*s in order from the first element(s) to the last, and the value
returned by for-each is unspecified. If more than one *list* is given
and not all lists have the same length, for-each terminates when the
shortest list runs out. The *list*s can be circular, but it is an error
if all of them are circular.

It is an error for *proc* to mutate any of the lists.

    (let ((v (make-vector 5)))
      (for-each (lambda (i)
                  (vector-set! v i (* i i)))
                '(0 1 2 3 4))
      v)                                \ev  \#(0 1 4 9 16)%

(string-for-each *proc *string<sub>1</sub>* *string<sub>2</sub>* … *)
 procedure  
It is an error if *proc* does not accept as many arguments as there are
*string*s.

The arguments to string-for-each are like the arguments to string-map,
but string-for-each calls *proc* for its side effects rather than for
its values. Unlike string-map, string-for-each is guaranteed to call
*proc* on the elements of the *string*s in order from the first
element(s) to the last, and the value returned by string-for-each is
unspecified. If more than one *string* is given and not all strings have
the same length, string-for-each terminates when the shortest string
runs out. It is an error for *proc* to mutate any of the strings.

    (let ((v '()))
      (string-for-each
       (lambda (c) (set! v (cons (char->integer c) v)))
       "abcde")
      v)                         \ev  (101 100 99 98 97)%

(vector-for-each *proc *vector<sub>1</sub>* *vector<sub>2</sub>* … *)
 procedure  
It is an error if *proc* does not accept as many arguments as there are
*vector*s.

The arguments to vector-for-each are like the arguments to vector-map,
but vector-for-each calls *proc* for its side effects rather than for
its values. Unlike vector-map, vector-for-each is guaranteed to call
*proc* on the elements of the *vector*s in order from the first
element(s) to the last, and the value returned by vector-for-each is
unspecified. If more than one *vector* is given and not all vectors have
the same length, vector-for-each terminates when the shortest vector
runs out. It is an error for *proc* to mutate any of the vectors.

    (let ((v (make-list 5)))
      (vector-for-each
       (lambda (i) (list-set! v i (* i i)))
       '\#(0 1 2 3 4))
      v)                                \ev  (0 1 4 9 16)%

(call-with-current-continuation *proc*)  procedure  
(call/cc *proc*)  procedure  
It is an error if *proc* does not accept one argument.

The procedure call-with-current-continuation (or its equivalent
abbreviation call/cc) packages the current continuation (see the
rationale below) as an “escape procedure” and passes it as an argument
to *proc*. The escape procedure is a Scheme procedure that, if it is
later called, will abandon whatever continuation is in effect at that
later time and will instead use the continuation that was in effect when
the escape procedure was created. Calling the escape procedure will
cause the invocation of *before* and *after* thunks installed using
`dynamic-wind`.

The escape procedure accepts the same number of arguments as the
continuation to the original call to `call-with-current-continuation`.
Most continuations take only one value. Continuations created by the
call-with-values procedure (including the initialization expressions of
define-values, let-values, and let\*-values expressions), take the
number of values that the consumer expects. The continuations of all
non-final expressions within a sequence of expressions, such as in
lambda, case-lambda, begin, let, let\*, letrec, letrec\*, let-values,
let\*-values, let-syntax, letrec-syntax, parameterize, guard, case,
cond, when, and unless expressions, take an arbitrary number of values
because they discard the values passed to them in any event. The effect
of passing no values or more than one value to continuations that were
not created in one of these ways is unspecified.

The escape procedure that is passed to *proc* has unlimited extent just
like any other procedure in Scheme. It can be stored in variables or
data structures and can be called as many times as desired. However,
like the raise and error procedures, it never returns to its caller.

The following examples show only the simplest ways in which
call-with-current-continuation is used. If all real uses were as simple
as these examples, there would be no need for a procedure with the power
of call-with-current-continuation.

    (call-with-current-continuation
      (lambda (exit)
        (for-each (lambda (x)
                    (if (negative? x)
                        (exit x)))
                  '(54 0 37 -3 245 19))
        \schtrue))                        \ev  -3

    (define list-length
      (lambda (obj)
        (call-with-current-continuation
          (lambda (return)
            (letrec ((r
                      (lambda (obj)
                        (cond ((null? obj) 0)
                              ((pair? obj)
                               (+ (r (cdr obj)) 1))
                              (else (return \schfalse))))))
              (r obj))))))

    (list-length '(1 2 3 4))            \ev  4

    (list-length '(a b . c))            \ev  \schfalse%

*Rationale:*

A common use of call-with-current-continuation is for structured,
non-local exits from loops or procedure bodies, but in fact
call-with-current-continuation is useful for implementing a wide variety
of advanced control structures. In fact, raise and guard provide a more
structured mechanism for non-local exits.

Whenever a Scheme expression is evaluated there is a *continuation*
wanting the result of the expression. The continuation represents an
entire (default) future for the computation. If the expression is
evaluated at the REPL, for example, then the continuation might take the
result, print it on the screen, prompt for the next input, evaluate it,
and so on forever. Most of the time the continuation includes actions
specified by user code, as in a continuation that will take the result,
multiply it by the value stored in a local variable, add seven, and give
the answer to the REPL’s continuation to be printed. Normally these
ubiquitous continuations are hidden behind the scenes and programmers do
not think much about them. On rare occasions, however, a programmer
needs to deal with continuations explicitly. The
call-with-current-continuation procedure allows Scheme programmers to do
that by creating a procedure that acts just like the current
continuation.

(values *obj …*)  procedure  
Delivers all of its arguments to its continuation. The `values`
procedure might be defined as follows:

    (define (values . things)
      (call-with-current-continuation 
        (lambda (cont) (apply cont things))))%

(call-with-values *producer consumer*)  procedure  
Calls its *producer* argument with no arguments and a continuation that,
when passed some values, calls the *consumer* procedure with those
values as arguments. The continuation for the call to *consumer* is the
continuation of the call to `call-with-values`.

    (call-with-values (lambda () (values 4 5))
                      (lambda (a b) b))
                                                       \ev  5

    (call-with-values * -)                             \ev  -1%

(dynamic-wind *before thunk after*)  procedure  
Calls *thunk* without arguments, returning the result(s) of this call.
*Before* and *after* are called, also without arguments, as required by
the following rules. Note that, in the absence of calls to continuations
captured using `call-with-current-continuation`, the three arguments are
called once each, in order. *Before* is called whenever execution enters
the dynamic extent of the call to *thunk* and *after* is called whenever
it exits that dynamic extent. The dynamic extent of a procedure call is
the period between when the call is initiated and when it returns. The
*before* and *after* thunks are called in the same dynamic environment
as the call to dynamic-wind. In Scheme, because of
call-with-current-continuation, the dynamic extent of a call is not
always a single, connected time period. It is defined as follows:

-   The dynamic extent is entered when execution of the body of the
    called procedure begins.

-   The dynamic extent is also entered when execution is not within the
    dynamic extent and a continuation is invoked that was captured
    (using call-with-current-continuation) during the dynamic extent.

-   It is exited when the called procedure returns.

-   It is also exited when execution is within the dynamic extent and a
    continuation is invoked that was captured while not within the
    dynamic extent.

If a second call to dynamic-wind occurs within the dynamic extent of the
call to *thunk* and then a continuation is invoked in such a way that
the *after*s from these two invocations of dynamic-wind are both to be
called, then the *after* associated with the second (inner) call to
dynamic-wind is called first.

If a second call to dynamic-wind occurs within the dynamic extent of the
call to *thunk* and then a continuation is invoked in such a way that
the *before*s from these two invocations of dynamic-wind are both to be
called, then the *before* associated with the first (outer) call to
dynamic-wind is called first.

If invoking a continuation requires calling the *before* from one call
to dynamic-wind and the *after* from another, then the *after* is called
first.

The effect of using a captured continuation to enter or exit the dynamic
extent of a call to *before* or *after* is unspecified.

    (let ((path '())
          (c \#f))
      (let ((add (lambda (s)
                   (set! path (cons s path)))))
        (dynamic-wind
          (lambda () (add 'connect))
          (lambda ()
            (add (call-with-current-continuation
                   (lambda (c0)
                     (set! c c0)
                     'talk1))))
          (lambda () (add 'disconnect)))
        (if (< (length path) 4)
            (c 'talk2)
            (reverse path))))
        \lev (connect talk1 disconnect
                   connect talk2 disconnect)%

## Exceptions

This section describes Scheme’s exception-handling and exception-raising
procedures. For the concept of Scheme exceptions, see
section [\[errorsituations\]](#errorsituations). See also
[\[guard\]](#guard) for the guard syntax.

*Exception handler*s are one-argument procedures that determine the
action the program takes when an exceptional situation is signaled. The
system implicitly maintains a current exception handler in the dynamic
environment.

The program raises an exception by invoking the current exception
handler, passing it an object encapsulating information about the
exception. Any procedure accepting one argument can serve as an
exception handler and any object can be used to represent an exception.

(with-exception-handler **handler* *thunk**)  procedure  
It is an error if *handler* does not accept one argument. It is also an
error if *thunk* does not accept zero arguments.

The with-exception-handler procedure returns the results of invoking
*thunk*. *Handler* is installed as the current exception handler in the
dynamic environment used for the invocation of *thunk*.

    (call-with-current-continuation
     (lambda (k)
      (with-exception-handler
       (lambda (x)
        (display "condition: ")
        (write x)
        (newline)
        (k 'exception))
       (lambda ()
        (+ 1 (raise 'an-error))))))
            \ev exception
     \>{\em and prints}  condition: an-error

    (with-exception-handler
     (lambda (x)
      (display "something went wrong\backwhack{}n"))
     (lambda ()
      (+ 1 (raise 'an-error))))
     \>{\em prints}  something went wrong%

After printing, the second example then raises another exception.

(raise **obj**)  procedure  
Raises an exception by invoking the current exception handler on *obj*.
The handler is called with the same dynamic environment as that of the
call to raise, except that the current exception handler is the one that
was in place when the handler being called was installed. If the handler
returns, a secondary exception is raised in the same dynamic environment
as the handler. The relationship between *obj* and the object raised by
the secondary exception is unspecified.

(raise-continuable **obj**)  procedure  
Raises an exception by invoking the current exception handler on *obj*.
The handler is called with the same dynamic environment as the call to
raise-continuable, except that: (1) the current exception handler is the
one that was in place when the handler being called was installed, and
(2) if the handler being called returns, then it will again become the
current exception handler. If the handler returns, the values it returns
become the values returned by the call to raise-continuable.

    (with-exception-handler
      (lambda (con)
        (cond
          ((string? con)
           (display con))
          (else
           (display "a warning has been issued")))
        42)
      (lambda ()
        (+ (raise-continuable "should be a number")
           23)))
       {\it prints:} should be a number
       \ev 65%

(error **message* *obj* …*)  procedure  
*Message* should be a string.

Raises an exception as if by calling raise on a newly allocated
implementation-defined object which encapsulates the information
provided by *message*, as well as any *obj*s, known as the *irritants*.
The procedure error-object? must return #t on such objects.

    (define (null-list? l)
      (cond ((pair? l) \#f)
            ((null? l) \#t)
            (else
              (error
                "null-list?: argument out of domain"
                l))))%

(error-object? *obj*)  procedure  
Returns #t if *obj* is an object created by error or one of an
implementation-defined set of objects. Otherwise, it returns #f. The
objects used to signal errors, including those which satisfy the
predicates file-error? and read-error?, may or may not satisfy
error-object?.

(error-object-message *error-object*)  procedure  
Returns the message encapsulated by *error-object*.

(error-object-irritants *error-object*)  procedure  
Returns a list of the irritants encapsulated by *error-object*.

(read-error? *obj*)  procedure  
(file-error? *obj*)  procedure  
Error type predicates. Returns #t if *obj* is an object raised by the
read procedure or by the inability to open an input or output port on a
file, respectively. Otherwise, it returns #f.

## Environments and evaluation

(environment **l**i**s**t*<sub>1</sub> … *)  eval library procedure  
This procedure returns a specifier for the environment that results by
starting with an empty environment and then importing each *list*,
considered as an import set, into it. (See
section [\[libraries\]](#libraries) for a description of import sets.)
The bindings of the environment represented by the specifier are
immutable, as is the environment itself.

(scheme-report-environment *version*)  r5rs library procedure  
If *version* is equal to 5, corresponding to R<sup>5</sup>RS,
scheme-report-environment returns a specifier for an environment that
contains only the bindings defined in the R<sup>5</sup>RS library.
Implementations must support this value of *version*.

Implementations may also support other values of *version*, in which
case they return a specifier for an environment containing bindings
corresponding to the specified version of the report. If *version* is
neither 5 nor another value supported by the implementation, an error is
signaled.

The effect of defining or assigning (through the use of eval) an
identifier bound in a scheme-report-environment (for example car) is
unspecified. Thus both the environment and the bindings it contains may
be immutable.

(null-environment *version*)  r5rs library procedure  
If *version* is equal to 5, corresponding to R<sup>5</sup>RS, the
null-environment procedure returns a specifier for an environment that
contains only the bindings for all syntactic keywords defined in the
R<sup>5</sup>RS library. Implementations must support this value of
*version*.

Implementations may also support other values of *version*, in which
case they return a specifier for an environment containing appropriate
bindings corresponding to the specified version of the report. If
*version* is neither 5 nor another value supported by the
implementation, an error is signaled.

The effect of defining or assigning (through the use of eval) an
identifier bound in a scheme-report-environment (for example car) is
unspecified. Thus both the environment and the bindings it contains may
be immutable.

(interaction-environment)  repl library procedure  
This procedure returns a specifier for a mutable environment that
contains an implementation-defined set of bindings, typically a superset
of those exported by (scheme base). The intent is that this procedure
will return the environment in which the implementation would evaluate
expressions entered by the user into a REPL.

(eval *expr-or-def environment-specifier*)  eval library procedure  
If *expr-or-def* is an expression, it is evaluated in the specified
environment and its values are returned. If it is a definition, the
specified identifier(s) are defined in the specified environment,
provided the environment is not immutable. Implementations may extend
eval to allow other objects.

    (eval '(* 7 3) (environment '(scheme base)))
                                                       \ev  21

    (let ((f (eval '(lambda (f x) (f x x))
                   (null-environment 5))))
      (f + 10))
                                                       \ev  20
    (eval '(define foo 32)
          (environment '(scheme base)))
                                                       \ev {\it{} error is signaled}%

## Input and output

### Ports

Ports represent input and output devices. To Scheme, an input port is a
Scheme object that can deliver data upon command, while an output port
is a Scheme object that can accept data. Whether the input and output
port types are disjoint is implementation-dependent.

Different *port types* operate on different data. Scheme implementations
are required to support *textual ports* and *binary ports*, but may also
provide other port types.

A textual port supports reading or writing of individual characters from
or to a backing store containing characters using read-char and
write-char below, and it supports operations defined in terms of
characters, such as read and write.

A binary port supports reading or writing of individual bytes from or to
a backing store containing bytes using read-u8 and write-u8 below, as
well as operations defined in terms of bytes. Whether the textual and
binary port types are disjoint is implementation-dependent.

Ports can be used to access files, devices, and similar things on the
host system on which the Scheme program is running.

(call-with-port *port proc*)  procedure  
It is an error if *proc* does not accept one argument.

The call-with-port procedure calls *proc* with *port* as an argument. If
*proc* returns, then the port is closed automatically and the values
yielded by the *proc* are returned. If *proc* does not return, then the
port must not be closed automatically unless it is possible to prove
that the port will never again be used for a read or write operation.

*Rationale:* Because Scheme’s escape procedures have unlimited extent,
it is possible to escape from the current continuation but later to
resume it. If implementations were permitted to close the port on any
escape from the current continuation, then it would be impossible to
write portable code using both call-with-current-continuation and
call-with-port.

(call-with-input-file *string proc*)  file library procedure  
(call-with-output-file *string proc*)  file library procedure  
It is an error if *proc* does not accept one argument.

These procedures obtain a textual port obtained by opening the named
file for input or output as if by open-input-file or open-output-file.
The port and *proc* are then passed to a procedure equivalent to
call-with-port.

(input-port? *obj*)  procedure  
(output-port? *obj*)  procedure  
(textual-port? *obj*)  procedure  
(binary-port? *obj*)  procedure  
(port? *obj*)  procedure  
These procedures return #t if *obj* is an input port, output port,
textual port, binary port, or any kind of port, respectively. Otherwise
they return #f.

(input-port-open? *port*)  procedure  
(output-port-open? *port*)  procedure  
Returns #t if *port* is still open and capable of performing input or
output, respectively, and #f otherwise.

(current-input-port)  procedure  
(current-output-port)  procedure  
(current-error-port)  procedure  
Returns the current default input port, output port, or error port (an
output port), respectively. These procedures are parameter objects,
which can be overridden with parameterize (see
section [\[make-parameter\]](#make-parameter)). The initial bindings for
these are implementation-defined textual ports.

(with-input-from-file *string thunk*)  file library procedure  
(with-output-to-file *string thunk*)  file library procedure  
The file is opened for input or output as if by open-input-file or
open-output-file, and the new port is made to be the value returned by
current-input-port or current-output-port (as used by `(read)`,
`(write obj)`, and so forth). The *thunk* is then called with no
arguments. When the *thunk* returns, the port is closed and the previous
default is restored. It is an error if *thunk* does not accept zero
arguments. Both procedures return the values yielded by *thunk*. If an
escape procedure is used to escape from the continuation of these
procedures, they behave exactly as if the current input or output port
had been bound dynamically with parameterize.

(open-input-file *string*)  file library procedure  
(open-binary-input-file *string*)  file library procedure  
Takes a *string* for an existing file and returns a textual input port
or binary input port that is capable of delivering data from the file.
If the file does not exist or cannot be opened, an error that satisfies
file-error? is signaled.

(open-output-file *string*)  file library procedure  
(open-binary-output-file *string*)  file library procedure  
Takes a *string* naming an output file to be created and returns a
textual output port or binary output port that is capable of writing
data to a new file by that name.

If a file with the given name already exists, the effect is unspecified.
If the file cannot be opened, an error that satisfies file-error? is
signaled.

(close-port *port*)  procedure  
(close-input-port *port*)  procedure  
(close-output-port *port*)  procedure  
Closes the resource associated with *port*, rendering the *port*
incapable of delivering or accepting data. It is an error to apply the
last two procedures to a port which is not an input or output port,
respectively. Scheme implementations may provide ports which are
simultaneously input and output ports, such as sockets; the
close-input-port and close-output-port procedures can then be used to
close the input and output sides of the port independently.

These routines have no effect if the port has already been closed.

(open-input-string *string*)  procedure  
Takes a string and returns a textual input port that delivers characters
from the string. If the string is modified, the effect is unspecified.

(open-output-string)  procedure  
Returns a textual output port that will accumulate characters for
retrieval by get-output-string.

(get-output-string *port*)  procedure  
It is an error if *port* was not created with open-output-string.

Returns a string consisting of the characters that have been output to
the port so far in the order they were output. If the result string is
modified, the effect is unspecified.

    (parameterize
        ((current-output-port
          (open-output-string)))
        (display "piece")
        (display " by piece ")
        (display "by piece.")
        (newline)
        (get-output-string (current-output-port)))
    \lev "piece by piece by piece.\backwhack{}n"%

(open-input-bytevector *bytevector*)  procedure  
Takes a bytevector and returns a binary input port that delivers bytes
from the bytevector.

(open-output-bytevector)  procedure  
Returns a binary output port that will accumulate bytes for retrieval by
get-output-bytevector.

(get-output-bytevector *port*)  procedure  
It is an error if *port* was not created with open-output-bytevector.

Returns a bytevector consisting of the bytes that have been output to
the port so far in the order they were output.

### Input

If *port* is omitted from any input procedure, it defaults to the value
returned by (current-input-port). It is an error to attempt an input
operation on a closed port.

 

(read)  read library procedure  
(read *port*)  read library procedure  
The read procedure converts external representations of Scheme objects
into the objects themselves. That is, it is a parser for the
non-terminal datum (see sections [\[datum\]](#datum) and
[\[listsection\]](#listsection)). It returns the next object parsable
from the given textual input *port*, updating *port* to point to the
first character past the end of the external representation of the
object.

Implementations may support extended syntax to represent record types or
other types that do not have datum representations.

If an end of file is encountered in the input before any characters are
found that can begin an object, then an end-of-file object is returned.
The port remains open, and further attempts to read will also return an
end-of-file object. If an end of file is encountered after the beginning
of an object’s external representation, but the external representation
is incomplete and therefore not parsable, an error that satisfies
read-error? is signaled.

(read-char)  procedure  
(read-char *port*)  procedure  
Returns the next character available from the textual input *port*,
updating the *port* to point to the following character. If no more
characters are available, an end-of-file object is returned.

(peek-char)  procedure  
(peek-char *port*)  procedure  
Returns the next character available from the textual input *port*, but
*without* updating the *port* to point to the following character. If no
more characters are available, an end-of-file object is returned.

*Note:* The value returned by a call to peek-char is the same as the
value that would have been returned by a call to read-char with the same
*port*. The only difference is that the very next call to read-char or
peek-char on that *port* will return the value returned by the preceding
call to peek-char. In particular, a call to peek-char on an interactive
port will hang waiting for input whenever a call to read-char would have
hung.

(read-line)  procedure  
(read-line *port*)  procedure  
Returns the next line of text available from the textual input *port*,
updating the *port* to point to the following character. If an end of
line is read, a string containing all of the text up to (but not
including) the end of line is returned, and the port is updated to point
just past the end of line. If an end of file is encountered before any
end of line is read, but some characters have been read, a string
containing those characters is returned. If an end of file is
encountered before any characters are read, an end-of-file object is
returned. For the purpose of this procedure, an end of line consists of
either a linefeed character, a carriage return character, or a sequence
of a carriage return character followed by a linefeed character.
Implementations may also recognize other end of line characters or
sequences.

(eof-object? *obj*)  procedure  
Returns #t if *obj* is an end-of-file object, otherwise returns #f. The
precise set of end-of-file objects will vary among implementations, but
in any case no end-of-file object will ever be an object that can be
read in using read.

(eof-object)  procedure  
Returns an end-of-file object, not necessarily unique.

(char-ready?)  procedure  
(char-ready? *port*)  procedure  
Returns #t if a character is ready on the textual input *port* and
returns #f otherwise. If char-ready returns #t then the next read-char
operation on the given *port* is guaranteed not to hang. If the *port*
is at end of file then char-ready? returns #t.

*Rationale:* The char-ready? procedure exists to make it possible for a
program to accept characters from interactive ports without getting
stuck waiting for input. Any input editors associated with such ports
must ensure that characters whose existence has been asserted by
char-ready? cannot be removed from the input. If char-ready? were to
return #f at end of file, a port at end of file would be
indistinguishable from an interactive port that has no ready characters.

(read-string *k*)  procedure  
(read-string *k port*)  procedure  
Reads the next *k* characters, or as many as are available before the
end of file, from the textual input *port* into a newly allocated string
in left-to-right order and returns the string. If no characters are
available before the end of file, an end-of-file object is returned.

(read-u8)  procedure  
(read-u8 *port*)  procedure  
Returns the next byte available from the binary input *port*, updating
the *port* to point to the following byte. If no more bytes are
available, an end-of-file object is returned.

(peek-u8)  procedure  
(peek-u8 *port*)  procedure  
Returns the next byte available from the binary input *port*, but
*without* updating the *port* to point to the following byte. If no more
bytes are available, an end-of-file object is returned.

(u8-ready?)  procedure  
(u8-ready? *port*)  procedure  
Returns #t if a byte is ready on the binary input *port* and returns #f
otherwise. If u8-ready? returns #t then the next read-u8 operation on
the given *port* is guaranteed not to hang. If the *port* is at end of
file then u8-ready? returns #t.

(read-bytevector *k*)  procedure  
(read-bytevector *k port*)  procedure  
Reads the next *k* bytes, or as many as are available before the end of
file, from the binary input *port* into a newly allocated bytevector in
left-to-right order and returns the bytevector. If no bytes are
available before the end of file, an end-of-file object is returned.

(read-bytevector! *bytevector*)  procedure  
(read-bytevector! *bytevector port*)  procedure  
(read-bytevector! *bytevector port start*)  procedure  
(read-bytevector! *bytevector port start end*)  procedure  
Reads the next *e**n**d* − *s**t**a**r**t* bytes, or as many as are
available before the end of file, from the binary input *port* into
*bytevector* in left-to-right order beginning at the *start* position.
If *end* is not supplied, reads until the end of *bytevector* has been
reached. If *start* is not supplied, reads beginning at position 0.
Returns the number of bytes read. If no bytes are available, an
end-of-file object is returned.

### Output

If *port* is omitted from any output procedure, it defaults to the value
returned by (current-output-port). It is an error to attempt an output
operation on a closed port.

(write *obj*)  write library procedure  
(write *obj port*)  write library procedure  
Writes a representation of *obj* to the given textual output *port*.
Strings that appear in the written representation are enclosed in
quotation marks, and within those strings backslash and quotation mark
characters are escaped by backslashes. Symbols that contain non-ASCII
characters are escaped with vertical lines. Character objects are
written using the #`‘ ` notation.

If *obj* contains cycles which would cause an infinite loop using the
normal written representation, then at least the objects that form part
of the cycle must be represented using datum labels as described in
section [\[labelsection\]](#labelsection). Datum labels must not be used
if there are no cycles.

Implementations may support extended syntax to represent record types or
other types that do not have datum representations.

The write procedure returns an unspecified value.

(write-shared *obj*)  write library procedure  
(write-shared *obj port*)  write library procedure  
The write-shared procedure is the same as write, except that shared
structure must be represented using datum labels for all pairs and
vectors that appear more than once in the output.

(write-simple *obj*)  write library procedure  
(write-simple *obj port*)  write library procedure  
The write-simple procedure is the same as write, except that shared
structure is never represented using datum labels. This can cause
write-simple not to terminate if *obj* contains circular structure.

(display *obj*)  write library procedure  
(display *obj port*)  write library procedure  
Writes a representation of *obj* to the given textual output *port*.
Strings that appear in the written representation are output as if by
write-string instead of by write. Symbols are not escaped. Character
objects appear in the representation as if written by write-char instead
of by write.

The display representation of other objects is unspecified. However,
display must not loop forever on self-referencing pairs, vectors, or
records. Thus if the normal write representation is used, datum labels
are needed to represent cycles as in write.

Implementations may support extended syntax to represent record types or
other types that do not have datum representations.

The display procedure returns an unspecified value.

*Rationale:* The write procedure is intended for producing
machine-readable output and display for producing human-readable output.

(newline)  procedure  
(newline *port*)  procedure  
Writes an end of line to textual output *port*. Exactly how this is done
differs from one operating system to another. Returns an unspecified
value.

(write-char *char*)  procedure  
(write-char *char port*)  procedure  
Writes the character *char* (not an external representation of the
character) to the given textual output *port* and returns an unspecified
value.

(write-string *string*)  procedure  
(write-string *string port*)  procedure  
(write-string *string port start*)  procedure  
(write-string *string port start end*)  procedure  
Writes the characters of *string* from *start* to *end* in left-to-right
order to the textual output *port*.

(write-u8 *byte*)  procedure  
(write-u8 *byte port*)  procedure  
Writes the *byte* to the given binary output *port* and returns an
unspecified value.

(write-bytevector *bytevector*)  procedure  
(write-bytevector *bytevector port*)  procedure  
(write-bytevector *bytevector port start*)  procedure  
(write-bytevector *bytevector port start end*)  procedure  
Writes the bytes of *bytevector* from *start* to *end* in left-to-right
order to the binary output *port*.

(flush-output-port)  procedure  
(flush-output-port *port*)  procedure  
Flushes any buffered output from the buffer of output-port to the
underlying file or device and returns an unspecified value.

## System interface

Questions of system interface generally fall outside of the domain of
this report. However, the following operations are important enough to
deserve description here.

(load *filename*)  load library procedure  
(load *filename environment-specifier*)  load library procedure  
It is an error if *filename* is not a string.

An implementation-dependent operation is used to transform *filename*
into the name of an existing file containing Scheme source code. The
load procedure reads expressions and definitions from the file and
evaluates them sequentially in the environment specified by
*environment-specifier*. If *environment-specifier* is omitted,
(interaction-environment) is assumed.

It is unspecified whether the results of the expressions are printed.
The load procedure does not affect the values returned by
current-input-port and current-output-port. It returns an unspecified
value.

*Rationale:* For portability, load must operate on source files. Its
operation on other kinds of files necessarily varies among
implementations.

(file-exists? *filename*)  file library procedure  
It is an error if *filename* is not a string.

The file-exists? procedure returns #t if the named file exists at the
time the procedure is called, and #f otherwise.

(delete-file *filename*)  file library procedure  
It is an error if *filename* is not a string.

The delete-file procedure deletes the named file if it exists and can be
deleted, and returns an unspecified value. If the file does not exist or
cannot be deleted, an error that satisfies file-error? is signaled.

(command-line)  process-context library procedure  
Returns the command line passed to the process as a list of strings. The
first string corresponds to the command name, and is
implementation-dependent. It is an error to mutate any of these strings.

(exit)  process-context library procedure  
(exit *obj*)  process-context library procedure  
Runs all outstanding dynamic-wind *after* procedures, terminates the
running program, and communicates an exit value to the operating system.
If no argument is supplied, or if *obj* is #t, the exit procedure should
communicate to the operating system that the program exited normally. If
*obj* is #f, the exit procedure should communicate to the operating
system that the program exited abnormally. Otherwise, exit should
translate *obj* into an appropriate exit value for the operating system,
if possible.

The exit procedure must not signal an exception or return to its
continuation.

*Note:* Because of the requirement to run handlers, this procedure is
not just the operating system’s exit procedure.

(emergency-exit)  process-context library procedure  
(emergency-exit *obj*)  process-context library procedure  
Terminates the program without running any outstanding dynamic-wind
*after* procedures and communicates an exit value to the operating
system in the same manner as exit.

*Note:* The emergency-exit procedure corresponds to the \_exit procedure
in Windows and Posix.

(get-environment-variable *name*)  process-context library procedure  
Many operating systems provide each running process with an
*environment* consisting of *environment variables*. (This environment
is not to be confused with the Scheme environments that can be passed to
eval: see section [\[environments\]](#environments).) Both the name and
value of an environment variable are strings. The procedure
get-environment-variable returns the value of the environment variable
*name*, or #f if the named environment variable is not found. It may use
locale information to encode the name and decode the value of the
environment variable. It is an error if  
get-environment-variable can’t decode the value. It is also an error to
mutate the resulting string.

    (get-environment-variable "PATH") \lev "/usr/local/bin:/usr/bin:/bin"%

(get-environment-variables)  process-context library procedure  
Returns the names and values of all the environment variables as an
alist, where the car of each entry is the name of an environment
variable and the cdr is its value, both as strings. The order of the
list is unspecified. It is an error to mutate any of these strings or
the alist itself.

    (get-environment-variables) \lev (("USER" . "root") ("HOME" . "/"))%

(current-second)  time library procedure  
Returns an inexact number representing the current time on the
International Atomic Time (TAI) scale. The value 0.0 represents midnight
on January 1, 1970 TAI (equivalent to 8.000082 seconds before midnight
Universal Time) and the value 1.0 represents one TAI second later.
Neither high accuracy nor high precision are required; in particular,
returning Coordinated Universal Time plus a suitable constant might be
the best an implementation can do.

As of 2018, a TAI-UTC offset table can be found at <span
class="citation" cites="TAI"></span>.

(current-jiffy)  time library procedure  
Returns the number of *jiffies* as an exact integer that have elapsed
since an arbitrary, implementation-defined epoch. A jiffy is an
implementation-defined fraction of a second which is defined by the
return value of the jiffies-per-second procedure. The starting epoch is
guaranteed to be constant during a run of the program, but may vary
between runs.

*Rationale:* Jiffies are allowed to be implementation-dependent so that
current-jiffy can execute with minimum overhead. It should be very
likely that a compactly represented integer will suffice as the returned
value. Any particular jiffy size will be inappropriate for some
implementations: a microsecond is too long for a very fast machine,
while a much smaller unit would force many implementations to return
integers which have to be allocated for most calls, rendering
current-jiffy less useful for accurate timing measurements.

(jiffies-per-second)  time library procedure  
Returns an exact integer representing the number of jiffies per SI
second. This value is an implementation-specified constant.

    (define (time-length)
      (let ((list (make-list 100000))
            (start (current-jiffy)))
        (length list)
        (/ (- (current-jiffy) start)
           (jiffies-per-second))))%

(features)  procedure  
Returns a list of the feature identifiers which cond-expand treats as
true. It is an error to modify this list. Here is an example of what
features might return:

    (features) \ev
      (r7rs ratios exact-complex full-unicode
       gnu-linux little-endian 
       fantastic-scheme
       fantastic-scheme-1.0
       space-ship-control-system)%

## Derived expression types

This section gives syntax definitions for the derived expression types
in terms of the primitive expression types (literal, variable, call,
lambda, if, and set!), except for quasiquote.

Conditional derived syntax types:

    (define-syntax \ide{cond}
      (syntax-rules (else =>)
        ((cond (else result1 result2 ...))
         (begin result1 result2 ...))
        ((cond (test => result))
         (let ((temp test))
           (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               (result temp)
               (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               temp
               (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...))
         (if test (begin result1 result2 ...)))
        ((cond (test result1 result2 ...)
               clause1 clause2 ...)
         (if test
             (begin result1 result2 ...)
             (cond clause1 clause2 ...)))))

    (define-syntax \ide{case}
      (syntax-rules (else =>)
        ((case (key ...)
           clauses ...)
         (let ((atom-key (key ...)))
           (case atom-key clauses ...)))
        ((case key
           (else => result))
         (result key))
        ((case key
           (else result1 result2 ...))
         (begin result1 result2 ...))
        ((case key
           ((atoms ...) => result))
         (if (memv key '(atoms ...))
             (result key)))
        ((case key
           ((atoms ...) result1 result2 ...))
         (if (memv key '(atoms ...))
             (begin result1 result2 ...)))
        ((case key
           ((atoms ...) => result)
           clause clauses ...)
         (if (memv key '(atoms ...))
             (result key)
             (case key clause clauses ...)))
        ((case key
           ((atoms ...) result1 result2 ...)
           clause clauses ...)
         (if (memv key '(atoms ...))
             (begin result1 result2 ...)
             (case key clause clauses ...)))))

    (define-syntax \ide{and}
      (syntax-rules ()
        ((and) \sharpfoo{t})
        ((and test) test)
        ((and test1 test2 ...)
         (if test1 (and test2 ...) \sharpfoo{f}))))

    (define-syntax \ide{or}
      (syntax-rules ()
        ((or) \sharpfoo{f})
        ((or test) test)
        ((or test1 test2 ...)
         (let ((x test1))
           (if x x (or test2 ...))))))

    (define-syntax \ide{when}
      (syntax-rules ()
        ((when test result1 result2 ...)
         (if test
             (begin result1 result2 ...)))))

    (define-syntax \ide{unless}
      (syntax-rules ()
        ((unless test result1 result2 ...)
         (if (not test)
             (begin result1 result2 ...)))))

Binding constructs:

    (define-syntax \ide{let}
      (syntax-rules ()
        ((let ((name val) ...) body1 body2 ...)
         ((lambda (name ...) body1 body2 ...)
          val ...))
        ((let tag ((name val) ...) body1 body2 ...)
         ((letrec ((tag (lambda (name ...)
                          body1 body2 ...)))
            tag)
          val ...))))

    (define-syntax \ide{let*}
      (syntax-rules ()
        ((let* () body1 body2 ...)
         (let () body1 body2 ...))
        ((let* ((name1 val1) (name2 val2) ...)
           body1 body2 ...)
         (let ((name1 val1))
           (let* ((name2 val2) ...)
             body1 body2 ...)))))

The following letrec macro uses the symbol \<undefined> in place of an
expression which returns something that when stored in a location makes
it an error to try to obtain the value stored in the location. (No such
expression is defined in Verbatim.) A trick is used to generate the
temporary names needed to avoid specifying the order in which the values
are evaluated. This could also be accomplished by using an auxiliary
macro.

    (define-syntax \ide{letrec}
      (syntax-rules ()
        ((letrec ((var1 init1) ...) body ...)
         (letrec "generate\_temp\_names"
           (var1 ...)
           ()
           ((var1 init1) ...)
           body ...))
        ((letrec "generate\_temp\_names"
           ()
           (temp1 ...)
           ((var1 init1) ...)
           body ...)
         (let ((var1 <undefined>) ...)
           (let ((temp1 init1) ...)
             (set! var1 temp1)
             ...
             body ...)))
        ((letrec "generate\_temp\_names"
           (x y ...)
           (temp ...)
           ((var1 init1) ...)
           body ...)
         (letrec "generate\_temp\_names"
           (y ...)
           (newtemp temp ...)
           ((var1 init1) ...)
           body ...))))

    (define-syntax \ide{letrec*}
      (syntax-rules ()
        ((letrec* ((var1 init1) ...) body1 body2 ...)
         (let ((var1 <undefined>) ...)
           (set! var1 init1)
           ...
           (let () body1 body2 ...)))))%

    (define-syntax \ide{let-values}
      (syntax-rules ()
        ((let-values (binding ...) body0 body1 ...)
         (let-values "bind"
             (binding ...) () (begin body0 body1 ...)))
        
        ((let-values "bind" () tmps body)
         (let tmps body))
        
        ((let-values "bind" ((b0 e0)
             binding ...) tmps body)
         (let-values "mktmp" b0 e0 ()
             (binding ...) tmps body))
        
        ((let-values "mktmp" () e0 args
             bindings tmps body)
         (call-with-values 
           (lambda () e0)
           (lambda args
             (let-values "bind"
                 bindings tmps body))))
        
        ((let-values "mktmp" (a . b) e0 (arg ...)
             bindings (tmp ...) body)
         (let-values "mktmp" b e0 (arg ... x)
             bindings (tmp ... (a x)) body))
        
        ((let-values "mktmp" a e0 (arg ...)
            bindings (tmp ...) body)
         (call-with-values
           (lambda () e0)
           (lambda (arg ... . x)
             (let-values "bind"
                 bindings (tmp ... (a x)) body))))))

    (define-syntax \ide{let*-values}
      (syntax-rules ()
        ((let*-values () body0 body1 ...)
         (let () body0 body1 ...))

        ((let*-values (binding0 binding1 ...)
             body0 body1 ...)
         (let-values (binding0)
           (let*-values (binding1 ...)
             body0 body1 ...)))))

    (define-syntax \ide{define-values}
      (syntax-rules ()
        ((define-values () expr)
         (define dummy
           (call-with-values (lambda () expr)
                             (lambda args \schfalse))))
        ((define-values (var) expr)
         (define var expr))
        ((define-values (var0 var1 ... varn) expr)
         (begin
           (define var0
             (call-with-values (lambda () expr)
                               list))
           (define var1
             (let ((v (cadr var0)))
               (set-cdr! var0 (cddr var0))
               v)) ...
           (define varn
             (let ((v (cadr var0)))
               (set! var0 (car var0))
               v))))
        ((define-values (var0 var1 ... . varn) expr)
         (begin
           (define var0
             (call-with-values (lambda () expr)
                               list))
           (define var1
             (let ((v (cadr var0)))
               (set-cdr! var0 (cddr var0))
               v)) ...
           (define varn
             (let ((v (cdr var0)))
               (set! var0 (car var0))
               v))))
        ((define-values var expr)
         (define var
           (call-with-values (lambda () expr)
                             list)))))

    (define-syntax \ide{begin}
      (syntax-rules ()
        ((begin exp ...)
         ((lambda () exp ...)))))

The following alternative expansion for begin does not make use of the
ability to write more than one expression in the body of a lambda
expression. In any case, note that these rules apply only if the body of
the begin contains no definitions.

    (define-syntax begin
      (syntax-rules ()
        ((begin exp)
         exp)
        ((begin exp1 exp2 ...)
         (call-with-values
             (lambda () exp1)
           (lambda args
             (begin exp2 ...))))))

The following syntax definition of do uses a trick to expand the
variable clauses. As with letrec above, an auxiliary macro would also
work. The expression (if #f #f) is used to obtain an unspecific value.

    (define-syntax \ide{do}
      (syntax-rules ()
        ((do ((var init step ...) ...)
             (test expr ...)
             command ...)
         (letrec
           ((loop
             (lambda (var ...)
               (if test
                   (begin
                     (if \#f \#f)
                     expr ...)
                   (begin
                     command
                     ...
                     (loop (do "step" var step ...)
                           ...))))))
           (loop init ...)))
        ((do "step" x)
         x)
        ((do "step" x y)
         y)))

Here is a possible implementation of delay, force and delay-force. We
define the expression

    (delay-force \hyper{expression})%

to have the same meaning as the procedure call

    (make-promise \schfalse{} (lambda () \hyper{expression}))%

as follows

    (define-syntax delay-force
      (syntax-rules ()
        ((delay-force expression) 
         (make-promise \schfalse{} (lambda () expression)))))%

and we define the expression

    (delay \hyper{expression})%

to have the same meaning as:

    (delay-force (make-promise \schtrue{} \hyper{expression}))%

as follows

    (define-syntax delay
      (syntax-rules ()
        ((delay expression)
         (delay-force (make-promise \schtrue{} expression)))))%

where make-promise is defined as follows:

    (define make-promise
      (lambda (done? proc)
        (list (cons done? proc))))%

Finally, we define force to call the procedure expressions in promises
iteratively using a trampoline technique following <span
class="citation" cites="srfi45"></span> until a non-lazy result (i.e. a
value created by delay instead of delay-force) is returned, as follows:

    (define (force promise)
      (if (promise-done? promise)
          (promise-value promise)
          (let ((promise* ((promise-value promise))))
            (unless (promise-done? promise)
              (promise-update! promise* promise))
            (force promise))))%

with the following promise accessors:

    (define promise-done?
      (lambda (x) (car (car x))))
    (define promise-value
      (lambda (x) (cdr (car x))))
    (define promise-update!
      (lambda (new old)
        (set-car! (car old) (promise-done? new))
        (set-cdr! (car old) (promise-value new))
        (set-car! new (car old))))%

The following implementation of make-parameter and parameterize is
suitable for an implementation with no threads. Parameter objects are
implemented here as procedures, using two arbitrary unique objects
\<param-set!> and \<param-convert>:

    (define (make-parameter init . o)
      (let* ((converter
              (if (pair? o) (car o) (lambda (x) x)))
             (value (converter init)))
        (lambda args
          (cond
           ((null? args)
            value)
           ((eq? (car args) <param-set!>)
            (set! value (cadr args)))
           ((eq? (car args) <param-convert>)
            converter)
           (else
            (error "bad parameter syntax"))))))%

Then parameterize uses dynamic-wind to dynamically rebind the associated
value:

    (define-syntax parameterize
      (syntax-rules ()
        ((parameterize ("step")
                       ((param value p old new) ...)
                       ()
                       body)
         (let ((p param) ...)
           (let ((old (p)) ...
                 (new ((p <param-convert>) value)) ...)
             (dynamic-wind
              (lambda () (p <param-set!> new) ...)
              (lambda () . body)
              (lambda () (p <param-set!> old) ...)))))
        ((parameterize ("step")
                       args
                       ((param value) . rest)
                       body)
         (parameterize ("step")
                       ((param value p old new) . args)
                       rest
                       body))
        ((parameterize ((param value) ...) . body)
         (parameterize ("step")
                       ()
                       ((param value) ...)
                       body))))

The following implementation of guard depends on an auxiliary macro,
here called guard-aux.

    (define-syntax guard
      (syntax-rules ()
        ((guard (var clause ...) e1 e2 ...)
         ((call/cc
           (lambda (guard-k)
             (with-exception-handler
              (lambda (condition)
                ((call/cc
                   (lambda (handler-k)
                     (guard-k
                      (lambda ()
                        (let ((var condition))
                          (guard-aux
                            (handler-k
                              (lambda ()
                                (raise-continuable condition)))
                            clause ...))))))))
              (lambda ()
                (call-with-values
                 (lambda () e1 e2 ...)
                 (lambda args
                   (guard-k
                     (lambda ()
                       (apply values args)))))))))))))

    (define-syntax guard-aux
      (syntax-rules (else =>)
        ((guard-aux reraise (else result1 result2 ...))
         (begin result1 result2 ...))
        ((guard-aux reraise (test => result))
         (let ((temp test))
           (if temp 
               (result temp)
               reraise)))
        ((guard-aux reraise (test => result)
                    clause1 clause2 ...)
         (let ((temp test))
           (if temp
               (result temp)
               (guard-aux reraise clause1 clause2 ...))))
        ((guard-aux reraise (test))
         (or test reraise))
        ((guard-aux reraise (test) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               temp
               (guard-aux reraise clause1 clause2 ...))))
        ((guard-aux reraise (test result1 result2 ...))
         (if test
             (begin result1 result2 ...)
             reraise))
        ((guard-aux reraise
                    (test result1 result2 ...)
                    clause1 clause2 ...)
         (if test
             (begin result1 result2 ...)
             (guard-aux reraise clause1 clause2 ...)))))

    (define-syntax \ide{case-lambda}
      (syntax-rules ()
        ((case-lambda (params body0 ...) ...)
         (lambda args
           (let ((len (length args)))
             (letrec-syntax
                 ((cl (syntax-rules ::: ()
                        ((cl)
                         (error "no matching clause"))
                        ((cl ((p :::) . body) . rest)
                         (if (= len (length '(p :::)))
                             (apply (lambda (p :::)
                                      . body)
                                    args)
                             (cl . rest)))
                        ((cl ((p ::: . tail) . body)
                             . rest)
                         (if (>= len (length '(p :::)))
                             (apply
                              (lambda (p ::: . tail)
                                . body)
                              args)
                             (cl . rest))))))
               (cl (params body0 ...) ...)))))))

This definition of cond-expand does not interact with the features
procedure. It requires that each feature identifier provided by the
implementation be explicitly mentioned.

    (define-syntax cond-expand
      ;; Extend this to mention all feature ids and libraries
      (syntax-rules (and or not else r7rs library verbatim base)
        ((cond-expand)
         (syntax-error "Unfulfilled cond-expand"))
        ((cond-expand (else body ...))
         (begin body ...))
        ((cond-expand ((and) body ...) more-clauses ...)
         (begin body ...))
        ((cond-expand ((and req1 req2 ...) body ...)
                      more-clauses ...)
         (cond-expand
           (req1
             (cond-expand
               ((and req2 ...) body ...)
               more-clauses ...))
           more-clauses ...))
        ((cond-expand ((or) body ...) more-clauses ...)
         (cond-expand more-clauses ...))
        ((cond-expand ((or req1 req2 ...) body ...)
                      more-clauses ...)
         (cond-expand
           (req1
            (begin body ...))
           (else
            (cond-expand
               ((or req2 ...) body ...)
               more-clauses ...))))
        ((cond-expand ((not req) body ...)
                      more-clauses ...)
         (cond-expand
           (req
             (cond-expand more-clauses ...))
           (else body ...)))
        ((cond-expand (r7rs body ...)
                      more-clauses ...)
           (begin body ...))
        ;; Add clauses here for each
        ;; supported feature identifier.
        ;; Samples:
        ;; ((cond-expand (exact-closed body ...)
        ;;               more-clauses ...)
        ;;   (begin body ...))
        ;; ((cond-expand (ieee-float body ...)
        ;;               more-clauses ...)
        ;;   (begin body ...))
        ((cond-expand ((library (verbatim base))
                       body ...)
                      more-clauses ...)
          (begin body ...))
        ;; Add clauses here for each library
        ((cond-expand (feature-id body ...)
                      more-clauses ...)
           (cond-expand more-clauses ...))
        ((cond-expand ((library (name ...))
                       body ...)
                      more-clauses ...)
           (cond-expand more-clauses ...))))

# Standard Libraries

This section lists the exports provided by the standard libraries. The
libraries are factored so as to separate features which might not be
supported by all implementations, or which might be expensive to load.

The scheme library prefix is used for all standard libraries, and is
reserved for use by future standards.

**Base Library**

The (scheme base) library exports many of the procedures and syntax
bindings that are traditionally associated with Scheme. The division
between the base library and the other standard libraries is based on
use, not on construction. In particular, some facilities that are
typically implemented as primitives by a compiler or the run-time system
rather than in terms of other standard procedures or syntax are not part
of the base library, but are defined in separate libraries. By the same
token, some exports of the base library are implementable in terms of
other exports. They are redundant in the strict sense of the word, but
they capture common patterns of usage, and are therefore provided as
convenient abbreviations.

    {\cf *}                       {\cf +}
    {\cf -}                       {\cf ...}
    {\cf /}                       {\cf <}
    {\cf <=}                      {\cf =}
    {\cf =>}                      {\cf >}
    {\cf >=}                      {\cf \_}
    {\cf abs}                     {\cf and}
    {\cf append}                  {\cf apply}
    {\cf assoc}                   {\cf assq}
    {\cf assv}                    {\cf begin}
    {\cf binary-port?\ }           {\cf boolean=?}
    {\cf boolean?\ }               {\cf bytevector}
    {\cf bytevector-append}       {\cf bytevector-copy}
    {\cf bytevector-copy!}        {\cf bytevector-length}
    {\cf bytevector-u8-ref}       {\cf bytevector-u8-set!}
    {\cf bytevector?\ }            {\cf caar}
    {\cf cadr}
    {\cf call-with-current-continuation}
    {\cf call-with-port}          {\cf call-with-values}
    {\cf call/cc}                 {\cf car}
    {\cf case}                    {\cf cdar}
    {\cf cddr}                    {\cf cdr}
    {\cf ceiling}                 {\cf char->integer}
    {\cf char-ready?\ }            {\cf char<=?}
    {\cf char<?\ }                 {\cf char=?}
    {\cf char>=?\ }                {\cf char>?}
    {\cf char?\ }                  {\cf close-input-port}
    {\cf close-output-port}       {\cf close-port}
    {\cf complex?\ }               {\cf cond}
    {\cf cond-expand}             {\cf cons}
    {\cf current-error-port}      {\cf current-input-port}
    {\cf current-output-port}     {\cf define}
    {\cf define-record-type}      {\cf define-syntax}
    {\cf define-values}           {\cf denominator}
    {\cf do}                      {\cf dynamic-wind}
    {\cf else}                    {\cf eof-object}
    {\cf eof-object?\ }            {\cf eq?}
    {\cf equal?\ }                 {\cf eqv?}
    {\cf error}                   {\cf error-object-irritants}
    {\cf error-object-message}    {\cf error-object?}
    {\cf even?\ }                  {\cf exact}
    {\cf exact-integer-sqrt}      {\cf exact-integer?}
    {\cf exact?\ }                 {\cf expt}
    {\cf features}                {\cf file-error?}
    {\cf floor}                   {\cf floor-quotient}
    {\cf floor-remainder}         {\cf floor/}
    {\cf flush-output-port}       {\cf for-each}
    {\cf gcd}                     {\cf get-output-bytevector}
    {\cf get-output-string}       {\cf guard}
    {\cf if}                      {\cf include}
    {\cf include-ci}              {\cf inexact}
    {\cf inexact?\ }               {\cf input-port-open?}
    {\cf input-port?\ }            {\cf integer->char}
    {\cf integer?\ }               {\cf lambda}
    {\cf lcm}                     {\cf length}
    {\cf let}                     {\cf let*}
    {\cf let*-values}             {\cf let-syntax}
    {\cf let-values}              {\cf letrec}
    {\cf letrec*}                 {\cf letrec-syntax}
    {\cf list}                    {\cf list->string}
    {\cf list->vector}            {\cf list-copy}
    {\cf list-ref}                {\cf list-set!}
    {\cf list-tail}               {\cf list?}
    {\cf make-bytevector}         {\cf make-list}
    {\cf make-parameter}          {\cf make-string}
    {\cf make-vector}             {\cf map}
    {\cf max}                     {\cf member}
    {\cf memq}                    {\cf memv}
    {\cf min}                     {\cf modulo}
    {\cf negative?\ }              {\cf newline}
    {\cf not}                     {\cf null?}
    {\cf number->string}          {\cf number?}
    {\cf numerator}               {\cf odd?}
    {\cf open-input-bytevector}   {\cf open-input-string}
    {\cf open-output-bytevector}  {\cf open-output-string}
    {\cf or}                      {\cf output-port-open?}
    {\cf output-port?\ }           {\cf pair?}
    {\cf parameterize}            {\cf peek-char}
    {\cf peek-u8}                 {\cf port?}
    {\cf positive?\ }              {\cf procedure?}
    {\cf quasiquote}              {\cf quote}
    {\cf quotient}                {\cf raise}
    {\cf raise-continuable}       {\cf rational?}
    {\cf rationalize}             {\cf read-bytevector}
    {\cf read-bytevector!}        {\cf read-char}
    {\cf read-error?\ }            {\cf read-line}
    {\cf read-string}             {\cf read-u8}
    {\cf real?\ }                  {\cf remainder}
    {\cf reverse}                 {\cf round}
    {\cf set!}                    {\cf set-car!}
    {\cf set-cdr!}                {\cf square}
    {\cf string}                  {\cf string->list}
    {\cf string->number}          {\cf string->symbol}
    {\cf string->utf8}            {\cf string->vector}
    {\cf string-append}           {\cf string-copy}
    {\cf string-copy!}            {\cf string-fill!}
    {\cf string-for-each}         {\cf string-length}
    {\cf string-map}              {\cf string-ref}
    {\cf string-set!}             {\cf string<=?}
    {\cf string<?\ }               {\cf string=?}
    {\cf string>=?\ }              {\cf string>?}
    {\cf string?\ }                {\cf substring}
    {\cf symbol->string}          {\cf symbol=?}
    {\cf symbol?\ }                {\cf syntax-error}
    {\cf syntax-rules}            {\cf textual-port?}
    {\cf truncate}                {\cf truncate-quotient}
    {\cf truncate-remainder}      {\cf truncate/}
    {\cf u8-ready?\ }              {\cf unless}
    {\cf unquote}                 {\cf unquote-splicing}
    {\cf utf8->string}            {\cf values}
    {\cf vector}                  {\cf vector->list}
    {\cf vector->string}          {\cf vector-append}
    {\cf vector-copy}             {\cf vector-copy!}
    {\cf vector-fill!}            {\cf vector-for-each}
    {\cf vector-length}           {\cf vector-map}
    {\cf vector-ref}              {\cf vector-set!}
    {\cf vector?\ }                {\cf when}
    {\cf with-exception-handler}  {\cf write-bytevector}
    {\cf write-char}              {\cf write-string}
    {\cf write-u8}                {\cf zero?}

**Case-Lambda Library**

The (scheme case-lambda) library exports the case-lambda syntax.

    {\cf case-lambda}

**Char Library**

The (scheme char) library provides the procedures for dealing with
characters that involve potentially large tables when supporting all of
Unicode.

    {\cf char-alphabetic?\ }       {\cf char-ci<=?}
    {\cf char-ci<?\ }              {\cf char-ci=?}
    {\cf char-ci>=?\ }             {\cf char-ci>?}
    {\cf char-downcase}           {\cf char-foldcase}
    {\cf char-lower-case?\ }       {\cf char-numeric?}
    {\cf char-upcase}             {\cf char-upper-case?}
    {\cf char-whitespace?\ }       {\cf digit-value}
    {\cf string-ci<=?\ }           {\cf string-ci<?}
    {\cf string-ci=?\ }            {\cf string-ci>=?}
    {\cf string-ci>?\ }            {\cf string-downcase}
    {\cf string-foldcase}         {\cf string-upcase}

**Complex Library**

The (scheme complex) library exports procedures which are typically only
useful with non-real numbers.

    {\cf angle}                   {\cf imag-part}
    {\cf magnitude}               {\cf make-polar}
    {\cf make-rectangular}        {\cf real-part}

**CxR Library**

The (scheme cxr) library exports twenty-four procedures which are the
compositions of from three to four car and cdr operations. For example
caddar could be defined by

    (define caddar
      (lambda (x) (car (cdr (cdr (car x)))))){\rm.}%

The procedures car and cdr themselves and the four two-level
compositions are included in the base library. See
section [\[listsection\]](#listsection).

    {\cf caaaar}                  {\cf caaadr}
    {\cf caaar}                   {\cf caadar}
    {\cf caaddr}                  {\cf caadr}
    {\cf cadaar}                  {\cf cadadr}
    {\cf cadar}                   {\cf caddar}
    {\cf cadddr}                  {\cf caddr}
    {\cf cdaaar}                  {\cf cdaadr}
    {\cf cdaar}                   {\cf cdadar}
    {\cf cdaddr}                  {\cf cdadr}
    {\cf cddaar}                  {\cf cddadr}
    {\cf cddar}                   {\cf cdddar}
    {\cf cddddr}                  {\cf cdddr}

**Eval Library**

The (scheme eval) library exports procedures for evaluating Scheme data
as programs.

    {\cf environment}             {\cf eval}

**File Library**

The (scheme file) library provides procedures for accessing files.

    {\cf call-with-input-file}    {\cf call-with-output-file}
    {\cf delete-file}             {\cf file-exists?}
    {\cf open-binary-input-file}  {\cf open-binary-output-file}
    {\cf open-input-file}         {\cf open-output-file}
    {\cf with-input-from-file}    {\cf with-output-to-file}

**Inexact Library**

The (scheme inexact) library exports procedures which are typically only
useful with inexact values.

    {\cf acos}                    {\cf asin}
    {\cf atan}                    {\cf cos}
    {\cf exp}                     {\cf finite?}
    {\cf infinite?\ }              {\cf log}
    {\cf nan?\ }                   {\cf sin}
    {\cf sqrt}                    {\cf tan}

**Lazy Library**

The (scheme lazy) library exports procedures and syntax keywords for
lazy evaluation.

    {\cf delay}                   {\cf delay-force}
    {\cf force}                   {\cf make-promise}
    {\cf promise?}

**Load Library**

The (scheme load) library exports procedures for loading Scheme
expressions from files.

    {\cf load}

**Process-Context Library**

The (scheme process-context) library exports procedures for accessing
with the program’s calling context.

    {\cf command-line}            {\cf emergency-exit}
    {\cf exit}
    {\cf get-environment-variable}
    {\cf get-environment-variables}

**Read Library**

The (scheme read) library provides procedures for reading Scheme
objects.

    {\cf read}

**Repl Library**

The (scheme repl) library exports the interaction-environment procedure.

    {\cf interaction-environment}

**Time Library**

The (scheme time) library provides access to time-related values.

    {\cf current-jiffy}           {\cf current-second}
    {\cf jiffies-per-second}

**Write Library**

The (scheme write) library provides procedures for writing Scheme
objects.

    {\cf display}                 {\cf write}
    {\cf write-shared}            {\cf write-simple}

**R5RS Library**

The (scheme r5rs) library provides the identifiers defined by
R<sup>5</sup>RS, except that transcript-on and transcript-off are not
present. Note that the exact and inexact procedures appear under their
R<sup>5</sup>RS names inexact->exact and exact->inexact respectively.
However, if an implementation does not provide a particular library such
as the complex library, the corresponding identifiers will not appear in
this library either.

\* + - ... / \< \<= = => \> \>= \_ abs acos and angle append apply asin
assoc assq assv atan begin boolean?  caaaar caaadr caaar caadar caaddr
caadr caar cadaar cadadr cadar caddar cadddr caddr cadr
call-with-current-continuation call-with-input-file
call-with-output-file call-with-values car case cdaaar cdaadr cdaar
cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr
cdr ceiling char->integer char-alphabetic?  char-ci\<=? char-ci\<? 
char-ci=? char-ci>=?  char-ci>? char-downcase char-lower-case?
char-numeric?  char-ready? char-upcase char-upper-case?
char-whitespace?  char\<=? char\<?  char=? char>=?  char>? char? 
close-input-port close-output-port complex? cond cons cos
current-input-port current-output-port define define-syntax delay
denominator display do dynamic-wind else eof-object? eq?  equal? eqv? 
eval even?  exact->inexact exact?  exp expt floor for-each force gcd if
imag-part inexact->exact inexact?  input-port? integer->char integer?
interaction-environment lambda lcm length let let\* let-syntax letrec
letrec-syntax list list->string list->vector list-ref list-tail list? 
load log magnitude make-polar make-rectangular make-string make-vector
map max member memq memv min modulo negative? newline not
null-environment null? number->string number? numerator odd?
open-input-file open-output-file or output-port? pair?  peek-char
positive?  procedure? quasiquote quote quotient rational? rationalize
read read-char real-part real?  remainder reverse round
scheme-report-environment set! set-car! set-cdr! sin sqrt string
string->list string->number string->symbol string-append string-ci\<=? 
string-ci\<? string-ci=?  string-ci>=? string-ci>?  string-copy
string-fill! string-length string-ref string-set! string\<=?  string\<?
string=?  string>=? string>?  string? substring symbol->string symbol? 
syntax-rules tan truncate values vector vector->list vector-fill!
vector-length vector-ref vector-set! vector? with-input-from-file
with-output-to-file write write-char zero?

# Standard Feature Identifiers

An implementation may provide any or all of the feature identifiers
listed below for use by cond-expand and features, but must not provide a
feature identifier if it does not provide the corresponding feature.

r7rs

All R<sup>7</sup>RS Scheme implementations have this feature.

exact-closed

The algebraic operations +, -, \*, and expt where the second argument is
a non-negative integer produce exact values given exact inputs.

exact-complex

Exact complex numbers are provided.

ieee-float

Inexact numbers are IEEE 754 binary floating point values.

full-unicode

All Unicode characters present in Unicode version 6.0 are supported as
Scheme characters.

ratios

/ with exact arguments produces an exact result when the divisor is
nonzero.

posix

This implementation is running on a POSIX system.

windows

This implementation is running on Windows.

unix, darwin, gnu-linux, bsd, freebsd, solaris, ...

Operating system flags (perhaps more than one).

i386, x86-64, ppc, sparc, jvm, clr, llvm, ...

CPU architecture flags.

ilp32, lp64, ilp64, ...

C memory model flags.

big-endian, little-endian

Byte order flags.

The name of this implementation.

The name and version of this implementation.

# Language changes

1ex

### Incompatibilities with R<sup>5</sup>RS

This section enumerates the incompatibilities between this report and
the “Revised<sup>5</sup> report” <span class="citation"
cites="R5RS"></span>.

*This list is not authoritative, but is believed to be correct and
complete.*

-   Case sensitivity is now the default in symbols and character names.
    This means that code written under the assumption that symbols could
    be written FOO or Foo in some contexts and foo in other contexts can
    either be changed, be marked with the new #!fold-case directive, or
    be included in a library using the include-ci library declaration.
    All standard identifiers are entirely in lower case.

-   The syntax-rules construct now recognizes *\_* (underscore) as a
    wildcard, which means it cannot be used as a syntax variable. It can
    still be used as a literal.

-   The R<sup>5</sup>RS procedures exact->inexact and inexact->exact
    have been renamed to their R<sup>6</sup>RS names, inexact and exact,
    respectively, as these names are shorter and more correct. The
    former names are still available in the R<sup>5</sup>RS library.

-   The guarantee that string comparison (with string\<? and the related
    predicates) is a lexicographical extension of character comparison
    (with char\<? and the related predicates) has been removed.

-   Support for the # character in numeric literals is no longer
    required.

-   Support for the letters s, f, d, and l as exponent markers is no
    longer required.

-   Implementations of stringnumber are no longer permitted to return #f
    when the argument contains an explicit radix prefix, and must be
    compatible with read and the syntax of numbers in programs.

-   The procedures transcript-on and transcript-off have been removed.

### Other language changes since R<sup>5</sup>RS

This section enumerates the additional differences between this report
and the “Revised<sup>5</sup> report” <span class="citation"
cites="R5RS"></span>.

*This list is not authoritative, but is believed to be correct and
complete.*

-   Various minor ambiguities and unclarities in R<sup>5</sup>RS have
    been cleaned up.

-   Libraries have been added as a new program structure to improve
    encapsulation and sharing of code. Some existing and new identifiers
    have been factored out into separate libraries. Libraries can be
    imported into other libraries or main programs, with controlled
    exposure and renaming of identifiers. The contents of a library can
    be made conditional on the features of the implementation on which
    it is to be used. There is an R<sup>5</sup>RS compatibility library.

-   The expressions types include, include-ci, and cond-expand have been
    added to the base library; they have the same semantics as the
    corresponding library declarations.

-   Exceptions can now be signaled explicitly with raise,
    raise-continuable or error, and can be handled with
    with-exception-handler and the guard syntax. Any object can specify
    an error condition; the implementation-defined conditions signaled
    by error have a predicate to detect them and accessor functions to
    retrieve the arguments passed to error. Conditions signaled by read
    and by file-related procedures also have predicates to detect them.

-   New disjoint types supporting access to multiple fields can be
    generated with the define-record-type of SRFI 9 <span
    class="citation" cites="srfi9"></span>

-   Parameter objects can be created with make-parameter, and
    dynamically rebound with parameterize. The procedures
    current-input-port and current-output-port are now parameter
    objects, as is the newly introduced current-error-port.

-   Support for promises has been enhanced based on SRFI 45 <span
    class="citation" cites="srfi45"></span>.

-   *Bytevectors*, vectors of exact integers in the range from 0 to 255
    inclusive, have been added as a new disjoint type. A subset of the
    vector procedures is provided. Bytevectors can be converted to and
    from strings in accordance with the UTF-8 character encoding.
    Bytevectors have a datum representation and evaluate to themselves.

-   Vector constants evaluate to themselves.

-   The procedure read-line is provided to make line-oriented textual
    input simpler.

-   The procedure flush-output-port is provided to allow minimal control
    of output port buffering.

-   *Ports* can now be designated as *textual* or *binary* ports, with
    new procedures for reading and writing binary data. The new
    predicates input-port-open? and output-port-open? return whether a
    port is open or closed. The new procedure close-port now closes a
    port; if the port has both input and output sides, both are closed.

-   *String ports* have been added as a way to read and write characters
    to and from strings, and *bytevector ports* to read and write bytes
    to and from bytevectors.

-   There are now I/O procedures specific to strings and bytevectors.

-   The write procedure now generates datum labels when applied to
    circular objects. The new procedure write-simple never generates
    labels; write-shared generates labels for all shared and circular
    structure. The display procedure must not loop on circular objects.

-   The R<sup>6</sup>RS procedure eof-object has been added. Eof-objects
    are now required to be a disjoint type.

-   Syntax definitions are now allowed wherever variable definitions
    are.

-   The syntax-rules construct now allows the ellipsis symbol to be
    specified explicitly instead of the default ..., allows template
    escapes with an ellipsis-prefixed list, and allows tail patterns to
    follow an ellipsis pattern.

-   The syntax-error syntax has been added as a way to signal immediate
    and more informative errors when a macro is expanded.

-   The letrec\* binding construct has been added, and internal define
    is specified in terms of it.

-   Support for capturing multiple values has been enhanced with
    define-values, let-values, and let\*-values. Standard expression
    types which contain a sequence of expressions now permit passing
    zero or more than one value to the continuations of all non-final
    expressions of the sequence.

-   The case conditional now supports `=>` syntax analogous to cond not
    only in regular clauses but in the else clause as well.

-   To support dispatching on the number of arguments passed to a
    procedure, case-lambda has been added in its own library.

-   The convenience conditionals when and unless have been added.

-   The behavior of eqv? on inexact numbers now conforms to the
    R<sup>6</sup>RS definition.

-   When applied to procedures, eq? and eqv? are permitted to return
    different answers.

-   The R<sup>6</sup>RS procedures boolean=? and symbol=? have been
    added.

-   Positive infinity, negative infinity, NaN, and negative inexact zero
    have been added to the numeric tower as inexact values with the
    written representations `+inf.0`, `-inf.0`, `+nan.0`, and -0.0
    respectively. Support for them is not required. The representation
    `-nan.0` is synonymous with `+nan.0`.

-   The log procedure now accepts a second argument specifying the
    logarithm base.

-   The procedures map and for-each are now required to terminate on the
    shortest argument list.

-   The procedures member and assoc now take an optional third argument
    specifying the equality predicate to be used.

-   The numeric procedures finite?, infinite?, nan?, exact-integer?,
    square, and exact-integer-sqrt have been added.

-   The - and / procedures and the character and string comparison
    predicates are now required to support more than two arguments.

-   The forms #true and #false are now supported as well as #t and #f.

-   The procedures make-list, list-copy, list-set!, string-map,
    string-for-each, string->vector, vector-append, vector-copy,
    vector-map, vector-for-each, vector->string, vector-copy!, and
    string-copy! have been added to round out the sequence operations.

-   Some string and vector procedures support processing of part of a
    string or vector using optional *start* and *end* arguments.

-   Some list procedures are now defined on circular lists.

-   Implementations may provide any subset of the full Unicode
    repertoire that includes ASCII, but implementations must support any
    such subset in a way consistent with Unicode. Various character and
    string procedures have been extended accordingly, and case
    conversion procedures added for strings. String comparison is no
    longer required to be consistent with character comparison, which is
    based solely on Unicode scalar values. The new digit-value procedure
    has been added to obtain the numerical value of a numeric character.

-   There are now two additional comment syntaxes: `#;` to skip the next
    datum, and `#| ... |#` for nestable block comments.

-   Data prefixed with datum labels `#<n>=` can be referenced with
    `#<n>#`, allowing for reading and writing of data with shared
    structure.

-   Strings and symbols now allow mnemonic and numeric escape sequences,
    and the list of named characters has been extended.

-   The procedures file-exists? and delete-file are available in the
    `(scheme file)` library.

-   An interface to the system environment, command line, and process
    exit status is available in the `(scheme process-context)` library.

-   Procedures for accessing time-related values are available in the
    `(scheme time)` library.

-   A less irregular set of integer division operators is provided with
    new and clearer names.

-   The load procedure now accepts a second argument specifying the
    environment to load into.

-   The call-with-current-continuation procedure now has the synonym
    call/cc.

-   The semantics of read-eval-print loops are now partly prescribed,
    requiring the redefinition of procedures, but not syntax keywords,
    to have retroactive effect.

-   The formal semantics now handles dynamic-wind.

### Incompatibilities with R<sup>6</sup>RS

This section enumerates the incompatibilities between
R<sup>7</sup>RS and the “Revised<sup>6</sup> report” <span
class="citation" cites="R6RS"></span> and its accompanying Standard
Libraries document.

*This list is not authoritative, and is possibly incomplete.*

-   R<sup>7</sup>RS libraries begin with the keyword define-library
    rather than library in order to make them syntactically
    distinguishable from R<sup>6</sup>RS libraries. In
    R<sup>7</sup>RS terms, the body of an R<sup>6</sup>RS library
    consists of a single export declaration followed by a single import
    declaration, followed by commands and definitions. In
    R<sup>7</sup>RS, commands and definitions are not permitted directly
    within the body: they have to be wrapped in a begin library
    declaration.

-   There is no direct R<sup>6</sup>RS equivalent of the include,
    include-ci, include-library-declarations, or cond-expand library
    declarations. On the other hand, the R<sup>7</sup>RS library syntax
    does not support phase or version specifications.

-   The grouping of standardized identifiers into libraries is different
    from the R<sup>6</sup>RS approach. In particular, procedures which
    are optional in R<sup>5</sup>RS  either expressly or by implication,
    have been removed from the base library. Only the base library
    itself is an absolute requirement.

-   No form of identifier syntax is provided.

-   Internal syntax definitions are allowed, but uses of a syntax form
    cannot appear before its definition; the even/odd example given in
    R<sup>6</sup>RS is not allowed.

-   The R<sup>6</sup>RS exception system was incorporated as-is, but the
    condition types have been left unspecified. In particular, where
    R<sup>6</sup>RS requires a condition of a specified type to be
    signaled, R<sup>7</sup>RS says only “it is an error”, leaving the
    question of signaling open.

-   Full Unicode support is not required. Normalization is not provided.
    Character comparisons are defined by Unicode, but string comparisons
    are implementation-dependent. Non-Unicode characters are permitted.

-   The full numeric tower is optional as in R<sup>5</sup>RS, but
    optional support for IEEE infinities, NaN, and -0.0 was adopted from
    R<sup>6</sup>RS. Most clarifications on numeric results were also
    adopted, but the semantics of the R<sup>6</sup>RS procedures real?,
    rational?, and integer? were not adopted. (Note that the
    R<sup>5</sup>RS/R<sup>7</sup>RS semantics are available in
    R<sup>6</sup>RS using real-valued?, rational-valued?, and
    integer-valued?). The R<sup>6</sup>RS division operators div, mod,
    div-and-mod, div0, mod0 and div0-and-mod0 are not provided.

-   When a result is unspecified, it is still required to be a single
    value. However, non-final expressions in a body can return any
    number of values.

-   The semantics of map and for-each have been changed to use the SRFI
    1 <span class="citation" cites="srfi1"></span> early termination
    behavior. Likewise, assoc and member take an optional equal?
    argument as in SRFI 1, instead of the separate assp and memp
    procedures of R<sup>6</sup>RS.

-   The R<sup>6</sup>RS quasiquote clarifications have been adopted,
    with the exception of multiple-argument unquote and
    unquote-splicing.

-   The R<sup>6</sup>RS method of specifying mantissa widths was not
    adopted.

-   String ports are compatible with SRFI 6 <span class="citation"
    cites="srfi6"></span> rather than R<sup>6</sup>RS.

-   R<sup>6</sup>RS-style bytevectors are included, but only the
    unsigned byte (u8) procedures have been provided. The lexical syntax
    uses #u8 for compatibility with SRFI 4 <span class="citation"
    cites="srfi4"></span>, rather than the R<sup>6</sup>RS #vu8 style.

-   The utility macros when and unless are provided, but their result is
    left unspecified.

-   The remaining features of the Standard Libraries document were left
    to future standardization efforts.

# Additional material

1ex

The Scheme community website at http://schemers.org contains additional
resources for learning and programming, job and event postings, and
Scheme user group information.

A bibliography of Scheme-related research at
http://library.readscheme.org links to technical papers and theses
related to the Scheme language, including both classic papers and recent
research.

On-line Scheme discussions are held using IRC on the #scheme channel at
irc.freenode.net and on the Usenet discussion group comp.lang.scheme.

# Example

1ex

The procedure integrate-system integrates the system
*y*<sub>*k*</sub><sup>′</sup> = *f*<sub>*k*</sub>(*y*<sub>1</sub>,*y*<sub>2</sub>,…,*y*<sub>*n*</sub>), *k* = 1, …, *n*
of differential equations with the method of Runge-Kutta.

The parameter `system-derivative` is a function that takes a system
state (a vector of values for the state variables
*y*<sub>1</sub>, …, *y*<sub>*n*</sub>) and produces a system derivative
(the values
*y*<sub>1</sub><sup>′</sup>, …, *y*<sub>*n*</sub><sup>′</sup>). The
parameter `initial-state` provides an initial system state, and `h` is
an initial guess for the length of the integration step.

The value returned by integrate-system is an infinite stream of system
states.

    (define (integrate-system system-derivative
                              initial-state
                              h)
      (let ((next (runge-kutta-4 system-derivative h)))
        (letrec ((states
                  (cons initial-state
                        (delay (map-streams next
                                            states)))))
          states)))%

The procedure runge-kutta-4 takes a function, `f`, that produces a
system derivative from a system state. It produces a function that takes
a system state and produces a new system state.

    (define (runge-kutta-4 f h)
      (let ((*h (scale-vector h))
            (*2 (scale-vector 2))
            (*1/2 (scale-vector (/ 1 2)))
            (*1/6 (scale-vector (/ 1 6))))
        (lambda (y)
          ;; y is a system state
          (let* ((k0 (*h (f y)))
                 (k1 (*h (f (add-vectors y (*1/2 k0)))))
                 (k2 (*h (f (add-vectors y (*1/2 k1)))))
                 (k3 (*h (f (add-vectors y k2)))))
            (add-vectors y
              (*1/6 (add-vectors k0
                                 (*2 k1)
                                 (*2 k2)
                                 k3)))))))

    (define (elementwise f)
      (lambda vectors
        (generate-vector
         (vector-length (car vectors))
         (lambda (i)
           (apply f
                  (map (lambda (v) (vector-ref  v i))
                       vectors))))))

    (define (generate-vector size proc)
      (let ((ans (make-vector size)))
        (letrec ((loop
                  (lambda (i)
                    (cond ((= i size) ans)
                          (else
                           (vector-set! ans i (proc i))
                           (loop (+ i 1)))))))
          (loop 0))))

    (define add-vectors (elementwise +))

    (define (scale-vector s)
      (elementwise (lambda (x) (* x s))))%

The map-streams procedure is analogous to map: it applies its first
argument (a procedure) to all the elements of its second argument (a
stream).

    (define (map-streams f s)
      (cons (f (head s))
            (delay (map-streams f (tail s)))))%

Infinite streams are implemented as pairs whose car holds the first
element of the stream and whose cdr holds a promise to deliver the rest
of the stream.

    (define head car)
    (define (tail stream)
      (force (cdr stream)))%

The following illustrates the use of integrate-system in integrating the
system <span class="math display">$$C {dv_C \\over dt} = -i_L - {v_C
\\over R}$$</span> <span class="math display">$$L {di_L \\over dt} =
v_C$$</span> which models a damped oscillator.

    (define (damped-oscillator R L C)
      (lambda (state)
        (let ((Vc (vector-ref state 0))
              (Il (vector-ref state 1)))
          (vector (- 0 (+ (/ Vc (* R C)) (/ Il C)))
                  (/ Vc L)))))

    (define the-states
      (integrate-system
         (damped-oscillator 10000 1000 .001)
         '\#(1 0)
         .01))%

<div class="thebibliography">

999

Harold Abelson and Gerald Jay Sussman with Julie Sussman. *Structure and
Interpretation of Computer Programs, second edition.* MIT Press,
Cambridge, 1996.

Alan Bawden and Jonathan Rees. Syntactic closures. In *Proceedings of
the 1988 ACM Symposium on Lisp and Functional Programming*, pages 86–95.

S. Bradner. Key words for use in RFCs to Indicate Requirement Levels.
<http://www.ietf.org/rfc/rfc2119.txt>, 1997.

Robert G. Burger and R. Kent Dybvig. Printing floating-point numbers
quickly and accurately. In *Proceedings of the ACM SIGPLAN ’96
Conference on Programming Language Design and Implementation*,
pages 108–116.

William Clinger. How to read floating point numbers accurately. In
*Proceedings of the ACM SIGPLAN ’90 Conference on Programming Language
Design and Implementation*, pages 92–101. Proceedings published as
*SIGPLAN Notices* 25(6), June 1990.

William Clinger. Proper Tail Recursion and Space Efficiency. In
*Proceedings of the 1998 ACM Conference on Programming Language Design
and Implementation*, June 1998.

William Clinger. SRFI 6: Basic String Ports.
<http://srfi.schemers.org/srfi-6/>, 1999.

William Clinger, editor. The revised revised report on Scheme, or an
uncommon Lisp. MIT Artificial Intelligence Memo 848, August 1985. Also
published as Computer Science Department Technical Report 174, Indiana
University, June 1985.

William Clinger and Jonathan Rees. Macros that work. In *Proceedings of
the 1991 ACM Conference on Principles of Programming Languages*,
pages 155–162.

William Clinger and Jonathan Rees, editors. The revised<sup>4</sup>
report on the algorithmic language Scheme. In *ACM Lisp Pointers* 4(3),
pages 1–55, 1991.

Mark Davis. Unicode Standard Annex #44, Unicode Character Database.
<http://unicode.org/reports/tr44/>, 2010.

R. Kent Dybvig, Robert Hieb, and Carl Bruggeman. Syntactic abstraction
in Scheme. *Lisp and Symbolic Computation* 5(4):295–326, 1993.

Marc Feeley. SRFI 4: Homogeneous Numeric Vector Datatypes.
<http://srfi.schemers.org/srfi-4/>, 1999.

Carol Fessenden, William Clinger, Daniel P. Friedman, and Christopher
Haynes. Scheme 311 version 4 reference manual. Indiana University
Computer Science Technical Report 137, February 1983. Superseded
by <span class="citation" cites="Scheme84"></span>.

D. Friedman, C. Haynes, E. Kohlbecker, and M. Wand. Scheme 84 interim
reference manual. Indiana University Computer Science Technical Report
153, January 1985.

Martin Gardner. Mathematical Games: The fantastic combinations of John
Conway’s new solitaire game “Life.” In *Scientific American*,
223:120–123, October 1970.

*IEEE Standard 754-2008. IEEE Standard for Floating-Point Arithmetic.*
IEEE, New York, 2008.

*IEEE Standard 1178-1990. IEEE Standard for the Scheme Programming
Language.* IEEE, New York, 1991.

Richard Kelsey. SRFI 9: Defining Record Types.
<http://srfi.schemers.org/srfi-9/>, 1999.

Richard Kelsey, William Clinger, and Jonathan Rees, editors. The
revised<sup>5</sup> report on the algorithmic language Scheme.
*Higher-Order and Symbolic Computation*, 11(1):7-105, 1998.

Eugene E. Kohlbecker Jr. *Syntactic Extensions in the Programming
Language Lisp.* PhD thesis, Indiana University, August 1986.

Eugene E. Kohlbecker Jr., Daniel P. Friedman, Matthias Felleisen, and
Bruce Duba. Hygienic macro expansion. In *Proceedings of the 1986 ACM
Conference on Lisp and Functional Programming*, pages 151–161.

John McCarthy. Recursive Functions of Symbolic Expressions and Their
Computation by Machine, Part I. *Communications of the ACM*
3(4):184–195, April 1960.

MIT Department of Electrical Engineering and Computer Science. Scheme
manual, seventh edition. September 1984.

Peter Naur et al. Revised report on the algorithmic language Algol 60.
*Communications of the ACM* 6(1):1–17, January 1963.

Paul Penfield, Jr. Principal values and branch cuts in complex APL. In
*APL ’81 Conference Proceedings,* pages 248–256. ACM SIGAPL, San
Francisco, September 1981. Proceedings published as *APL Quote Quad*
12(1), ACM, September 1981.

Jonathan A. Rees and Norman I. Adams IV. T: A dialect of Lisp or,
lambda: The ultimate software tool. In *Conference Record of the 1982
ACM Symposium on Lisp and Functional Programming*, pages 114–122.

Jonathan A. Rees, Norman I. Adams IV, and James R. Meehan. The T manual,
fourth edition. Yale University Computer Science Department, January
1984.

Jonathan Rees and William Clinger, editors. The revised<sup>3</sup>
report on the algorithmic language Scheme. In *ACM SIGPLAN Notices*
21(12), pages 37–79, December 1986.

Olin Shivers. SRFI 1: List Library. <http://srfi.schemers.org/srfi-1/>,
1999.

Guy Lewis Steele Jr. and Gerald Jay Sussman. The revised report on
Scheme, a dialect of Lisp. MIT Artificial Intelligence Memo 452, January
1978.

Guy Lewis Steele Jr. Rabbit: a compiler for Scheme. MIT Artificial
Intelligence Laboratory Technical Report 474, May 1978.

Michael Sperber, R. Kent Dybvig, Mathew Flatt, and Anton van Straaten,
editors. *The revised<sup>6</sup> report on the algorithmic language
Scheme.* Cambridge University Press, 2010.

Guy Lewis Steele Jr. *Common Lisp: The Language, second edition.*
Digital Press, Burlington MA, 1990.

Gerald Jay Sussman and Guy Lewis Steele Jr. Scheme: an interpreter for
extended lambda calculus. MIT Artificial Intelligence Memo 349, December
1975.

Joseph E. Stoy. *Denotational Semantics: The Scott-Strachey Approach to
Programming Language Theory.* MIT Press, Cambridge, 1977.

Texas Instruments, Inc. TI Scheme Language Reference Manual. Preliminary
version 1.0, November 1985.

Andre van Tonder. SRFI 45: Primitives for Expressing Iterative Lazy
Algorithms. <http://srfi.schemers.org/srfi-45/>, 2002.

Martin Gasbichler, Eric Knauel, Michael Sperber and Richard Kelsey. How
to Add Threads to a Sequential Language Without Getting Tangled Up.
*Proceedings of the Fourth Workshop on Scheme and Functional
Programming*, November 2003.

International Earth Rotation Service. Historical table of TAI-UTC
offsets. <http://maia.usno.navy.mil/ser7/tai-utc.dat>

</div>
