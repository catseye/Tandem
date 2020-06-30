Tandem
======

**Tandem** is an experimental rewriting language where the rewrite rules form a Kleene algebra.

The object being rewritten by a Tandem program is a collection of labelled stacks. This can be thought of as a finite mapping from strings to strings. The strings are always rewritten at the left edge, so they are effectively stacks.

A Tandem program consists of a single rewrite rule along with zero or more pragmas. The rewrite rule is applied to an initial state to possibly obtain a final state. The rule is applied only once, however it may contain subcomponents that are applied many times.

Rewrite rules
-------------

A rewrite rule is defined inductively as follows:

-   **0** is a rewrite rule.
-   **1** is a rewrite rule.
-   If *l* is a character and *s* and *t* are strings, then:
    -   *ls* → *t* is a rewrite rule.
    -   *ls*… → *t* is a rewrite rule.
    -   *ls*… → *t*… is a rewrite rule.
-   If Ri and Rj are rewrite rules, then Ri | Rj is a rewrite rule.
-   If Ri and Rj are rewrite rules, then Ri & Rj is a rewrite rule.
-   If R is a rewrite rule, then R\* is a rewrite rule.
-   Nothing else is a rewrite rule.

A rewrite rule may either match and change the state, match and make no change to the state (e.g. replacing X with X), or fail to match (in which case the state does not change).

In the following, we say M(R,S) for the set of labelled stacks that would be matched by R in some state S. These are the redexes of R in S. If M(R,S) is the empty set then R fails to match on S.

Tandem is, at its core, nondeterministic: a rule can match multiple redexes, and the language gives no guidance as to which of those redexes it should select for rewriting. Thus it is up to an implementation to decide what to do when multiple redexes match.

However, it is expected that most implementations of Tandem will provide a “strictly deterministic” mode of execution where, if a rule matches more than one redex during program execution, an unrecoverable error to the effect of “multiple rewrite choices encountered” occurs.

#### 0 and 1

**0** is an artifical rewrite rule that always fails to match. **1** is an artifical rewrite rule that always succeeds in matching, and changes nothing. These are the identity elements for disjunction and conjunction respectively. More will be said about them later.

#### Individual rewrite rules

In an individual rewrite rule, *l* identifies the stack which will be rewritten. In this section “the stack” means “the stack labelled with *l*”.

There are three variations on individual rewrite rules.

Variation 1: *s* is not followed by the special signifier …: If the stack equals *s* exactly, then the stack will become *t* exactly.

Variation 2: *s* is followed by the special signifier … but *t* is not: If the stack begins with *s*, then the stack will become *t* exactly.

Variation 3: *s* is followed by the special signifier … and so is *t*: If the stack begins with *s*, then that *s* will be removed from the stack and *t* will be prepended on the stack in its place.

Examples follow. If the stack labelled R contains only `name`, replace it with `mud`:

`Rname → mud`

If the stack labelled Q starts with `m`, replace the whole thing with `mud`:

`Qm… → mud`

Strip a leading `0` from the stack labelled A if one is present:

`A0… → …`

If the top element of A is `0` and the element under it is `1`, swap them:

`A01… → 10…`

#### Disjunction

Ri | Rj means that either Ri can be applied or Rj can be applied. If both Ri would fail to match and Rj would fail to match, then Ri | Rj fails to match.

M(Ri | Rj, S) = M(Ri, S) ∪ M(Rj, S).

Ri | Rj can introduce nondeterminism, if both Ri and Rj match; see note on nondeterminism above.

Examples follow. Rewrite `a` to `b`, or `b` to `a`, on the stack labelled A, whichever (if either) is applicable:

`Aa → b | Ab → a`

#### Conjunction

Ri & Rj means to first apply Ri to obtain a modified state, then apply Rj to the modified state. If Ri does not match, or Rj does not match (on the modified state) then Ri & Rj does not match, and no changes are made to the state.

M(Ri & Rj, S) = M(Rj, S') where S' is the modified state (assuming M(Ri, S) is not empty; otherwise M(Ri & Rj, S) is empty).

Examples follow. Rewrite `a` to `b` on the stack labelled X, and `b` to `a` on the stack labelled Y, assuming both of these rewrites are possible. (If only one is possible, nothing happens):

`Xa → b & Yb → a`

Never matches, because even if the first rewrite suceeded, the second one could not possibly succeed:

`Xa → b & Xc → d`

(Note that this definition of & is different from the original (pre-June 25th) definition; the previous definition was incompatible with the definition of a Kleene algebra, and apparently basically flawed as an component in this algebraic structure to boot.)

#### Asteration

R\* means: repeatedly apply R while M(R, S) is nonempty for the current state S. R\* is always considered to succeed in matching, even if R was not matched even once.

Examples follow. Strip *all* leading `0`s from the stack labelled A:

`A0…→…*`

Syntax
------

`&` has a higher precedence than `|`. `*` is postfix and has the highest precedence of all. Enclose expressions in parentheses `(` and `)` to get around the precedence rules.

For ease of typing, `->` is allowed as a synonym for `→`, and `...` is allowed as a synonym for `…`.

A stack may be labelled with any string. The most common choice is a single uppercase Latin alphabetic character, in which case no special syntax is needed. For other names, the label must be enclosed in double-quotes.

`Ab→c`
“`A`”`b→c`
“`29glerph`”`b→c`

The strings to be rewritten may also be quoted, and this is how one can get spaces and special forms such as `->` and `...` onto a stack.

`Ab→" c"`
“`->`”“`->`”`->`“`->`”

Spaces between the label, the strings, the ..., and the -&gt; are otherwise ignored. The following are equivalent:

`Ab…→c…`
`A b … → c …`

Inside a quoted string, three escape sequences are recognized:

-   `\"`: a literal double-quote
-   `\\`: a literal backslash
-   `\{xxxx}`: a Unicode character given by its Unicode code point xxxx given in hexadecimal

As a matter of syntactic sugar, the format of the … forms of the individual rules may be reversed by preceding the rule with a `%` symbol. This is convenient for when you want to think of the stack as having its top element on the right edge of the string, rather than the left. (It is not stored differently in any way, it is merely a sugar on the notation).

`%C…a → …b`
`%C…foo → foo`

Initial State
-------------

At the beginning of a Tandem program with rewrite rule R, it can be assumed that, under normal conditions, for each label mentioned in the program, there exists a labelled stack in the collection with that label and it is initialized with an empty string. This leads to the following idiom to initialize stacks with initial values:

`S→0 |`
`T→$ |`
`K→1111 |`
`...etc...`

Pragmas
-------

A pragma is a special string enclosed in braces (`{` and `}`) which may influence how the Tandem program in question is to be interpreted.

### `{B:...}`, `{S:...}`, `{C:...}`

See “Input and output” below.

### `{!...}`

Comment. Any character except `}` may appear between the `!` and the closing `}`. Comments are ignored by the implementation.

Input and Output
----------------

No implementation of Tandem is required to implement I/O, however implementations are encouraged to provide, at the least, some facility for the user to inspect the state of the collection of stacks after the rewriting has been applied. Even better if they are given a way to specify the initial contents of one or more stacks.

That said, if an implementation wants to implement I/O, there are several I/O disciplines that Tandem might define. A Tandem program can indicate it expects a particular I/O discipline via a pragma.

### Batch I/O

The pragma `{B:i,o}` where *i* and *o* are stack labels indicates a program expects batch I/O in the following manner.

At the beginning of the program, standard input to the program is loaded onto the stack labelled *i*. The top of this stack will be the first character read from the input. An empty stack indicates end of input.

At the conclusion of the program, the contents of the stack *o* are written to the standard output. Because it is useful to accumulate output as the program runs, the top of this stack will be the *last* character sent to the output. It may also be convenient to use the `%` sugar to make accumuluating output look more natural.

### Stream I/O

The pragma `{S:...}` indicates the program expects stream I/O. INCOMPLETE.

### Console I/O

The pragma `{C:...}` indicates the program expects reactive console I/O. INCOMPLETE.

Example programs
----------------

### Rudimentary examples

[Hello, world!](Hello,_world! "wikilink"), using batch I/O.

`{B:I,O}%O… → `“`Hello,`` ``world!`”

[Cat program](Cat_program "wikilink"), using batch I/O. Note that this only supports inputs consisting of `0`'s and `1`'s, so a better title might be “binary cat program”.

`{B:I,O}`
`(`
`  Q→0`
`  Q0→0 & I0…→… & %O…→…0 |`
`  Q0→0 & I1…→… & %O…→…1 |`
`  Q0→1 & I→`
`)*`

[Reverse cat](Reverse_cat "wikilink") program, using batch I/O. Unlike the above this will accept arbitrary characters in the input. On the other hand, it is rather cheaty. To make it a valid program, there needs to be a rewrite rule, so we choose one that we know will simply never succeed.

`{B:B,B}0`

### Implementing Automata in Tandem

Writing finite automata, push-down automata, Turing machines, and other automata is quite natural in Tandem, because transition rules such as “In state 4, if the next character in the input is `g`, consume it and push `$` onto the stack and go to state 9” translate quite straightforwardly to rewrite rules such as

`Q4 → 9 & Ig… → … & K… → $…`

#### Finite-state automaton

Here is a [finite-state automaton](finite-state_automaton "wikilink") that recognizes all strings beginning with `cat` or `cot`.

`{B:I,O}`
`(`
`  Q → 0 |`
`  O → N |`
`  Q0 → 1 & Ic… → … |`
`  Q1 → 2 & Ia… → … |`
`  Q1 → 2 & Io… → … |`
`  Q2 → 3 & It → & O… → Y`
`)*`

#### Push-down automaton

Here is a [push-down automaton](push-down_automaton "wikilink") that recognizes strings of nested parentheses.

`{B:I,O}`
`(`
`  O → N |`
`  Q → 0 |`
`  K → $ |`
`  Q0 → 1 & I`“`(`”`… → … & K… → $… |`
`  Q1 → 1 & I`“`(`”`… → … & K… → X… |`
`  Q1 → 1 & I`“`)`”`… → … & KX… → … |`
`  Q1 → 0 & I`“`)`”`… → … & K$… → … |`
`  Q0 → 2 & I → & O… → Y`
`)*`

#### Turing machine

Here is an arbitrarily-chosen [Turing machine](Turing_machine "wikilink"):

-   In state 0, if the symbol on tape is:
    -   0, enter state 1;
    -   1, move head right one square, and remain in state 0.
-   In state 1, if the symbol on tape is:
    -   0, write 1, move head left one square, and remain in state 1;
    -   1, move head right one square, and enter state 2.
-   In state 2, if the symbol on tape is:
    -   0, write 1, move head right one square, and remain in state 2;
    -   1, write 0 and enter state 3.
-   In state 3, halt.

Here we implement it in Tandem. We store the left half of the tape reversed in the L stack and the right half in the R stack. The tape cell under the tape head is the top element of the R stack.

`(`
`  Q → 0 |`
`  L →   |`
`  R → 111110 |`
`  Q0 → 1 & R0… → 0…                  |`
`  Q0 → 0 & R1… → …   & %L… → …1 |`
`  Q1 → 1 & R0… → …   & %L… → …1 |`
`  Q1 → 2 & R1… → 01… & %L…0 → … |`
`  Q1 → 2 & R1… → 11… & %L…1 → … |`
`  Q2 → 2 & R0… → …   & %L… → …1 |`
`  Q2 → 3 & R1… → 0…`
`)*`

#### Minsky machine

In a [Minsky machine](Minsky_machine "wikilink") there are two kinds of instructions:

-   increment some register and jump to the next instruction
-   decrement some register and jump to the next instruction, unless it is already zero, in which jump to some other instruction.

Using unary for the register contents, the first kind can be written as

`R... -> X... & Q5 -> 6`

The second kind can be written as

`RX... -> ... & Q5 -> 6 | R-> & Q5 -> 3`

where the “next instruction” after instruction 5, is instruction 6 (and “some other instruction” is, in this instance, instruction 3.)

Comparison with Conventional Rewrite Systems
--------------------------------------------

A conventional rewrite system presents a collection of rewrite rules, and picks one to apply at each step, and iterates this process until it can find no rule in its collection that applies.

In Tandem, there is only one rule and its application is only performed once. Disjunction (Ri | Rj) corresponds to picking a rule to apply from a collection of rules. Asteration (R\*) corresponds to the iterative process of applying rules until none apply.

Conjunction (Ri & Rj) is itended to serve as a way to write conditional rewrite rules, but the comparison is approximate. In Tandem's case, instead of purely being a condition, the LHS of the conjunction is some other part of the stack-collection that acts as a “gate” on the rewrite rule -- but this LHS may get rewritten itself too. In a sense, multiple matching redexes can get rewritten at once, so long as they all match -- and this, by the way, is where the name “tandem” comes from.

In most conventional rewrite systems, the redex (the part of the object to be rewritten) is searched for at each rewrite step. For instance, in a term rewriting language, the leftmost-bottommost matching subterm might be rewritten at each step. In Tandem, the stacks are labelled and it is always the top of the stack that is rewritten, so essentially no actual searching takes place.

Algebraic properties
--------------------

Disjunction is associative and commutative and idempotent, like set union.

Disjunction has as its identity element an artificial rewrite rule which we call **0** and which never succeeds in matching. For all R, R | **0** = **0** | R = R. **0** may appear directly in a program, as a symbolic constant for a rule.

Conjunction is associative and idempotent.

Conjunction has as its identity element an artificial rewrite rule which we call **1** and which always succeeds in matching and always makes no change when it is applied. For all R, R & **1** = **1** & R = R. **1** may appear directly in a program, as a symbolic constant for a rule.

When they are combined, we can see that conjunction left-distributes over disjunction: Ri & (Rj | Rk) = (Ri & Rj) | (Ri & Rk). This is justified intuitively by the following: if A matches, then one of B or C matches, a rewrite can occur; also, if one of A matches then B matches, or A matches then C matches, a rewrite can occur; and the possible rewrites that can occur in both scenarios are the same.

From these properties we can conclude that the set of rules with disjunction and conjunction forms a semiring.

The author believes that, using distributivity, any combination of conjunctions and disjunctions can be written in [disjunctive normal form](https://en.wikipedia.org/wiki/Disjunctive_normal_form), that is, a disjunction of conjuctive clauses.

It is also the case that asteration (R\*) is equivalent to

**`1`**` | R | R & R | R & R & R | ...etc...`

This is justified intuitively by saying we either cannot apply R but succeed and make no changes (**1**), or we apply R once, and (if we can) we apply R again, and (if we can) we apply R again, and so forth.

When asteration is added, the semiring becomes a [Kleene algebra](https://en.wikipedia.org/wiki/Kleene_algebra).

It's possible to write any Tandem program as a single asteration around a single, asteration-less expression. The Turing machine example proves this; in the worst case you could rewrite your program as a TM and simulate it like that. This is more due to the “single while loop folk theorem” than to any algebraic property; however the author is hopeful that one could produce an algebraic justification for the property, by showing that every Tandem program can be put in a disjunctive normal form with a single asteration.

### Commutors and Distributors

We can also observe that, even though Ri & Rj is non-commutative (first Ri is applied, then Rj,) if the set of labels involved in some particular Ri is disjoint with the set of labels involved in some particular Rj then for those particular rules, Ri & Rj commutes.

All the example programs given so far, including the Turing machine example, have conjunctive clauses where the rules commute. So it's possible to write any Tandem program with commuting conjunctive clauses, even though & is not in general commutative.

Also, even though & and | are left-distributive, they are not right-distributive in general. However, if, in (Ri | Rj) & Rk, the subrule (Ri | Rj) is guaranteed to terminate, then it may be distributed, to (Ri & Rk) | (Rj & Rk).

Since we are informed that [fast-and-loose reasoning is morally correct](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/fast+loose.pdf), we may choose to assume the rules will always terminate and make these algebraic manipulations without feeling at all guilty about it.

Computational class
-------------------

A brief study of how the Turing machine in the example code section was implemented in Tandem should suffice to convince there is a straightforward method for implementing Turing machines in Tandem, which will work for any Turing machine; therefore Tandem is Turing-complete.

It's interesting to consider subsets of the language though.

Without \* it is not Turing-complete because it can only make a bounded number of rewrites.

Without & it is (the author believes) not Turing-complete, because each rule can only rewrite a single stack -- there is no way to “communicate” between the stacks.

Without the *ls*… → *t*… form it is (the author believes) not Turing-complete, as all the labelled values are no longer stacks, but only registers of bounded size; it is complete only for finite-state automata.

If only one stack can have *ls*… → *t*… form rules applied to it, then it is (the author believes) complete only for the push-down automata.

Acknowledgements
----------------

Many thanks to <User:arseniiv> for pointing out problems with the initial formulation of the algebra, as well as finding errors in the example programs.
