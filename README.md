# Datomos

A generic library for implement Datalog databases.

Datalog is a declarative logic programming language. It is similar to
Prolog but smaller in scope and thus easier to implement and embed in
a program. A Datalog program declares facts, rules, and
queries. Evaluating a program answers queries with new tables of
information.

If your program needs to be able to answer queries on graphs or simple
relationships then you might want to check out this library. If you
need to query a list of nodes in a certain state or gather a list of
dependencies then you want Datalog.

## Datalog in a Nutshell

    parent(mary, bob).
    parent(bob, john).

    ancestor(X, Y) :- parent(X, Y).
    ancestor(X, Y) :- parent(X, Z), parent(Z, Y).

    ?- ancestor(mary, X)

The first two lines are a kind of _atom_ known as _facts_. Things that
are true. The next two are _rules_: the first part is a _predicate
atom_ and everything after the `:-` is the _body_. The body is made up
of the logical _conjunction_ of one or more atoms.

The final line is a _query_. When this program is evaluated it will
return a table of all values for `X`, where `X` is an ancestor of
`mary`. We should get `[bob, john]`.
