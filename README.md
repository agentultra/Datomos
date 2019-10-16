# Datomos

I don't know what this is yet. I simply have a few hunches that seem
related and might form to become a good idea. Consider this a
manifesto.

1. Datalog, Deadalus and BloomL all seem like great ways to develop
   distributed, eventually consistent databases and programs
1. Datomic has shown that Datalog-like query languages are useful
1. Append-only, immutable, event-driven architectures are also useful
   both in theory and practice
1. Although notions of schema also change over time
1. I also wrote a library for handling changes to schema over time using types
1. Maybe it would be nice to have a database that captures the notion
   of a stream of events as they relate to a particular version of a
   schema and projection handlers which can also refer to those versions
1. Which would allow us to modify the schema over time and have a
   type-safe way of upgrading the data with it

## Facts ##

A fact, an atom, a datum, a tuple: the smallest unit of
information. This is what is stored and queried in databases both
relational and logical.

In Datomos a fact is a term with an empty body. I think Datomos will
likely extend this notion of a fact with more type information in
order to account for concepts such as *time* and *schema*.

## Rules ##

Both extensional and intensional rules extend facts with a body of
predicates. This could be extended by Datomos to include notions about
the schema version.

## Schema ##

I think the notion of *attributes* from Datomic are a neat idea. I'd
like to extend this concept with a notion of _version_ for the schema
that rules and facts may refer to.

The thinking here is that business rules change and what we understand
today to be the record of a patient in a triage system may change when
we learn more about the process after some real-world use of our
system.

Our database should allow us to see the data, as it existed, when we
used the prior notion. However it should also be possible to
seamlessly migrate past facts to our new schema and to query
our data in either view of the world.

I started a library in Haskell called `DataVersion` which I think
could really push these ideas further.

## Event Sourcing ##

In it's simplest form an *event sourced* application builds it's
current state from a fold over the past events that *change* the
state.

Many applications use this pattern: distributed version control,
relational databases, and a couple of applications I've helped
develop.

One thing all of these systems have in common is that they all
implement this architecture in different ways.

I'd like to experiment with adding one more to the pile. A really nice
log of events uses the same *ubiquitous language* that users of the
system use to describe their problem domain. A database that stores
facts using a language familiar to programmers is *not* ubiquitous: we
shouldn't see words such as *create*, *remove*, *update*, or *delete*.

An example to illustrate what would make a *good* event/fact: an
triage system for a hospital emergency room. The nurse running the
station doesn't *create a new patient*. I've never interviewed a
triage nurse but I feel safe assuming they probably use different
terminology and sequences of events to describe the process of
admitting a person to the hospital. It's that precise sequence of
events that should be stored!

And as our notion of what information is relevant to those events, the
schema of the event, should also be stored. Views of our data
(projections) should be able to declare which version of the schema
we're talking about. And our database should be able to, with some
help of the programmer, declare how to migrate schema. And all of it
should be verified by a type checker to make sure we haven't missed
anything and that we don't accidentally cross streams (a common
problem I see in event sourced systems).
