commons-core
==============

# What is commons-core 

It's just another light weighted Java common utilities library.

# What it solves

The current problems it solves are:

- adt: some abstract data structures, such as bitmap (binary rank & select)
- codec: common encoding and decoding interfaces
- condition: now that Java introduced the Predicate interface, this is to be refactored or deprecated
- configuration: configuration management
- event: similar to guava event bus, better designed and DI framework friendly
- numbertext: converting numbers into text representation
- permutation: generic permutation and combination tools
- precondition: precondition check, that has a better designed exception hierarchy, instead of using NPE, etc.
- stopwatch: a stop watch simulation, to measure time elapse
- taskqueue: a managed, single threaded event queue interface, to be implemented
- other: some other interface and contract designs for some commonly used tools in design patterns

# How to use

Available in maven central:
```
<dependency>
    <groupId>org.raistlic.lib</groupId>
    <artifactId>commons-core</artifactId>
    <version>1.2</version>
</dependency>
```
