[![Build Status](https://travis-ci.com/raistlic/raistlic-lib-commons-core.svg?branch=master)](https://travis-ci.com/raistlic/raistlic-lib-commons-core)

commons-core
==============

# What is commons-core 

It's just another light weighted Java common utilities library.

# What it solves

The current problems it solves are:

- (IN PROGRESS) adt: some abstract data structures, such as bitmap (binary rank & select)
- (PROD READY) codec: common encoding and decoding interfaces
- (PROD READY) predicate: some commonly used implementations of the java Predicate interface
- (PROD READY) config: configuration management
- (IN PROGRESS) event: similar to guava event bus, DI framework friendly
- (PROD READY) numbertext: converting numbers into text representation, available implementations support English and Chinese
- (PROD READY) permutation: generic permutation and combination tools
- (PROD READY) precondition: precondition check, that has a better designed exception hierarchy, instead of using NPE, etc.
- (PROD READY) stopwatch: a stop watch simulation, to measure time elapse
- (IN PROGRESS) taskqueue: a managed, single threaded event queue interface, to be implemented
- (IN PROGRESS) util: some other interface and contract designs for some commonly used tools in design patterns

*IN PROGRESS*: interface not fully implemented yet, or it might change/be deprecated in the future.

*PROD READY*: feature fully implemented with reasonable unit test coverage, breaking changes to existing interfaces are not expected.

# How to use

Available in maven central:
```
<dependency>
    <groupId>org.raistlic.lib</groupId>
    <artifactId>commons-core</artifactId>
    <version>1.4</version>
</dependency>
```

Or if you use gradle:
```
repositories {
  mavenCentral()
}

dependencies {
  compile 'org.raistlic.lib:commons-core:1.4'
}
```

# Javadoc (1.4)

[http://raistlic.github.io/raistlic-lib-commons-core/](http://raistlic.github.io/raistlic-lib-commons-core/)

# Sample Code - Precondition Check

``` java
/**
 * Search some records, say, the method can be invoked only when the state of the object meets some 
 * kind of requirement, and can only be called in non-daemon thread.
 *
 * @param someCriteria some kind of criteria, cannot be {@code null}, must match some pattern.
 * @param offset the offset index of the search result portion to return, cannot be less than {@code 0}.
 * @param limit the maximum size of the search result portion to return, cannot be less than {@code 0}.
 * @throws InvalidParameterException when any of the parameters are invalid.
 * @throws InvalidStateException when the object's state does not match the requirement for calling 
 *         this method
 * @throws InvalidContextException when the calling thread does not match the requirement
 */
public List<Record> searchRecords(String someCriteria, int offset, int limit) {

  Precondition.param(someCriteria, "someCriteria").notNull();
  Precondition.param(someCriteria, "someCriteria").matchesPattern(this.someRegexPattern);
  Precondition.param(offset, "offset").greaterThanOrEqualTo(0);
  Precondition.param(limit, "limit").greaterThanOrEqualTo(0);
  Precondition.state(this.state, "stateName").matches(st -> {
    // Predicate code goes here
    return checkResult;
  });
  Precondition.threadContext().isNotDaemon();
  
  // search code goes here ...
  
  return result;
}

```