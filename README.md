commons-core
==============

# What is commons-core 

It's just another light weighted Java common utilities library.

# What it solves

The current problems it solves are:

- adt: some abstract data structures, such as bitmap (binary rank & select)
- codec: common encoding and decoding interfaces
- predicate: some commonly used implementations of the java Predicate interface
- config: configuration management
- event: similar to guava event bus, better designed and DI framework friendly
- numbertext: converting numbers into text representation, available implementations support English and Chinese
- permutation: generic permutation and combination tools
- precondition: precondition check, that has a better designed exception hierarchy, instead of using NPE, etc.
- stopwatch: a stop watch simulation, to measure time elapse
- taskqueue: a managed, single threaded event queue interface, to be implemented
- util: some other interface and contract designs for some commonly used tools in design patterns

# How to use

Available in maven central:
```
<dependency>
    <groupId>org.raistlic.lib</groupId>
    <artifactId>commons-core</artifactId>
    <version>1.2</version>
</dependency>
```

# Sample Code - Precondition Check

```
/**
 * Search some records, say, the method can be invoked only when the state of the object meets some 
 * kind of requirement, and can only be called in non-daemon thread.
 *
 * @param someCriteria some kind of criteria, cannot be {@code null}, must match some pattern.
 * @param offset the offset index of the search result portion to return, cannot be less than {@code 0}.
 * @param limit the maximum size of the search result portion to return, cannot be less than {code 0}.
 * @throws InvalidParameterException when any of the parameters are invalid.
 * @throws InvalidStateException when the object's state does not match the requirement for calling 
 *         this method
 * @throws InvalidContextException when the calling thread does not match the requirement
 */
public List<Record> searchRecords(String someCriteria, int offset, int limit) {

  Precondition.param(someCriteria, "someCriteria").notNull();
  Precondition.param(someCriteria, "someCriteria").matchesPattern(this.someRegexPattern);
  Precondition.param(offset, "offset").noLessThan(0);
  Precondition.param(limit, "limit").noLessThan(0);
  Precondition.state(this.state, "stateName").matches(st -> {
    // Predicate code goes here
    return checkResult;
  });
  Precondition.threadContext().isNotDaemon();
  
  // search code goes here ...
  
  return result;
}

```