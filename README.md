# oneup

NIF powered, lock-free global counters for erlang using the c++11 atomic package.

Oneup is an alternative to `ets:update_counter` with improved throughput for multiple writers without the lock contention that can happen with ets.

### Status

oneup is not currently used in production and is subject to API changes. 

### Usage

Create a new counter. This returns a reference that is required for further oneup operations. There is no public registry of oneup counters. Oneup counters are garbage collected like any other erlang primitive e.g. binary or tuple.

```erlang
C = oneup:new_counter().
```

Increment the counter. Any number of processes can safely increment a counter. A 64 bit signed long is used to hold the value, not an erlang auto number. The maximum value is (2^31-1) or less depending on architecture.

```erlang
ok = oneup:inc(C).
ok = oneup:inc2(C, 10).
```

Retrieve the result. Any number of processes can safely read a counter.

```erlang
11 = oneup:get().
```

### Performance

The benchnmark suite can be run by executing the following from the oneup directory:

    make perfs

This runs a benchmarks of oneup and `ets:update_counter` tests. Testing has shown that oneup can achieve upto 100 times the throughput of ets when multiple processes write to a single key.
