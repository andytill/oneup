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

Increment or set the counter. Any number of processes can safely increment or set a counter. A 64 bit signed long is used to hold the value, not an erlang auto number. The maximum value is (2^63-1) or less depending on architecture.

```erlang
ok = oneup:inc(C). %% value of C becomes 1
ok = oneup:inc2(C, 10).  %% value of C becomes 11
11 = oneup:set(C, 200). %% set to 200 and return previous value
```

Set min or max. 

```
300 = oneup:set_max(C, 300). %% set to max of current and new value and return max
300 = oneup:set_max(C, 100).
100 = oneup:set_min(C, 100). %% set to min of current and new value and return min
100 = oneup:set_min(C, 300).
```

Retrieve the result. Any number of processes can safely read a counter.

```erlang
11 = oneup:get(C).
```

### Performance

The benchnmark suite can be run by executing the following from the oneup directory:

    make perfs

This runs a benchmarks of oneup and `ets:update_counter` tests. Testing has shown that oneup can achieve upto 100 times the throughput of ets when multiple processes write to a single key.
