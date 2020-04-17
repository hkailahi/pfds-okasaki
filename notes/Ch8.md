# Ch 8 - Lazy Rebuilding

- [Ch 8 - Lazy Rebuilding](#ch-8---lazy-rebuilding)
  - [Achieving Balance](#achieving-balance)
  - [Batched Rebuilding](#batched-rebuilding)
    - [Amortization  - Similarites to Banker's Queue](#amortization---similarites-to-bankers-queue)
    - [Red Black Trees](#red-black-trees)
  - [Global Rebuilding](#global-rebuilding)
    - [Eliminating Amortization - Similarites to Real-Time Queue](#eliminating-amortization---similarites-to-real-time-queue)
  - [Lazy Rebuilding](#lazy-rebuilding)
    - [Hood-Melville Queues](#hood-melville-queues)
  - [Batched vs Global vs Lazy Rebuilding](#batched-vs-global-vs-lazy-rebuilding)

## Achieving Balance

> For most balanced structures, there is a notion of *perfect balance*, which is a configuration that minimizes the cost of subsequent operations. However, since it is usually too expensive to restore perfect balance after every update, most implementations settle for approximations of perfect balance that are at most a constant factor slower (Ex. AVL trees, red-black trees)
- PFDS pg. 99 (8.1 Batched Rebuilding)

## Batched Rebuilding

> Provided no update disturbs the balance too drastically, an attractive alternative is to postpone **rebalancing** until after a sequence of updates, and then to rebalance the entire structure, restoring it to perfect balance. We call this approach **batched rebuilding**.
>
> **Batched rebuilding** yields good amortized time bounds provided that:
>
> (1) the data structure is not rebuilt too often
> (2) individual updates do not excessively degrade the performance of later operations.
>
> More precisely, condition (1) states that, if one hopes to achieve a bound of `O(f(n))` amortized time per operation, and the rebuilding transformation requires `O(g(n))` time, then the rebuilding transformation cannot be executed any more frequently than every `c • g(n)/f(n)` operations, for some constant `c`.

### Amortization  - Similarites to Banker's Queue

Batched rebuilding reminded me of the amortization strategy of the banker's queues. The **banker's queue**, from here on BQ, kept primary copy of queue split between a forward front list and reversed rear list.

BQs keep an invariant `|c * front| > |rear|` for some constant `c`. On each update operation, we move one step closer to the invariants bound until we hit an expensive operation which pays the entire cost of reversing the rear and appending to the front.

Unlike batched rebuilding, only do rebalancing/frontloading (move front to back, rather than reconstructing entire queue from scratch).

We get amortized O(1) queue operations, which includes infrequent expensive O(n) rebalancing based on the invariant.

```
λ> putStrLn $ prettyBuildBQHistory 20
Front: []				Rear: []
Front: [1]				Rear: []
Front: [1]				Rear: [2]
Front: [1,2,3]				Rear: []
Front: [1,2,3]				Rear: [4]
Front: [1,2,3]				Rear: [5,4]
Front: [1,2,3]				Rear: [6,5,4]
Front: [1,2,3,4,5,6,7]				Rear: []
Front: [1,2,3,4,5,6,7]				Rear: [8]
Front: [1,2,3,4,5,6,7]				Rear: [9,8]
Front: [1,2,3,4,5,6,7]				Rear: [10,9,8]
Front: [1,2,3,4,5,6,7]				Rear: [11,10,9,8]
Front: [1,2,3,4,5,6,7]				Rear: [12,11,10,9,8]
Front: [1,2,3,4,5,6,7]				Rear: [13,12,11,10,9,8]
Front: [1,2,3,4,5,6,7]				Rear: [14,13,12,11,10,9,8]
Front: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]				Rear: []
Front: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]				Rear: [16]
Front: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]				Rear: [17,16]
Front: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]				Rear: [18,17,16]
Front: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]				Rear: [19,18,17,16]
Front: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]				Rear: [20,19,18,17,16]
```

### Red Black Trees

In Excercise 8.1, we revisit Red-Black Trees. Before we omitted a `delete` operation, but now we can get it in amortized O(1) time by


## Global Rebuilding

> (**Global rebuilding** is) a technique for eliminating the amortization from batched rebuilding (...) The basic idea is to execute the rebuilding transformation incrementally, performing a few steps per normal operation. This can be usefully viewed as running the rebuilding transformation as a **coroutine**. The tricky part of **global rebuilding** is that the coroutine must be started early enough that it can finish by the time the rebuilt structure is needed.

> Concretely, **global rebuilding** is accomplished by maintaining two copies of each object. The **primary**, or **working**, copy is the ordinary structure. The **secondary copy** is the one that is being gradually rebuilt. All queries and updates operate on the working copy. When the secondary copy is completed, it becomes the new working copy and the old working copy is discarded. A new secondary copy might be started immediately, or the object may carry on for a while without a secondary structure, before eventually starting the next rebuilding phase.

### Eliminating Amortization - Similarites to Real-Time Queue

Global rebuilding reminded me to the strategy of elimination amortization in the **real-time queue**. The **real-time queue**, from here on RTQ, kept primary copy of queue split between a forward front list and reversed rear list. Rather than a secondary copy, the RTQ keeps a schedule of unevaluated work.

On each update operation, some unevaluated debt is paid and removed from the schedule. Each operation performs enough of an incremental rotation such that whenever a new rebalancing/frontloading is required, the previous reversal has been completed.

```
λ> demoRTQ 20
┌──────────────────────────────────────────┬──────────────────────────────────────────┬──────────────────────────────────────────┬──────────────────────────────────────────┐
│                 Command                  │                  Front                   │                   Rear                   │                 Schedule                 │
╞══════════════════════════════════════════╪══════════════════════════════════════════╪══════════════════════════════════════════╪══════════════════════════════════════════╡
│                  empty                   │                    []                    │                    []                    │                    []                    │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                  snoc 1                  │                   [_1]                   │                    []                    │                   [1]                    │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                  snoc 2                  │                   [1]                    │                   [2]                    │                    []                    │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                  snoc 3                  │                 [_1,_2,_3]               │                    []                    │                 [1,2,3]                  │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                  snoc 4                  │                 [1,_2,_3]                │                   [4]                    │                  [2,3]                   │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                  snoc 5                  │                 [1,2,_3]                 │                  [5,4]                   │                   [3]                    │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                  snoc 6                  │                 [1,2,3]                  │                 [6,5,4]                  │                    []                    │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                  snoc 7                  │             [_1,_2,_3,_4,_5,_6,_7]       │                    []                    │             [1,2,3,4,5,6,7]              │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                  snoc 8                  │             [1,_2,_3,_4,_5,_6,_7]        │                   [8]                    │              [2,3,4,5,6,7]               │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                  snoc 9                  │             [1,2,_3,_4,_5,_6,_7]         │                  [9,8]                   │               [3,4,5,6,7]                │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                 snoc 10                  │             [1,2,3,_4,_5,_6,_7]          │                 [10,9,8]                 │                [4,5,6,7]                 │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                 snoc 11                  │             [1,2,3,4,_5,_6,_7]           │               [11,10,9,8]                │                 [5,6,7]                  │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                 snoc 12                  │             [1,2,3,4,5,_6,_7]            │              [12,11,10,9,8]              │                  [6,7]                   │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                 snoc 13                  │             [1,2,3,4,5,6,_7]             │            [13,12,11,10,9,8]             │                   [7]                    │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                 snoc 14                  │             [1,2,3,4,5,6,7]              │           [14,13,12,11,10,9,8]           │                    []                    │
├──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────────┤
│                 snoc 15                  │  [_1,_2,_3,_4,_5,_6,_7,_8,_9,            │                    []                    │  [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]   │
│                                          │   _10,_11,_12,_13,_14,_15]               │                                          │                                          │
└──────────────────────────────────────────┴──────────────────────────────────────────┴──────────────────────────────────────────┴──────────────────────────────────────────┘
```

## Lazy Rebuilding

### Hood-Melville Queues


**Hood-Melville Queues**, from here on **HMQ**, is a **real-time-queue** implementation. Unlike **global rebuilding**, HMQs do not execute the rebuilding transformation (i.e., the rotation) concurrently with the normal operations; rather, it pays for the rebuilding transformation concurrently with the normal operations, but then executes the transformation all at once at some point after it has been paid for.

In essence, we have replaced the complications of explicitly or implicitly coroutining the rebuilding transformation with the simpler mechanism of lazy evaluation. We call this variant of global rebuilding **lazy rebuilding**.

```
λ> demoHMQ 20
┌───────────────────────┬─────────┬───────────────────────┬──────────────────────────────────────────────────────────────┬─────────┬───────────────────────┐
│        Command        │ Size(F) │         Front         │                           Rotation                           │ Size(R) │         Rear          │
╞═══════════════════════╪═════════╪═══════════════════════╪══════════════════════════════════════════════════════════════╪═════════╪═══════════════════════╡
│         empty         │    0    │          []           │                             Idle                             │    0    │          []           │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 1         │    1    │          [1]          │                             Idle                             │    0    │          []           │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 2         │    1    │          [1]          │                             Idle                             │    1    │          [2]          │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 3         │    3    │          [1]          │                    Appending 1 [1] [2,3]                     │    0    │          []           │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 4         │    3    │        [1,2,3]        │                             Idle                             │    1    │          [4]          │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 5         │    3    │        [1,2,3]        │                             Idle                             │    2    │         [5,4]         │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 6         │    3    │        [1,2,3]        │                             Idle                             │    3    │        [6,5,4]        │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 7         │    7    │        [1,2,3]        │              Reversing 2 [3] [2,1] [5,4] [6,7]               │    0    │          []           │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 8         │    7    │        [1,2,3]        │                Appending 3 [3,2,1] [4,5,6,7]                 │    1    │          [8]          │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 9         │    7    │        [1,2,3]        │                Appending 1 [1] [2,3,4,5,6,7]                 │    2    │         [9,8]         │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 10        │    7    │    [1,2,3,4,5,6,7]    │                             Idle                             │    3    │       [10,9,8]        │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 11        │    7    │    [1,2,3,4,5,6,7]    │                             Idle                             │    4    │      [11,10,9,8]      │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 12        │    7    │    [1,2,3,4,5,6,7]    │                             Idle                             │    5    │    [12,11,10,9,8]     │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 13        │    7    │    [1,2,3,4,5,6,7]    │                             Idle                             │    6    │   [13,12,11,10,9,8]   │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 14        │    7    │    [1,2,3,4,5,6,7]    │                             Idle                             │    7    │ [14,13,12,11,10,9,8]  │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 15        │   15    │    [1,2,3,4,5,6,7]    │   Reversing 2 [3,4,5,6,7] [2,1] [13,12,11,10,9,8] [14,15]    │    0    │          []           │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 16        │   15    │    [1,2,3,4,5,6,7]    │   Reversing 4 [5,6,7] [4,3,2,1] [11,10,9,8] [12,13,14,15]    │    1    │         [16]          │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 17        │   15    │    [1,2,3,4,5,6,7]    │   Reversing 6 [7] [6,5,4,3,2,1] [9,8] [10,11,12,13,14,15]    │    2    │        [17,16]        │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 18        │   15    │    [1,2,3,4,5,6,7]    │     Appending 7 [7,6,5,4,3,2,1] [8,9,10,11,12,13,14,15]      │    3    │      [18,17,16]       │
├───────────────────────┼─────────┼───────────────────────┼──────────────────────────────────────────────────────────────┼─────────┼───────────────────────┤
│        snoc 19        │   15    │    [1,2,3,4,5,6,7]    │     Appending 5 [5,4,3,2,1] [6,7,8,9,10,11,12,13,14,15]      │    4    │     [19,18,17,16]     │
└───────────────────────┴─────────┴───────────────────────┴──────────────────────────────────────────────────────────────┴─────────┴───────────────────────┘
```

## Batched vs Global vs Lazy Rebuilding

> **Global rebuilding** has two advantages over **batched rebuilding**: it is suitable for implementing persistent data structures and it yields **worst-case** bounds rather than **amortized** bounds.
>
> **Lazy rebuilding** shares the first advantage, but, at least in its simplest form, yields **amortized** bounds. However, if desired, **worst-case** bounds can often be recovered using the **scheduling** techniques.
>
> For example, the **real-time queues** in Section 7.2 combine **lazy rebuilding** with scheduling to achieve **worst-case** bounds. In fact, the combination of **lazy rebuilding** and **scheduling** can be viewed as an instance of **global rebuilding** in which the coroutines are reified in a particularly simple way using lazy evaluation.