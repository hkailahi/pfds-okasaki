# Ch 5 - Amortization and Persistence via Lazy Evaluation

- [Ch 5 - Amortization and Persistence via Lazy Evaluation](#ch-5---amortization-and-persistence-via-lazy-evaluation)
  - [Banker's Queue](#bankers-queue)

## Banker's Queue

The **banker's queue**, from here on BQ, kept primary copy of queue split between a forward front list and reversed rear list.

BQs keep an invariant `|c * front| > |rear|` for some constant `c`. On each update operation, we move one step closer to the invariants bound until we hit an expensive operation which pays the entire cost of reversing the rear and appending to the front.

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