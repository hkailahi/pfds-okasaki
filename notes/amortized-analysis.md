# Amortized Analysis

Aside from book (obviously), the following resources are helpful:
[1] (Cornell CS3110 - Lecture 21)[https://www.cs.cornell.edu/courses/cs3110/2012sp/lectures/lec21-amortized/lec21.html]
[2] (Cornell CS3110 - Section Notes 21)[https://www.cs.cornell.edu/courses/cs3110/2012sp/recitations/rec21.html]
[3] (MIT OCW - Amoritized Analysis - Erik Demaine)[https://youtu.be/3MpzavN3Mco]

The following is pulled directly from [1], but includes some futher context.

## Aggregate Method

Consider inserting to Java vectors, which double in size everytime they run out of empty cells.

- O(n) worst case (when doubling)
- Amortized O(1) cost
- Number of doublings exponentially decreases
- Simple analysis of `n` inserts is O(n^2), amortized shows it's better w/ O(n)

```
           +--+
Insert 11  |11|
  (double) +--+
           +--+--+
Insert 12  |11|12|
  (double) +--+--+
           +--+--+--+--+
Insert 13  |11|12|13|  |
           +--+--+--+--+
           +--+--+--+--+
Insert 14  |11|12|13|14|
  (double) +--+--+--+--+
           +--+--+--+--+--+--+--+--+
Insert 15  |11|12|13|14|15|  |  |  |
           +--+--+--+--+--+--+--+--+
           +--+--+--+--+--+--+--+--+
Insert 16  |11|12|13|14|15|16|  |  |
           +--+--+--+--+--+--+--+--+
```

Analysis might look like
```
ci i
  | (i−1) `is power of` 2 == 0 = i
  | otherwise                  = 1
 OR
ci i = 1 + (di i)
 where
   di
    | (i−1) `is power of` 2 == 0 = i - 1
    | otherwise        = 0

i = ith operation, ci = cost; di = doubling cost; si = size of vector

i   | 1  2  3  4  5  6  7  8  9 10
----|-----------------------------
i-1 | 0  1  2  3  4  5  6  7  8  9
----|------------------------------
di  | 0  1  2  0  4  0  0  0  8  0
si  | 1  2  4  4  8  8  8  8 16 16
ci  | 1  2  3  1  5  1  1  1  9  1
```

So `Σ_1≤i≤n ci ≤ n + Σ_0≤j≤m 2^j−1` where `m = log(n-1)`

## Banker's Method

The aggregate method directly seeks a bound on the overall running time of a sequence of operations. In contrast, the accounting method seeks to find a payment of a number of extra time units charged to each individual operation such that the sum of the payments is an upper bound on the total actual cost

Intuitively, one can think of maintaining a bank account. Low-cost operations are charged a little bit more than their true cost, and the surplus is deposited into the bank account for later use. High-cost operations can then be charged less than their true cost, and the deficit is paid for by the savings in the bank account. In that way we spread the cost of high-cost operations over the entire sequence. The charges to each operation must be set large enough that the balance in the bank account always remains positive, but small enough that no one operation is charged significantly more than its actual cost.

```
Σ_1≤i≤n ci ≤ Σ_1≤i≤n c'i, where:
- ci is true cost
- c'i is charge of i-th operation
- xi is (b_(i-1) + c'i)
- bi is balance after i-th insert (xi - ci)

For charge of 3 units:

i   | 1  2  3  4  5  6  7  8  9 10
----|-----------------------------
i-1 | 0  1  2  3  4  5  6  7  8  9
----|-----------------------------
si  | 1  2  4  4  8  8  8  8 16 16
c'i | 3  3  3  3  3  3  3  3  3  3
----|-----------------------------
ci  | 1  2  3  1  5  1  1  1  9  1
xi  | 3  5  6  6  8  6  8 10 12  6
bi  | 2  3  3  5  3  5  7  9  3  4
```

## Physicist's Method

Suppose we can define a potential function Φ on states of a data structure with the following properties:

- Φ(h0) = 0, where h0 is the initial state of the data structure.
- Φ(ht) ≥ 0 for all states h_t of the data structure occurring during the course of the computation.

Intuitively, the potential function will keep track of the precharged time at any point in the computation. It measures how much saved-up time is available to pay for expensive operations. It is analogous to the bank balance in the banker's method. But interestingly, it depends only on the current state of the data structure, irrespective of the history of the computation that got it into that state.

We then define the amortized time of an operation as `c + Φ(h') − Φ(h)` where:
- c is the actual cost of the operation
- h and h' are the states of the data structure before and after the operation, respectively

Thus the amortized time is the actual time plus the change in potential.

Ideally, Φ should be defined so that the amortized time of each operation is small. Thus the change in potential should be positive for low-cost operations and negative for high-cost operations.

Ex.
Consider a sequence of n operations taking actual times c0, c1, c2, ..., cn−1 and producing data structures h1, h2, ..., hn starting from h0. The total amortized time is the sum of the individual amortized times:
= (c0 + Φ(h1) − Φ(h0)) + (c1 + Φ(h2) − Φ(h1)) + ... + (cn−1 + Φ(hn) − Φ(hn−1))
= c0 + c1 + ... + cn−1 + Φ(hn) − Φ(h0)
= c0 + c1 + ... + cn−1 + Φ(hn).
Therefore the amortized time for a sequence of operations overestimates of the actual time by Φ(hn), which by assumption is always positive. Thus the total amortized time is always an upper bound on the actual time.

```
For dynamically resizable arrays with resizing by doubling, we can use the potential function:
  `Φ(h) = 2n − m` where:
  - n is the current number of elements (2*i)
  - m is the current length of the array

t   | 0  1  2  3  4  5  6  7  8  9 10
----|--------------------------------
2n  | 0  2  4  6  8 10 12 14 16 18 20
m   | 0  1  2  4  4  8  8  8  8 16 16
Φht | 0  1  2  2  4  2  4  6  8  2  4
ci  | 0  1  2  3  1  5  1  1  1  9  1
```