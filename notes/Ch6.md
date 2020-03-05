# Ch 6 - Amortization and Persistence via Lazy Evaluation

Explore how lazy evaluation can mediate the conflict between amortization and persistence, and adapt both the banker's and physicist's methods to account for lazy evaluation.

## 6.1 Execution Traces and Logical Time

- **Execution Trace**
  - a Directed Graph whose nodes represent operations of interest, usually just update operations on the data type in question
    - An edge from `v` to `v'` indicates that operation `v` fuses some result of operation `v`
      - **Logical History** (`v^`)
        - Of operation `v`, is the set of all operations on which the result of `v` depends (including `v` itself)
    -  **Logical Future**
        - Of a node `v` is any path from `v` to a "terminal node" (i.e., a node with out-degree zero)
          - If there is more than one such path, then node v has multiple logical futures.
     - Cycles arise from recursively defined objects

From Exercies 6.1:

=====================================
| Execution Trace | Logical Futures |
|-----------------|-----------------|
|      empty      |        4        |
|        |        |        |        |
|        a        |        4        |
|        |        |        |        |
|        b        |        4        |
|       / \       |       / \       |
|      c   d      |      2   2      |
|     / \ / \     |     / \ / \     |
|    f   e   g    |    1   1   1    |
=====================================

============================================
|              Logical Futures             |
|               of a / empty               |
|------------------------------------------|
| History  | History | History  |  History |
|  of f    | #1 of e | #2 of e  |   of g   |
|   (f^)   |       (e^)         |   (g^)   |
|------------------------------------------|
|    empty |  empty  |  empty   | empty    |
|      |   |    |    |    |     |   |      |
|      a   |    a    |    a     |   a      |
|      |   |    |    |    |     |   |      |
|      b   |    b    |    b     |   b      |
|     /    |   /     |     \    |    \     |
|    c     |  c      |      d   |     d    |
|   /      |   \     |     /    |      \   |
|  f       |    e    |    e     |       g  |
============================================

- d

## 6.2 Reconciling Amortization and Persistence

### 6.2.1 The Role of Lazy Evaluation

- Evaluation Orders
  - Call-by-Value
    - Strict evaluation
  - Call-by-Name
    - Lazy evaluation w/o memoization
  - Call-by-need
    - Lazy evaluation w/ memoization
- Amortization
  - Impossible in call-by-value or call-by-name evaluation orders since continuous application of the same function always takes exactly the same amount of time (without side-effects).
  - Possible in call-by-need
    - Consider `fix f x`
      - If `x` contains some suspended component that is needed by `f`, then the first application of `f` to `x` forces the (potentially expensive) evaluation of that component and memoizes the result. Subsequent operations may then access the memoized result directly.

### 6.2.2 A Framework for Analyzing Lazy Data Structures

- Lazy evaluation is necessary to implement amortized data structures purely functionally
  - Analyzing lazy programs by pretending that they are actually strict is completely inadequate for lazy amortized data structures
- Classifying Costs
  - Unshared cost
    - of an operation is the actual time it would take to execute the operation under the assumption that every suspension in the system at the beginning of the operation has already been forced and memoized
    - i.e., under the assumption that force always takes 0(1) time, except for those suspensions that are created and forced within the same operation
  - Shared cost
    - of an operation is the time that it would take to execute every suspension created but not evaluated by the operation (under the same assumption as above)
    - 2 kinds of total shared costs
      - Realized costs
        - the shared costs for suspensions that are executed during the overall computation
      - Unrealized costs
        - the shared costs for suspensions that are never executed.
  - Complete cost
    - of an operation is the sum of its shared and unshared costs. Note that the complete cost is what the actual cost of the operation would be if lazy evaluation were replaced with strict evaluation.