# Chapter 2 - Persistence

- Functional DS are persistent
  - Updating a functional data structure does not destroy the existing version
    - Creates a new version that coexists with the old one
  - Persistence is achieved by "copying" the affected nodes of a data structure and making all changes in the "copy" rather than in the original.
    - Because nodes are never modified directly (immutatable), all nodes that are unaffected by an update can be "shared" between the old and new versions of the data structure without worrying that a change in one version will inadvertently be visible to the other.
- Functional style greatly simplified by automatic garbage collection
  - Crucial to reclaim the space of copies that are no longer needed, but the pervasive sharing of nodes makes manual garbage collection awkward.

## 2.1 Lists

- `xs ++ ys == zs`
  - Catenates (i.e., appends) two lists
  - Imperative impl
    - 0(1) time, O(1) space
    - Maintains pointers to both the first and last cell in each list, then modifies the last cell of the first list to point to the first cell of the second list
    - Destroys inputs (xs and ys)
  - Functional impl
    - O(xs) time, O(xs) space
      - Can be O(1) w/o sacrificing persistence
    - Copies first list, but shares second
      - zs shares/points to ys
    - Access to xs, ys, and zs
- Update
  - Copy nodes from root to node in question, insert, then share remaining nodes

## 2.2 Binary Search Trees

- Two orders of Evaluation
  - Strict
    - Arguments to a function are evaluated before the body of the function
    - Asymptotic complexity analysis
      - Easier to reasoning about 
        - Exactly which subexpressions will be evaluated, and when, is for the most part syntactically apparent
      - Can describe worst-case DSs, but not amortized ones
  - Lazy
    - Arguments are evaluated in a demand-driven fashion; they are initially passed in unevaluated form and are evaluated only when (and if!) the computation needs the results to continue
      - Once a given argument is evaluated, the value of memoized
    - Asymptotic complexity analysis
      - More difficult to reason about
      - Can describe amortized DSs, but not worst-case ones 

## Observations/Thoughts/Etc

https://www.briancallander.com/posts/pfds/okasakipfdsc02