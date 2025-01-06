# Haskell Lazy Evaluation Bug

This repository demonstrates a subtle bug in Haskell code related to lazy evaluation and the interaction of `filter` and infinite lists.  The code attempts to find the first even number greater than 10 in an infinite list.  However, due to Haskell's lazy evaluation, the `filter` function continues processing the infinite list even after finding the desired element, leading to potential performance issues or non-termination.

The `bug.hs` file contains the buggy code.  The `bugSolution.hs` file provides a corrected version that addresses the lazy evaluation issue using `takeWhile` or similar techniques.

This example highlights the importance of understanding lazy evaluation in Haskell and employing appropriate techniques to handle infinite lists efficiently.
