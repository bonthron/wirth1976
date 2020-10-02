# wirth
## Niklaus Wirth's Algorithms + Data Structures 1976

Revisiting Niklaus Wirth's classic book. Still relevent 45 years later.
Pascal was used throughout the book. Amazingly, the code still compiles (unchanged) using the 2020 version of Free Pascal. https://www.freepascal.org/

I'm re-working the algorithms in JavaScript, Python, Scheme, and the original Pascal.

### Insertion Sort
This method is widely used by card players. It is a stable sort, it leaves the order of items with equal keys unchanged.

The number of key comparisons in the i-th sift is at most i - 1, at least 1, and -assumuing that all permutations of the n keys are equally probable- 1/2 in average.
C_min = n - 1
C_max = 1/2(n^2 + n) -1

The least numbers occur if the items are originally in order; the worst case occurs if the items are originally in reverse order.

O(n^2)

*Divergence from Wirth*:
Because there are two termination conditions, Wirth uses the "sentinel technique" of setting the current item in the iteration as a terminating value at index zero. This increases the index range by one. This seems unneccessarily complicated than simply checking both termination conditions, and introduces a duplicate value in your data, which, itself could lead to subsequent errors.

The recursive versions increase memory consumption from O(1) to O(N) 



