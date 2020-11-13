# wirth
## Niklaus Wirth's Algorithms + Data Structures 1976

Revisiting Niklaus Wirth's classic book. \
45 years later- it remains highly relevant. \
The code examples are Pascal. Amazingly, the code compiles (unchanged) using the 2020 version of Free Pascal. https://www.freepascal.org/

In addition to the original Pascal, I'm re-working the algorithms in **JavaScript, Python, and Scheme**. \
Hacking Pascal is old-school. 

![cover](wirth1.jpg?raw=true)

*Divergence from Wirth*: Whereas Wirth indexes arrays from 1, in all cases, I've used zero-based indexing. Pascal differentiates between functions and procedures; Wirth prefers procedures, I've preferred functions. 
	
Although the Pascal is very close to the book, for JavaScript and Python I've made small changes to make the code more idiomatic to those languages. Because Scheme avoids assignment and prefers recursion, this code is the most radically different and may different performance and complexity characteristics. 

- Insertion Sort
- Straight Selection Sort
- Straight Exchange Sort
- Heapsort

### Insertion Sort
This method is widely used by card players. It is a stable sort, it leaves the order of items with equal keys unchanged.

The number of key comparisons in the i-th sift is at most i - 1, at least 1, and -assuming that all permutations of the n keys are equally probable- 1/2 in average. \
C_min = n - 1 \
C_max = 1/2(n^2 + n) -1 

The least numbers occur if the items are originally in order; the worst case occurs if the items are originally in reverse order.

O(n^2) 

*Divergence from Wirth*:
Because there are two termination conditions, Wirth uses the "sentinel technique" of setting the current item in the iteration as a terminating value at index zero. This increases the index range by one. This seems unnecessarily complicated than simply checking both termination conditions and introduces a duplicate value in the data, which, itself could lead to subsequent errors.

The recursive versions increase memory consumption from O(1) to O(N) 

Inside the box! The **Modern Fisher Yates Shuffle** included (to give me something to sort), also one of my favorite algorithms. 
The Fisher-Yates shuffle, in its original form, was described in 1938 by Ronald Fisher and Frank Yates in their book Statistical tables for biological, agricultural and medical research. The modern version of the Fisher-Yates shuffle, designed for computer use, was introduced by Richard Durstenfeld in 1964 and popularized by Donald E. Knuth in The Art of Computer Programming.  O(n)

![cartoon](wirth2.jpg?raw=true)

### Straight Selection Sort
Straight Selection is in some sense the opposite of straight insertion: Straight insertion considers in each step only the one next item of the source sequence and all items of the destination array to find the insertion point; straight selection considers all items of the source array to find the one withe least key and to deposit it as the one next item of the destination sequence. 

This is not a stable sort. \
O(n^2) \
worst case: O(n^2) swaps \
best case: O(1) swaps 

We may conclude in general the algorithm of straight selection is to be preferred over straight insertion.

![cartoon](wirth3.jpg?raw=true)


### Straight Exchange Sort
a.k.a. Bubblesort \
If we view the array to be in a vertical instead of a horizontal position, and the items as bubbles in a water tank with "weights" according to their keys, then each pass over the array results in the ascention of a bubble to its appropriate level of weight.

**Exchange sort is inferior to both straight insertion and straight selection; in fact, the bubblesort has hardly anything to recommend it except its catchy name!**

Unlike modern versions, Wirth loops from right to left; I assume to fit the analogy of bubbles rising.

O(n^2) \
worst case: O(n^2) swaps \
best case: O(1) swaps \


### Heap Sort
The method of sorting by straight selection is based on the repeated selection of the least key among *n* items. Straight selection can be improved by retaining from each scan **more information** than just the identification of the single least item. With *n*/2 comparisons it is possible to determine the smaller key of each pair of items, with another *n*/4 comparison the smaller of each pair can be selected, and so on; the second step now consists of descending down along the path marked by the least key and eliminating it. Each of the *n* selection steps requires only log n comparisons. Therefore, the selection process requires only on the order of n log n elementary operations in addition to the n steps required by the construction of the tree. **This is a very significant improvement over the straight methods requiring n^2 steps.**

The heap is defined as a binary tree that can be constructed *in situ* as an array. Heapsort is an in-place algorithm, but it is not a stable sort.

Although somewhat slower than quicksort, it has the advantage of a more favorable worst-case runtime. Heapsort was invented by J. W. J. Williams in 1964. In the same year, R. W. Floyd published an improved version that could sort the array in-place. 

O(n log n) 
