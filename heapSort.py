
from random import randrange
import math


# Heapsort 

# The method of sorting by straight selection is based on the repeated selection of the least
# key among n items. Straight selection can be improved by retaining from each scan more information
# than just the identification of the single least item. With n/2 comparisons it is possible to
# determine the smaller key of each pair of items, with another n/4 comparison the smaller of each
# pair can be selected, and so on; the second step now consists of descending down along the path
# marked by the least key and eliminating it. Each of the n selection steps requires only log n
# comparisons. Therefore, the selection process requires only on the order of n log n elementary
# operations in addition to the n steps required by the construction of the tree. This is a very
# significant improvement over the straight methods requiring n^2 steps.

# The heap is defined as a binary tree that can be constructed in situ as an array.
# Heapsort is an in-place algorithm, but it is not a stable sort.

# Although somewhat slower than quicksort, it has the advantage of a more favorable worst-case runtime.
# Heapsort was invented by J. W. J. Williams in 1964.
# In the same year, R. W. Floyd published an improved version that could sort the array in-place. 

# O(n log n) 




# --------------------------------------------------------------------------- fisher_yates_shuffle
#
# The Fisher-Yates shuffle, in its original form, was described in 1938 by Ronald Fisher and Frank Yates
# in their book Statistical tables for biological, agricultural and medical research.
# The modern version of the Fisher-Yates shuffle, designed for computer use, was introduced by Richard Durstenfeld
# in 1964 and popularized by Donald E. Knuth in The Art of Computer Programming.
# O(n)

def fisher_yates_shuffle(l):

    lst = l.copy()

    for i in range((len(lst) - 1), 1, -1):

        rand = randrange(i)
        a = lst[i]
        b = lst[rand]

        lst[rand] = a
        lst[i] = b

    return lst


# --------------------------------------------------------------------------- sift
#  sift, will "heapify" when called repeatedly

def sift(l, L, R):

    lst = l.copy()
    i = L
    j = (2 * i) + 1
    x = lst[i]

    while (j <= R):

        if (j < R):
            if (lst[j].key < lst[j + 1].key):     # pick the larger of the pair
                j = j + 1

        if (lst[i].key >= lst[j].key):
            return lst

        lst[i] = lst[j]  # swap
        lst[j] = x      

        i = j
        j = (2 * i) + 1

    return lst


# --------------------------------------------------------------------------- heapsort
#  O(n log n)
#  Space complexity: O(1) 
#  This is not a stable sort.

#  This is a faithful translation from Wirth's original Pascal. But..
#  I think some better variable names and some helper functions could improve its readability.

def heapsort(l):

    lst = l.copy()
    n = len(lst)
     
    L = math.floor(n/2)
    R = n - 1

    while (L > 0):
        L = L - 1
        lst = sift(lst, L, R)
        
    while (R > 0):
        x = lst[0]
        lst[0] = lst[R]
        lst[R] = x
        R = R - 1
        lst = sift(lst, 0, R)
        
    return lst



# simple record object
class Rec:
  def __init__(self, key, value):
      self.key = key
      self.value = value

  def __str__(self):
        return '{self.key}:{self.value}'.format(self=self)    

    
lst = [ Rec(1,"A"),
        Rec(2, "B"),
        Rec(3, "C"),
        Rec(4, "D"),
        Rec(5, "E"),
        Rec(6, "F"),
        Rec(7, "G"),
        Rec(8, "H"),
        Rec(9, "I"),
        Rec(10, "J") ]    


shuf_lst =  fisher_yates_shuffle(lst)
for x in shuf_lst: print(x)


sort_lst = heapsort(shuf_lst)
for x in sort_lst: print(x)


