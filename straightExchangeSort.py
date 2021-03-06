
from random import randrange

# Straight Exchange Sort
# a.k.a. Bubblesort 

# If we view the array to be in a vertical instead of a horizontal position,
# and the items as bubbles in a water tank with "weights" according to their keys,
# then each pass over the array results in the ascention of a bubble to its
# appropriate level of weight.

# Exchange sort is inferior to both straight insertion and straight selection;
# in fact, the bubblesort has hardly anything to recommend it except its catchy name!

# Unlike modern versions, Wirth loops from right to left; I assume to fit the analogy
# of bubbles rising.

# O(n^2) 
# worst case: O(n^2) swaps 
# best case: O(1) swaps 


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


# --------------------------------------------------------------------------- exchange_sort
#  O(n2)
# aka bubblesort
# Exchange sort is inferior to both straight insertion and straight selection;
# in fact, the bubblesort has hardly anything to recommend it except its catchy name!

def exchange_sort(l):

    lst = l.copy()
    n = len(lst)

    for i in range(1, n):
        for j in range((n - 1), 0, -1):
            if(lst[j-1].key > lst[j].key):
                x = lst[j-1]

                lst[j-1] = lst[j]
                lst[j] = x
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

sort_lst = exchange_sort(shuf_lst)
for x in sort_lst: print(x)
