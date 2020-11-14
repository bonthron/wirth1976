
from random import randrange


# Insertion Sort

# This method is widely used by card players. It is a stable sort, it leaves the order of items
# with equal keys unchanged.

# The number of key comparisons in the i-th sift is at most i - 1, at least 1,
# and -assuming that all permutations of the n keys are equally probable- 1/2 in average. 
# C_min = n - 1 
# C_max = 1/2(n^2 + n) -1 

# The least numbers occur if the items are originally in order; the worst case occurs if the
# items are originally in reverse order.

# O(n^2) 

# Divergence from Wirth:
# Because there are two termination conditions, Wirth uses the "sentinel technique" of setting
# the current item in the iteration as a terminating value at index zero. This increases the index
# range by one. This seems unnecessarily complicated than simply checking both termination conditions
# and introduces a duplicate value in the data, which, itself could lead to subsequent errors.




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


# --------------------------------------------------------------------------- insert_recursive
def insert_recursive(lst, x):

    if len(lst) == 0:
        return [x]
    else:
       car = lst[0]
       cdr = lst[1:]       
       if(x.key <= car.key):
           return [x] + lst
       else:
           return [car] + insert_recursive(cdr, x)


# --------------------------------------------------------------------------- insertion_sort_recursive
def insertion_sort_recursive(lst):

    if len(lst) == 0:
        return []
    else:
        car = lst[0]
        cdr = lst[1:]
        return insert_recursive(insertion_sort_recursive(cdr), car)



# --------------------------------------------------------------------------- insertion_sort
#  stable sort
#  O(n2)

def insertion_sort(l):

    lst = l.copy()
    for i in range(len(lst)):

        x = lst[i]
        j = (i - 1)

        while((j >= 0) and (lst[j].key > x.key)):
            lst[j + 1] = lst[j]
            j -= 1

        lst[j + 1] = x
        
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

#sort_lst = insertion_sort_recursive(shuf_lst)
#for x in sort_lst: print(x)

sort_lst = insertion_sort(shuf_lst)
for x in sort_lst: print(x)
