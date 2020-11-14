
from random import randrange


# Straight Selection Sort

# Straight Selection is in some sense the opposite of straight insertion: Straight insertion
# considers in each step only the one next item of the source sequence and all items of the
# destination array to find the insertion point; straight selection considers all items of
# the source array to find the one withe least key and to deposit it as the one next item of
# the destination sequence. 

# This is not a stable sort. 
# O(n^2) 
# worst case: O(n^2) swaps 
# best case: O(1) swaps 

# We may conclude in general the algorithm of straight selection is to be preferred over straight insertion.



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


# --------------------------------------------------------------------------- swap_list_items
def swap_list_items(lst, i, j):

    l = lst.copy()
    a = l[i]
    b = l[j]
    l[i] = b    
    l[j] = a
    return l

    
# --------------------------------------------------------------------------- smallest_index
def smallest_index(lst, start_at):

    def inner(x, l, i, j):
        if not l:
            return i
        else:
            cdr = l[1:]
            if ((j > i) and (l[0].key < x.key)):
                return inner(l[0], cdr, j, (j + 1))
            else:
                return inner(x, cdr, i, (j + 1))
                
    return inner(lst[start_at], lst, start_at, 0)

    
# --------------------------------------------------------------------------- selection_sort_recursive
def selection_sort_recursive(lst):

    def inner(l, i):
        if i == (len(l) - 1):
            return l
        else:
            s = smallest_index(l, i)
            return inner(swap_list_items(l, i, s), (i + 1))
        
    return inner(lst.copy(), 0)
    


# --------------------------------------------------------------------------- selection_sort
#  not a stable sort
#  O(n2)

def selection_sort(l):

    lst = l.copy()
    n = len(lst)

    for i in range(n):
        x = lst[i]
        k = i
        for j in range((i+1), n):

            if(lst[j].key < x.key):
                x = lst[j]
                k = j
                
        lst[k] = lst[i]
        lst[i] = x
        
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

#sort_lst = selection_sort_recursive(shuf_lst)
#for x in sort_lst: print(x)

sort_lst = selection_sort(shuf_lst)
for x in sort_lst: print(x)
