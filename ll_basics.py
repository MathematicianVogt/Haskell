# ll_basics.py

"""Some basic list operations to make lists in Python look more like they
   do in functional languages
"""
from collections import namedtuple

LL_Node = namedtuple( "LL_Node", ( "value", "next" ) )
"Normal node in a linked list"

LL_Empty = namedtuple( "LL_Empty", () )
"Right-hand list terminator"

def head( llist ):
    "Return the first element in the list llist."
    return llist.value

def tail( llist ):
    "Return a list of everything but the first element from the list llist."
    return llist.next

def cons( value, llist ):
    "Return a new list consisting of the value prepended to llist."
    return LL_Node( value, llist )

def is_empty( llist ):
    "Return True iff llist has no elements."
    return isinstance( llist, LL_Empty )