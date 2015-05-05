# %pylab --no-import-all inline
from __future__ import division
import matplotlib.pyplot as plt
import random
import time
import itertools
import urllib
import csv

alltours = itertools.permutations

cities = {1, 2, 3}

print list(alltours(cities))



Point = complex
City  = Point

def X(point):
    "The x coordinate of a point."
    return point.real

def Y(point):
    "The y coordinate of a point."
    return point.imag

def distance(A, B):
    "The distance between two points."
    return abs(A - B)

A = City(3, 0)
B = City(0, 4)
print distance(A, B)
