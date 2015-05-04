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

list(alltours(cities))
