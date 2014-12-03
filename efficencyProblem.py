## PROBLEM
## given set "B" {b1, b2, ..., bN } of length "N", find the range of values in set B
## that returns the highest sum.


# Generates a list with random integers from -10:10, with N entries
import random
# Empty list that gets added to
B = []
# For N iterations...
for i in range(0,1000):
    # Randomly returns either a 1 or 2
    negpos = random.randint(1,2)
    # If a 1 is returned, pick a positive integer from 1:10 and add it to B
    if negpos == 1:
        B.extend([random.randint(1,10)])
    # If a 2 is returned, pick a negative integer from 1:10 and add it to B
    if negpos == 2:
        B.extend([random.randint(1,10)*-1])


################# o(n^2) ##################
## Creates 2 index values to iterate through. While on idex value stays constant on
## a single integer in set B, the other index values iterates through the rest of the list.
## Indexed values are summed together, and total is redefined when the sum of
## integers from i to j excede previous values. If sum from i to j is less than total, total stays constant.
##########
total = 0
for i in range(0,len(B)):
    for j in range(0,len(B)):
        if sum(B[i:j]) >= total:
            total = sum(B[i:j])
print total

################## o(n) ##################
## Greedy algorithm
## Iterates through the list of B, and sums along the way. If toal drops below 0, than
## total gets redefined as 0 and the summation begins at the next index value.
##########
total = 0
peak = []
trough = []
indexpeak = []
indextrough = []
for i in range(0,len(B)):
    if B[i] < 0:
        peak.extend([total])
        indexpeak.extend([i-1])
    total = total + B[i]
    if total < 0:
        trough.extend([total])
        # only marks a trough when total falls below 0
        indextrough.extend([i+1])
        total = 0
print max(peak)
# print indexpeak
# truepeakindex = indexpeak[peak.index(max(peak))]
# print truepeakindex
# print trough
# print indextrough
# print total
