## given set "A" {a1, a2, ..., aN } of length "N", find the range of values in set A
## that returns the highest sum.

A = [2, 4, -7, 3, -1, 2 , 6, 3, 2, -10, -3, -3, 2]



######### o(n^3) #########


######### o(n^2) #########
total = 0
for i in range(0,len(A)):
    for j in range(0,len(A)):
        if sum(A[i:j]) >= total:
            total = sum(A[i:j])
print total


######### o(n) #########
# Greedy algorithm
total = 0
peak = []
trough = []
indexpeak = []
indextrough = []
for i in range(0,len(A)):
    if A[i] < 0:
        peak.extend([total])
        indexpeak.extend([i-1])
    total = total + A[i]
    if total < 0:
        trough.extend([total])
        # only marks a trough when total falls below 0
        indextrough.extend([i+1])
        total = 0
# print peak
# print indexpeak
truepeakindex = indexpeak[peak.index(max(peak))]
print truepeakindex
# print trough
print indextrough
