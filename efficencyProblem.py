## given set "A" {a1, a2, ..., aN } of length "N", find the range of values in set A
## that returns the highest sum.

A = [2, 4, -7, 3, -1, 2 , 6, 3, 2, -10, -3, 1, 2]

# o(n^3)
total = 0
for i in range(0,len(A)+1):
    for j in range(0,len(A)+1):
        if sum(A[i:j]) >= total:
            total = sum(A[i:j])
print total

# o(n^2)
total = 0
test = 0
for i in range(0,len(A)+1):
    if test <= total:
        test = A[i]
    
