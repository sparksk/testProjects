setA = c(1,3,2,3,3,2,4,3)
setB = c(2,2,2,4,1,3,3,4)
setC = c(6,5,5,6,5,4,5,4)
setD = c(10,12,10,8,12,8,12,10)

matrix = as.matrix(rbind(setA,setB,setC,setD))
matrix2 = as.matrix(rbind(setA,setB,setC,setD))
matrix = cbind(matrix, matrix2)

for(a in 1:length(setA/2)) {
	for(b in 1:length(setB/2)) {
		for(c in 1:length(setC/2)) {
			for(d in 1:length(setD/2)) {
				if(sum(matrix[1,a],matrix[2,b],matrix[3,c]) == matrix[4,d]) {
					if(sum(matrix[1,a+1],matrix[2,b+1],matrix[3,c+1]) == matrix[4,d+1]) {
						#etc etc. Continue this for 8 times
					}
				}
			}
		}
	}
}