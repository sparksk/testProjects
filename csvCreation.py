##Path location where new csv file will be stored
csvPath = 'C:/Users/Sparks/Desktop/desk/code/GitHub/testproject'

object1 = 'test1'
object2 = 'test2'
object3 = 'test3'

##Opens the csv file to write to (can change 'w' to 'a' to append to)
with open(csvPath+'/test.csv', "w") as DataTable:
    ##Defines the number of columns and the header of each column
    toWrite = "{0},{1},{2}\n".format('column x','column y','column z')
    ##add more data
    toWrite += "{0},{1},{2}\n".format(object1, object2, object3)
    ##writes the additions to the csv file
    DataTable.write(toWrite)

##Reading in csv files
import csv
with open('test.csv', 'rb') as f:
    reader = csv.reader(f)
    for row in reader:
        print row
