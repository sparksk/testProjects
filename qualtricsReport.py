import pandas as pd
import os
os.chdir("/Users/sparks/Desktop")

qualtrics_df = pd.read_csv("/Users/sparks/Google Drive/temp/onGround_and_aerial.csv", skiprows=1)

# print qualtrics_df['ResponseID']
# qualtrics_df.iterrows()

# for index, row in qualtrics_df.iterrows():
#     # print row[0]
#     for col in range(0, len(row)):
#         print row[col]


with open("Output.txt", "w") as text_file:
    text_file.write("{}\n".format("Participant 1:"))
    text_file.write("{}\n".format("I said fuck you"))




# import pandas as pd
# inp = [{'c1':10, 'c2':100}, {'c1':11,'c2':110}, {'c1':12,'c2':120}]
# df = pd.DataFrame(inp)
# print df
#
# for index, row in df.iterrows():
#     print row['c1'], row['c2']
