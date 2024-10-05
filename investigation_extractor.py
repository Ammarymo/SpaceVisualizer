'''
input: path_to_file as an argument
output: a tsv of extracted information, you can specify more information by editing the code
'''
import sys
import pandas as pd

path_to_file = str(sys.argv[1])
with open(path_to_file, 'r') as file:
    data = file.readlines()


fields_to_extract = {
    'Study Identifier': None,
    'Study Title': None,
    'Study Description': None,
    #'Comment[Flight Program]': None,
    #'Comment[Project Identifier]': None,
    #'Comment[Project Title]': None,
    #'Comment[Project Type]': None,
    #'Comment[Space Program]': None,
    #'Study Factor Name': None,
    #'Study Protocol Description': None,
    #'Study Protocol Parameters Name': None
}


for line in data:
    line = line.strip() 
    
    for field in fields_to_extract:
        if line.startswith(field):
            fields_to_extract[field] = line.split('\t')[-1]

# Step 4: Organize the extracted information into a pandas DataFrame
df = pd.DataFrame([fields_to_extract])

# Step 5: Output the DataFrame
df.to_csv(f"{df['Study Identifier'].item()}_investigation_df.tsv", sep="\t")
