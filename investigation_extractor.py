'''
input: path_to_file as an argument
output: a tsv of extracted information, you can specify more information by editing the code
'''
import sys
import pandas as pd

path_to_file = str(sys.argv[1])
with open(path_to_file, 'r') as file:
    data = file.readlines()


# Step 2: Define the fields to extract
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

# Step 3: Parse the file and extract the relevant information
for line in data:
    line = line.strip()  # Clean the line
    
    # Check if the line starts with any of the fields we are interested in
    for field in fields_to_extract:
        if line.startswith(field):
            # Extract the part after the tab and store it in the dictionary
            fields_to_extract[field] = line.split('\t')[-1]

# Step 4: Organize the extracted information into a pandas DataFrame
df = pd.DataFrame([fields_to_extract])

# Step 5: Output the DataFrame
df.to_csv(f"{df['Study Identifier'].item()}_investigation_df.tsv", sep="\t")
