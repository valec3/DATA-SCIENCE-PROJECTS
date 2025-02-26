# SCRIPTS TO CLEAN DATA (PANDA)
import pandas as pd

df = pd.read_csv('data.csv') 

# Drop rows with missing values
df = df.dropna()

# Drop duplicates
df = df.drop_duplicates()

# Drop columns
df = df.drop(columns=['column1', 'column2'])

# Rename columns
df = df.rename(columns={'old_name': 'new_name'})

# Change data type
df['column'] = df['column'].astype('int')

# Save to csv
df.to_csv('data_clean.csv', index=False)

