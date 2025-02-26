# SCRIPTS TO GET DATA INFO (PANDA)

import pandas as pd

df = pd.read_csv('data.csv')

info = df.info()
