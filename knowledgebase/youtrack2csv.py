import pandas as pd
import numpy as np
import csv

df = pd.read_csv('issues.csv', dtype={'Zendesk Debugger FAQ Number': str})
df.set_index('Issue Id')
df = df[['Zendesk Debugger Identifier Text', 'Zendesk-Debugger-Error-Code', 'Summary', 'Issue Id', 'Zendesk Debugger FAQ Number']]
df = df[~df['Zendesk Debugger Identifier Text'].isin(['No zendesk debugger identifier text'])]
df = df[~df['Zendesk-Debugger-Error-Code'].isin(['No zendesk-debugger-error-code'])]
df.to_csv('tmpknowledge.csv', index = False, header = False, quotechar='"', quoting=csv.QUOTE_ALL)
