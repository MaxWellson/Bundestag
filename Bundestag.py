# -*- coding: utf-8 -*-
"""
Created on Thu Jun  8 11:31:52 2017

@author: Max
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os

indir = "C:\\Users\\Max\\Documents\\Github\\Bundestag"

df = None

for root, dirs, filenames in os.walk(indir):
    for f in filenames:
        if f.endswith('.xls'):
            print (f[0:8])
            df_temp = pd.read_excel(os.path.join(root,f))
            df_temp['Datum'] = pd.to_datetime(f[0:8],yearfirst = True, format = "%Y/%m/%d")
            
            if df is None:
                df = df_temp
            else:
                df = df.append(df_temp, ignore_index = True)

# Datentypen korrekt ändern

df[['ja','nein','Enthaltung','ungültig','nichtabgegeben']].astype('int')

# Group by Sitzungsnummer and Abgeordneter

# df = df[df.Wahlperiode == 18]

# change Party to categorical variable

df['Fraktion/Gruppe'].map({'BÜ90/GR':'DIE GRÜNEN',
                                                  'BÜNDNIS`90/DIE GRÜNEN':'DIE GRÜNEN',
                                                  'DIE LINKE.':'DIE LINKE',
                                                  'DIE LINKE':'DIE LINKE',
                                                  'fraktionslos':'Fraktionslos',
                                                  'Fraktionslos':'Fraktionslos',
                                                  'CDU/CSU':'CDU/CSU',
                                                  'SPD':'SPD'})

df['Fraktion/Gruppe'] = df['Fraktion/Gruppe'].astype('category')

df['Fraktion/Gruppe'].cat.categories

# Dataframe speichern um weiter in R bearbeiten zu könnnen

df.to_csv(indir+"\\data.csv", header = True, sep = ';')

# Erst mal herausfinden wofür die jeweilige Partei mit Mehrheit gestimmt hat

df_partei = df.groupby(['Sitzungnr','Fraktion/Gruppe']).aggregate(np.sum)

df_partei.drop(df_partei.columns[[0,1,3]], axis = 1, inplace = True)

df_partei['Mehrheit'] = None

def f(row):
    if (row['Enthaltung'] > row['ja']) & (row['Enthaltung'] > row['nein']) : 
        val = 'enthalten'         
    elif row['ja'] > row['nein']:
        val = 'ja'
    elif row['nein'] > row['ja']:
        val = 'nein'
    else:
        val = 'enthalten'
    return val

df_partei['Mehrheit'] = df_partei.apply(f,axis = 1)
df_partei = df_partei.reset_index()

df_partei = df_partei[['Sitzungnr','Fraktion/Gruppe','Mehrheit']]

### Mit Daten pro Abgeordneter mergen

df = pd.merge(df, df_partei, on = ['Sitzungnr','Fraktion/Gruppe'],how='left')

### Neues Feld Stimme erstellen zur einfachereren Verarbeitung

def Stimme(row):
    if   (row['ja'] == 1):
        val = 'ja'
    elif (row['nein'] == 1):
        val = 'nein'
    elif (row['Enthaltung'] == 1):
        val = 'enthalten'
    elif (row['ungültig'] == 1):
        val = 'ungültig'
    elif (row['nichtabgegeben'] == 1) :
        val = 'nichtabgegeben'
    else:
        val = None
    return val

df['Stimme'] = df.apply(Stimme, axis = 1)

### Stimmabgaben bei denen Abgeordnete gegen ihre Partei gestimmt haben

def gegen(row):
    if  (row['Stimme'] == 'ja') & (row['Mehrheit'] == 'nein'):
        val = 1
    elif (row['Stimme'] == 'nein') & (row['Mehrheit'] == 'ja'):
        val = 1
    else:
        val = 0
    return val

df['Gegenstimme'] = df.apply(gegen, axis = 1)

df_rebels = df.groupby(['Bezeichnung','Fraktion/Gruppe']).aggregate(np.sum).sort_values('Gegenstimme', ascending = False)

### Wie viele namentliche Abstimmungen gab es insgesamt?

### 





