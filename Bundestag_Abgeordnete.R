# Packages

require(XLConnect)
require(dplyr)
require(tidyr)

# Directory wo csv-Tabellen liegen

indir = "C:\\Users\\Max\\Documents\\Github\\Bundestag"
outdir= "C:\\Users\\Max\\DOcuments\\Blog\\D3"
setwd(indir)

df = read.csv(paste(indir,"data.csv",sep="\\"),sep=";")

df = subset(df, Wahlperiode == 18)

# Variablen umbenennen 

names(df)[names(df) == 'Name'] = 'Nachname'
names(df)[names(df) == 'Bezeichnung'] = 'Name'
names(df)[names(df) == 'Fraktion.Gruppe'] = 'Fraktion'

# Change orientation of dataset

df$Votum[df$ja == 1] = 'Ja'
df$Votum[df$nein == 1] = 'Nein'
df$Votum[df$nichtabgegeben == 1] = 'Nicht abgegeben'
df$Votum[df$ungültig == 1] = 'Ungültig'
df$Votum[df$Enthaltung == 1] = 'Enthaltung'

df$Votum = as.factor(df$Votum)

# Doppelte/Ähnliche Namen anpassen

df$Name = as.character(df$Name)

df$Name[df$Name == 'Albert Rupprecht'] = 'Albert Rupprecht (Weiden)'
df$Name[df$Name == 'Alexander S. Neu'] = 'Dr. Alexander S. Neu'
df$Name[df$Name == 'Andreas Jung'] = 'Andreas Jung (Konstanz)'
df$Name[df$Name == 'Andreas Scheuer'] = 'Dr. Andreas Scheuer'
df$Name[df$Name == 'Annette Schavan'] = 'Dr. Annette Schavan'
df$Name[df$Name == 'Dr. André Berghegger'] = 'Dr. Andre Berghegger'
df$Name[df$Name == 'Dr. André Hahn'] = 'Dr. Andre Hahn'
df$Name[df$Name == 'Dr. Dr. h.c. Karl A. Lamers'] = 'Dr. Karl A. Lamers (Heidelberg)'
df$Name[df$Name == 'Julia Bartz'] = 'Julia Obermeier'
df$Name[df$Name == 'Marianne Schieder'] = 'Marianne Schieder (Schwandorf)'
df$Name[df$Name == 'Michaela Engelmeier'] = 'Michaela Engelmeier-Heite'
df$Name[df$Name == 'Ronja Kemmer'] = 'Ronja Schmitt (Althengstett)'
df$Name[df$Name == 'Ronja Schmitt'] = 'Ronja Schmitt (Althengstett)'
df$Name[df$Name == 'Daniela Kolbe'] = 'Daniele Kolbe (Leipzig)'

# Parteien anpassen

df$Fraktion = as.character(df$Fraktion)

df$Fraktion[df$Fraktion == 'BÜNDNIS`90/DIE GRÜNEN'] = 'DIE GRÜNEN'
df$Fraktion[df$Fraktion == 'BÜ90/GR'] = 'DIE GRÜNEN'
df$Fraktion[df$Fraktion == 'DIE LINKE.'] = 'DIE LINKE'
df$Fraktion[df$Fraktion == 'fraktionslos'] = 'Fraktionslos'
df$Fraktion[df$Fraktion == 'CDU/CSU'] = 'CDU_CSU'

df$Fraktion = as.factor(df$Fraktion)

# Mehrheit berechnen

df_sum_1 = df %>%
          group_by(Datum,Abstimmnr) %>%
          summarise(Anzahl_Votumn = sum(ja,nein,Enthaltung),
                    Anzahl_Ja_Stimmen = sum(ja),
                    Anzahl_Nein_Stimmen = sum(nein),
                    Anzahl_Enthaltungen = sum(Enthaltung))

df_sum_2 = df %>%
          group_by(Datum,Abstimmnr,Fraktion) %>%
          summarise(Ja_Stimmen = sum(ja), Nein_Stimmen = sum(nein), Enthaltung_Stimmen = sum(Enthaltung))

df_sum = full_join(df_sum_2,df_sum_1, by = c('Datum','Abstimmnr'))

df_sum = subset(df_sum, Fraktion != 'Fraktionslos')

# Mehrheitsvotum aller Parteien bestimmen

df_sum["Votum_gesamt"] = NA

df_sum$Votum_gesamt[df_sum$Anzahl_Ja_Stimmen > df_sum$Anzahl_Nein_Stimmen] = 'Ja'
df_sum$Votum_gesamt[df_sum$Anzahl_Ja_Stimmen < df_sum$Anzahl_Nein_Stimmen] = 'Nein'

df_sum$Votum_gesamt = factor(df_sum$Votum_gesamt, levels = levels(df$Votum))

# Mehrheitsstimmen der jeweiligen Parteien bestimmen

df_sum["Votum_Partei"] = NA

df_sum$Votum_Partei[df_sum$Ja_Stimmen > df_sum$Nein_Stimmen] = 'Ja'
df_sum$Votum_Partei[df_sum$Ja_Stimmen < df_sum$Nein_Stimmen] = 'Nein'
df_sum$Votum_Partei[df_sum$Enthaltung_Stimmen > df_sum$Ja_Stimmen & df_sum$Enthaltung_Stimmen > df_sum$Nein_Stimmen] = 'Enthaltung'

df_sum$Votum_Partei = factor(df_sum$Votum_Partei, levels = levels(df$Votum))

# Votum der Partei an df joinen

df = left_join(df,df_sum,by= c('Datum','Abstimmnr','Fraktion'))

# Nur notwendige Variablen behalten

df = df[c('Wahlperiode','Datum','Sitzungnr','Abstimmnr','AbgNr','Name','Fraktion','Votum','Votum_gesamt','Votum_Partei')]

# Anzahl abweichende Votumn pro Partei und Abgeordnetem berechnen

df_abweichler = subset(df,Votum != Votum_Partei & Votum != 'Nicht abgegeben' & Votum != 'Enthaltung') %>%
        group_by(Fraktion,Name) %>%
        summarise(Anzahl_abweichende_Stimmen_von_Partei = n()) %>%
        arrange(desc(Anzahl_abweichende_Stimmen_von_Partei))

# Herausfinden mit welcher Partei gewählt wurde wenn abgewichen wurde

test = subset(df, Votum != Votum_Partei & Votum != 'Nicht abgegeben' & Votum != 'Enthaltung') %>%
        inner_join(df_sum, by = c('Votum'='Votum_Partei', 'Datum'='Datum','Abstimmnr'='Abstimmnr'))

names(test)[names(test) == 'Fraktion.x'] <- 'eigene_Fraktion'
names(test)[names(test) == 'Fraktion.y'] <- 'gestimmt_mit_Fraktion'

# Wenn mit mehr als einer Partei gewählt wird normalisieren nach Anzahl der Parteien mit denen gestimmt wurde

test1 = test %>%
  group_by(Datum,Sitzungnr,Abstimmnr,Name) %>%
  summarise(Gewichtete_Stimme = 1/n())

test = left_join(test,test1[c('Datum','Sitzungnr','Abstimmnr','Name','Gewichtete_Stimme')],by= c('Datum','Sitzungnr','Abstimmnr','Name'))


df_abweichler_partei = test %>%
        group_by(eigene_Fraktion,Name,gestimmt_mit_Fraktion) %>%
        summarise(Anzahl_Stimmen_mit_fremder_Partei = sum(Gewichtete_Stimme))
        

# Anzahl Stimmen mit eigener Partei auswerten

df_summary_2 = subset(df, Votum == Votum_Partei & Votum != 'Nicht abgegeben' & Votum != 'Enthaltung') 


df_summary_2 = df_summary_2 %>%
        group_by(Fraktion,Name) %>%
        summarise(Anzahl_Stimmen_mit_eigener_Partei = n())

df_summary_3 = test %>%
        group_by(eigene_Fraktion,Name) %>%
        summarise(Anzahl_Stimmen_mit_fremder_Partei = sum(Gewichtete_Stimme))

# An ursprünglichen Dataframe hinzufügen

df_summary = left_join(df_summary_2,df_abweichler[c('Name','Anzahl_abweichende_Stimmen_von_Partei')],by = ('Name'))

df_summary$Anzahl_abweichende_Stimmen_von_Partei[is.na(df_summary$Anzahl_abweichende_Stimmen_von_Partei)] = 0

df_summary['Anzahl_abgegebene_Stimmen'] = NA

df_summary$Anzahl_abgegebene_Stimmen = df_summary$Anzahl_abweichende_Stimmen_von_Partei + df_summary$Anzahl_Stimmen_mit_eigener_Partei

# Politische Orientierung der Abgeordneten innerhalb der Partei berechnen

df_abweichler_partei_2 = df_abweichler_partei %>%
  spread(gestimmt_mit_Fraktion,Anzahl_Stimmen_mit_fremder_Partei)

df_score = df_summary %>%
  spread(Fraktion,Anzahl_Stimmen_mit_eigener_Partei)

df_score = df_summary[c('Name','Fraktion','Anzahl_abgegebene_Stimmen')] %>%
  left_join(df_abweichler_partei_2,by = c('Name'='Name','Fraktion' = 'eigene_Fraktion'))

df_score[is.na(df_score)] = 0

df_score$Fraktion = factor(df_score$Fraktion)


# Anzahl Stimmen für eigene Partei jeweiliger Spalte zuordnen (Verbesserung: HardCoding erstetzen!!!)

df_score_CDU = subset(df_score,Fraktion == 'CDU_CSU')
df_score_CDU['CDU_CSU'] = df_score_CDU$Anzahl_abgegebene_Stimmen - df_score_CDU$`DIE GRÜNEN` - df_score_CDU$`DIE LINKE` - df_score_CDU$SPD

df_score_GR = subset(df_score,Fraktion == 'DIE GRÜNEN')
df_score_GR['DIE GRÜNEN'] = df_score_GR$Anzahl_abgegebene_Stimmen - df_score_GR$`CDU_CSU` - df_score_GR$`DIE LINKE` - df_score_GR$SPD

df_score_LI = subset(df_score,Fraktion == 'DIE LINKE')
df_score_LI['DIE LINKE'] = df_score_LI$Anzahl_abgegebene_Stimmen - df_score_LI$`CDU_CSU` - df_score_LI$`DIE GRÜNEN` - df_score_LI$SPD

df_score_SPD = subset(df_score,Fraktion == 'SPD')
df_score_SPD['SPD'] = df_score_SPD$Anzahl_abgegebene_Stimmen - df_score_SPD$`CDU_CSU` - df_score_SPD$`DIE LINKE` - df_score_SPD$`DIE GRÜNEN`

df_score  = rbind(df_score_CDU,df_score_GR,df_score_LI,df_score_SPD)

df_score = left_join(df_score,df_summary[c('Name','Anzahl_Stimmen_mit_eigener_Partei')],by = c('Name'))

df_score['Proz_Abweichungen'] = NA
df_score$Proz_Abweichungen = 1 - (df_score$Anzahl_Stimmen_mit_eigener_Partei / df_score$Anzahl_abgegebene_Stimmen)

df_score[c('CDU_CSU_proz','DIE GRUENEN_proz','DIE_LINKE_proz','SPD_proz')] = NA
df_score['CDU_CSU_proz'] = df_score['CDU_CSU'] / df_score['Anzahl_abgegebene_Stimmen']
df_score['DIE_GRUENEN_proz'] = df_score['DIE GRÜNEN'] / df_score['Anzahl_abgegebene_Stimmen']
df_score['DIE_LINKE_proz'] = df_score['DIE LINKE'] / df_score['Anzahl_abgegebene_Stimmen']
df_score['SPD_proz'] = df_score['SPD'] / df_score['Anzahl_abgegebene_Stimmen']


df_output = df_score[c('Name','Fraktion','Anzahl_abgegebene_Stimmen','CDU_CSU_proz','DIE_GRUENEN_proz','DIE_LINKE_proz','SPD_proz','Proz_Abweichungen')]

df_output = df_output %>%
  arrange(desc(Proz_Abweichungen))

df_output[,4:8] = round(df_output[,4:8],4)

df_output = df_output[1:20,] 

# %>%
#   mutate_all(as.character)
# 
# df_output[df_output == '0'] = '0.00000'

# df_output$`CDU_CSU`[df_output$`CDU_CSU` == '0.00'] = '0'
# df_output$`DIE LINKE`[df_output$`DIE LINKE` == '0.00'] = '0'
# df_output$`DIE GRÜNEN`[df_output$`DIE GRÜNEN` == '0.00'] = '0'
# df_output$SPD[df_output$SPD == '0.00'] = '0'



setwd(outdir)


write.xlsx(as.data.frame(df_output),'df_output.xlsx')

setwd(indir)

# Analyse von welcher Partei Abweichler für welche andere Partei stimmen

df_frak = df_score %>%
  group_by(Fraktion) %>%
  summarise(`CDU/CSU` = sum(CDU_CSU)/sum(Anzahl_abgegebene_Stimmen),
            `DIE GRÜNEN` = sum(`DIE GRÜNEN`)/sum(Anzahl_abgegebene_Stimmen),
            `DIE LINKE` = sum(`DIE LINKE`)/sum(Anzahl_abgegebene_Stimmen),
            SPD = sum(SPD)/sum(Anzahl_abgegebene_Stimmen))


df_frak[df_frak == 0] = NaN

df_frak[,2:5] = round(df_frak[,2:5],6)

df_frak = df_frak %>%
  mutate_all(as.character)

df_frak[df_frak == 'NaN'] = '0.00'
df_frak[df_frak == 'CDU_CSU'] = 'CDU/CSU'



setwd(outdir) 
# con = file('df_frak.xlsx',encoding="UTF-8")
write.xlsx(df_frak,'df_frak.xlsx')

setwd(indir)

