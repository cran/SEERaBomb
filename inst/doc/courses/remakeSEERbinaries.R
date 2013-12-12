.seerHome="/data/SEER" 
library(RSQLite)
m=dbDriver("SQLite")
con=dbConnect(m,dbname=file.path(.seerHome,"00/all.db"))
dbListTables(con)
dbListFields(con,"lymyleuk") # see what we're missing, e.g. ICD10
library(SEERaBomb)
(df=getFields())
head(df,20)
picks=c("casenum","reg","race","sex","agedx","yrbrth",
    "seqnum","yrdx","histo2","histo3","radiatn","agerec",
    "siterwho","ICD9","ICD10","histrec","numprims","COD","dthclass","odthclass","surv")
(df=pickFields(df,picks))
mkSEER(df,dataset="92",SQL=TRUE) 
mkSEER(df,dataset="73",SQL=TRUE)
mkSEER(df,dataset="00",SQL=TRUE)

con=dbConnect(m,dbname=file.path(.seerHome,"00/all.db"))
dbListFields(con,"lymyleuk") # new ones now there
