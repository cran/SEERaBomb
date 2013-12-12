# NOTE: This script assumes that the SEER data is in /data/SEER.  Please change
# this line to match your data location if it differs
.seerHome="/data/SEER" 
rm(list=ls()) # note: dot variables defined above persist through such cleanings
library(RSQLite)
m=dbDriver("SQLite")
library(plyr)
con=dbConnect(m,dbname=file.path(.seerHome,"00/all.db"))
d=dbGetQuery(con, "SELECT * from lymyleuk where histo3==9945") # 1371/year
# d=dbGetQuery(con, "SELECT * from lymyleuk where histo2==9945") # 0/year code is new to O3
(d<-ddply(d, .(sex), summarise,cases=length(agerec))) 
pops=dbGetQuery(con, "SELECT * from pops")
(pop<-ddply(pops, .(popsex), summarise,py=sum(population)))
(pyM=pop$py[1])
(pyF=pop$py[2])
(M=d$cases[1])
(F=d$cases[2])
(crudeIm=M/pyM)
(crudeIf=F/pyF)
(malesPerYear=151*crudeIm)
(femalesPerYear=178*crudeIm)
(malesPerYear+femalesPerYear)*1e6

# double check CML value
con=dbConnect(m,dbname=file.path(.seerHome,"00/all.db"))
d=dbGetQuery(con, "SELECT * from lymyleuk where histo2=9863") # 4518/year
# d=dbGetQuery(con, "SELECT * from lymyleuk where histo3=9863") # 3634/year
# d=dbGetQuery(con, "SELECT * from lymyleuk where ICD9=2051") # 5924/year
(d<-ddply(d, .(sex), summarise,cases=length(agerec))) 
             pops=dbGetQuery(con, "SELECT * from pops")
(pop<-ddply(pops, .(popsex), summarise,py=sum(population)))
(pyM=pop$py[1])
(pyF=pop$py[2])
(M=d$cases[1])
(F=d$cases[2])
(crudeIm=M/pyM)
(crudeIf=F/pyF)
(malesPerYear=151*crudeIm)
(femalesPerYear=178*crudeIm)
(malesPerYear+femalesPerYear)*1e6

             