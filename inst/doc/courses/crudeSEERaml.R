.seerHome="/data/SEER" 
rm(list=ls()) # note: dot variables defined above persist through such cleanings
getUS=function(d,pop) {
  (pyM=pop$py[1])
  (pyF=pop$py[2])
  (d<-ddply(d, .(sex), summarise,cases=length(agerec))) 
  (M=d$cases[1])
  (F=d$cases[2])
  (crudeIm=M/pyM)
  (crudeIf=F/pyF)
  (malesPerYear=151*crudeIm)
  (femalesPerYear=178*crudeIm)
  (malesPerYear+femalesPerYear)*1e6
} 

library(RSQLite)
m=dbDriver("SQLite")
con=dbConnect(m,dbname=file.path(.seerHome,"00/all.db"))
dbListTables(con)
pops=dbGetQuery(con, "SELECT * from pops")
library(plyr)
(pop<-ddply(pops, .(popsex), summarise,py=sum(population)))

dbListFields(con,"lymyleuk")
# d=dbGetQuery(con, "SELECT * from lymyleuk where histo2=9863") # 4518/year =CML
d=dbGetQuery(con, "SELECT * from lymyleuk where histo2=9863 and COD=35022") # 596/year=CML mortality
getUS(d,pop) 
# d=dbGetQuery(con, "SELECT * from lymyleuk where histo3=9863") # 3634/year CML not coded as 9875 or 9876  
# d=dbGetQuery(con, "SELECT * from lymyleuk where histo3=9875") # 834/year BCR-ABL+ CML 
# d=dbGetQuery(con, "SELECT * from lymyleuk where histo3=9876") # 49/year BCR-ABL neg CML 
# d=dbGetQuery(con, "SELECT * from lymyleuk where ICD9=2051") # 5924/year =CML+CMML
# d=dbGetQuery(con, "SELECT * from lymyleuk where histo3==9945") # 1371/year =CMML
d=dbGetQuery(con, "SELECT * from lymyleuk where ICD9=2050") # 12437/year = AML
# d=dbGetQuery(con, "SELECT * from lymyleuk where ICD9=2070") # 260/year = erythroleukemia
# d=dbGetQuery(con, "SELECT * from lymyleuk where ICD9=2072") # 131/year = Megakaryocytic leukemia
d=dbGetQuery(con, "SELECT * from lymyleuk where ICD9=2050 and COD>=20010 and COD<=37000")#8510/year 
d=dbGetQuery(con, "SELECT * from lymyleuk where ICD9=2050 and COD=35021") # 5929/year = AML deaths
d=dbGetQuery(con, "SELECT * from lymyleuk where COD=35021") # 7465/year = AML deaths 
length(unique(d$casenum)) # 9388/9541 = 98.3% unique
d=dbGetQuery(con, "SELECT * from lymyleuk where COD=35023") # 607/year = other myeloid deaths 
d=dbGetQuery(con, "SELECT * from other where COD=35021") # 1379/year AML death  other e.g. MDS incidence 
d=dbGetQuery(con, "SELECT * from other where COD=35022") # 109/year CML death other incidence 
# I'm not seeing a COD code for MDS in http://seer.cancer.gov/codrecode/1969+_d09172004/index.html
getUS(d,pop)
head(d)
amlIDs=d$casenum
d=dbGetQuery(con, "SELECT * from other where histo3>9979 and histo3<9990") # 14241/year = MDS
getUS(d,pop)
mdsIDs=d$casenum
length(mdsIDs)
length(amlIDs)
length(intersect(amlIDs,mdsIDs)) #144 implies AML secondary to MDS not coded as AML


filecanc=c('breast','digothr','malegen','femgen','other','respir','colrect','lymyleuk','urinary')
lc=vector("list",9)
ld=vector("list",9)
for (k in c(1:9))
{
  d=dbGetQuery(con, paste("SELECT * from",filecanc[k],"where COD=35021")) 
  ld[[k]]=d
  lc[[k]]=d$casenum
}

sapply(lc,length)
(tot=sum(sapply(lc,length)))
ulc=unlist(lc)
(prct=length(unique(ulc))/length(ulc)) # percent AML deaths with unique IDs
prct*sum(sapply(ld,getUS,pop=pop)) #8700 AML deaths per year in the US

# now try to get at MDS mortality. No COD code for it complicates matters
d=dbGetQuery(con, "SELECT * from other where histo3>9979 and histo3<9990 and dthclass=1")
getUS(d,pop) # 3772


# maybe not at steady state, so try after 2005
# head(d)
d=dbGetQuery(con, "SELECT * from other where histo3>9979 and histo3<9990 and dthclass=1 and yrdx>2005")
# head(pops)
pops=dbGetQuery(con, "SELECT * from pops where popyear>2005")
(pop<-ddply(pops, .(popsex), summarise,py=sum(population)))
getUS(d,pop)  # 3604, I was expecting this to be higher that 3772, not lower. Go with ave of ~3700  

# switch back to all years and check that dthclass includes most of the deaths by AML
pops=dbGetQuery(con, "SELECT * from pops")
(pop<-ddply(pops, .(popsex), summarise,py=sum(population)))
d=dbGetQuery(con, "SELECT * from other where histo3>9979 and histo3<9990 and dthclass=1")
getUS(d,pop) # 3,700
mdsIDs=d$casenum

d=dbGetQuery(con, "SELECT * from other where histo3>9979 and histo3<9990 and COD=35021")
getUS(d,pop) # 1,068
# d=dbGetQuery(con, "SELECT * from other where COD=35021") 
# getUS(d,pop) 
amlIDs=d$casenum

length(mdsIDs)
length(amlIDs)
(prct=length(intersect(amlIDs,mdsIDs))/length(amlIDs)) 

