# NOTE: This script assumes that the SEER data is in /data/SEER 
.seerHome="/data/SEER" 
rm(list=ls()) # note: dot variables defined above persist through such cleanings
# install.packages("RSQLite")
# install.packages("bbmle")
# install.packages("ggplot2")
library(RSQLite)
m=dbDriver("SQLite")
library(plyr)
library(bbmle)
library(ggplot2)

graphics.off()
windows(height=4,width=8,xpos=-100,ypos=-100) #need to control the device size to
con=dbConnect(m,dbname=file.path(.seerHome,"00/all.db"))
dbListTables(con)
dbListFields(con,"pops")
dbListFields(con,"respir")
# pop=dbGetQuery(con,"SELECT * from pops where popage>6 and popage<19 and poprace=1")
#Note: whites only looked the same
pop=dbGetQuery(con,"SELECT * from pops where popage>6 and popage<19") 
head(pop)
(pop<-ddply(pop, .(popage,popsex), summarise,py=sum(population)))
head(pop,20)

d=dbGetQuery(con,"SELECT * from respir where ICD9>=1620 and ICD9<=1629 
and histO3=8140 and seqnum<2 and agerec>6 and agerec<19") # adenos
# and race=1 and histO3=8140 and seqnum<2 and agerec>6 and agerec<19")
head(d)
(d<-ddply(d, .(agerec,sex), summarise,cases=length(agerec))) 
head(cbind(d,pop)) # just to see that they match up
d=cbind(d,py=pop[,"py"]) # only take the non-redundant py column
head(d)

d9=dbGetQuery(con,"SELECT * from respir where ICD9>=1620 and ICD9<=1629 
and histO3>=8070 and histO3<=8079 and seqnum<2 and agerec>6 and agerec<19") # squames
# and race=1 and  histO3>=8070 and histO3<=8079 and seqnum<2 and agerec>6 and agerec<19")
head(d9)

d9<-ddply(d9, .(agerec,sex), summarise,cases=length(agerec))
d9=cbind(d9,py=pop[,"py"])
head(d9)
head(d)

(d=cbind(rbind(d,d9),code=gl(2,dim(d9)[1],labels=c("adenocarcinoma","squamous cell"))) ) 
head(d)
d=transform(d,incid=1e6*cases/py)
d$sex=factor(d$sex,labels=c("Male","Female"))
age=c(0.5,3,seq(7.5,87.5,5))
d$age=age[d$agerec+1]
head(d,15)
names(d)[5]="Histology" # make legend start with capital

(p <- ggplot(d,aes(x=age,y=incid,col=Histology))+geom_point(size=5)
 + labs(title="SEER Lung Cancer Incidence",x="Age (years)",
        y=expression(paste("Cases per ",10^6," Person-Years")))    
 + scale_y_log10(limits=c(1,1500)) )
(p=p + facet_grid(~ sex))
  mythem=theme( 
  plot.title = element_text(size = rel(2)),
  axis.title = element_text(size = rel(1.7)),
  axis.text = element_text(size = rel(1.4)),
  strip.text = element_text(size = rel(1.4))  )
p=p+mythem
(p=p+theme(legend.position = c(.33, .2),  #           legend.direction = 'vertical',
           legend.title = element_text(size = rel(1.4)) ,
           legend.text = element_text(size = rel(1.4))  ) )  

##############


popall=dbGetQuery(con,"SELECT * from pops")
(popall<-ddply(popall, .(popsex), summarise,py=sum(population)))
dall=dbGetQuery(con,"SELECT * from respir where ICD9>=1620 and ICD9<=1629")
(Nall=ddply(dall, .(sex), summarise,cases=length(agerec)))

dSQ=dbGetQuery(con, 
"SELECT * from respir where ICD9>=1620 and ICD9<=1629 and histO3>=8070 and histO3<=8079")
(NSQ=ddply(dSQ, .(sex), summarise,cases=length(agerec)))

dAD=dbGetQuery(con,"SELECT * from respir where ICD9>=1620 and ICD9<=1629 and histO3=8140")
(NAD=ddply(dAD, .(sex), summarise,cases=length(agerec)))
sum(Nall)
sum(NSQ)
sum(NSQ)/sum(Nall)
sum(NAD)
sum(NAD)/sum(Nall)

sum(popall)
NSQ/Nall
0.85*0.2
NAD/Nall

###########################################################3
filecanc=c('breast','digothr','malegen','femgen','other','respir','colrect','lymyleuk','urinary')
load("/data/SEER/00/pops.RData") # this loads in pops
pyf=pym=vector(3,mode="list"); 
for (i in 0:18) { for (r in 1:2) {
  pym[[r]][i+1]=with(pops,sum(population[(popsex==1)&(popage==i)&(poprace==r)]))
  pyf[[r]][i+1]=with(pops,sum(population[(popsex==2)&(popage==i)&(poprace==r)]))}
                  pym[[3]][i+1]=with(pops,sum(population[(popsex==1)&(popage==i)&(poprace>2)]))
                  pyf[[3]][i+1]=with(pops,sum(population[(popsex==2)&(popage==i)&(poprace>2)])) }
if(length(grep("linux",R.Version()$os))) windows <- function( ... ) X11( ... )
graphics.off();
k=6
load("/data/SEER/00/respir.RData") # this loads in DF
DF=DF[!is.na(DF$ICD9),]  # get rid of missing ICD9 entries
canc="Lung";code=list(c(1622,1629))}
j=1
windows(width=12,height=5,xpos=-250)
par(mfrow=c(1,3),mar=c(4.7,0,2,0),oma=c(0,7.1,4,0),lwd=3,cex.lab=2.8,cex.axis=2.5,cex.main=2.8)
Indx=(DF$ICD9>=code[[j]][1])&(DF$ICD9<=code[[j]][2])&(DF$numprims==1) 
incdf=incdm=vector(3,mode="list"); 
casesf=casesm=vector(3,mode="list"); 
for (i in 1:3) {
  if (i==3) d=DF[Indx&(DF$race>2)&(DF$race<98),] else 
    d=DF[Indx&(DF$race==i),] 
  if ((k!=1)&(k!=4)) incdm[[i]]=hist(d$agerec[d$sex==1],breaks=c(seq(-.5,17.5,1),100),plot=FALSE)$counts/pym[[i]]
  if (k!=3) incdf[[i]]=hist(d$agerec[d$sex==2],breaks=c(seq(-.5,17.5,1),100),plot=FALSE)$counts/pyf[[i]]
  if ((k!=1)&(k!=4)) casesm[[i]]=hist(d$agerec[d$sex==1],breaks=c(seq(-.5,17.5,1),100),plot=FALSE)$counts
  if (k!=3) casesf[[i]]=hist(d$agerec[d$sex==2],breaks=c(seq(-.5,17.5,1),100),plot=FALSE)$counts
}	
if ((k!=1)&(k!=3)&(k!=4))  # both sexes 
  df=data.frame(age=rep(c(0.5,3,seq(7.5,87.5,5)),6),race=rep(c("white","black","asian"),each=19,times=2),
                sex=factor(rep(c("male","female"),each=57),levels=c("male","female")),
                incd=c(unlist(incdm),unlist(incdf)),cases=c(unlist(casesm),unlist(casesf)) ,py=c(unlist(pym),unlist(pyf)) )
if ((k==1)|(k==4))  df=data.frame(age=rep(c(0.5,3,seq(7.5,87.5,5)),3),race=rep(c("white","black","asian"),each=19),
                                  sex=factor(rep("female",each=57),levels=c("male","female")),incd=unlist(incdf),
                                  cases=unlist(casesf),py=unlist(pyf)  )
if (k==3) 	df=data.frame(age=rep(c(0.5,3,seq(7.5,87.5,5)),3),race=rep(c("white","black","asian"),each=19),
                         sex=factor(rep("male",each=57),levels=c("male","female")),incd=unlist(incdm),
                         cases=unlist(casesm),py=unlist(pym)  )
df=subset(df,(age>20)&(incd>0))
#		lapply(df,class)
rng=range(df$incd)
for (i in 1:3) {
  di=subset(df,race==c("white","black","asian")[i])
  #			summary(lmf<-glm(cases~age+sex+offset(log(py)),family=poisson,data=di))
  with(di,plot(age,incd,log="y",xlab="Age",type='p',col=c("blue","red")[as.numeric(sex)],pch=as.numeric(sex),
               ylab="",cex=2,yaxt="n",ylim=rng))
  if (i==1) {axis(side=2,las=1, at=c(1e-6,1e-5,1e-4,1e-3,1e-2),labels=expression(1,10,10^2,10^3,10^4))
             mtext(expression(paste("Cases per ",10^6," Person-Years")),side=2,line=3.8,cex=2)}
  mtext(paste(sum(di$cases),c("whites","blacks","others")[i]),side=3,line=.5,cex=1.5,adj=0.5)
  legend("bottomright",c("Males","Females"),col=c("blue","red"),pch=1:2,text.col=c("blue","red"),bty="n",cex=2)
} # i loop over races
title(paste("SEER",canc[j],"Cancer Incidence 2000-2010"),cex=3,outer=T)


######################################## Repeat above using ggplot and SQL
graphics.off()
windows(height=7,width=8,xpos=-100,ypos=-100) #need to control the device size to
# make things work across computers/screen sizes
con=dbConnect(m,dbname=file.path(.seerHome,"73/all.db"))
#######################################################################
dbListTables(con)
dbListFields(con,"pops")
dbListFields(con,"respir")
pop=dbGetQuery(con,"SELECT * from pops where popage>5 and popage<19")
head(pop)
(pop<-ddply(pop, .(popage,popsex,popyear), summarise,py=sum(population)))
pop$dec= cut(pop$popyear,breaks=c(1972,1984,1996,2010),labels=c("73to84","85to96","97to10"))
(pop<-ddply(pop, .(popage,popsex,dec), summarise,py=sum(py)))
head(pop,20)

d=dbGetQuery(con, 
             "SELECT * from respir where ICD9>=1620 and ICD9<=1629 and histO2=8140 and seqnum<2 and agerec>5 and agerec<19")
head(d)
(d<-ddply(d, .(agerec,sex,yrdx), summarise,cases=length(agerec))) 
d$decade= cut(d$yrdx,breaks=c(1972,1984,1996,2011),labels=c("73to84","85to96","97to10"))
head(d)
(d<-ddply(d, .(agerec,sex,decade), summarise,cases=sum(cases)))
head(cbind(d,pop)) # just to see that they match up
d=cbind(d,py=pop[,"py"]) # only take the non-redundant py column
head(d)

d9=dbGetQuery(con, 
              "SELECT * from respir where ICD9>=1620 and ICD9<=1629 and histO2>=8070 and histO2<=8079 and seqnum<2 and agerec>5 and agerec<19")
head(d9)

d9<-ddply(d9, .(sex,agerec,yrdx), summarise,cases=length(agerec))
d9$decade= cut(d9$yrdx,breaks=c(1972,1984,1996,2010),labels=c("73to84","85to96","97to10"))
d9<-ddply(d9, .(agerec,sex,decade), summarise,cases=sum(cases))
d9=cbind(d9,py=pop[,"py"])
head(d9)
(d=cbind(rbind(d,d9),code=gl(2,dim(d9)[1],labels=c("Ad","Sq"))) ) 
head(d)
d=transform(d,incid=1e6*cases/py)
d$sex=factor(d$sex,labels=c("Male","Female"))
age=c(0.5,3,seq(7.5,87.5,5))
d$age=age[d$agerec+1]
head(d,15)
names(d)[3]="Decade" # make legend start with capital
(p <- ggplot(d,aes(x=age,y=incid,shape=Decade))+geom_point(size=5)
 + labs(title="SEER Lung Cancer Incidence",x="Age (years)",
        y=expression(paste("Cases per ",10^6," Person-Years")))    
 + scale_y_log10(limits=c(1,1500)) )
(p=p + facet_grid(code ~ sex))
mythem=theme( 
  plot.title = element_text(size = rel(3)),
  axis.title = element_text(size = rel(2.4)),
  axis.text = element_text(size = rel(2)),
  strip.text = element_text(size = rel(2.4))  )
p=p+mythem
(p=p+theme(legend.position = c(.8, .6),  #           legend.direction = 'vertical',
           legend.title = element_text(size = rel(1.4)) ,
           legend.text = element_text(size = rel(1.4))  ) )  

# ggsave(p,file="/users/radivot/igv/trends.wmf")
# ggsave(p,file="/users/radivot/case/grants/sachs/HSCepi/figs/trends.png")

