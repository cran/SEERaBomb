# nagaCML.R (see documentation in nagaCML.pdf)
# NOTICE: you must have the A-bomb file HEMA87.DAT to run this, see gettingData.pdf in doc. 
# If you have it but not in /data/abomb, you must update read.table below. 

rm(list=ls(all=TRUE))
cols=c("city","sex","doseg","agexg","calg","kerma","PY","adjPY","num.entering",
		"age","agex","tsx","cal","sv","gam","neut","lymphoma","NHL","leukemia","AML","ALL","CML","ATL","MM")      
d<-read.table("/data/abomb/HEMA87.DAT", header=F,col.names=cols);
d=d[d$adjPY>0,] #remove two recs with zero py
d=d[d$kerma==1,] # take only kerma < 4 Gy
d$py=10^4*d$adjPY
d$calg=as.integer(cut(d$calg,c(0,2,4,6,8,10)))
m=d[d$sex==1,] # males only
f=d[d$sex==2,] # females only

flin1<-function(x,df,agem){ c0=x[1];k=x[2];L=x[3:7]
	with(df,{mn = (exp(c0+k*(age-agem)) + sv*exp(L[calg]))*py;
				-sum(CML*log(mn) - mn)})}
flin2<-function(x,df,agem){ c0=x[1];k=x[2];L=x[3:7]; cN=x[8]
	with(df,{mn = (exp(c0+k*(age-agem)) + sv*exp(L[calg]))*exp(cN*((agex>10)*(city-1)))*py;
				-sum(CML*log(mn) - mn)})}
agem=0
agem=55  # center ages. Either way the city effect is significant
X0=c(c0=-13,k=0.05,rep(-10,5),-2)
solm=optim(X0,flin2,df=m,agem=agem,hessian=TRUE,control=list(maxit=400)) 
devM2=2*(solm$value+sum(sapply(m$CML,function(x) x*ifelse(x>0,log(x),0)) - m$CML) )
solf=optim(X0,flin2,df=f,agem=agem,hessian=TRUE,control=list(maxit=400))
devF2=2*(solf$value+sum(sapply(f$CML,function(x) x*ifelse(x>0,log(x),0)) - f$CML) )

X0=c(c0=-13,k=0.05,rep(-10,5))
sol=optim(X0,flin1,df=m,agem=agem,hessian=TRUE,control=list(maxit=400)) 
devM1=2*(sol$value+sum(sapply(m$CML,function(x) x*ifelse(x>0,log(x),0)) - m$CML) )
sol=optim(X0,flin1,df=f,agem=agem,hessian=TRUE,control=list(maxit=400))
devF1=2*(sol$value+sum(sapply(f$CML,function(x) x*ifelse(x>0,log(x),0)) - f$CML) )
devF1-devF2  # city parameter is significant for females
devM1-devM2  # city parameter is significant for males
getCI<-function(sol) {
	sig=sqrt(diag(solve(sol$hessian)))
	upper=signif(sol$par+1.96*sig,5)
	lower=signif(sol$par-1.96*sig,5)
	point=signif(sol$par,5)
	exp(cbind(point,lower,upper))}
getCI(solm)[8,]  #Male Nasaki HSC reserve is ~20% of Hiroshima
getCI(solf)[8,]  #female Nasaki HSC reserve is ~30% of Hiroshima
