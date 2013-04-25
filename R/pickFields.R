pickFields<-function(sas,picks=c("casenum","reg","race","sex","agedx","yrbrth",
                                 "seqnum","yrdx","histo2","histo3","radiatn",
                                 "agerec","ICD9","histrec","numprims","COD","surv") ){
  subsas=sas 
  rownames(subsas)<-subsas$names
  subsas=subsas[picks,]
  N=length(picks)
  # db file size increases if the following is set to strings instead of integers
  subsas=cbind(subsas,type="integer",stringsAsFactors=FALSE)  
  # the following appear to be the only columns with letters in their names
  if("siteo2" %in% subsas$names) subsas["siteo2","type"]="string"
  if("ICD10" %in% subsas$names) subsas["ICD10","type"]="string"
  if("eod13" %in% subsas$names) subsas["eod13","type"]="string"
  if("eod2" %in% subsas$names) subsas["eod2","type"]="string"
  if (picks[1]=="casenum") outdf=subsas[1,,drop=F] else  
    outdf=data.frame(start=1,width=subsas$start[1]-1,names=" ",desc=" ",type="string")
  for (i in 2:N) 
    if (subsas$start[i]==(up<-subsas$start[i-1]+subsas$width[i-1]) ) 
      outdf=rbind(outdf,subsas[i,]) else {
        outdf=rbind(outdf,data.frame(start=up,width=(subsas$start[i]-up),names=" ",desc=" ",type="string")) 
        outdf=rbind(outdf,subsas[i,])
      }
  if ((up<-subsas$start[i]+subsas$width[i])<332)
    outdf=rbind(outdf,data.frame(start=up,width=(332-up),names=" ",desc=" ",type="string")) 
  outdf$type=as.character(outdf$type)
  outdf	 
}  # pickFields(df)