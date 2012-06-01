pickFields<-function(sas,picks=c("casenum","reg","race","sex","agedx","yrbrth",
				"seqnum","yrdx","histo2","histo3","radiatn",
				"agerec","ICD9","histrec","cssO3","numprims","survtm","COD") ){
	subsas=sas
	rownames(subsas)<-subsas$names
	subsas=subsas[picks,]
	N=length(picks)
	if ("survtm" %in% picks) {
		srvi=which(subsas$names=="survtm")
		if (srvi>1) top=subsas[1:(srvi-1),] else top=NULL
		if (srvi<N) bottom=subsas[(srvi+1):N,] else bottom=NULL
		middle=rbind(subsas[srvi,],subsas[srvi,])
		middle$width=c(2,2)
		middle$start[2]=middle$start[1]+2
		middle$names=c("srvy","srvm")
		rownames(middle)=c("srvy","srvm")
		subsas=rbind(top,middle,bottom)
		N=N+1
	}
	subsas=cbind(subsas,type="integer",stringsAsFactors=FALSE)  # db file size increases if this is set to strings
	# the following appear to be the only two columns with letters in their names
	if("siteo2" %in% subsas$names) subsas["siteo2","type"]="string"
	if("ICD10" %in% subsas$names) subsas["ICD10","type"]="string"
	if (picks[1]=="casenum") outdf=subsas[1,,drop=F] else  
		outdf=data.frame(start=1,width=subsas$start[1]-1,names=" ",desc=" ",type="string")
	for (i in 2:N) 
	  if (subsas$start[i]==(up<-subsas$start[i-1]+subsas$width[i-1]) ) 
		  outdf=rbind(outdf,subsas[i,]) else {
		  outdf=rbind(outdf,data.frame(start=up,width=(subsas$start[i]-up),names=" ",desc=" ",type="string")) 
		  outdf=rbind(outdf,subsas[i,])
	  }
    if ((up<-subsas$start[i]+subsas$width[i])<301)
		outdf=rbind(outdf,data.frame(start=up,width=(301-up),names=" ",desc=" ",type="string")) 
	outdf$type=as.character(outdf$type)
 	outdf	 
}
