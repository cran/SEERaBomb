getFields<-function(seerHome="/data/SEER"){
	sas=readLines(dir(pattern="*.sas",path=file.path(seerHome,"incidence"),full.names=TRUE)[1],warn=F)
	sas=sas[-c(1:5)]
	(sas=sas[nchar(sas)>15]) # remove trailing rows
	sas=gsub("[\\@\\$\\.]","",sas)  # an OR of three characters, @, $ or .
	sas=sub("char","",sas,ignore.case = TRUE)      # note one Char (not char) typo so ignore case
	sas=sub("/\\*","",sas); sas=sub("\\*/","",sas) # remove SAS comments
	sas=sub('^ +','',sas); 	sas=sub(' +$','',sas)  # remove front and back space
	sas=sub('/$','',sas)     # remove typo of SAS comment not closed at the end
	sas=gsub(' +', ' ',sas)  # take internal spaces of more than one to one
	sas=strsplit(sas," ")
	start=as.numeric(sapply(sas,function(x) x[[1]]))
	names=sapply(sas,function(x) x[[2]])
	width=as.numeric(sapply(sas,function(x) x[[3]]))
	desc=sapply(sas,function(x) paste(x[4:length(x)],collapse=" "))
	sas=SAS=data.frame(start,width,names,desc,stringsAsFactors=F)
	sas$names=tolower(gsub("_","",sas$names)); sas$names=gsub("v$","",sas$names)  # clean names a little
	# these are ones I disliked enough to change. Mostly I'm taking the SAS file names of fields.
	sas$names[12]="modx" ;	sas$names[13]="yrdx"
	sas$names[90]="ICD9" ;	sas$names[91]="ICD10"
	sas$names[99]="cssO3";	sas$desc[99]="Collaborative Stage (CS) Schema v0203"  # CS was not defined!
	sas$names[112]="COD" ; 	sas$desc[36]="Collaborative Stage (CS) Tumor Size"
#	tmp=cbind(SAS[,1:3],sas[,3:4]); colnames(tmp)[3:4]<-c("SAS","SEERaBomb");tmp
#    library(hwriter); hwrite(tmp, 'fieldNames.html', row.bgcolor='#ffdc98')#=file in doc directory 
	sas
}

#pathPrep()
#fid=file("/Users/radivot/case/active/seer/SEERaBomb/R/getFields.R")
#ss=readLines(fid)
#iconv(ss, "latin1", "ASCII", "byte") 
