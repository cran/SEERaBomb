getFields<-function(seerHome="~/data/SEER"){
	sas=readLines(dir(pattern="*.sas",path=file.path(seerHome,"incidence"),full.names=TRUE)[1],warn=F)
	sas=sas[-c(1:5)]
	# (sas=sas[nchar(sas)>15]) # remove trailing rows #TR they removed them
	sas=gsub("[\\@\\$\\.]","",sas)  # an OR of three characters, @, $ or .
	# sas=sub("char","",sas,ignore.case = TRUE)      # note one Char (not char) typo so ignore case 
	sas=sub("char","",sas)      #they fixed it by 2017
	sas=sub("/\\*","",sas); sas=sub("\\*/","",sas) # remove SAS comments
	sas=sub('^ +','',sas); 	sas=sub(' +$','',sas)  # remove front and back space
	# sas=sub('/$','',sas)     # remove typo of SAS comment not closed at the end #fixed!
	sas=gsub(' +', ' ',sas)  # take internal spaces of more than one to one
	sas=gsub(' ;','',sas)  # remove from new chemo row (last line)
	sas=strsplit(sas," ")
	start=as.numeric(sapply(sas,function(x) x[[1]]))
	names=sapply(sas,function(x) x[[2]])
	width=as.numeric(sapply(sas,function(x) x[[3]]))
	desc=sapply(sas,function(x) paste(x[4:length(x)],collapse=" "))
	sas=data.frame(start,width,sasnames=names,names,desc,stringsAsFactors=F)
	sas$names=tolower(gsub("_","",sas$names)); sas$names=gsub("v$","",sas$names)  # clean names a little
	# these are ones I disliked enough to change. Mostly I'm taking the SAS file names of fields.
 	# sas[which(sas$names=="datemo"),"names"]="modx" 
 	sas[which(sas$names=="mdxrecmp"),"names"]="modx" 
	sas$names[which(sas$names=="yeardx")]="yrdx" 
	# sas$names[which(sas$names=="dateyr")]="yrdx" 
	sas$names[which(sas$names=="icdoto9")]="ICD9" 
	sas$names[which(sas$names=="icdot10")]="ICD10" 
	# sas$names[which(sas$names=="icd5dig")]="COD" 
	sas$names[which(sas$names=="codpub")]="COD" 
	sas$names[which(sas$names=="age1rec")]="agerec" 
	sas$names[which(sas$names=="race1")]="race" 
	sas$names[which(sas$names=="pubcsnum")]="casenum" 
	sas$names[which(sas$names=="srvtimemon")]="surv" 
	sas$names[which(sas$names=="radiatnr")]="radiatn" 
	sas$names[which(sas$names=="chemorxrec")]="chemo" 
	# to spell out Collaborative Stage = CS, change a desc
  sas$desc[which(sas$names=="cstumsiz")]="Collaborative Stage (CS) Tumor Size"
  sas
###  uncomment and run this when field names and/or positions change
#   setwd("SEERaBomb/inst/doc")
# 	tmp=sas; colnames(tmp)[3:4]<-c("SAS","SEERaBomb");tmp
#   library(hwriter); hwrite(tmp, 'fieldNames.html', row.bgcolor='#ffdc98')#=file in doc directory
# 	sas
}

#pathPrep()
#fid=file("/Users/radivot/case/active/seer/SEERaBomb/R/getFields.R")
#ss=readLines(fid)
#iconv(ss, "latin1", "ASCII", "byte") 
