mkSEER<-function(df,seerHome="/data/SEER",dataset=c("yr2000_2009","yr1973_2009","yr1992_2009"), SQL = FALSE){
#	if (!require(LaF)) print("R Package LaF must be installed")
	colTypes=c("integer","string",rep("integer",5),"double")    # double, integer, categorical and string
	colWidths=c(4,7,2,1,1,1,2, 10) 
	colNames = c('popyear','X0','popseer','poprace','origin','popsex', 'popage','population')
	dataset=match.arg(dataset)
	f=switch(dataset,
			yr2000_2009 = file.path(seerHome,"populations/expanded.race.by.hispanic/yr2000_2009.ca_ky_lo_nj_ga/19agegroups.txt"),
			yr1973_2009 = file.path(seerHome,"populations/white_black_other/yr1973_2009.seer9/19agegroups.txt"),
			yr1992_2009 = file.path(seerHome,"populations/expanded.race.by.hispanic/yr1992_2009.seer9.plus.sj_la_rg_ak/19agegroups.txt"))
	ptm <- proc.time()
	laf<-laf_open_fwf(f,column_types=colTypes,column_widths=colWidths,column_names = colNames)
	pops<-laf[,colNames[-c(2,5)]]
	dir.create(file.path(seerHome, dataset),showWarnings = FALSE)
	save(pops,file=fp<-file.path(seerHome, dataset,"pops.RData"))  
	delT=proc.time() - ptm  
	cat("The grouped ages population file of SEER dataset ",dataset," was successfully written to:\n ",fp," in ",delT[3]," seconds.\n",sep="")
	if (SQL) {m <- dbDriver("SQLite")
		con <- dbConnect(m, dbname = dbf<-file.path(seerHome, dataset,"all.db"))
		dbWriteTable(con, "pops", pops,overwrite=TRUE)
	}	
	cancers=c('breast','digothr','malegen','femgen','other','respir','colrect','lymyleuk','urinary','test') 
	p=switch(dataset,
			yr2000_2009 = file.path(seerHome,"incidence/yr2000_2009.ca_ky_lo_nj_ga/"),
			yr1973_2009 = file.path(seerHome,"incidence/yr1973_2009.seer9/"),
			yr1992_2009 = file.path(seerHome,"incidence/yr1992_2009.sj_la_rg_ak/"))
	ptm <- proc.time()
	y=df[which(df$names!=" "),"names"]; cat("The following fields will be written:");	print(y)
	for (k in 1:9)	{	
		laf<-laf_open_fwf(paste(p,toupper(cancers[k]),'.TXT',sep=""), 
				column_types=df$type,    # double, integer, categorical and string
				column_widths=df$width)
		DF=laf[,which(df$names!=" ")]
		colnames(DF)<-y
#		DF=transform(DF,surv=12*srvy+srvm) # get rid of package check note "no visible binding"
		DF$surv=12*DF$srvy+DF$srvm
		if (SQL) dbWriteTable(con, cancers[k], DF,overwrite=TRUE)
		save(DF,file=fp<-paste(seerHome,"/", dataset,"/",cancers[k],'.RData',sep=""))
		cat("The following file was just written:",fp,"\n",sep="")
	}
	delT=proc.time() - ptm  
	cat("Cancer files of SEER ",dataset," were written to ",file.path(seerHome, dataset)," in ",delT[3]," seconds.\n")
	if (SQL) {print(dbListTables(con)); 	
	dbDisconnect(con)
	print(file.info(dbf)) }
}
