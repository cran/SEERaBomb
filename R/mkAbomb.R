mkAbomb<-function(AbombHome="/data/Abomb"){
  colNames = c(
    "city","sex","un4gy","distg","agexg","ageg","doseg",
    "calg","upy","py","subjects","gdist","agex","age","year",
    "NHL","HL","MM","ALL","OLL","ALLtot","CLL","HCL","CLLtot","ATL",
    "AML","OML","AMoL","AMLtot","CML","othleuk","nonCLL","leuktot",
    "tot","sv","gam","neut")
  
#   cols87=c("city","sex","doseg","agexg","calg","kerma","upy","py","subjects",
#          "age","agex","tsx","cal","sv","gam","neut",
#            "lymphoma","NHL","leukemia","AML","ALL","CML","ATL","MM")      
  
  desc=c(
    "Hiroshima=1, Nagasaki=2",
    "Male=1, Female=2",
    "Under 4 Gy of Shielded Kerma Total (G+N) indicator",
    "Ground distance categories",
    "Age at exposure categories",     
    "Attained age categories",     
    "Marrow dose (gamma + 10*neutron) categories",
    "Calendar Time",	  
    "Unadjusted person years at risk",
    "Adjusted person years at risk",
    "The number of subjects first at risk in each cell",
    "Person-year weighted mean Ground distance in meters",
    "Person-year weighted mean Age at exposure in years",
    "Person-year weighted mean Attained age in years",
    "Person-year weighted mean year",
    "Non Hodgkin lymphoma",
    "Hodgkin lymphoma",
    "Myeloma",
    "Acute lymphoblastic leukemia",
    "Aleukemia/subleukemic lymphoid leukemia",
    "total ALL",
    "Chronic lymphocytic leukemia",
    "Hairy cell leukemia",
    "Total CLL",
    "Adult T-cell leukemia",
    "Acute myeloid leukemia",
    "a-/sub-leukemic myeloid leukemia, or myeloid leukemia NOS",
    "Acute monocytic leukemia",
    "Total AML",
    "Chronic myeloid leukemia",
    "Other leukemia",
    "Non-CLL/non-ATL leumemia",
    "Total leukemia",
    "All events",
    "DS02 Bone Marrow dose Sv",
    "DS02 Bone Marrow Gamma",
    "DS02 Bone Marrow Neutron")
  df=data.frame(colNames,desc)
  d=read.csv(file.path(AbombHome,"lsshempy.csv"),col.names=colNames)
  save(d,file=fp<-file.path(AbombHome, "lsshempy.RData"))  
  cat("Abomb data lsshempy was written to:",fp,"\n",sep="")
  m <- dbDriver("SQLite")
  con <- dbConnect(m, dbname = dbf<-file.path(AbombHome, "lsshempy.db"))
  dbWriteTable(con, "hempy", d,overwrite=TRUE)
  print(dbListTables(con)); 	
  dbDisconnect(con)
  print(file.info(dbf))
  df
}
