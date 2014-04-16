mkHema87<-function(AbombHome="~/data/Abomb"){
  cols87=c("city","sex","doseg","agexg","calg","kerma","upy","py","subjects",
         "age","agex","tsx","cal","sv","gam","neut",
           "lymphoma","NHL","leukemia","AML","ALL","CML","ATL","MM")      
  d<-read.table(file.path(AbombHome,"HEMA87.DAT"), header=F,col.names=cols87);
  m <- dbDriver("SQLite")
  con <- dbConnect(m, dbname = dbf<-file.path(AbombHome, "hema87.db"))
  dbWriteTable(con, "hema87", d,overwrite=TRUE)
  print(dbListTables(con)); 	
  dbDisconnect(con)
  save(d,file=fp<-file.path(AbombHome, "hema87.RData"))  
  print(file.info(dbf))
}
