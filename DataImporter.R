library(stringr)
lab.dir<-"C:/MaestriaUFG/BigData/Laboratorio/Laboratorio1"
lab.datadir<-str_c(lab.dir,"/datos_pacientes")

patients.dates<-c("Dia_0", "Dia_30", "Dia_60", "Dia_90", "Dia_120", "Dia_180" , "Dia_240", "Dia_300", "Dia_360")

patients.db<-data.frame()

for(i in 1:100){
   current.file<-str_c(lab.datadir,"/datos_",i,".txt")
   print(paste("Processing: ",current.file)) 
   current.dataframe<-read.csv(file=file.path(current.file),header = FALSE,sep = "" )
   colnames(current.dataframe)[colnames(current.dataframe)=="V1"]<-"p_edate"
   colnames(current.dataframe)[colnames(current.dataframe)=="V2"]<-"p_health"
   current.dataframe["p_pdate"]<-patients.dates
   patient<-str_c("patient_",i)
   current.dataframe["patient"]<-c(patient,patient,patient,
                                   patient,patient,patient,
                                   patient,patient,patient)
   patients.db<-rbind(patients.db,current.dataframe)
 
}


table_prog_efect<-table(patients.db$p_edate,patients.db$p_pdate)
table_prog_efect
summary()


prop.table(table_prog_efect,1)
prop.table(table_prog_efect,2)
margin.table(table_prog_efect,1)
margin.table(table_prog_efect,2)

str(patients.db)


