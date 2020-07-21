library(stringr)
RootFolder<-"C:/MaestriaUFG/BigData/Laboratorio/Laboratorio1/datos_pacientes"
id_rev<-c(1, 2, 3, 4, 5, 6 , 7, 8, 9)
myDB<-data.frame()
ProcesarArchivo<-function(NumeroArchivo){
  print(paste("Procesando Archivo :",NumeroArchivo))
  ArchivoAProcesar<-str_c(RootFolder,"/datos_",NumeroArchivo,".txt")
  print(ArchivoAProcesar)
  myDataFrame<-read.csv(file=file.path(ArchivoAProcesar),header = FALSE,sep = "" )
  colnames(myDataFrame)[colnames(myDataFrame)=="V1"]<-"dia"
  colnames(myDataFrame)[colnames(myDataFrame)=="V2"]<-"porc"
  myDataFrame["idrev"]<-id_rev
  patient<-str_c("idpax_",NumeroArchivo)
  myDataFrame["idpax"]<-c(patient,patient,patient,patient,patient,patient,patient,patient,patient)
  print(myDataFrame)
  return(myDataFrame)
  
}

for(i in 1:100){
  myDB<-rbind(myDB,ProcesarArchivo(i))
}

