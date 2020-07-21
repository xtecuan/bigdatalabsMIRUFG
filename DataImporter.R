library(stringr)
library(dplyr)
lab.dir<-"C:/MaestriaUFG/BigData/Laboratorio/Laboratorio1"
lab.datadir<-str_c(lab.dir,"/datos_pacientes")

patients.dates<-c(0, 30,60,90,120,180,240,300, 360)

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

punto3set <-patients.db[,c("p_edate","p_health")]

agrupadosporconsultaefectiva<- punto3set %>%
   group_by(p_edate) %>%
    summarise(Media=mean(p_health),Mediana=median(p_health), Minimo=min(p_health),Maximo=max(p_health))

plot(agrupadosporconsultaefectiva$p_edate,
     y=agrupadosporconsultaefectiva$Media,
     ylab = "Media Porcentaje de Salud",xlab = "Fecha de Consulta Efeciva",
     type="b",
     main="Gráfica de agrupación por fecha efectiva\n de consulta contra la Media del Porcentaje de Salud"
     )

punto31set <-patients.db[,c("p_pdate","p_health")]

agrupadosporconsultaprogramada<- punto31set %>%
   group_by(p_pdate) %>%
   summarise(Media=mean(p_health),Mediana=median(p_health), Minimo=min(p_health),Maximo=max(p_health))

plot(agrupadosporconsultaprogramada$p_pdate,
     y=agrupadosporconsultaprogramada$Mediana,
     ylab = "Mediana Porcentaje de Salud",xlab = "Fecha de Consulta Programada",
     type="b",
     main="Gráfica de agrupación por fecha Programada\n de consulta contra la Mediana del Porcentaje de Salud"
)


