###Librerias Usadas para el script
library(stringr)
library(dplyr)
library(ggplot2)
library(reshape)

###Inicio carga de datos 100 archivos TXT con los datos de los pacientes

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
### Se genera un data.frame llamado patients.db con los 900 registros de los pacientes
### names(patients.db)
### El data frame tiene las columnas "p_edate"  "p_health" "p_pdate"  "patient" 
###Fin carga de datos 100 archivos TXT con los datos de los pacientes


### Inicio Set de datos, agrupamiento y grafica para Punto 3
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
### Fin Set de datos,agrupamiento y grafica para Punto 3

### Inicio Set de datos, agrupamiento y grafica para Punto 3.1
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
### Fin Set de datos, agrupamiento y grafica para Punto 3.1

### Inicio Funcion para generar grafica para cada paciente en formato jpeg
crearImagenPaciente<-function(cname,cdata){
  
  
  dev.print(jpeg, file = file.path(str_c(dirimagenes,cname,".jpg")), width = 766, height = 485)
  png(file = file.path(str_c(dirimagenes,cname,".jpg")), bg = "transparent")
  plot(cdata$p_edate,
       y=cdata$p_health,
       ylab = paste("Porcentaje de Salud ",cname),xlab = paste("Fecha de Consulta Efeciva ",cname),
       type="b",
       main=paste("Gráfica de fecha efectiva\n de consulta contra el Porcentaje de Salud ",cname)
  )
  dev.off()
  
}
### Fin Funcion para generar grafica para cada paciente en formato jpeg

###Inicio Proceso batch para generar las imagenes de graficas de los pacientes
dirimagenes<-"C:/MaestriaUFG/BigData/Laboratorio/Laboratorio1/imagenes_tendencia_pacientes/"
for(i in 1:100){
  cname<-str_c("patient_",i)
  print(cname)
  cdata<-patients.db[patients.db$patient==cname,]
  print(cdata)
  crearImagenPaciente(cname,cdata)
}
###Fin Proceso batch para generar las imagenes de graficas de los pacientes


###Inicio Proceso de seleccion de 10 pacientes y elaboracion de su grafica de series
getHealth<-function(id){
  return(patients.db[patients.db$patient==str_c('patient_',id),]$p_health)
}

lasfechas<-patients.db[patients.db$patient=='patient_1',]$p_pdate


data <- data.frame(pdate = lasfechas,
                   patient_1=getHealth(1),
                   patient_10=getHealth(10),
                   patient_20=getHealth(20),
                   patient_30=getHealth(30),
                   patient_40=getHealth(40),
                   patient_50=getHealth(50),
                   patient_60=getHealth(60),
                   patient_70=getHealth(70),
                   patient_80=getHealth(80),
                   patient_90=getHealth(90),
                   patient_100=getHealth(100))
Molten <- melt(data, id.vars = "pdate")
ggplot(Molten, aes(x = pdate, y = value, colour = variable)) + geom_line()
###Fin Proceso de seleccion de 10 pacientes y elaboracion de su grafica de series
