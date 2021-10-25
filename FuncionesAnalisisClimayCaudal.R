#'/////////////////////////////////////////////////////////////////////////////
#' FILE: "Analisis Series de Clima y Caudal"
#' AUTHOR: Angelica Moncada, SEI Latinoamerica
#' CREATED: 2017
#' MODIFIED: 2021
#' STATUS: working
#' PURPOSE:
#' an?lisis exploratorio de los datos con el fin de determinar cambios y/o tendencias en la serie hidroclimatol?gica
#' verificar el supuesto de estacionalidad, consistencia y homogeneidad, sin dejar aparte, en algunos casos, la independencia entre \
#' las observaciones utilizando m?todos gr?ficos 
#' Los m?todos gr?ficos, como herramienta exploratoria de los datos, se usan con dos prop?sitos: revelar las caracter?sticas de una 
#' posible distribuci?n o las relaciones que existen entre las variables, que de otra manera no podr?an ser descubiertas (Maidment, 1993). 
#'/////////////////////////////////////////////////////////////////////////////
library(raster)
library(hydroTSM)
library(hydroGOF) 
library(lattice)
library(ggplot2) #Este paquete sirve para realizar gr?ficos m?s elaborados
library(grid) #para crear divisi?n de graficos ggplot2
library(car) #para recodificar variables
library(reshape) #para modificar base de datos para graficos ggplot2
library(zoo) #para convertir datos en formato series de tiempo
library(sp)
library(gtools) #paquete para permutacion y combinacion
library(mgcv) #paquete para analisis ENSO, stat_smooth(method = "gam", formula = y ~ s(x),...)
library(geosphere) #paquete para calculo de distancias entre coordenadas geograficas
library(lfstat)
library(GGally)
library(lubridate)
library(scales)
library(rgdal)
library(ggmap)
library(ggrepel)
library(ggcorrplot)
library(dplyr) #orden de data frames
library(tidyr)
library(tibbletime)
library(cowplot)
library(egg)
estadisticas = function(x){ #calcula las estadisticas descriptivas de un vector
  
  library(e1071)
  Varianza=var(x,na.rm=TRUE) 
  Desv.Est = sd(as.numeric(x),na.rm=TRUE)
  iqr=IQR(x,na.rm=TRUE)
  As=skewness(x,na.rm=TRUE)
  if (length(na.exclude(x))>=4){k=kurtosis(x,na.rm=TRUE, type = 2)}else {k=NA}
  Min = min(x,na.rm=TRUE)
  Qu_1st = quantile(x,.25,na.rm=TRUE)
  Median = quantile(x,.50,na.rm=TRUE)
  Mean = mean(x,na.rm=TRUE) 
  Qu_3rd= quantile(x,.75,na.rm=TRUE)
  Max  = max(x,na.rm=TRUE)
  Coef_Var=Desv.Est/Mean*100
  n=length(x)
  x_NA=length(which(is.na(x)))
  x_porcentajeNA =x_NA/n*100
  x_summary = cbind(Varianza,Desv.Est,Coef_Var,iqr,As,k,Min,Qu_1st,Median,Mean,Qu_3rd,Max,n,x_NA,x_porcentajeNA)
  x_summary=round(as.numeric(x_summary),2)
}
which.nonnum = function(x) {
  data = is.na(suppressWarnings(as.numeric(as.character(x))));which(data & !is.na(x))}
Calidad=function (DatesVentana,DataVentana) {   
  
  #Creates table with "SD","Min", "Qu 1st","Median","Mean","Qu 3rd ","Max","NA's" for each station
  matrix = as.data.frame(matrix(NA, nrow=length(stations),ncol=17))
  colnames(matrix) = c("Station","Periodo","Varianza","Desv Est","CV","IQR","Asimetria","Curtosis","Min", "Qu 1st","Median","Mean","Qu 3rd ","Max","n","NAs","%NAs Periodo")
  matrix[,1] = as.character(stations)
  matrix[,2] = PeriodoRegistro
  
  for (s in 1:ncol(DataVentana)){matrix[s,3:ncol(matrix)] = estadisticas(DataVentana[,s])}
  
  Data_Calidad=cbind(DatesVentana,DataVentana)
  colnames(Data_Calidad)=c("Dates",stations)
  
  dat.m = reshape2::melt(Data_Calidad,id.vars="Dates", measure.vars=as.character(colnames(DataVentana))) #convierte el archivo en un archivo vertical donde una columna es la estacion 
  names(dat.m)=c("Date","Estacion","Valores")
  dat.m$Date=rep(DatesVentana)
  
  #str(dat.m) 
  dat.m=na.exclude(dat.m)
  dat.m$Type=rep("NA")
  dat.m$Type2=rep("NA")
  
  Data_Calidad=rep(NA,5)
  names(Data_Calidad)=c("Date","Estacion","Valores","Type","Type2")
  #no numericos
  Control=dat.m[which.nonnum(dat.m$Valores),]
  ControlCalidad="No Numerico"
  if (nrow(Control)>0) {
    Control$Type=rep("No Numerico")
    Control$Type2=Control$Type; Data_Calidad=rbind(Data_Calidad,Control)
  }
  
  
  #negativos
  if (IncluirControlNegativos[clima[pos]]==TRUE) {
    Control=dat.m[which(as.numeric(dat.m$Valores)<0),]
    ControlCalidad=c(ControlCalidad,
                     "Negativo")
    if (nrow(Control)>0) {
      Control$Type="Negativo";Control$Type2=Control$Type;Data_Calidad=rbind(Data_Calidad,Control)
    }
  }
  
  #limite superior e inferior 
  if (!is.na(LimiteInferior[clima[pos]])) {
    Control=dat.m[which(as.numeric(dat.m$Valores)<LimiteInferior[clima[pos]]),]
    ControlCalidad=c(ControlCalidad,
                     paste(c("Limite Inferior (",LimiteInferior[clima[pos]],")"),collapse=""))
    if (nrow(Control)>0) {
      Control$Type=paste(c("Limite Inferior (",LimiteInferior[clima[pos]],")"),collapse="")
      Control$Type2=Control$Type;Data_Calidad=rbind(Data_Calidad,Control)
    }
  }
  
  if (!is.na(LimiteSuperior[clima[pos]])) {
    Control=dat.m[which(as.numeric(dat.m$Valores)>LimiteSuperior[clima[pos]]),]
    ControlCalidad=c(ControlCalidad,
                     paste(c("Limite Superior (",LimiteSuperior[clima[pos]],")"),collapse=""))
    
    if (nrow(Control)>0) {
      Control$Type=paste(c("Limite Superior (",LimiteSuperior[clima[pos]],")"),collapse="")
      Control$Type2=Control$Type;Data_Calidad=rbind(Data_Calidad,Control)
    }
  }
  
  if (!is.null(nrow(Data_Calidad))){
    Data_Calidad=Data_Calidad[order(Data_Calidad$Estacion,Data_Calidad$Date),]   
    
    if(require("lubridate")==FALSE){install.packages("lubridate", dep = T)} 
    library(lubridate)
    
    Data_Calidad=cbind(year(ymd(Data_Calidad[,1])),month(ymd(Data_Calidad[,1])),day(ymd(Data_Calidad[,1])),Data_Calidad)
    colnames(Data_Calidad)=c(colnames(YearMonthDay_Diaria),"Date","Estacion","Valores","Type","Type2")
    
    x=Data_Calidad
    x$check=rep(NA)
    x$check2=rep(NA)
    for (i in 1:nrow(x)){
      x[i,9]=paste(c(as.character(x[i,4]),"-",as.character(x[i,5])),collapse="")
      x[i,10]=paste(c(as.character(x[i,5]),"-",as.character(x[i,8])),collapse="")
    }
    
    #length(unique(x$check))
    x = x[!duplicated(x$check),]
    x = na.exclude(x)
    #nrow(x) # for checking , this should be equal to 2nd line output
    
    Data_Calidad=x
    Data_Calidad[,c(4,8,9,10)]=NULL
    
    #############
    names_SummaryCalidad=c(colnames(matrix[,c(1,2,15,17)]),ControlCalidad)
    SummaryCalidad=cbind(matrix[,c(1,2,15,17)], as.data.frame(matrix(NA,nrow=nrow(matrix),ncol=length(ControlCalidad))))
    colnames(SummaryCalidad)=names_SummaryCalidad
    SummaryCalidad$`%Total`=rep(NA)
    sumCol = function(x,c) {x=x[4:c];sum(x)}
    
    for (i in 1:nrow(SummaryCalidad)){
      for (j in 5:(length(as.vector(ControlCalidad))+4)){
        c=j-4
        check=paste(c(as.character(SummaryCalidad[i,1]),"-",as.character(ControlCalidad[c])),collapse="")
        SummaryCalidad[i,j]=round(nrow(x[which(x$check2==check),])/SummaryCalidad[i,3]*100,2)
      }
      SummaryCalidad[i,(length(as.vector(ControlCalidad))+5)]=sumCol(SummaryCalidad[i,],(length(as.vector(ControlCalidad))+4))
    }
    
    #segun el resultado de control de calidad, todos los valores identificados se reemplazan por NA
    Reemplazar=Data_Calidad
    for (i in 1:nrow(Reemplazar)){
      f=which(DatesVentana==as.Date(paste(c(Reemplazar[i,1],"/",Reemplazar[i,2],"/",Reemplazar[i,3]),collapse="")))
      c=which(names(DataVentana)==Reemplazar[i,4])
      DataVentana[f,c]=NA
    }
    
    for (i in 1:ncol(DataVentana)){
      DataVentana[,i]=as.numeric(DataVentana[,i])
    }
    
    DataWrite=cbind(YearMonthDay_Diaria,DataVentana)
    colnames(DataWrite)=c("Year","Month","Day",stations)
    plotpath = paste(c(dir_out,"\\",N_ControlCalidad,"_",type_fileComplete,"_conNA_ControlCalidad_",PeriodoRegistro,".csv"),collapse="")
    write.csv(DataWrite,plotpath, row.names=FALSE,na="")
    assign("DataVentana",DataVentana,.GlobalEnv)
    
  } else {
    Data_Calidad=as.data.frame(matrix(NA,nrow=1,ncol=6))
    colnames(Data_Calidad)=c("Year",     "Month",    "Day",      "Estacion", "Valores",  "Type"  )
    
    names_SummaryCalidad=c(colnames(matrix[,c(1,2,15,17)]),ControlCalidad)
    SummaryCalidad=cbind(matrix[,c(1,2,15,17)], as.data.frame(matrix(NA,nrow=nrow(matrix),ncol=length(ControlCalidad))))
    colnames(SummaryCalidad)=names_SummaryCalidad
    SummaryCalidad$`%Total`=SummaryCalidad$`%NAs`
    
  }
  
  assign("Data_Calidad",Data_Calidad,.GlobalEnv)
  
  plotpath = paste(c(dir_out,"\\",N_ControlCalidad,"_","ControlCalidad_",PeriodoRegistro,".csv"),collapse="")
  write.csv(Data_Calidad,plotpath, row.names=FALSE,na="")
  
  assign("SummaryCalidad",SummaryCalidad,.GlobalEnv)
  
  plotpath = paste(c(dir_out,"\\",N_ControlCalidad,"_","ControlCalidad_Summary_",PeriodoRegistro,".csv"),collapse="")
  write.csv(SummaryCalidad[order(SummaryCalidad$`%Total`),],plotpath, row.names=FALSE,na="")
  
  #grafica control de calidad
  #obtener los datos a graficar
  x=SummaryCalidad[order(SummaryCalidad$`%Total`),]
  dat.m = reshape2::melt(x,id.vars="Station", measure.vars=as.character(colnames(x))[4:ncol(x)]) #convierte el archivo en un archivo vertical donde una columna es la estacion 
  colnames(dat.m)=c("Estacion","CC","Valores")
  dat.m$Valores=as.numeric(dat.m$Valores)
  #asignar los niveles, estaciones en orden alfabetico
  dat.m[,1] =factor(dat.m[,1], levels=as.character(x[,1]))
  
  #grafico de barras de control de calidad de todas las estaciones
  plotpath = paste(c(dir_out,"\\",N_ControlCalidad,"_","ControlCalidad_Summary_",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_bar(stat = "identity",aes(fill=Estacion),position="dodge")+
    ylab("Porcentaje (%)")+
    xlab("Estaciones")+
    ggtitle(paste(c("Control de Calidad ",type_fileComplete),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
    facet_grid(CC~., scales="free")+
    theme(legend.position="none")
    #geom_text(aes(label=Valores), position=position_dodge(width=0.9), vjust=--0.3,size=3,colour="black")
    ggsave(plotpath,width =40 , height = 22,units = "cm")
}
Data_a_MensualyAnual=function (DataVentana){
  #calculo agregacion de la variable en mensual y anual 
  nyears=year(ymd(date2))-year(ymd(date1))+1
  
  DataVentanaMensual=as.data.frame(matrix(NA, nrow=nyears*12, ncol=ncol(DataVentana)))
  colnames(DataVentanaMensual)=colnames(DataVentana)
  
  DataVentanaAnual=as.data.frame(matrix(NA, nrow=nyears, ncol=ncol(DataVentana)))
  colnames(DataVentanaAnual)=colnames(DataVentana)
  
  for (s in 1:ncol(DataVentana)){
    Data_zoo=zoo(as.numeric(DataVentana[,s]),DatesVentana)
    DataVentanaMensual[,s] =coredata(daily2monthly(Data_zoo, FUN=as.character(Fun_file[clima[pos]]),na.rm=FALSE))
    DataVentanaAnual[,s] =coredata(daily2annual(Data_zoo, FUN=as.character(Fun_file[clima[pos]]),na.rm=FALSE))
    #dates = time(zoo)
    #values=coredata(zoo)
  }
  
  assign("DataVentanaMensual",DataVentanaMensual,.GlobalEnv)
  assign("DataVentanaAnual",DataVentanaAnual,.GlobalEnv)
  
  #para graficar en ggplot, convierte data mensual en vertical 
  x=cbind(YearMonthDay_Mensual,DataVentanaMensual)
  dat.m = reshape2::melt(x,id.vars="Year", measure.vars=as.character(colnames(DataVentana))) #convierte el archivo en un archivo vertical donde una columna es la estacion 
  colnames(dat.m)=c("Year","Estacion","Valores")
  dat.m$Valores=as.numeric(dat.m$Valores)
  dat.m$Month=rep(1:12)
  dat.m$Day=rep(0)
  #asignar los niveles, estaciones en orden alfabetico
  dat.m[,2] =factor(dat.m[,2], levels=stations)
  dat.m = dat.m[,c(1,4,5,2,3)]
  DataVentanaMensual_v=dat.m
  assign("DataVentanaMensual_v",DataVentanaMensual_v,.GlobalEnv)
  
  #para graficar en ggplot, convierte data anual en vertical 
  x=cbind(YearMonthDay_Anual,DataVentanaAnual)
  dat.m = reshape2::melt(x,id.vars="Year", measure.vars=as.character(colnames(DataVentana))) #convierte el archivo en un archivo vertical donde una columna es la estacion 
  colnames(dat.m)=c("Year","Estacion","Valores")
  dat.m$Month=rep(0)
  dat.m$Day=rep(0)
  dat.m$Valores=as.numeric(dat.m$Valores)
  #asignar los niveles, estaciones en orden alfabetico
  dat.m[,2] =factor(dat.m[,2], levels=stations)
  dat.m = dat.m[,c(1,4,5,2,3)]
  DataVentanaAnual_v=dat.m
  assign("DataVentanaAnual_v",DataVentanaAnual_v,.GlobalEnv)
}
Data_a_Anual=function (DataVentanaMensual){
  #calculo agregacion de la variable en mensual y anual 
  nyears=year(ymd(date2))-year(ymd(date1))+1
  
  DataVentanaAnual=as.data.frame(matrix(NA, nrow=nyears, ncol=ncol(DataVentanaMensual)))
  colnames(DataVentanaAnual)=colnames(DataVentanaMensual)
  
  for (s in 1:ncol(DataVentanaMensual)){
    Data_zoo=zoo(as.numeric(DataVentanaMensual[,s]),DatesM)
    DataVentanaAnual[,s] =coredata(monthly2annual(Data_zoo, FUN=as.character(Fun_file[clima[pos]]),na.rm=FALSE))
    #dates = time(zoo)
    #values=coredata(zoo)
  }
  
  assign("DataVentanaAnual",DataVentanaAnual,.GlobalEnv)
  
  #para graficar en ggplot, convierte data anual en vertical 
  x=cbind(YearMonthDay_Anual,DataVentanaAnual)
  dat.m = reshape2::melt(x,id.vars="Year", measure.vars=as.character(colnames(DataVentanaMensual))) #convierte el archivo en un archivo vertical donde una columna es la estacion 
  colnames(dat.m)=c("Year","Estacion","Valores")
  dat.m$Month=rep(0)
  dat.m$Day=rep(0)
  dat.m$Valores=as.numeric(dat.m$Valores)
  #asignar los niveles, estaciones en orden alfabetico
  dat.m[,2] =factor(dat.m[,2], levels=stations)
  dat.m = dat.m[,c(1,4,5,2,3)]
  DataVentanaAnual_v=dat.m
  assign("DataVentanaAnual_v",DataVentanaAnual_v,.GlobalEnv)
}
base_breaks = function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}
Valores_ENSO=function (date1, date2) {
  #if(require(XML)==FALSE){ install.packages("XML", dep = T)}
  #require(XML) #para descargar tablas de paginas de internet
  #readHTMLTable("http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml")
  
  ENSO=read.table(filenameENSO, header = TRUE, check.names = F, stringsAsFactors = F)
  
  #  http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_change.shtml
  #ENSO=read.table("http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt",header = TRUE)
  ENSO[,3:4]=NULL
  
  ENSO$COD=NA
  ENSO$SUM=NA
  ENSO$ENSO_COD=NA
  # 1 Ni?a, 2 Normal, 3 Ni?o
  
  
  # 1 en columna ENSO_COD cuando el valor es menor o igual que -0.5
  for (i in 1:nrow(ENSO)) {if (ENSO[i,3]<=-0.5){ENSO[i,4]=1}else{ENSO[i,4]=0}}
  
  ENSO[1,5]=ENSO[1,4]
  for (i in 2:nrow(ENSO)) {if (ENSO[i,4]!=0) {ENSO[i,5]=ENSO[i,4]+ENSO[i-1,5]} else {ENSO[i,5]=0}}
  
  count=0
  for (i in 1:nrow(ENSO)) {
    if (ENSO[i,5]!=0) {
      count=count+1
      if (count>=5) {
        ENSO[(i:(i-count+1)),6]=1
      }
    } else { 
      count=0
    }
  }
  
  # 3 en columna ENSO_COD cuando el valor es mayor o igual que 0.5
  for (i in 1:nrow(ENSO)) {if (ENSO[i,3]>=0.5){ENSO[i,4]=1}else{ENSO[i,4]=0}}
  
  ENSO[1,5]=ENSO[1,4]
  for (i in 2:nrow(ENSO)) {if (ENSO[i,4]!=0) {ENSO[i,5]=ENSO[i,4]+ENSO[i-1,5]} else {ENSO[i,5]=0}}
  
  #ENSO$ENSO_COD1=ENSO$ENSO_COD
  count=0
  for (i in 1:nrow(ENSO)) {
    if (ENSO[i,5]!=0) {
      count=count+1
      if (count>=5) {
        ENSO[(i:(i-count+1)),6]=3
      }
    } else { 
      count=0
    }
  }
  
  for (i in 1:nrow(ENSO)) {
    if (ENSO[i,5]!=0) {count=count+1} 
    if (ENSO[i,5]==0) {if (count>=5) {ENSO[(i-1):(i-count),6]=3; count=0} else {count=0}}
  }
  
  ENSO[which(is.na(ENSO$ENSO_COD)),6]=2
  ENSO$ENSO=NA
  ENSO[which(ENSO$ENSO_COD==1),7]="Ni?a"
  ENSO[which(ENSO$ENSO_COD==2),7]="Normal"
  ENSO[which(ENSO$ENSO_COD==3),7]="Ni?o"
  
  ENSO$COD=NULL
  ENSO$SUM=NULL
  
  date1ENSO = paste(c(ENSO[1,1],"/",ENSO[1,2],"/",1),collapse="") # create date object of first row of data
  date2ENSO = paste(c(ENSO[nrow(ENSO),1],"/",ENSO[nrow(ENSO),2],"/",1),collapse="") # create date object of last row of data
  ENSO$Date=seq(as.Date(date1ENSO),as.Date(date2ENSO),"month") # create date sequence for entire series
  ENSO=ENSO[,c(6,1:5)]
  colnames(ENSO)=c( "Date","Year","Month","ANOM","ENSO_COD","ENSO")
  
  date2DataVentana=paste(c(year(ymd(date2)),"/",month(ymd(date2)),"/",1),collapse="")
  
  ENSOVentana=ENSO[which(ENSO$Date == date1):which(ENSO$Date == date2DataVentana),]
  assign("ENSOVentana",ENSOVentana,.GlobalEnv)
}
G_ENSO_AnomMensual=function(dir){
  
  Valores_ENSO(date1, date2)
  
  ENSO=ENSOVentana
  ENSO[,1]=NULL
  
  Data=cbind(YearMonthDay_Mensual,DataVentanaMensual)
  station=names(DataVentanaMensual)
  
  prom=aggregate(Data,list(Month=Data$Month),mean, na.rm=TRUE)
  desv=aggregate(Data,list(Month=Data$Month),sd, na.rm=TRUE)
  prom[,2:4]=NULL
  desv[,2:4]=NULL
  
  setwd(dir)
  dir.create("Graficos_anomalias",showWarnings=F)
  
  Data=DataVentanaMensual
  Anom=as.data.frame(matrix(0,nrow(Data),ncol(Data)))
  colnames(Anom)=colnames(Data)
  
  for(i in 1:ncol(Data)){  
    if (as.character(Fun_file[clima[pos]])=="mean"){Anom[,i]=Data[,i]-prom[,i+1]}
    if (as.character(Fun_file[clima[pos]])=="sum") {Anom[,i]=(Data[,i]-prom[,i+1])/as.numeric(unlist(rep(desv[i+1],nyears)))}
  }
  
  plotpath = paste(c(dir,"\\","Anomalias_",PeriodoRegistro,".csv"),collapse="")
  write.csv(cbind(YearMonthDay_Mensual,Anom),plotpath, row.names=FALSE,na="")
  
  DataAnom=cbind(Anom,DatesM)
  DataENSO=cbind(DatesM,ENSOVentana)
  DataENSO$ENSO=factor(DataENSO$ENSO,levels=c("Ni?o","Normal","Ni?a"))
  for (j in 1:ncol(Data)) {
    plotpath=paste(c(dir,"\\Graficos_anomalias\\",station[j],".jpg"),collapse="")
    ggplot(DataENSO, aes(x=DatesM, y=ANOM)) +  
      geom_bar(stat = "identity",   aes(fill = ENSO),alpha=0.5)+
      theme(legend.title =element_text(color="white") ,legend.position="right")+ 
      ylab(paste(c("ONI \nAnomalias ",clima.var2[clima[pos]]),collapse=""))+
      scale_fill_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ", breaks=c("Ni?o","Normal","Ni?a"),labels=c("Ni?o","Normal","Ni?a"))+
      geom_line(data=DataAnom,aes(x=DatesM,y=DataAnom[,j],colour=paste(c("Anomalias ",clima.var2[clima[pos]]),collapse="")),size=0.5)+
      scale_color_manual(values=c("black"))+
      scale_x_date(date_breaks= "1 year",date_minor_breaks = "1 month",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
      theme(axis.text.x=element_text(size=8, angle=90,hjust=0.95,vjust=0.2))+
      ggtitle(paste(c("Anomalias ",clima.var2[clima[pos]]," Mensual ",station[j]),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))+
      xlab("Fecha")
    ggsave(plotpath,width =40 , height = 22,units = "cm")
  }
}
G_ENSO_PromMensual=function(dir){
  
  Valores_ENSO(date1, date2)
  
  ENSO=ENSOVentana
  ENSO[,1]=NULL
  
  Data=DataVentanaMensual
  station=names(Data)
  
  DataENSO=cbind(Data,ENSO,DatesM)
  
  setwd(dir)
  dir.create("Graficos_plot",showWarnings=F)
  
  type_fileComplete2=paste(c(clima.var2[clima[pos]]," Mensual"),collapse = "")
  type_fileComplete3=paste(c(clima.var2[clima[pos]],"_Mensual"),collapse = "")
  DataENSO$ENSO=factor(DataENSO$ENSO,levels=c("Ni?o","Normal","Ni?a"))
  DataENSO$ENSO_COD=factor(DataENSO$ENSO_COD,levels=as.character(c(3,2,1)))
  
  DataENSO1=DataENSO
  DataENSO1[,((ncol(DataENSO)-1):ncol(DataENSO))]=NULL
  DataENSO1$Month=as.numeric(as.character(DataENSO$Month))
  DataENSO1$ENSO_COD=as.numeric(as.character(DataENSO$ENSO_COD))
  DataENSO1$ENSO_Month=DataENSO1$ENSO_COD*100+DataENSO1$Month
  min=aggregate(DataENSO1,list(ENSO_Month=DataENSO1$ENSO_Month),min, na.rm=TRUE)
  min=round(rbind(min[25:36,],min[13:24,],min[1:12,]),2)
  
  data_gAll=as.data.frame(matrix(NA,nrow=1,ncol=6))
  colnames(data_gAll)=c("Month",type_fileComplete3, "ENSO","Normal","p", "Estacion" )
  data_gAll2=data_gAll
  
  for (j in 1:ncol(Data)) {    
    
    g=ggplot(DataENSO,aes(x=Month,y=DataENSO[,j],colour=ENSO))+
      geom_point(alpha=0.5,size=0.5)+
      stat_smooth(method = "gam", formula = y~s(x),fullrange=T,size=1,se=FALSE)+
      xlab("Meses")+
      ylab(type_fileComplete2)+
      scale_x_continuous(breaks=1:12, labels=namesMonth)+
      theme(axis.text.x=element_text(vjust=0.5, size=10))+
      scale_colour_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("Ni?o","Normal","Ni?a"),labels=c("Ni?o","Normal","Ni?a"))+
      ggtitle(paste(c(type_fileComplete2," ","Promedio ENSO ",station[j]),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    
    if (nrow(ggplot_build(g)$data[[2]])>0){
      p=round(seq(3,80,by=7),0)
      p1=c(p,p+80,p+160)
      y=round(ggplot_build(g)$data[[2]][p1,3],2)
      data_g=as.data.frame(matrix(NA,ncol=3,nrow=36))
      colnames(data_g)=c("Month",type_fileComplete3,"ENSO")
      data_g$Month=rep(1:12,3)
      data_g[,2]=as.numeric(y)
      data_g$ENSO=factor(c(rep("Ni?o",12),rep("Normal",12),rep("Ni?a",12)))
      for (r in 1:36){
        if (data_g[r,2]<min[r,j+1]){data_g[r,2]=min[r,j+1]}
      }
      
      plotpath=paste(c(dir,"\\Graficos_plot\\",station[j],".jpg"),collapse="")
      ggplot(DataENSO,aes(x=Month,y=DataENSO[,j],colour=ENSO))+
        geom_point(alpha=0.5,size=0.5)+
        geom_line(data=data_g,aes(x=Month,y=data_g[,2],colour=ENSO),size=1.2)+
        xlab("Meses")+
        ylab(type_fileComplete2)+
        scale_x_continuous(breaks=1:12, labels=namesMonth)+
        theme(axis.text.x=element_text(vjust=0.5, size=10))+
        scale_colour_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("Ni?o","Normal","Ni?a"),labels=c("Ni?o","Normal","Ni?a"))+
        ggtitle(paste(c(type_fileComplete2," ","Promedio ENSO ",station[j]),collapse=""))+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
      ggsave(plotpath,width =40 , height = 22,units = "cm")
      
      plotpath = paste(c(dir,"\\Graficos_plot\\Prom_ENSO_",station[j],"_",PeriodoRegistro,".csv"),collapse="")
      write.csv(data_g,plotpath, row.names=FALSE,na="")
      
      data_g$Normal=rep(data_g[13:24,2])
      data_g$p=(data_g[,2]-data_g[,4])/data_g[,4]*100
      data_g$ENSO=factor(data_g$ENSO,levels=c("Ni?o","Normal","Ni?a"))
      data_g$Month=factor(data_g$Month)
      data_g1=rbind(data_g[1:12,],data_g[25:36,])
      data_g1$Estacion=rep(station[j])
      data_g2= data_g
      data_g2$Estacion=rep(station[j])
      
      plotpath=paste(c(dir,"\\Graficos_plot\\","Comparacion porcentual ",station[j],".jpg"),collapse="")
      ggplot(data_g1,aes(x=Month,y=p,fill=ENSO))+
        geom_bar(stat="identity",position=position_dodge())+
        xlab("Meses")+
        ylab("Diferencia Porcentual (%)")+
        theme(axis.text.x=element_text(vjust=0.5, size=10))+
        scale_fill_manual(values=c("firebrick3","dodgerblue2"), name=" ",breaks=c("Ni?o","Ni?a"),labels=c("Ni?o","Ni?a"))+
        ggtitle(paste(c("Comparaci?n porcentual respecto a la fase normal del fen?meno ENSO ",station[j]),collapse=""))+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
      ggsave(plotpath,width =40 , height = 22,units = "cm")
      
      data_gAll=rbind(data_gAll,data_g1)
      data_gAll2=rbind(data_gAll2,data_g2)
    }
  }
  
  #data_gAll=na.exclude(data_gAll)
  #data_gAll2=na.exclude(data_gAll2)
  
  plotpath = paste(c(dir,"\\Prom_ENSO_",PeriodoRegistro,".csv"),collapse="")
  write.csv(data_gAll2,plotpath, row.names=FALSE,na="")
  
  data_gAll$Month=as.numeric(as.character(data_gAll$Month))
  plotpath=paste(c(dir,"\\Graficos_plot\\",type_fileComplete2," Comparacion porcentual.jpg"),collapse="")
  ggplot(data_gAll,aes(x=Month,y=p,fill=ENSO))+
    geom_bar(stat="identity",position=position_dodge())+
    scale_x_continuous(breaks=1:12, labels=namesMonth)+
    xlab("Meses")+
    ylab("Diferencia Porcentual (%)")+
    theme(axis.text.x=element_text(vjust=0.5, angle=90,size=10))+
    scale_fill_manual(values=c("firebrick3","dodgerblue2"), name=" ",breaks=c("Ni?o","Ni?a"),labels=c("Ni?o","Ni?a"))+
    ggtitle(paste(c("Comparaci?n porcentual respecto a la fase normal del fen?meno ENSO"),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    facet_wrap( ~ Estacion, scales = "free")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  DataENSO=merge(DataVentanaMensual_v,ENSO,c("Year","Month"))
  DataENSO$ENSO=factor(DataENSO$ENSO,levels=c("Ni?o","Normal","Ni?a"))
  DataENSO$ENSO_COD=factor(DataENSO$ENSO_COD,levels=as.character(c(3,2,1)))
  DataENSO$Month=as.numeric(as.character(DataENSO$Month))
  #str(data_g)
  #str(data_gAll2)
  data_gAll2$Month=as.numeric(data_gAll2$Month)
  data_gAll2$ENSO=factor(data_gAll2$ENSO,levels = c("Ni?o","Normal","Ni?a"))
  
  plotpath=paste(c(dir,"\\Graficos_plot\\",type_fileComplete2," ","Promedio ENSO",".jpg"),collapse="")
  ggplot(DataENSO,aes(x=Month,y=Valores,colour=ENSO))+
    geom_point(alpha=0.5,size=0.5)+
    geom_line(data=data_gAll2,aes(x=Month,y=data_gAll2[,2],colour=ENSO),size=1.2)+
    xlab("Meses")+
    ylab(type_fileComplete2)+
    scale_x_continuous(breaks=1:12, labels=namesMonth)+
    theme(axis.text.x=element_text(angle=90, vjust=0.5, size=10))+
    scale_colour_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("Ni?o","Normal","Ni?a"),labels=c("Ni?o","Normal","Ni?a"))+
    ggtitle(paste(c(type_fileComplete2," ","Promedio ENSO"),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    facet_wrap( ~ Estacion, scales = "free")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
}
G_ENSO_boxplot=function(dir){
  
  Valores_ENSO(date1, date2)
  
  ENSO=ENSOVentana
  ENSO[,1]=NULL
  
  Data=DataVentanaMensual
  station=names(Data)
  
  DataENSO=cbind(Data,ENSO,DatesM)
  
  setwd(dir)
  dir.create("Graficos_boxplot",showWarnings=F)
  
  DataENSO$ENSO=factor(DataENSO$ENSO,levels=c("Ni?o","Normal","Ni?a"))
  DataENSO$ENSO_COD=factor(DataENSO$ENSO_COD,levels=as.character(c(3,2,1)))
  type_fileComplete2=paste(c(clima.var2[clima[pos]]," Mensual"),collapse = "")
  
  for (j in 1:ncol(Data)) {    
    plotpath=paste(c(dir,"\\Graficos_boxplot\\",station[j],".jpg"),collapse="")
    g= ggplot(DataENSO,aes(x=factor(Month),y=DataENSO[,j],colour=ENSO_COD))+
      geom_jitter(alpha=0.5,size=0.1)+
      geom_boxplot(aes(fill=ENSO_COD),colour = 'black', outlier.size=0.8)+
      xlab("Meses")+
      ylab(type_fileComplete2)+
      scale_x_discrete(breaks=1:12, labels=namesMonth)+
      theme(axis.text.x=element_text(vjust=0.5, size=10))+
      scale_fill_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("3","2","1"),labels=c("Ni?o","Normal","Ni?a"))+
      scale_colour_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("3","2","1"),labels=c("Ni?o","Normal","Ni?a"))+
      ggtitle(paste(c(type_fileComplete2," ENSO ",station[j]),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    g
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    #y=ggplot_build(g)$data[[2]][,c(-1,-7,-8,-9,-10,-11,-17)]
    #y=as.data.frame(unlist(ggplot_build(g)$data[[2]][,c(-1,-7,-8,-9,-10,-11,-17)]),ncol=17,nrow=36)
    #plotpath = paste(c(dir,"\\Graficos_boxplot\\",station[j],"_",PeriodoRegistro,".csv"),collapse="")
    #write.csv(y,plotpath, row.names=FALSE,na="")
  }
  
  DataENSO=merge(DataVentanaMensual_v,ENSO,c("Year", "Month"))
  DataENSO$ENSO=factor(DataENSO$ENSO,levels=c("Ni?o","Normal","Ni?a"))
  DataENSO$ENSO_COD=factor(DataENSO$ENSO_COD,levels=as.character(c(3,2,1)))
  plotpath=paste(c(dir,"\\Graficos_boxplot\\",type_fileComplete2," ","ENSO",".jpg"),collapse="")
  ggplot(DataENSO,aes(x=factor(Month),y=Valores,colour=ENSO_COD))+
    geom_jitter(alpha=0.5,size=0.1)+
    geom_boxplot(aes(fill=factor(ENSO_COD)),colour = 'black', outlier.size=0.8)+
    xlab("Meses")+
    ylab(type_fileComplete2)+
    scale_x_discrete(breaks=1:12, labels=namesMonth)+
    theme(axis.text.x=element_text(angle=90, vjust=0.5, size=10))+
    scale_fill_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("3","2","1"),labels=c("Ni?o","Normal","Ni?a"))+
    scale_colour_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("3","2","1"),labels=c("Ni?o","Normal","Ni?a"))+
    ggtitle(paste(c(type_fileComplete2," "," ENSO"),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    facet_wrap( ~ Estacion, scales = "free")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  
} 
GraficasEDA=function(dat.m,dir_out,type_fileComplete,N_Graficas,addggtitle) {
  
  #Diagramas de Puntos de todas las estaciones
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_Diagrama de puntos (Log)",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Valores, y=Estacion,colour=Estacion))+
    geom_point(size=0.5)+
    scale_x_continuous(trans = log_trans(), breaks = base_breaks(),labels = prettyNum)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    xlab(paste(c(type_fileComplete," (Escala logaritmica)"),collapse=""))+
    ylab("Estaciones")+
    ggtitle(paste(c("Diagrama de Puntos ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_Diagrama de puntos ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Valores, y=Estacion,colour=Estacion))+
    geom_point(size=0.5)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    xlab(type_fileComplete)+
    ylab("Estaciones")+
    scale_x_continuous(breaks =round(seq(from=min(dat.m$Valores),to=max(dat.m$Valores),by=(max(dat.m$Valores)-min(dat.m$Valores))/50),-1))+
    ggtitle(paste(c("Diagrama de Puntos ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #Box Plot de todas las estaciones
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_BoxPlot (Log) ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1, colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),labels = prettyNum)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(paste(c(type_fileComplete," (Escala logaritmica)"),collapse=""))+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_BoxPlot ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1, colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(type_fileComplete)+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #paquete para formato de eje fecha en x
  if(require("scales")==FALSE){install.packages("scales", dep = T)} 
  library(scales)
  
  if (length(unique(as.character(dat.m$Estacion)))>1) {
    #serie de tiempo sobrepuesta de todas las estaciones
    plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_serie de tiempo ",PeriodoRegistro,".jpg"),collapse="")
    ggplot(dat.m,aes(x=Dates, y=Valores,colour=Estacion))+
      geom_line(alpha=0.5,size=0.5)+
      #geom_bar(stat = "identity",aes(fill=Estacion))+
      scale_x_date(date_breaks= "1 year",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
      theme(axis.text.x=element_text(size=8, angle=90,hjust=0.95,vjust=0.2))+
      ylab(type_fileComplete)+
      xlab("Estaciones")+
      ggtitle(paste(c("Serie de tiempo ",type_fileComplete," ", addggtitle),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    ggsave(plotpath,width =40 , height = 22,units = "cm")
  }
  
  #serie de tiempo diaria de cada estacion separada en una unica grafica 
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_serie de tiempo por Estacion ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Dates, y=Valores,colour=Estacion))+
    geom_line(size=0.5)+
    #geom_bar(stat = "identity",aes(fill=Estacion))+
    scale_x_date(date_breaks= "10 years",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
    theme(axis.text.x=element_text(size=6,hjust=0.95,vjust=0.2))+ #, angle=90
    ylab(type_fileComplete)+
    xlab("Fecha")+
    ggtitle(paste(c("Serie de tiempo ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    facet_wrap(~ Estacion, scales = "free")+
    theme(legend.position="none")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  
  #histogramas
  x=rep(1,nrow(DataVentana))
  sturges=nclass.Sturges(x)
  library(plyr)
  vline = ddply(dat.m, "Estacion", summarise, grp.mean=mean(Valores,na.rm=TRUE))
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_Histograma ",PeriodoRegistro,".jpg"),collapse="")
  h= ggplot(dat.m,aes(Valores,colour=Estacion, fill=Estacion))+
    geom_histogram(bins=sturges,aes(y=..density..),alpha=0.3 ,position="identity") + 
    geom_freqpoly(bins=sturges,aes(y=..density..) ,position="identity",colour="grey17")+
    geom_vline(data=vline, aes(xintercept=grp.mean), color="black",linetype="dashed", size=1)+
    ylab("Densidad")+
    ggtitle(paste(c("Histograma ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  h
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_Histograma_log ",PeriodoRegistro,".jpg"),collapse="")
  h+scale_x_log10()+ggtitle(paste(c("Histograma ",type_fileComplete," (Escala logaritmica)"," ", addggtitle),collapse=""))
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #qq plots
  #qqnorm(DataVentana[,1])
  #qqline(DataVentana[,1])
  
  UniqueEstacion=as.character(unique(dat.m$Estacion))
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),3], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_QQ plot ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(sample=Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(type_fileComplete)+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #valores log10
  dat.m$Valores2=dat.m$Valores+1
  dat.m$log10Valores=log10(dat.m$Valores2)
  
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),7], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_QQ plot_log ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(sample=log10Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(paste("log10(",type_fileComplete," + 1)",sep=""))+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
}
GraficasEDAmultiplot=function(dat.m,dir_out,type_fileComplete,N_Graficas,addggtitle) {
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete," ",PeriodoRegistro,".jpg"),collapse="")
  plotpathlog = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete," (log) ",PeriodoRegistro,".jpg"),collapse="")
  
  #Box Plot de todas las estaciones
  EDA_M1log = ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1, colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),labels = prettyNum)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(paste(c(type_fileComplete," (Escala logaritmica)"),collapse=""))+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  
  EDA_M1 = ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1, colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(type_fileComplete)+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  
  #paquete para formato de eje fecha en x
  if(require("scales")==FALSE){install.packages("scales", dep = T)} 
  library(scales)
  
  #serie de tiempo diaria de cada estacion separada en una unica grafica 
  EDA_M5 = ggplot(dat.m,aes(x=Dates, y=Valores,colour=Estacion))+
    geom_line(size=0.5)+
    #geom_bar(stat = "identity",aes(fill=Estacion))+
    scale_x_date(date_breaks= "10 years",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
    theme(axis.text.x=element_text(size=6,hjust=0.95,vjust=0.2))+ #, angle=90
    ylab(type_fileComplete)+
    xlab("Fecha")+
    ggtitle(paste(c("Serie de tiempo ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    facet_wrap(~ Estacion, scales = "free")+
    theme(legend.position="none")
  
  #serie de tiempo diaria boxplot con eje x a?o
  #dat.m1=dat.m[1:(2*length(DatesVentana)),]
  EDA_M2log = ggplot(dat.m,aes(x=MesDia, y=Valores,colour=Estacion))+
    geom_boxplot(size=0.5,alpha=0.5,outlier.size = 0.1)+
    stat_summary(fun.y=mean,col='black',size=1,geom='line',aes(group=1))+
    ylab(paste(c(type_fileComplete," (Escala logaritmica)"),collapse=""))+
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),labels = prettyNum)+
    #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
    # labels = trans_format("log10", math_format(10^.x)))+
    xlab("Fecha")+
    ggtitle(paste(c("Boxplot diario ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    scale_x_discrete(breaks=breaks,labels=labelsEDA)+
    facet_wrap(~ Estacion, scales = "free")
  
  EDA_M2 = ggplot(dat.m,aes(x=MesDia, y=Valores,colour=Estacion))+
    geom_boxplot(size=0.5,alpha=0.5,outlier.size = 0.1)+
    stat_summary(fun.y=mean,col='black',size=1,geom='line',aes(group=1))+
    ylab(type_fileComplete)+
    xlab("Fecha")+
    ggtitle(paste(c("Boxplot diario ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    scale_x_discrete(breaks=breaks,labels=labelsEDA)+
    facet_wrap(~ Estacion, scales = "free")
  
  #histogramas
  x=rep(1,nrow(DataVentana))
  sturges=nclass.Sturges(x)
  library(plyr)
  vline = ddply(dat.m, "Estacion", summarise, grp.mean=mean(Valores,na.rm=TRUE))
  
  EDA_M3 = ggplot(dat.m,aes(Valores,colour=Estacion, fill=Estacion))+
    geom_histogram(bins=sturges,aes(y=..density..),alpha=0.3 ,position="identity") + 
    geom_freqpoly(bins=sturges,aes(y=..density..) ,position="identity",colour="grey")+
    geom_vline(data=vline, aes(xintercept=grp.mean), color="black",linetype="dashed", size=1)+
    ylab("Densidad")+
    ggtitle(paste(c("Histograma ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  
  
  EDA_M3log = EDA_M3 +scale_x_log10()+ggtitle(paste(c("Histograma ",type_fileComplete," (Escala logaritmica)"," ", addggtitle),collapse=""))
  
  #qq plots
  #qqnorm(DataVentana[,1])
  #qqline(DataVentana[,1])
  
  UniqueEstacion=as.character(unique(dat.m$Estacion))
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),3], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  EDA_M4 = ggplot(dat.m,aes(sample=Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(type_fileComplete)+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  
  #valores log10
  dat.m$Valores2=dat.m$Valores+1
  dat.m$log10Valores=log10(dat.m$Valores2)
  
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),7], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  EDA_M4log = ggplot(dat.m,aes(sample=log10Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(paste("log10(",type_fileComplete," + 1)",sep=""))+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  
  #Diagramas de Puntos de todas las estaciones
  EDA_M6log =ggplot(dat.m,aes(x=Valores, y=Estacion,colour=Estacion))+
    geom_point(size=0.5)+
    scale_x_continuous(trans = log_trans(), breaks = base_breaks(),labels = prettyNum)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    xlab(paste(c(type_fileComplete," (Escala logaritmica)"),collapse=""))+
    ylab("Estaciones")+
    ggtitle(paste(c("Diagrama de Puntos ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  
  EDA_M6=ggplot(dat.m,aes(x=Valores, y=Estacion,colour=Estacion))+
    geom_point(size=0.5)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    xlab(type_fileComplete)+
    ylab("Estaciones")+
    scale_x_continuous(breaks =round(seq(from=min(dat.m$Valores),to=max(dat.m$Valores),by=(max(dat.m$Valores)-min(dat.m$Valores))/50),-1))+
    ggtitle(paste(c("Diagrama de Puntos ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  
  jpeg(filename = plotpath, restoreConsole = TRUE,width =40 , height = 22,units = "cm",res = 300)
  multiplot(EDA_M1,EDA_M3,EDA_M5,EDA_M2,EDA_M4,EDA_M6,cols=2)
  dev.off()
  
  jpeg(filename = plotpathlog, restoreConsole = TRUE,width =40 , height = 22,units = "cm",res = 300)
  multiplot(EDA_M1log,EDA_M3log,EDA_M5,EDA_M2log,EDA_M4log,EDA_M6log,cols=2)
  dev.off()
  
}
GraficasEDA_M=function(dat.m,dir_out,type_fileComplete,N_Graficas,addggtitle) {
  
  #Box Plot de todas las estaciones
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_BoxPlot (Log) ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1, colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),labels = prettyNum)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(paste(c(type_fileComplete," (Escala logaritmica)"),collapse=""))+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_BoxPlot ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1, colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(type_fileComplete)+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #paquete para formato de eje fecha en x
  if(require("scales")==FALSE){install.packages("scales", dep = T)} 
  library(scales)
  
  if (length(unique(as.character(dat.m$Estacion)))>1) {
    #serie de tiempo sobrepuesta de todas las estaciones
    plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_serie de tiempo ",PeriodoRegistro,".jpg"),collapse="")
    ggplot(dat.m,aes(x=Dates, y=Valores,colour=Estacion))+
      geom_line(alpha=0.5,size=0.5)+
      #geom_bar(stat = "identity",aes(fill=Estacion))+
      scale_x_date(date_breaks= "1 year",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
      theme(axis.text.x=element_text(size=8, angle=90,hjust=0.95,vjust=0.2))+
      ylab(type_fileComplete)+
      xlab("Estaciones")+
      ggtitle(paste(c("Serie de tiempo ",type_fileComplete," ", addggtitle),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    ggsave(plotpath,width =40 , height = 22,units = "cm")
  }
  
  #serie de tiempo diaria de cada estacion separada en una unica grafica 
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_serie de tiempo por Estacion ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Dates, y=Valores,colour=Estacion))+
    geom_line(size=0.5)+
    #geom_bar(stat = "identity",aes(fill=Estacion))+
    scale_x_date(date_breaks= "10 years",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
    theme(axis.text.x=element_text(size=6,hjust=0.95,vjust=0.2))+ #, angle=90
    ylab(type_fileComplete)+
    xlab("Fecha")+
    ggtitle(paste(c("Serie de tiempo ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    facet_wrap(~ Estacion, scales = "free")+
    theme(legend.position="none")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #serie de tiempo diaria boxplot con eje x a?o
  #dat.m1=dat.m[1:(2*length(DatesVentana)),]
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_BoxPlot Mensual_log ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=MesDia, y=Valores,colour=Estacion))+
    geom_boxplot(size=0.5,alpha=0.5)+
    stat_summary(fun.y=mean,col='black',size=1,geom='line',aes(group=1))+
    ylab(paste(c(type_fileComplete," (Escala logaritmica)"),collapse=""))+
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),labels = prettyNum)+
    #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
    # labels = trans_format("log10", math_format(10^.x)))+
    xlab("Fecha")+
    ggtitle(paste(c("Boxplot Mensual ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    #scale_x_discrete(breaks=breaks,labels=labels)+
    facet_wrap(~ Estacion, scales = "free")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_BoxPlot Mensual ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=MesDia, y=Valores,colour=Estacion))+
    geom_boxplot(size=0.5,alpha=0.5)+
    stat_summary(fun.y=mean,col='black',size=1,geom='line',aes(group=1))+
    ylab(type_fileComplete)+
    xlab("Fecha")+
    ggtitle(paste(c("Boxplot Mensual ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    #scale_x_discrete(breaks=breaks,labels=labels)+
    facet_wrap(~ Estacion, scales = "free")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #histogramas
  x=rep(1,nrow(DataVentanaMensual))
  sturges=nclass.Sturges(x)
  library(plyr)
  vline = ddply(dat.m, "Estacion", summarise, grp.mean=mean(Valores,na.rm=TRUE))
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_Histograma ",PeriodoRegistro,".jpg"),collapse="")
  h= ggplot(dat.m,aes(Valores,colour=Estacion, fill=Estacion))+
    geom_histogram(bins=sturges,aes(y=..density..),alpha=0.3 ,position="identity") + 
    geom_freqpoly(bins=sturges,aes(y=..density..) ,position="identity",colour="grey")+
    #geom_density(aes(y= 16 * ..count..),alpha=0,colour="black")+
    geom_vline(data=vline, aes(xintercept=grp.mean), color="black",linetype="dashed", size=1)+
    ylab("Densidad")+
    ggtitle(paste(c("Histograma ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  h
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_Histograma_log ",PeriodoRegistro,".jpg"),collapse="")
  h+scale_x_log10()+ggtitle(paste(c("Histograma ",type_fileComplete," (Escala logaritmica)"," ", addggtitle),collapse=""))
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #qq plots
  #qqnorm(DataVentana[,1])
  #qqline(DataVentana[,1])
  
  UniqueEstacion=as.character(unique(dat.m$Estacion))
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),5], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_QQ plot ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(sample=Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(type_fileComplete)+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #valores log10
  dat.m$Valores2=dat.m$Valores+1
  dat.m$log10Valores=log10(dat.m$Valores2)
  
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),9], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_QQ plot_log ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(sample=log10Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(paste("log10(",type_fileComplete," + 1)",sep=""))+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
}
GraficasEDA_Mmultiplot=function(dat.m,dir_out,type_fileComplete,N_Graficas,addggtitle) {
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete," ",PeriodoRegistro,".jpg"),collapse="")
  plotpathlog = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete," (log) ",PeriodoRegistro,".jpg"),collapse="")
  #Box Plot de todas las estaciones
  EDA_M1log =  ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1,colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),labels = prettyNum)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(paste(c(type_fileComplete," (Escala logaritmica)"),collapse=""))+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  
  EDA_M1 =  ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1, colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(type_fileComplete)+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  
  #paquete para formato de eje fecha en x
  if(require("scales")==FALSE){install.packages("scales", dep = T)} 
  library(scales)
  
  #serie de tiempo diaria boxplot con eje x a?o
  #dat.m1=dat.m[1:(2*length(DatesVentana)),]
  EDA_M2log  =  ggplot(dat.m,aes(x=MesDia, y=Valores,colour=Estacion))+
    geom_boxplot(size=0.5,alpha=0.5)+
    stat_summary(fun.y=mean,col='black',size=1,geom='line',aes(group=1))+
    ylab(paste(c(type_fileComplete," (Escala logaritmica)"),collapse=""))+
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),labels = prettyNum)+
    #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
    # labels = trans_format("log10", math_format(10^.x)))+
    xlab("Fecha")+
    ggtitle(paste(c("Boxplot Mensual ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    #scale_x_discrete(breaks=breaks,labels=labels)+
    facet_wrap(~ Estacion, scales = "free")
  
  EDA_M2  =   ggplot(dat.m,aes(x=MesDia, y=Valores,colour=Estacion))+
    geom_boxplot(size=0.5,alpha=0.5)+
    stat_summary(fun.y=mean,col='black',size=1,geom='line',aes(group=1))+
    ylab(type_fileComplete)+
    xlab("Fecha")+
    ggtitle(paste(c("Boxplot Mensual ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    #scale_x_discrete(breaks=breaks,labels=labels)+
    facet_wrap(~ Estacion, scales = "free")
  
  #histogramas
  x=rep(1,nrow(DataVentanaMensual))
  sturges=nclass.Sturges(x)
  library(plyr)
  vline = ddply(dat.m, "Estacion", summarise, grp.mean=mean(Valores,na.rm=TRUE))
  
  EDA_M3  = ggplot(dat.m,aes(Valores,colour=Estacion, fill=Estacion))+
    geom_histogram(bins=sturges,aes(y=..density..),alpha=0.3 ,position="identity") + 
    geom_freqpoly(bins=sturges,aes(y=..density..) ,position="identity",colour="grey")+
    #geom_density(aes(y= 16 * ..count..),alpha=0,colour="black")+
    geom_vline(data=vline, aes(xintercept=grp.mean), color="black",linetype="dashed", size=1)+
    ylab("Densidad")+
    ggtitle(paste(c("Histograma ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  
  EDA_M3log  = EDA_M3 +scale_x_log10()+ggtitle(paste(c("Histograma ",type_fileComplete," (Escala logaritmica)"," ", addggtitle),collapse=""))
  
  #qq plots
  #qqnorm(DataVentana[,1])
  #qqline(DataVentana[,1])
  
  UniqueEstacion=as.character(unique(dat.m$Estacion))
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),5], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  EDA_M4  =   ggplot(dat.m,aes(sample=Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(type_fileComplete)+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  
  #valores log10
  dat.m$Valores2=dat.m$Valores+1
  dat.m$log10Valores=log10(dat.m$Valores2)
  
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),9], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  EDA_M4log =ggplot(dat.m,aes(sample=log10Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(paste("log10(",type_fileComplete," + 1)",sep=""))+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  
  #serie de tiempo diaria de cada estacion separada en una unica grafica 
  EDA_M5=ggplot(dat.m,aes(x=Dates, y=Valores,colour=Estacion))+
    geom_line(size=0.5)+
    scale_x_date(date_breaks= "10 years",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
    theme(axis.text.x=element_text(size=6,hjust=0.95,vjust=0.2))+ #, angle=90
    ylab(type_fileComplete)+
    xlab("Fecha")+
    ggtitle(paste(c("Serie de tiempo ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    facet_wrap(~ Estacion, scales = "free")+
    theme(legend.position="none")
  
  jpeg(filename = plotpath, restoreConsole = TRUE,width =40 , height = 22,units = "cm",res = 300)
  multiplot(EDA_M1,EDA_M3,EDA_M5,EDA_M2,EDA_M4,cols=2)
  dev.off()
  
  jpeg(filename = plotpathlog, restoreConsole = TRUE,width =40 , height = 22,units = "cm",res = 300)
  multiplot(EDA_M1log,EDA_M3log,EDA_M5,EDA_M2log,EDA_M4log,cols=2)
  dev.off()
  
}
GraficasEDA_A=function(dat.m,dir_out,type_fileComplete,N_Graficas,addggtitle) {
  
  #Box Plot de todas las estaciones
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_BoxPlot (Log) ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1, colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),labels = prettyNum)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(paste(c(type_fileComplete," (Escala logaritmica)"),collapse=""))+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_BoxPlot ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1, colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(type_fileComplete)+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #paquete para formato de eje fecha en x
  if(require("scales")==FALSE){install.packages("scales", dep = T)} 
  library(scales)
  
  if (length(unique(as.character(dat.m$Estacion)))>1) {
    #serie de tiempo sobrepuesta de todas las estaciones
    plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_serie de tiempo ",PeriodoRegistro,".jpg"),collapse="")
    ggplot(dat.m,aes(x=Dates, y=Valores,colour=Estacion))+
      geom_line(alpha=0.5,size=0.1)+
      #geom_bar(stat = "identity",aes(fill=Estacion))+
      scale_x_date(date_breaks= "1 year",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
      theme(axis.text.x=element_text(size=8, angle=90,hjust=0.95,vjust=0.2))+
      ylab(type_fileComplete)+
      xlab("Estaciones")+
      ggtitle(paste(c("Serie de tiempo ",type_fileComplete," ", addggtitle),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    ggsave(plotpath,width =40 , height = 22,units = "cm")
  }
  
  #serie de tiempo diaria de cada estacion separada en una unica grafica 
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_serie de tiempo por Estacion ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(x=Dates, y=Valores,colour=Estacion))+
    geom_line(size=0.5)+
    #geom_bar(stat = "identity",aes(fill=Estacion))+
    scale_x_date(date_breaks= "2 years",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
    theme(axis.text.x=element_text(size=6,hjust=0.95,vjust=0.2))+ #, angle=90
    ylab(type_fileComplete)+
    xlab("Fecha")+
    ggtitle(paste(c("Serie de tiempo ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    facet_wrap(~ Estacion, scales = "free")+
    theme(legend.position="none")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #histogramas
  x=rep(1,nrow(DataVentanaAnual))
  sturges=nclass.Sturges(x)
  library(plyr)
  vline = ddply(dat.m, "Estacion", summarise, grp.mean=mean(Valores,na.rm=TRUE))
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_Histograma ",PeriodoRegistro,".jpg"),collapse="")
  h= ggplot(dat.m,aes(Valores,colour=Estacion, fill=Estacion))+
    geom_histogram(bins=sturges,aes(y=..density..),alpha=0.3 ,position="identity") + 
    geom_freqpoly(bins=sturges,aes(y=..density..) ,position="identity",colour="grey")+
    #geom_density(aes(y= 16 * ..count..),alpha=0,colour="black")+
    geom_vline(data=vline, aes(xintercept=grp.mean), color="black",linetype="dashed", size=1)+
    ylab("Densidad")+
    ggtitle(paste(c("Histograma ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  h
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_Histograma_log ",PeriodoRegistro,".jpg"),collapse="")
  h+scale_x_log10()+ggtitle(paste(c("Histograma ",type_fileComplete," (Escala logaritmica)"," ", addggtitle),collapse=""))
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #qq plots
  #qqnorm(DataVentana[,1])
  #qqline(DataVentana[,1])
  
  UniqueEstacion=as.character(unique(dat.m$Estacion))
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),5], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_QQ plot ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(sample=Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(type_fileComplete)+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #valores log10
  dat.m$Valores2=dat.m$Valores+1
  dat.m$log10Valores=log10(dat.m$Valores2)
  
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),9], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete,"_QQ plot_log ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m,aes(sample=log10Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(paste("log10(",type_fileComplete," + 1)",sep=""))+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  ggsave(plotpath,width =40 , height = 22,units = "cm")
}
GraficasEDA_Amultiplot=function(dat.m,dir_out,type_fileComplete,N_Graficas,addggtitle) {
  
  plotpath = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete," ",PeriodoRegistro,".jpg"),collapse="")
  plotpathlog = paste(c(dir_out,"\\",N_Graficas,"_",type_fileComplete," (log) ",PeriodoRegistro,".jpg"),collapse="")
  
  #Box Plot de todas las estaciones
  EDA_M1log =  ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1, colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),labels = prettyNum)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(paste(c(type_fileComplete," (Escala logaritmica)"),collapse=""))+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  
  EDA_M1 =  ggplot(dat.m,aes(x=Estacion, y=Valores,colour=Estacion))+
    geom_jitter(size=0.1, colour="grey")+
    geom_boxplot(size=0.5,alpha=0.5)+
    #scale_y_log10(breaks=c(.01,.1,1,10,100,1000),labels=c(.01,.1,1,10,100,1000))+
    ylab(type_fileComplete)+
    xlab("Estaciones")+
    ggtitle(paste(c("Boxplot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=10,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")
  
  #paquete para formato de eje fecha en x
  if(require("scales")==FALSE){install.packages("scales", dep = T)} 
  library(scales)
  
  #serie de tiempo diaria de cada estacion separada en una unica grafica 
  EDA_M4 =  ggplot(dat.m,aes(x=Dates, y=Valores,colour=Estacion))+
    geom_line(size=0.5)+
    #geom_bar(stat = "identity",aes(fill=Estacion))+
    scale_x_date(date_breaks= "2 years",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
    theme(axis.text.x=element_text(size=6,hjust=0.95,vjust=0.2))+ #, angle=90
    ylab(type_fileComplete)+
    xlab("Fecha")+
    ggtitle(paste(c("Serie de tiempo ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    facet_wrap(~ Estacion, scales = "free")+
    theme(legend.position="none")
  
  #histogramas
  x=rep(1,nrow(DataVentanaAnual))
  sturges=nclass.Sturges(x)
  library(plyr)
  vline = ddply(dat.m, "Estacion", summarise, grp.mean=mean(Valores,na.rm=TRUE))
  
  EDA_M2 =   ggplot(dat.m,aes(Valores,colour=Estacion, fill=Estacion))+
    geom_histogram(bins=sturges,aes(y=..density..),alpha=0.3 ,position="identity") + 
    geom_freqpoly(bins=sturges,aes(y=..density..) ,position="identity",colour="grey")+
    #geom_density(aes(y= 16 * ..count..),alpha=0,colour="black")+
    geom_vline(data=vline, aes(xintercept=grp.mean), color="black",linetype="dashed", size=1)+
    ylab("Densidad")+
    ggtitle(paste(c("Histograma ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  
  EDA_M2log = EDA_M2 +scale_x_log10()+ggtitle(paste(c("Histograma ",type_fileComplete," (Escala logaritmica)"," ", addggtitle),collapse=""))
  
  #qq plots
  #qqnorm(DataVentana[,1])
  #qqline(DataVentana[,1])
  
  UniqueEstacion=as.character(unique(dat.m$Estacion))
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),5], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  EDA_M3 =  ggplot(dat.m,aes(sample=Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(type_fileComplete)+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  
  #valores log10
  dat.m$Valores2=dat.m$Valores+1
  dat.m$log10Valores=log10(dat.m$Valores2)
  
  intsl=as.data.frame(matrix(NA,nrow=length(UniqueEstacion),ncol=3))
  colnames(intsl)=c("Estacion","slope","int")
  for (i in 1:length(UniqueEstacion)) {
    y = quantile(dat.m[which(as.character(dat.m$Estacion)==UniqueEstacion[i]),9], c(0.25, 0.75),na.rm=TRUE) # Find the 1st and 3rd quartiles
    x = qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
    intsl[i,1] = UniqueEstacion[i]
    intsl[i,2] = diff(y) / diff(x)             # Compute the line slope
    intsl[i,3] = y[1] - intsl[i,2] * x[1]           # Compute the line intercept
  }
  
  EDA_M3log =  ggplot(dat.m,aes(sample=log10Valores,colour=Estacion))+
    stat_qq(distribution=qnorm,size=1) + 
    geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
    ylab(paste("log10(",type_fileComplete," + 1)",sep=""))+
    ggtitle(paste(c("QQ plot ",type_fileComplete," ", addggtitle),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))+
    theme(axis.text.x=element_text(size=7, angle=0,hjust=0.95,vjust=0.2))+
    theme(legend.position="none")+
    facet_wrap(~ Estacion, scales = "free")
  
  jpeg(filename = plotpath, restoreConsole = TRUE,width =40 , height = 22,units = "cm",res = 300)
  multiplot(EDA_M1,EDA_M3,EDA_M2,EDA_M4,cols=2)
  dev.off()
  
  jpeg(filename = plotpathlog, restoreConsole = TRUE,width =40 , height = 22,units = "cm",res = 300)
  multiplot(EDA_M1log,EDA_M3log,EDA_M2log,EDA_M4,cols=2)
  dev.off()
}
multiplot = function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots = c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout = matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx = as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}