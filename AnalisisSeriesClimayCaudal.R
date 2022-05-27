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
options(warn=1)
rm(list=ls()) # removes variables from environment
source("FuncionesAnalisisClimayCaudal.R")
theme_update(plot.title = element_text(hjust = 0.5)) # fija el titulo de las graficas como centrado
start = Sys.time() # stores the time at the start of the script run (to calculate total time to run the script)
#rstudioapi::getSourceEditorContext()$path
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  

#'/////////////////////////////////////////////////////////////////////////////

#######################################################################################################
################    Variables definidas por el usuario   ##############################################
#######################################################################################################

dir_file="C:\\Users\\angel\\OneDrive\\Documentos\\GitHub\\AnalisisClimayCaudal\\Ejemplo" ###### fijar el directorio de trabajo!!!!!

name_file="Clima.csv"# las primeras tres columnas correponden a Anio, Mes, Dia
rowData=17 #fila en donde empieza la informacion 

AreasQ_File=paste0("AreasAferentes_Q.csv")

#para generar un mapa con el contorno de la cuenca y las estaciones
file_ShapeCatchs="Cuencas"
file_ShapeRios="drenajes" #opcional, si no se tiene se debe desactivar (FALSE) la opcion de IncluirRios
labelsize=2 # tama?o de las etiquetas del nombre de la estaciones en el mapa 

#perido en que se desea ejecutar el analisis
dateIni = "1989/01/01" ##en caso de querer un periodo de tiempo diferente al contenido en el archivo, si no NULL
dateFin = "2015/12/31" ##en caso de querer un periodo de tiempo diferente al contenido en el archivo, si no NULL

#secciones que se desean ejecutar, TRUE para ejecutar, FALSE para no ejecutar. 
GenerarArchivoEstadisticas=TRUE
ControlCalidad=TRUE
GraficasDiarias=TRUE
Promedios=TRUE
AgregarMensualyAnual=TRUE
MensualyGraficas=TRUE
AnualyGraficas=TRUE
GenerarPeriodoRegistro=TRUE
GenerarMapa=TRUE
  IncluirRios=TRUE
RegresionesMensuales=TRUE
Analisis_ENSO=TRUE
CorrelogramaPQ=TRUE

#######################################################################################################
################    a partir de aqui no se necesita hacer modificaciones   ############################
#######################################################################################################
setwd(dir_file)
# archivo ENSO con las columnas  YR   MON  TOTAL ClimAdjust ANOM
#https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt
filenameENSO ="C:\\Users\\angel\\Dropbox\\SEI WEAP\\Utiles\\Scripts\\AnalisisClima\\detrend.nino34.ascii.txt"
DataC = read.csv(name_file, check.names = F, stringsAsFactors = F,header = F)
recordr=length(which(!is.na(as.numeric(DataC[,1]))))
recordc=length(which(!is.na(as.numeric(DataC[1,]))))
DataC=DataC[2:recordr,1:recordc]
namesMonth=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic") # Crea una tabla con los nombres de los meses

clima.var =as.character(DataC[1,6:ncol(DataC)])
clima.var2=tolower(iconv(clima.var, to='ASCII//TRANSLIT')) #elimina caracteres acentos y otros caracteres 
type_file= as.character(DataC[2,6:ncol(DataC)])
units_file=as.character(DataC[3,6:ncol(DataC)])
Fun_file=as.character(DataC[4,6:ncol(DataC)]) #seleccione la funcion para agregar los datos de forma mensual y anual, puede ser sum o mean
IncluirControlNegativos=as.logical(DataC[5,6:ncol(DataC)]) # si se desea comprobar si existen valores negativos
LimiteInferior=as.numeric(DataC[6,6:ncol(DataC)]) # fijar limite inferior si se desea comprobar que valores estan por debajo del limite
LimiteSuperior=as.numeric(DataC[7,6:ncol(DataC)])  # fijar limite superior si se desea comprobar que valores estan por encima del limite
Tipo=as.character(DataC[8,6:ncol(DataC)])  #"Periodo Completo"  o "Diaria" #para "Periodo Completo" se calcula la desviacion estandar de toda la serie, para "Diaria" se calcula la desviacion estandar para cada dia del a?o
Extremos_NumDesv.Est=as.numeric(DataC[9,6:ncol(DataC)]) # en caso que se quiera analizar los extremos fijar el numero de desviaciones estantar para que el dato sea considerado extremo
Nas_admitidos=as.numeric(DataC[10,6:ncol(DataC)]) #porcentaje de datos faltantes admitidos, se elimina la estacion si tiene un mayor porcentaje en las siguientes secciones
h_estaciones=t(rbind(as.character(DataC[15,6:ncol(DataC)]),as.numeric(DataC[13,6:ncol(DataC)])))
h_estaciones=unique(h_estaciones)
Coordinates =as.data.frame(t(DataC[c(15,12,11,13),6:ncol(DataC)]))
Coordinates=unique(Coordinates)
Coordinates[,2] = round(as.numeric(as.character(Coordinates[,2])),2)
Coordinates[,3] = round(as.numeric(as.character(Coordinates[,3])),2)
Coordinates[,4] = round(as.numeric(as.character(Coordinates[,4])),2)
#str(Coordinates)
#head(Coordinates)
#stations_project=sub("\\.","", sub("\\.", " ", sub("\\.", " ", Coordinates[,1]))) 
Coordinates[,1]=as.character(Coordinates[,1])
stations_project=Coordinates[,1]
colnames(Coordinates)=c("Station","Longitude","Latitude","h")
colour="blue" # color para el triangulo hacia arriba que representa la estacion de la variable
colour_filtroNA = "red" #genera un mapa igual al de GenerarMapa y a?ade las estaciones que pasaron el filtro NA con el color seleccionado, es decir con un menor %NA 
lon_project = mean(as.numeric(as.character(Coordinates[,2])),na.rm=TRUE) 
lat_project = mean(as.numeric(as.character(Coordinates[,3])),na.rm=TRUE) 

clima=which(!is.na(Nas_admitidos))
clima=c(clima,(ncol(DataC)-5+1))

for (pos in 1:(length(clima)-1)){
    
  ############# inicio
  #####################
        Data=DataC[(rowData-1):nrow(DataC),c(3:5,(clima[pos]+5):(clima[pos+1]+5-1))]
        colnames(Data)=DataC[(rowData-2),c(3:5,(clima[pos]+5):(clima[pos+1]+5-1))]
        for (i in 1:ncol(Data)){Data[,i]=as.numeric(as.character(Data[,i])) }
        
        type_fileComplete = paste(c(clima.var[clima[pos]]," ",type_file[clima[pos]]),collapse="") #defining the title of your output documents
        type_fileComplete = tolower(iconv(type_fileComplete, to='ASCII//TRANSLIT')) #elimina caracteres acentos y otros caracteres 
        
        map_title = paste(c("Estaciones " ,clima.var2[clima[pos]]," ",type_file[clima[pos]]),collapse="" )# # titulo del mapa
        #str(Data)
        #head(Data[,1:7])
        
        date1 = paste(c(Data[1,1],"/",Data[1,2],"/",Data[1,3]),collapse="") # create date object of first row of data
        date2 = paste(c(Data[nrow(Data),1],"/",Data[nrow(Data),2],"/",Data[nrow(Data),3]),collapse="") # create date object of last row of data
        Dates = seq(as.Date(date1),as.Date(date2),"days") # create date sequence for entire series
        
        Data[,1:3] = NULL # deletes unnecessary columns
        
        if (!is.null(dateIni)){date1 = dateIni} 
        if (!is.null(dateFin)){date2 = dateFin} 
        
        DataVentana=Data[which(Dates==date1):which(Dates==date2),]
        for (i in 1:ncol(DataVentana)){
          DataVentana[,i]=as.numeric(as.character(DataVentana[,i]))
        }
        
        stations =as.character(colnames(Data))
        colnames(Data)=stations
        #stations = sub("\\.","", sub("\\.", " ", sub("\\.", " ", stations)))
        
        #str(DataVentana)
        DatesVentana = seq(as.Date(date1),as.Date(date2),"days") # create date sequence for entire series
        DatesM = seq(as.Date(date1),as.Date(date2),"month") # create date sequence for entire series
        DatesA = seq(as.Date(date1),as.Date(date2),"year") # create date sequence for entire series
        
        MesDiaVentana=month(ymd(DatesVentana))*100+day(ymd(DatesVentana))
        MesDiaVentanaFormat=as.character(format(as.Date(DatesVentana,format="%Y/%m/%d"),"%d %b"))
        YearMesVentana=year(ymd(DatesVentana))*100+month(ymd(DatesVentana))
        YearMesDiaVentana=year(ymd(DatesVentana))*10000+month(ymd(DatesVentana))*100+day(ymd(DatesVentana))
        
        nyears=year(ymd(date2))-year(ymd(date1))+1
        
        YearMonthDay_Diaria=as.data.frame(matrix(0,ncol=3,nrow=length(DatesVentana)))
        colnames(YearMonthDay_Diaria)=c("Year","Month","Day")
        YearMonthDay_Diaria$Year=year(ymd(DatesVentana))
        YearMonthDay_Diaria$Month=month(ymd(DatesVentana))
        YearMonthDay_Diaria$Day=day(ymd(DatesVentana))
        
        YearMonthDay_Mensual=as.data.frame(matrix(0,ncol=3,nrow=length(DatesM)))
        colnames(YearMonthDay_Mensual)=c("Year","Month","Day")
        YearMonthDay_Mensual$Year=year(ymd(DatesM))
        YearMonthDay_Mensual$Month=month(ymd(DatesM))
        
        YearMonthDay_Anual=as.data.frame(matrix(0,ncol=3,nrow=length(DatesA)))
        colnames(YearMonthDay_Anual)=c("Year","Month","Day")
        YearMonthDay_Anual$Year=year(ymd(DatesA))
        
        PeriodoRegistro = paste(c(format(as.Date(date1),'%Y'),"-",format(as.Date(date2),'%Y')),collapse="")
        
        setwd(dirname(dir_file))
        Carpeta_Out=paste0(basename(dir_file),"_Out_",PeriodoRegistro)
        dir.create(Carpeta_Out,showWarnings=F)
        dir_out1 = paste0(dirname(dir_file),"\\",Carpeta_Out)
        setwd(dir_file)
        
        N_ControlCalidad=1
        N_GenerarArchivoEstadisticas=1
        N_GraficasDiarias=1
        N_AgregarMensualyAnualyGraficas=1
        MatrixLimites = NULL # se reemplaza cuando se ejecuta ControlCalidadAtipicos, en todo el script solo se ejecuta una vez!!!!
        MatrixLimitesUp =NULL # se reemplaza cuando se ejecuta ControlCalidadAtipicos, en todo el script solo se ejecuta una vez!!!!
        N_GenerarPeriodoRegistro=1
        N_GenerarMapa=1
        N_ENSO=1
        N_RegresionesMensuales=1
        N_Promedios=1
        
        setwd(dir_out1)
        Carpeta_Out=as.character(clima.var2[clima[pos]])
        dir.create(Carpeta_Out,showWarnings=F)
        dir_out = paste(c(dir_out1,"\\",Carpeta_Out),collapse="")
        
  ##################### 

  #################### secciones  DIARIAS
  if (GenerarArchivoEstadisticas==TRUE) {
      
      #Creates table with "SD","Min", "Qu 1st","Median","Mean","Qu 3rd ","Max","NA's" for each station ordened by %NAs
      matrix = as.data.frame(matrix(NA, nrow=length(stations),ncol=20))
      colnames(matrix) = c("Station","Periodo","n Periodo","NAs Periodo","%NAs Periodo","Varianza","sd","CV","IQR","Asimetria","Curtosis","Min", "Qu 1st","Median","Mean","Qu 3rd ","Max","n","NAs","%NAs")
      matrix[,1] = as.character(stations)
      matrix[,2] = PeriodoRegistro
      matrix[,3] = length(DatesVentana)
      
      FechasRegistro=as.data.frame(matrix(NA,ncol=1,nrow=length(stations)))
      colnames(FechasRegistro)=c("Periodo Registro")
      
      for (s in 1:ncol(DataVentana)){
        matrix[s,4] = length(which(is.na(DataVentana[,s])))
        matrix[s,5] = round(matrix[s,4]/matrix[s,3]*100,2)
        if (length(which(is.na(DataVentana[,s])))<nrow(DataVentana)){
          x=cbind(DatesVentana,DataVentana[,s])
          x=na.exclude(x)
          r1=which(as.Date(x[1,1])==DatesVentana)
          r2=which(as.Date(x[nrow(x),1])==DatesVentana)
          DataVentana1=DataVentana[r1:r2,s]
          FechasRegistro[s,1]=paste(as.character(as.Date(x[1,1]))," - ",as.character(as.Date(x[nrow(x),1])),sep="")
          matrix[s,6:ncol(matrix)] = estadisticas(DataVentana1)
        } else {matrix[s,ncol(matrix)]=100}
      }
      
      writeMatrix=cbind(matrix,FechasRegistro)
      writeMatrix=writeMatrix[,c(1:5,21,18:20,6:17)]
      
      plotpath = paste(c(dir_out,"\\",N_GenerarArchivoEstadisticas,"_",type_fileComplete,"_Summary_",PeriodoRegistro,".csv"),collapse="")
      write.csv(writeMatrix[order(writeMatrix$`%NAs Periodo`),],plotpath, row.names=FALSE,na="")
      
      colnames(matrix) = c("Station","Periodo","n Periodo","NAs Periodo","%NAsP","Varianza","sd","CV","IQR","As","k","Min", "1stQu","Median","Mean","3rdQu","Max","n","NAs","%NAs")
      x=as.data.frame(matrix[order(matrix$`%NAsP`),])
      Datag = reshape2::melt(x[,c(1,7:17,20,5)],id.vars="Station", measure.vars=colnames(x)[c(7:17,20,5)]) #convierte el archivo en un archivo vertical donde una columna es la estacion 
      names(Datag)=c("Estacion","Estadistico","Valores")
      Datag$Estacion=factor(Datag$Estacion,levels=as.character(x[,1]))
      
      #grafica con el % de NAs en cada estacion ordenada en el eje x de menor a mayor %
      plotpath = paste(c(dir_out,"\\",N_GenerarArchivoEstadisticas,"_",type_fileComplete,"_Estadisticos_",PeriodoRegistro,".jpg"),collapse="")
      ggplot(Datag,aes(x=Estacion, y=Valores,colour=Estacion))+
        geom_bar(stat = "identity",aes(fill=Estacion),position = 'dodge')+
        xlab("Estaciones")+
        ylab("")+
        ggtitle(paste(c("Estadisticos ",type_fileComplete),collapse=""))+
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
        theme(legend.position="none")+
        #geom_text(aes(label=`%NAs`), position=position_dodge(width=0.9),size=3,colour="black",angle=90)
        facet_grid(Estadistico ~. ,scales="free")
      ggsave(plotpath,width =40 , height = 22,units = "cm")
      
      DataVentana[,which(matrix$`%NAs`==100)]=NULL
      stations =as.character(colnames(DataVentana))
      
      #N_GenerarArchivoEstadisticas=N_GenerarArchivoEstadisticas+1
    }
  if (ControlCalidad==TRUE) {
    #funcion calidad
    Calidad(DatesVentana,DataVentana)
    #N_ControlCalidad=N_ControlCalidad+1
  }
  if (GenerarPeriodoRegistro==TRUE){
    
    ## Plot a Period of Record Graph
    # the existence of data at each timestep with a blue line. 
    ## Plot time-series graphs of each of your climate stations 
    
    #create a Record Table, which will contain information as to the 
    #presence/absence of a data point at each timestep for each station.
    Record_Data = as.data.frame(!is.na(DataVentana))
    Record_Data[Record_Data == TRUE] = "No NA"
    Record_Data[Record_Data == FALSE] = "NA"
    #Record_Data1 = Record_Data + as.data.frame(matrix(rep(0:(ncol(Record_Data)-1),each=nrow(Record_Data)),ncol=ncol(Record_Data),nrow=nrow(Record_Data)))
    
    Record_Data=cbind(DatesVentana,Record_Data)
    colnames(Record_Data)=c("Dates",stations)
    #str(Record_Data[,1:5])
    
    dat.m = reshape2::melt(Record_Data,id.vars="Dates", measure.vars=names(Record_Data)[2:ncol(Record_Data)]) #convierte el archivo en un archivo vertical donde una columna es la estacion 
    names(dat.m)=c("Date","Estacion","Valores")
    dat.m$Date=rep(DatesVentana)
    #str(dat.m) 
    #head(dat.m)
    stations_data=levels(as.factor(as.character(dat.m$Estacion)))
    
    filename ="Periodo del Registro"
    plotpath = paste(c(dir_out, "\\",N_GenerarPeriodoRegistro,"_", filename," ",type_fileComplete," ",PeriodoRegistro, ".jpg"),collapse="") #creates a pdf path to produce a graphic of the span of records in the Data
    title=paste(c(filename,": ",type_fileComplete),collapse="") #creates a pdf path to produce a graphic of the span of records in the Data
    dat.m$Valores=factor(dat.m$Valores,levels=c("No NA","NA"))
    ggplot(dat.m,aes(x=Date, y=Estacion,colour=Valores))+
      geom_point(size=0.5)+
      scale_x_date(date_breaks= "1 years",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
      theme(axis.text.x=element_text(size=8, angle=90,hjust=0.95,vjust=0.2))+
      ylab("Estaciones")+
      xlab("Fecha")+
      scale_colour_manual(values=c("blue","red"), name=" ",labels=c("No NA","NA"))+
      ggtitle(title)+
      theme(plot.title = element_text(lineheight=.8, face="bold"))#+
    #theme(legend.position="none")
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    ########################################################################
    #######################################################################
    #create a Record Table, which will contain information as to the 
    #presence/absence of a data point at each timestep for each station.
    Record_Data = as.data.frame(!is.na(DataVentana))
    Record_Data[Record_Data == TRUE] = NA
    Record_Data[Record_Data == FALSE] = 1
    #Record_Data1 = Record_Data + as.data.frame(matrix(rep(0:(ncol(Record_Data)-1),each=nrow(Record_Data)),ncol=ncol(Record_Data),nrow=nrow(Record_Data)))
    
    Record_Data=cbind(DatesVentana,Record_Data)
    colnames(Record_Data)=c("Dates",stations)
    #str(Record_Data[,1:5])
    
    dat.m = reshape2::melt(Record_Data,id.vars="Dates", measure.vars=names(Record_Data)[2:ncol(Record_Data)]) #convierte el archivo en un archivo vertical donde una columna es la estacion 
    names(dat.m)=c("Date","Estacion","Valores")
    dat.m$Date=rep(DatesVentana)
    dat.m=na.exclude(dat.m)
    #str(dat.m) 
    #head(dat.m)
    stations_data=levels(as.factor(as.character(dat.m$Estacion)))
    
    filename ="Periodo del Registro NAs"
    plotpath = paste(c(dir_out, "\\",N_GenerarPeriodoRegistro,"_", filename," ",type_fileComplete," ",PeriodoRegistro, ".jpg"),collapse="") #creates a pdf path to produce a graphic of the span of records in the Data
    title=paste(c(filename,": ",type_fileComplete),collapse="") #creates a pdf path to produce a graphic of the span of records in the Data
    
    ggplot(dat.m,aes(x=Date, y=factor(Estacion)))+
      geom_point(colour="red",size=0.5)+
      scale_x_date(date_breaks= "1 years",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
      theme(axis.text.x=element_text(size=8, angle=90,hjust=0.95,vjust=0.2))+
      ylab("Estaciones")+
      xlab("Fecha")+
      ggtitle(title)+
      theme(plot.title = element_text(lineheight=.8, face="bold"))+
      theme(legend.position="none")
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    ########################################################################
    #######################################################################
    #create a Record Table, which will contain information as to the 
    #presence/absence of a data point at each timestep for each station.
    Record_Data = as.data.frame(!is.na(DataVentana))
    Record_Data[Record_Data == TRUE] = 1
    Record_Data[Record_Data == FALSE] = NA
    #Record_Data1 = Record_Data + as.data.frame(matrix(rep(0:(ncol(Record_Data)-1),each=nrow(Record_Data)),ncol=ncol(Record_Data),nrow=nrow(Record_Data)))
    
    Record_Data=cbind(DatesVentana,Record_Data)
    colnames(Record_Data)=c("Dates",stations)
    #str(Record_Data[,1:5])
    
    dat.m = reshape2::melt(Record_Data,id.vars="Dates", measure.vars=names(Record_Data)[2:ncol(Record_Data)]) #convierte el archivo en un archivo vertical donde una columna es la estacion 
    names(dat.m)=c("Date","Estacion","Valores")
    dat.m$Date=rep(DatesVentana)
    dat.m=na.exclude(dat.m)
    #str(dat.m) 
    #head(dat.m)
    stations_data=levels(as.factor(as.character(dat.m$Estacion)))
    
    filename ="Periodo del Registro Sin NAs"
    plotpath = paste(c(dir_out, "\\",N_GenerarPeriodoRegistro,"_", filename," ",type_fileComplete," ",PeriodoRegistro, ".jpg"),collapse="") #creates a pdf path to produce a graphic of the span of records in the Data
    title=paste(c(filename,": ",type_fileComplete),collapse="") #creates a pdf path to produce a graphic of the span of records in the Data
    
    ggplot(dat.m,aes(x=Date, y=Estacion))+
      geom_point(colour="blue",size=0.5)+
      scale_x_date(date_breaks= "1 years",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
      theme(axis.text.x=element_text(size=8, angle=90,hjust=0.95,vjust=0.2))+
      ylab("Estaciones")+
      xlab("Fecha")+
      ggtitle(title)+
      theme(plot.title = element_text(lineheight=.8, face="bold"))+
      theme(legend.position="none")
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    ########################################################################
    #######################################################################
    #create a Record Table, which will contain information as to the 
    #presence/absence of a data point at each timestep for each station.
    Record_Data = as.data.frame(!is.na(DataVentana))
    Record_Data[Record_Data == TRUE] = 1
    Record_Data[Record_Data == FALSE] =0
    Record_Data$N_Obs=rowSums(Record_Data)
    Record_Data=cbind(DatesVentana,Record_Data)
    #str(Record_Data)
    
    dat.m =Record_Data[,c(1,ncol(Record_Data))]  #convierte el archivo en un archivo vertical donde una columna es la estacion 
    names(dat.m)=c("Date","Valores")
    #str(dat.m) 
    #head(dat.m)
    
    filename ="Numero de Observaciones del Periodo de Registro"
    
    plotpath = paste(c(dir_out, "\\",N_GenerarPeriodoRegistro,"_", filename," ",PeriodoRegistro, ".csv"),collapse="") #creates a pdf path to produce a graphic of the span of records in the Data
    write.csv(dat.m,plotpath, row.names=FALSE,na="")
    
    plotpath = paste(c(dir_out, "\\",N_GenerarPeriodoRegistro,"_", filename," ",PeriodoRegistro, ".jpg"),collapse="") #creates a pdf path to produce a graphic of the span of records in the Data
    title=paste(c(filename,": ",type_fileComplete),collapse="") #creates a pdf path to produce a graphic of the span of records in the Data
    
    ggplot(dat.m,aes(x=Date, y=Valores))+
      geom_line(colour="blue",size=0.5)+
      scale_x_date(date_breaks= "1 years",date_minor_breaks = "1 year",limits=c(as.Date(date1),as.Date(date2)),labels = date_format("%Y"))+ 
      theme(axis.text.x=element_text(size=8, angle=90,hjust=0.95,vjust=0.2))+
      ylab("Numero de Observaciones")+
      xlab("Fecha")+
      ggtitle(title)+
      theme(plot.title = element_text(lineheight=.8, face="bold"))+
      theme(legend.position="none")+
      scale_y_continuous(breaks=1:length(stations), labels=1:length(stations), limits=c(0,length(stations)))
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    ########################################################################
    #######################################################################
    #N_GenerarPeriodoRegistro=N_GenerarPeriodoRegistro+1
   }  
  if (GenerarMapa==TRUE) {
    # Spatial Distribution
    
    # let's take a look at how the stations are __spatially distributed__ within your study site, 
    # and explore which stations have precipitation and/or temperature data available within any 
    # designated period of record. This will help us assess the availability of data in a spatial sense, 
    # again informing what an optimal period of record may be, and helping to inform us about any holes 
    # in data that may affect future workflows and analyses.
    
    ## Create a Station Map
    ########################################################################
    #######################################################################
    #create a Record Table, which will contain NA as to the 
    #presence/absence of a data point NA at each timestep for each station.
    Record_Data = as.data.frame(!is.na(DataVentana))
    Record_Data[Record_Data == TRUE] = 1
    Record_Data[Record_Data == FALSE] = NA
    Record_Data=cbind(DatesVentana,Record_Data)
    colnames(Record_Data)=c("Dates",as.character(stations))
    #str(Record_Data[,1:5])
    
    dat.m = reshape2::melt(Record_Data,id.vars="Dates", measure.vars=names(Record_Data)[2:ncol(Record_Data)]) #convierte el archivo en un archivo vertical donde una columna es la estacion 
    names(dat.m)=c("Date","Estacion","Valores")
    dat.m$Date=rep(DatesVentana)
    #str(dat.m) 
    dat.m=na.exclude(dat.m)
    #head(dat.m)
    stations_data=unique(as.character(dat.m$Estacion))
    
    coordinates_def =  Coordinates[which(as.character(Coordinates[,1]) %in% stations_data),] ## Coordinates for stations from which we are using precip data
    
    #map = leaflet() %>%  addTiles(group = "OpenStreetMap")
    #for (i in 1:nrow(coordinates_def)){
    #  map = map %>%
    #    addCircleMarkers(lng = coordinates_def$Longitude[i], lat = coordinates_def$Latitude[i], 
    #                     radius = 8, weight = 1, opacity = 0.7, fillOpacity = 0.7, label = coordinates_def$Station[i],
    #                     labelOptions = labelOptions(noHide = T))
    #}
    # map
    

    ####### mapa estaciones
    map <- get_map(c(left = min(coordinates_def$Longitude)-0.5, bottom = min(coordinates_def$Latitude)-0.5, 
                     right = max(coordinates_def$Longitude)+0.5, top = max(coordinates_def$Latitude)+0.5))
    #ggmap(map)
    
    project = ggplot2::fortify(readOGR(dir_file,file_ShapeCatchs))
    mapEstaciones=ggmap(map,maprange = TRUE,legend = "right",extent = "panel") + # plots the backgound map
      geom_polygon(aes(x=long, y=lat,group = group),size=0.5,colour="darkgreen",data=project,alpha=0.4) # plots the polygon of your project area
    #mapEstaciones
    if (IncluirRios==TRUE) {
        projectRios = fortify(readOGR(dir_file,file_ShapeRios))
        mapEstaciones=mapEstaciones+
          geom_path(aes(x=long, y=lat, group = group),size=0.6,colour="blue4",data=projectRios,alpha=0.4) # plots the polygon of your project area
    } 
    #mapEstaciones
    mapEstaciones=
      mapEstaciones+
      #geom_point(data = as.data.frame(Coordinates), aes(x=as.numeric(Coordinates[,"Longitude"]), y=as.numeric(Coordinates[,"Latitude"])),
      #           colour='white',size=3,shape=19)+ # plots all the stations in black circles
      #geom_text_repel(data = Coordinates, aes(x=as.numeric(Coordinates[,"Longitude"]), y=as.numeric(Coordinates[,"Latitude"]), 
      #                                        label = stations_project),color="White",size=labelsize) + # plots the names of all the stations
      labs(title=map_title)+ # adds a title to the map
      geom_point(data = as.data.frame(coordinates_def), 
                 aes(x=as.numeric(coordinates_def[,"Longitude"]), y=as.numeric(coordinates_def[,"Latitude"])),
                 colour="blue",size=2.3,shape=17) + # plots all the stations in black circles
      geom_text_repel(data = coordinates_def, aes(x=coordinates_def$Longitude, y=coordinates_def$Latitude, 
                                              label = coordinates_def$Station), color="black",size=labelsize) +  # plots the names of all the stations
      theme(panel.background = element_rect(fill = alpha("grey85", 0)), panel.ontop = TRUE, legend.position = "none",panel.grid = element_line(size=0.3))
    mapEstaciones             
    
    pathmap = paste(c(dir_out,"\\",N_GenerarMapa,"_",map_title," ",PeriodoRegistro,".jpg"),collapse="")
    ggsave(pathmap) # saves the map as a JPG
    #N_GenerarMapa=N_GenerarMapa+1
  }
  if (GraficasDiarias==TRUE){
    
    #obtener los datos a graficar
    x=cbind(DatesVentana,DataVentana)
    dat.m = reshape2::melt(x,id.vars="DatesVentana", measure.vars=as.character(colnames(DataVentana))) #convierte el archivo en un archivo vertical donde una columna es la estacion 
    colnames(dat.m)=c("Dates","Estacion","Valores")
    dat.m$Valores=as.numeric(dat.m$Valores)
    dat.m$Dates=rep(DatesVentana)
    dat.m$MesDia=rep(MesDiaVentana)
    dat.m$MesDiaf=MesDiaVentanaFormat
    #dat.m = na.exclude(dat.m)
    #asignar los niveles, estaciones en orden alfabetico
    dat.m[,2] =factor(dat.m[,2], levels=stations)
    
    levels=dat.m$MesDia[order(unique(dat.m$MesDia))]; levels[60]=229
    levels1=dat.m$MesDiaf[order(unique(dat.m$MesDia))];levels1[60]="29 feb."
    w=c(1,   8,  16,  24,  31,  38,  46,  53,  60,  68,  76,  84,  91,  98, 106, 114, 121, 128, 136, 144, 152, 160, 167, 174, 182, 190, 198, 206, 213, 220, 228, 236,
        244, 252, 259, 266, 274, 282, 290, 298, 305, 312, 320, 328, 335, 342, 350, 358, 366)
    m=c(31,  121 ,213, 335) #meses c(1 , 31,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
    breaks=levels[w]
    labelsEDA=levels1[w]
    labelsEDA[!(w %in% m)]=""
    dat.m$MesDia=factor(as.character(dat.m$MesDia),levels=levels)
    dat.m=na.exclude(dat.m)

    GraficasEDA(dat.m=dat.m,dir_out=dir_out,type_fileComplete=type_fileComplete,N_Graficas=N_GraficasDiarias,addggtitle="") 
    
    #N_GraficasDiarias=N_GraficasDiarias+1
  }
  if (clima.var2[clima[pos]]=="precipitacion") {DataVentanaPrecipitacion=DataVentana}
  if (clima.var2[clima[pos]]=="caudal") {DataVentanaCaudal=DataVentana}

  ####################MENSUALES Y ANUALES
  if (AgregarMensualyAnual==TRUE){
    
    Data_a_MensualyAnual(DataVentana)
    
    plotpath = paste(c(dir_out,"\\",N_AgregarMensualyAnualyGraficas,"_",clima.var2[clima[pos]]," Mensual_",PeriodoRegistro,".csv"),collapse="")
    write.csv(cbind(YearMonthDay_Mensual,DataVentanaMensual),plotpath, row.names=FALSE,na="")
    
    plotpath = paste(c(dir_out,"\\",N_AgregarMensualyAnualyGraficas,"_",clima.var2[clima[pos]]," Anual_",PeriodoRegistro,".csv"),collapse="")
    write.csv(cbind(YearMonthDay_Anual,DataVentanaAnual),plotpath, row.names=FALSE,na="")
   
    #N_AgregarMensualyAnualyGraficas=N_AgregarMensualyAnualyGraficas+1
  }
  if (Promedios==TRUE) {
    
    DataPromedio=cbind(MesDiaVentana,DataVentana)
    DataPromedio=aggregate(DataPromedio[,2:ncol(DataPromedio)], by = list(MesDia = DataPromedio$MesDiaVentana),mean, na.rm=TRUE)
    DataPromedio$Mes=DataPromedio$MesDia%/%100
    DataPromedio$Dia=DataPromedio$MesDia-DataPromedio$Mes*100
    DataPromedio=DataPromedio[,c((ncol(DataPromedio)-1):ncol(DataPromedio),1:(ncol(DataPromedio)-2))]
    colnames(DataPromedio)=c("Month","Day","Month-Day", colnames(DataVentana))
    
    plotpath = paste(c(dir_out,"\\",N_Promedios," ",clima.var2[clima[pos]]," promedio Diaria multianual ",PeriodoRegistro,".csv"),collapse="")
    write.csv(DataPromedio,plotpath, row.names=FALSE,na="")
    
    DataPromedio=cbind(YearMonthDay_Mensual,DataVentanaMensual)
    DataPromedio=aggregate(DataPromedio[,4:ncol(DataPromedio)], by = list(Month = DataPromedio$Month),mean, na.rm=TRUE)
     
    plotpath = paste(c(dir_out,"\\",N_Promedios," ",clima.var2[clima[pos]]," promedio Mensual multianual ",PeriodoRegistro,".csv"),collapse="")
    write.csv(DataPromedio,plotpath, row.names=FALSE,na="")
    
    DataPromedio=DataVentanaAnual
    DataPromedio=colMeans(DataPromedio, na.rm=TRUE)
    
    plotpath = paste(c(dir_out,"\\",N_Promedios," ",clima.var2[clima[pos]]," promedio Anual multianual ",PeriodoRegistro,".csv"),collapse="")
    write.csv(t(DataPromedio),plotpath, row.names=FALSE,na="")
    
    #N_Promedios=N_Promedios+1
  }
  if (AgregarMensualyAnual==TRUE && MensualyGraficas==TRUE){
    #############
    #### Mensuales
    type_fileComplete2=paste(c(clima.var2[clima[pos]]," Mensual"),collapse = "")
    dat.m=DataVentanaMensual_v
    dat.m$Month=factor(dat.m$Month)
    dat.m$Dates=rep(as.Date(DatesM))
    dat.m$MesDia=dat.m$Month
    
    GraficasEDA_M(dat.m=dat.m,dir_out=dir_out,type_fileComplete=type_fileComplete2,N_Graficas=N_AgregarMensualyAnualyGraficas,addggtitle="") 
    
    #estadisticos 
    #Creates table with "SD","Min", "Qu 1st","Median","Mean","Qu 3rd ","Max","NA's" for each station ordened by %NAs
    matrix = as.data.frame(matrix(NA, nrow=length(stations),ncol=17))
    colnames(matrix) = c("Station","Periodo","Varianza","Desv Est","CV","IQR","Asimetria","Curtosis","Min", "Qu 1st","Median","Mean","Qu 3rd ","Max","n","NAs","%NAs")
    matrix[,1] = as.character(stations)
    matrix[,2] = PeriodoRegistro
    
    x=DataVentanaMensual
    for (s in 1:ncol(x)){
      matrix[s,3:ncol(matrix)] = estadisticas(x[,s])
    }
    
    plotpath = paste(c(dir_out,"\\",N_AgregarMensualyAnualyGraficas,"_",type_fileComplete2,"_Summary_",PeriodoRegistro,".csv"),collapse="")
    write.csv(matrix[order(matrix$`%NAs`),],plotpath, row.names=FALSE,na="")
    
    x=matrix[order(matrix$`%NAs`),]
    Datag = reshape2::melt(x,id.vars="Station", measure.vars=names(x)[c(4:14,17)]) #convierte el archivo en un archivo vertical donde una columna es la estacion 
    names(Datag)=c("Estacion","Estadistico","Valores")
    Datag$Estacion=factor(Datag$Estacion,levels=as.character(x[,1]))
    
    #grafica con el % de NAs en cada estacion ordenada en el eje x de menor a mayor %
    plotpath = paste(c(dir_out,"\\",N_AgregarMensualyAnualyGraficas,"_",type_fileComplete2,"_Estadisticos_",PeriodoRegistro,".jpg"),collapse="")
    ggplot(Datag,aes(x=Estacion, y=Valores,colour=Estacion))+
      geom_bar(stat = "identity",aes(fill=Estacion),position = 'dodge')+
      xlab("Estaciones")+
      ylab("")+
      ggtitle(paste(c("Estadisticos ",type_fileComplete2),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))+
      theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
      theme(legend.position="none")+
      #geom_text(aes(label=`%NAs`), position=position_dodge(width=0.9),size=3,colour="black",angle=90)
      facet_grid(Estadistico ~. ,scales="free")
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    #############
    #N_AgregarMensualyAnualyGraficas=N_AgregarMensualyAnualyGraficas+1

    }
  if (AgregarMensualyAnual==TRUE && AnualyGraficas==TRUE){
    
    #############
    #### Anuales
    type_fileComplete2=paste(c(clima.var2[clima[pos]]," Anual"),collapse = "")
    dat.m=DataVentanaAnual_v
    dat.m$Year=factor(dat.m$Year)
    dat.m$Dates=rep(as.Date(DatesA))
    dat.m$MesDia=dat.m$Year
    
    GraficasEDA_A(dat.m=dat.m,dir_out=dir_out,type_fileComplete=type_fileComplete2,N_Graficas=N_AgregarMensualyAnualyGraficas,addggtitle="") 
    
    #estadisticos 
    #Creates table with "SD","Min", "Qu 1st","Median","Mean","Qu 3rd ","Max","NA's" for each station ordened by %NAs
    matrix = as.data.frame(matrix(NA, nrow=length(stations),ncol=17))
    colnames(matrix) = c("Station","Periodo","Varianza","Desv Est","CV","IQR","Asimetria","Curtosis","Min", "Qu 1st","Median","Mean","Qu 3rd ","Max","n","NAs","%NAs")
    matrix[,1] = as.character(stations)
    matrix[,2] = PeriodoRegistro
    
    x=DataVentanaAnual
    for (s in 1:ncol(x)){
      matrix[s,3:ncol(matrix)] = estadisticas(x[,s])
    }
    
    plotpath = paste(c(dir_out,"\\",N_AgregarMensualyAnualyGraficas,"_",type_fileComplete2,"_Summary_",PeriodoRegistro,".csv"),collapse="")
    write.csv(matrix[order(matrix$`%NAs`),],plotpath, row.names=FALSE,na="")
    
    x=matrix[order(matrix$`%NAs`),]
    Datag = reshape2::melt(x,id.vars="Station", measure.vars=names(x)[c(4:14,17)]) #convierte el archivo en un archivo vertical donde una columna es la estacion 
    names(Datag)=c("Estacion","Estadistico","Valores")
    Datag$Estacion=factor(Datag$Estacion,levels=as.character(x[,1]))
    
    #grafica con el % de NAs en cada estacion ordenada en el eje x de menor a mayor %
    plotpath = paste(c(dir_out,"\\",N_AgregarMensualyAnualyGraficas,"_",type_fileComplete2,"_Estadisticos_",PeriodoRegistro,".jpg"),collapse="")
    ggplot(Datag,aes(x=Estacion, y=Valores,colour=Estacion))+
      geom_bar(stat = "identity",aes(fill=Estacion),position = 'dodge')+
      xlab("Estaciones")+
      ylab("")+
      ggtitle(paste(c("Estadisticos ",type_fileComplete2),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))+
      theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
      theme(legend.position="none")+
      #geom_text(aes(label=`%NAs`), position=position_dodge(width=0.9),size=3,colour="black",angle=90)
      facet_grid(Estadistico ~. ,scales="free")
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    #############
    #N_AgregarMensualyAnualyGraficas=N_AgregarMensualyAnualyGraficas+1
    }
  if (RegresionesMensuales==TRUE) {
    setwd(dir_out)
    Carpeta_Out=as.character("RegresionesMensuales")
    dir.create(Carpeta_Out,showWarnings=F)
    dir_outg = paste(c(dir_out,"\\",Carpeta_Out),collapse="")
    
    DataRegresion=cbind(YearMonthDay_Mensual,DataVentanaMensual)
    dat.m = reshape2::melt(DataRegresion,id.vars="Month", measure.vars=colnames(DataRegresion)[4:ncol(DataRegresion)]) #convierte el archivo en un archivo vertical donde una columna es la estacion 
    colnames(dat.m)=c("Month","Estacion","Valores")
    
    h_estacionesDataRegresion = data.frame(h_estaciones[which(as.character(h_estaciones[,1]) %in% colnames(DataVentanaMensual)),])
    colnames(h_estacionesDataRegresion)=c("Estacion","h")
    h_estacionesDataRegresion$h=as.numeric(as.character(h_estacionesDataRegresion$h))
    dat.m=merge(dat.m,h_estacionesDataRegresion,by="Estacion")
    
    intsl=as.data.frame(matrix(NA,nrow=12,ncol=4))
    colnames(intsl)=c("Month","slope","int","Cor")
    for (s in 1:12){
      y = dat.m$Valores[which(dat.m$Month==s)]
      x = dat.m$h[which(dat.m$Month==s)]
      lmxy=lm(y~x)
      intsl[s,1] = s
      intsl[s,2] = as.numeric(lm(y~x)$coefficients[2])          # Compute the line slope
      intsl[s,3] = as.numeric(lm(y~x)$coefficients[1])          # Compute the line intercept
      intsl[s,4] = cor(x, y, use = "pairwise.complete.obs", method = "pearson")
    }
    
    plotpath = paste(c(dir_outg,"\\",N_RegresionesMensuales," RegresionesMensuales_Mensual_",PeriodoRegistro,".csv"),collapse="")
    write.csv(intsl,plotpath, row.names=FALSE,na="")
    
    plotpath = paste(c(dir_outg,"\\",N_RegresionesMensuales," RegresionesMensuales__Mensual_",PeriodoRegistro,".jpg"),collapse="")
    dat.m=na.exclude(dat.m)
    #str(dat.m) 
    #head(dat.m)
    d= ggplot(dat.m,aes(x=h, y=Valores,colour=Month))+
      geom_point(size=0.6)+
      geom_abline(data=intsl, aes(intercept=int, slope=slope), col="black",size=1)+
      ylab(paste(c(clima.var2[clima[pos]]," ","Diaria"),collapse=""))+
      xlab(paste(c("h"),collapse=""))+
      ggtitle(paste(c("Regresion lineal mensual ",clima.var2[clima[pos]]," ","diaria"),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))+
      facet_wrap(~ Month, scales = "free")+
      theme(legend.position="none")
    
    datalevels=data.frame(x=Inf, y=Inf, label=as.character(intsl[,4]),color="black",Month=1:12)
    datalevels$label=as.character(datalevels$label)
    for (n in 1:nrow(datalevels)){
      datalevels[n,3]=paste(c("T=",round(intsl[n,3],2),round(intsl[n,2],5),"h"," Corr= ",round(intsl[n,4],2)),collapse="")
    }
    #str(datalevels)
    
    d + geom_text(data=datalevels,aes(x,y,label=label, group=NULL),inherit.aes=FALSE,vjust=1,hjust=1) 
    
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    #N_RegresionesMensuales = N_RegresionesMensuales+1
    
    #library(GGally)
    #ggpairs(DataD[,1:15]) # no funciona bien para mas de 15 columnas
    #ggscatmat(DataD[,1:15]) # no funciona bien para mas de 15 columnas
  }
  if (Analisis_ENSO==TRUE) {
    
    setwd(dir_out)
    Carpeta_Outg=paste(c(N_ENSO,"_ENSO_",PeriodoRegistro),collapse="")
    dir.create(Carpeta_Outg,showWarnings=F)
    dir_outg = paste(c(dir_out,"\\",Carpeta_Outg),collapse="")
    
    Valores_ENSO(date1, date2)
    
    plotpath = paste(c(dir_outg,"\\","ENSO_",PeriodoRegistro,".csv"),collapse="")
    write.csv(ENSOVentana,plotpath, row.names=FALSE,na="")
    
    ENSOVentana_g=ENSOVentana
    ENSOVentana_g$ENSO_COD=factor(ENSOVentana_g$ENSO_COD)
    plotpath=paste(c(dir_outg,"\\","ENSO_",PeriodoRegistro,".jpg"),collapse="")
    ggplot(ENSOVentana_g,aes(x=Month,y=Year,fill=ENSO_COD))+
      geom_tile(colour="grey50")+
      xlab("Meses")+
      ylab("A?os")+
      scale_x_continuous(breaks=1:12, labels=namesMonth)+
      scale_y_continuous(breaks=min(ENSOVentana_g$Year):max(ENSOVentana_g$Year), labels=unique(ENSOVentana_g$Year))+
      theme(axis.text.x=element_text(vjust=0.5, size=10))+
      scale_fill_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name="", breaks=c(3,2,1),labels=c("Ni?o","Normal","Ni?a"))+
      ggtitle(paste(c("Fases ENSO ",PeriodoRegistro),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    plotpath=paste(c(dir_outg,"\\","ONI_",PeriodoRegistro,".jpg"),collapse="")
    ggplot(ENSOVentana, aes(x=Date, y=ANOM)) +  
      geom_bar(stat = "identity",   aes(fill = ENSO))+
      ylab(paste(c("ONI"),collapse=""))+
      scale_fill_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ", breaks=c("Ni?o","Normal","Ni?a"),labels=c("Ni?o","Normal","Ni?a"))+
      scale_x_date(date_breaks= "1 year",date_minor_breaks = "1 month",limits=c(as.Date(date1),as.Date(date2)),date_labels =("%Y"))+ 
      theme(axis.text.x=element_text(size=8, angle=90,hjust=0.95,vjust=0.2))+
      ggtitle(paste(c("ONI"),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))+
      xlab("Fecha")+
      geom_hline(aes(yintercept=0.5))+
      geom_hline(aes(yintercept=-0.5))
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    Data_a_MensualyAnual(DataVentana)
    
    G_ENSO_AnomMensual(dir_outg)
    
    G_ENSO_PromMensual(dir_outg)
    
    G_ENSO_boxplot(dir_outg)
  }    
        
}

difftime(Sys.time(),start)


if (CorrelogramaPQ==TRUE && exists("DataVentanaPrecipitacion") && exists("DataVentanaCaudal")) {
  
  setwd(dir_out1)
  Carpeta_Outg=paste(c("Correlograma_PQ"),collapse="")
  dir.create(Carpeta_Outg,showWarnings=F)
  dir_outg = paste(c(dir_out1,"\\",Carpeta_Outg),collapse="")
  setwd(dir_outg)
  
  #correlogramas
  ##########################################################################################################################
  CCQP=1
  #diario
  DataQ=DataVentanaCaudal
  DataP=DataVentanaPrecipitacion
  periodo="diario"
  if (CCQP==1){
  #pearson
  method="pearson"
  cor_PQ=matrix(NA,nrow=ncol(DataQ),ncol=ncol(DataP))
  colnames(cor_PQ)=colnames(DataP)
  rownames(cor_PQ)= colnames(DataQ)
  for (q in 1:ncol(DataQ)) {
    for (p in 1:ncol(DataP)){
      dataQP=cbind(DataQ[,q],DataP[,p])
      cor_PQ[q,p] = round(cor(dataQP, use = "pairwise.complete.obs", method =method),2)[1,2]
    }
  }
  
  plotpath = paste(c(dir_outg,"\\CorrelogramaPQ_",method," ",periodo," ",PeriodoRegistro,".csv"),collapse="")
  write.csv(cor_PQ,plotpath, row.names=TRUE,na="")
  
  dat.m=reshape2::melt(cor_PQ)
  colnames(dat.m)=c("EstacionQ","EstacionP","Corr")
  dat.m$EstacionQ=factor(dat.m$EstacionQ,levels=as.character(rownames(cor_PQ)))
  dat.m$EstacionP=factor(dat.m$EstacionP,levels=as.character(colnames(cor_PQ)))
  #str(dat.m)
  
  plotpath = paste(c(dir_outg,"\\","Coeficiente de CorrelacionPQ_",method," ",periodo," ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m, aes(EstacionQ,EstacionP, fill=Corr)) +
    geom_tile(height=0.8, width=0.8) +
    scale_fill_gradient2(mid="white", high="darkred", low="blue3",midpoint=0,limits=c(-1, 1)) +
    theme_minimal() +
    coord_equal() +
    labs(x="Caudal",y="Precipitacion",fill="Corr") +
    #geom_text(aes(label= as.character(dat.m$Corr)))+
    theme(axis.text.x=element_text( angle=90, hjust=0.95,vjust=0.2, margin=margin(-3,0,0,0)),
          axis.text.y=element_text( margin=margin(0,-3,0,0)),
          panel.grid.major=element_blank()) +  
    ggtitle(paste(c("Coeficiente de Correlacion PQ ",periodo," ",method),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  #spearman
  method="spearman"
  cor_PQ=matrix(NA,nrow=ncol(DataQ),ncol=ncol(DataP))
  colnames(cor_PQ)=colnames(DataP)
  rownames(cor_PQ)= colnames(DataQ)
  for (q in 1:ncol(DataQ)) {
    for (p in 1:ncol(DataP)){
      dataQP=cbind(DataQ[,q],DataP[,p])
      cor_PQ[q,p] = round(cor(dataQP, use = "pairwise.complete.obs", method =method),2)[1,2]
    }
  }
  
  plotpath = paste(c(dir_outg,"\\CorrelogramaPQ_",method," ",periodo," ",PeriodoRegistro,".csv"),collapse="")
  write.csv(cor_PQ,plotpath, row.names=TRUE,na="")
  
  dat.m=reshape2::melt(cor_PQ)
  colnames(dat.m)=c("EstacionQ","EstacionP","Corr")
  dat.m$EstacionQ=factor(dat.m$EstacionQ,levels=as.character(rownames(cor_PQ)))
  dat.m$EstacionP=factor(dat.m$EstacionP,levels=as.character(colnames(cor_PQ)))
  #str(dat.m)
  
  plotpath = paste(c(dir_outg,"\\","Coeficiente de CorrelacionPQ_",method," ",periodo," ",PeriodoRegistro,".jpg"),collapse="")
  ggplot(dat.m, aes(EstacionQ,EstacionP, fill=Corr)) +
    geom_tile(height=0.8, width=0.8) +
    scale_fill_gradient2(mid="white", high="darkred", low="blue3",midpoint=0,limits=c(-1, 1)) +
    theme_minimal() +
    coord_equal() +
    labs(x="Caudal",y="Precipitacion",fill="Corr") +
    #geom_text(aes(label= as.character(dat.m$Corr)))+
    theme(axis.text.x=element_text( angle=90, hjust=0.95,vjust=0.2, margin=margin(-3,0,0,0)),
          axis.text.y=element_text( margin=margin(0,-3,0,0)),
          panel.grid.major=element_blank()) +  
    ggtitle(paste(c("Coeficiente de Correlacion PQ ",periodo," ",method),collapse=""))+
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  }
  
  Data_a_MensualyAnual(DataVentana=DataVentanaPrecipitacion)
  DataP=DataVentanaMensual
  rm(DataVentanaMensual)
  Data_a_MensualyAnual(DataVentanaCaudal)
  DataQ=DataVentanaMensual
  periodo="mensual"
  if (CCQP==1){
    #pearson
    method="pearson"
    cor_PQ=matrix(NA,nrow=ncol(DataQ),ncol=ncol(DataP))
    colnames(cor_PQ)=colnames(DataP)
    rownames(cor_PQ)= colnames(DataQ)
    for (q in 1:ncol(DataQ)) {
      for (p in 1:ncol(DataP)){
        dataQP=cbind(DataQ[,q],DataP[,p])
        cor_PQ[q,p] = round(cor(dataQP, use = "pairwise.complete.obs", method =method),2)[1,2]
      }
    }
    
    plotpath = paste(c(dir_outg,"\\CorrelogramaPQ_",method," ",periodo," ",PeriodoRegistro,".csv"),collapse="")
    write.csv(cor_PQ,plotpath, row.names=TRUE,na="")
    
    dat.m=reshape2::melt(cor_PQ)
    colnames(dat.m)=c("EstacionQ","EstacionP","Corr")
    dat.m$EstacionQ=factor(dat.m$EstacionQ,levels=as.character(rownames(cor_PQ)))
    dat.m$EstacionP=factor(dat.m$EstacionP,levels=as.character(colnames(cor_PQ)))
    #str(dat.m)
    
    plotpath = paste(c(dir_outg,"\\","Coeficiente de CorrelacionPQ_",method," ",periodo," ",PeriodoRegistro,".jpg"),collapse="")
    ggplot(dat.m, aes(EstacionQ,EstacionP, fill=Corr)) +
      geom_tile(height=0.8, width=0.8) +
      scale_fill_gradient2(mid="white", high="darkred", low="blue3",midpoint=0,limits=c(-1, 1)) +
      theme_minimal() +
      coord_equal() +
      labs(x="Caudal",y="Precipitacion",fill="Corr") +
      #geom_text(aes(label= as.character(dat.m$Corr)))+
      theme(axis.text.x=element_text( angle=90, hjust=0.95,vjust=0.2, margin=margin(-3,0,0,0)),
            axis.text.y=element_text( margin=margin(0,-3,0,0)),
            panel.grid.major=element_blank()) +  
      ggtitle(paste(c("Coeficiente de Correlacion PQ ",periodo," ",method),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    #spearman
    method="spearman"
    cor_PQ=matrix(NA,nrow=ncol(DataQ),ncol=ncol(DataP))
    colnames(cor_PQ)=colnames(DataP)
    rownames(cor_PQ)= colnames(DataQ)
    for (q in 1:ncol(DataQ)) {
      for (p in 1:ncol(DataP)){
        dataQP=cbind(DataQ[,q],DataP[,p])
        cor_PQ[q,p] = round(cor(dataQP, use = "pairwise.complete.obs", method =method),2)[1,2]
      }
    }
    
    plotpath = paste(c(dir_outg,"\\CorrelogramaPQ_",method," ",periodo," ",PeriodoRegistro,".csv"),collapse="")
    write.csv(cor_PQ,plotpath, row.names=TRUE,na="")
    
    dat.m=reshape2::melt(cor_PQ)
    colnames(dat.m)=c("EstacionQ","EstacionP","Corr")
    dat.m$EstacionQ=factor(dat.m$EstacionQ,levels=as.character(rownames(cor_PQ)))
    dat.m$EstacionP=factor(dat.m$EstacionP,levels=as.character(colnames(cor_PQ)))
    #str(dat.m)
    
    plotpath = paste(c(dir_outg,"\\","Coeficiente de CorrelacionPQ_",method," ",periodo," ",PeriodoRegistro,".jpg"),collapse="")
    ggplot(dat.m, aes(EstacionQ,EstacionP, fill=Corr)) +
      geom_tile(height=0.8, width=0.8) +
      scale_fill_gradient2(mid="white", high="darkred", low="blue3",midpoint=0,limits=c(-1, 1)) +
      theme_minimal() +
      coord_equal() +
      labs(x="Caudal",y="Precipitacion",fill="Corr") +
      #geom_text(aes(label= as.character(dat.m$Corr)))+
      theme(axis.text.x=element_text( angle=90, hjust=0.95,vjust=0.2, margin=margin(-3,0,0,0)),
            axis.text.y=element_text( margin=margin(0,-3,0,0)),
            panel.grid.major=element_blank()) +  
      ggtitle(paste(c("Coeficiente de Correlacion PQ ",periodo," ",method),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    ggsave(plotpath,width =40 , height = 22,units = "cm")
  }
  
  Data_a_MensualyAnual(DataVentanaPrecipitacion)
  DataP=DataVentanaAnual
  rm(DataVentanaAnual)
  Data_a_MensualyAnual(DataVentanaCaudal)
  DataQ=DataVentanaAnual
  periodo="anual"
  if (CCQP==1){
    #pearson
    method="pearson"
    cor_PQ=matrix(NA,nrow=ncol(DataQ),ncol=ncol(DataP))
    colnames(cor_PQ)=colnames(DataP)
    rownames(cor_PQ)= colnames(DataQ)
    for (q in 1:ncol(DataQ)) {
      for (p in 1:ncol(DataP)){
        dataQP=cbind(DataQ[,q],DataP[,p])
        cor_PQ[q,p] = round(cor(dataQP, use = "pairwise.complete.obs", method =method),2)[1,2]
      }
    }
    
    plotpath = paste(c(dir_outg,"\\CorrelogramaPQ_",method," ",periodo," ",PeriodoRegistro,".csv"),collapse="")
    write.csv(cor_PQ,plotpath, row.names=TRUE,na="")
    
    dat.m=reshape2::melt(cor_PQ)
    colnames(dat.m)=c("EstacionQ","EstacionP","Corr")
    dat.m$EstacionQ=factor(dat.m$EstacionQ,levels=as.character(rownames(cor_PQ)))
    dat.m$EstacionP=factor(dat.m$EstacionP,levels=as.character(colnames(cor_PQ)))
    #str(dat.m)
    
    plotpath = paste(c(dir_outg,"\\","Coeficiente de CorrelacionPQ_",method," ",periodo," ",PeriodoRegistro,".jpg"),collapse="")
    ggplot(dat.m, aes(EstacionQ,EstacionP, fill=Corr)) +
      geom_tile(height=0.8, width=0.8) +
      scale_fill_gradient2(mid="white", high="darkred", low="blue3",midpoint=0,limits=c(-1, 1)) +
      theme_minimal() +
      coord_equal() +
      labs(x="Caudal",y="Precipitacion",fill="Corr") +
      #geom_text(aes(label= as.character(dat.m$Corr)))+
      theme(axis.text.x=element_text( angle=90, hjust=0.95,vjust=0.2, margin=margin(-3,0,0,0)),
            axis.text.y=element_text( margin=margin(0,-3,0,0)),
            panel.grid.major=element_blank()) +  
      ggtitle(paste(c("Coeficiente de Correlacion PQ ",periodo," ",method),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    #spearman
    method="spearman"
    cor_PQ=matrix(NA,nrow=ncol(DataQ),ncol=ncol(DataP))
    colnames(cor_PQ)=colnames(DataP)
    rownames(cor_PQ)= colnames(DataQ)
    for (q in 1:ncol(DataQ)) {
      for (p in 1:ncol(DataP)){
        dataQP=cbind(DataQ[,q],DataP[,p])
        cor_PQ[q,p] = round(cor(dataQP, use = "pairwise.complete.obs", method =method),2)[1,2]
      }
    }
    
    plotpath = paste(c(dir_outg,"\\CorrelogramaPQ_",method," ",periodo," ",PeriodoRegistro,".csv"),collapse="")
    write.csv(cor_PQ,plotpath, row.names=TRUE,na="")
    
    dat.m=reshape2::melt(cor_PQ)
    colnames(dat.m)=c("EstacionQ","EstacionP","Corr")
    dat.m$EstacionQ=factor(dat.m$EstacionQ,levels=as.character(rownames(cor_PQ)))
    dat.m$EstacionP=factor(dat.m$EstacionP,levels=as.character(colnames(cor_PQ)))
    #str(dat.m)
    
    plotpath = paste(c(dir_outg,"\\","Coeficiente de CorrelacionPQ_",method," ",periodo," ",PeriodoRegistro,".jpg"),collapse="")
    ggplot(dat.m, aes(EstacionQ,EstacionP, fill=Corr)) +
      geom_tile(height=0.8, width=0.8) +
      scale_fill_gradient2(mid="white", high="darkred", low="blue3",midpoint=0,limits=c(-1, 1)) +
      theme_minimal() +
      coord_equal() +
      labs(x="Caudal",y="Precipitacion",fill="Corr") +
      #geom_text(aes(label= as.character(dat.m$Corr)))+
      theme(axis.text.x=element_text( angle=90, hjust=0.95,vjust=0.2, margin=margin(-3,0,0,0)),
            axis.text.y=element_text( margin=margin(0,-3,0,0)),
            panel.grid.major=element_blank()) +  
      ggtitle(paste(c("Coeficiente de Correlacion PQ ",periodo," ",method),collapse=""))+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    ggsave(plotpath,width =40 , height = 22,units = "cm")
  }
  ##########################################################################################################################
  
  setwd(dir_out1)
  Carpeta_Outg=paste(c("Graficas_PQ"),collapse="")
  dir.create(Carpeta_Outg,showWarnings=F)
  dir_outg = paste(c(dir_out1,"\\",Carpeta_Outg),collapse="")
  setwd(dir_outg)
  dir_outg1=dir_outg
  
  DataQ=DataVentanaCaudal
  DataP=DataVentanaPrecipitacion
  
  setwd(dir_file)
  #convertir m3/s a mm diarios
  AreasQ=read.csv(AreasQ_File, check.names = F, stringsAsFactors = F,header = T)
  for (i in 1:ncol(DataQ)){
    DataQ[,i]=DataQ[,i]/rep(AreasQ[which(colnames(DataQ)[1]==AreasQ$Estacion),2],nrow(DataQ))*60*60*24*1000
  }
  
  setwd(dir_outg1)
  Carpeta_Outg="Serie completa"
  dir.create(Carpeta_Outg,showWarnings=F)
  dir_outg = paste(c(dir_outg1,"\\",Carpeta_Outg),collapse="")
  setwd(dir_outg)
  #q/p for all the time serie
  ##########################################################################################################################
  dbp=cbind(DatesVentana,DataP)
  filep=reshape2::melt(dbp,"DatesVentana")
  colnames(filep)=c("Dates","Gauge","pm")
  filep$EM=paste0(as.character(filep$Dates),filep$Gauge)
  
  dbq=cbind(DatesVentana,DataQ)

  dbq1=dbp
  for (i in 1:ncol(DataQ)){
    dbq1[,2:ncol(dbq1)]=dbq[,i+1]
    fileq=reshape2::melt(dbq1,"DatesVentana")
    colnames(fileq)=c("Dates","Gauge","qm")
    fileq$EM=paste0(as.character(fileq$Dates),fileq$Gauge)
    
    file=merge(filep,fileq[,c("EM","qm")],"EM")
    file$EM=NULL
    
  ts<- ggplot(file, aes(Dates))+
    geom_col(data=file, aes(y=pm, color = "pm"), fill = "#00AFBB", colour= "#00AFBB", alpha = 0.4)+
    geom_line(data=file, aes(y=qm, col = "qm"), colour = "red",  size = .8)+
    labs(title =paste0("Serie de caudal estacion ",colnames(DataQ)[i]," y cada serie de estacion de precipitacion"), y = "P y Q [mm]") +  #caption = "Fuente: EPMAPS y FONAG", 
    scale_x_date(date_breaks= "3 years",date_minor_breaks = "1 year",limits=c(as.Date(min(file$Dates)),as.Date(max(file$Dates))),date_labels =("%Y"))+ 
    facet_wrap(facets = vars(Gauge), scales = "free_y") #+
  #theme_bw()
  #ts
  plotpath = paste0(colnames(DataQ)[i], " Q y P serie diaria.jpg") #creates a pdf path to produce a graphic of the span of records in the Data
  ggsave(plotpath,width =40 , height = 22,units = "cm",ts)
  
  file$q_p=file$qm/file$pm
  ts =  ggplot(file, aes(x=Dates, y=q_p, fill="Gauge"))+
    geom_line(colour= "blue", size = 0.8) +
    labs(title =paste0("Serie diaria relacion porcentual estacion caudal ",colnames(DataQ)[i]," y cada estacion de precipitacion"), y = "Relaci?n Q/P [%]") +  #caption = "Fuente: EPMAPS y FONAG", 
    scale_x_date(date_breaks= "3 years",date_minor_breaks = "1 year",limits=c(as.Date(min(file$Dates)),as.Date(max(file$Dates))),date_labels =("%Y"))+ 
    #scale_x_continuous(breaks = seq(1,12,by=1),labels = c('En','Fb','Mr','Ab','My','Jn','Jl','Ag','Sp','Oc','Nv','Dc')) +
    scale_y_continuous(labels = scales::percent_format())+
    facet_wrap(facets = vars(Gauge), scales = "free_y") #+
  #theme_bw()
  plotpath = paste0(colnames(DataQ)[i], " Q y P relacion serie completa.jpg") #creates a pdf path to produce a graphic of the span of records in the Data
  ggsave(plotpath,width =40 , height = 22,units = "cm",ts)
  
  }  
  ##########################################################################################################################  
  
  setwd(dir_outg1)
  Carpeta_Outg="Serie mensual y anual"
  dir.create(Carpeta_Outg,showWarnings=F)
  dir_outg = paste(c(dir_outg1,"\\",Carpeta_Outg),collapse="")
  setwd(dir_outg)
  #mensual y anual
  ##########################################################################################################################
  dbp=cbind(DatesVentana,YearMonthDay_Diaria,DataP)
  dbq=cbind(DatesVentana,YearMonthDay_Diaria,DataQ)
  
  #here a column of yearmonth is created and added to the database
  dbp$YearMonth=year(dbp$Dates)*100+month(dbp$Dates)
  dbq$YearMonth=year(dbq$Dates)*100+month(dbq$Dates)
  
  #here the aggregation from daily to monthly is done. For precipitation corresponds to a sum while for streamflow a average among values
  pmonth <- aggregate(dbp[,5:(ncol(dbp)-1)], by=list(Year=dbp$Year, Month=dbp$Month),sum,na.rm=F)
  #qmonth <- aggregate(dbq[,5:(ncol(dbq)-1)], by=list(Year=dbq$Year, Month=dbq$Month),mean,na.rm=T)
  qmonth <- aggregate(dbq[,5:(ncol(dbq)-1)], by=list(Year=dbq$Year, Month=dbq$Month),sum,na.rm=F)
  
  #here the aggregation from daily to monthly is done. For precipitation corresponds to a sum while for streamflow a average among values
  panual <- aggregate(dbp[,5:(ncol(dbp)-1)], by=list(Year=dbp$Year),sum,na.rm=T)
  #qmonth <- aggregate(dbq[,5:(ncol(dbq)-1)], by=list(Year=dbq$Year),mean,na.rm=T)
  qanual <- aggregate(dbq[,5:(ncol(dbq)-1)], by=list(Year=dbq$Year),sum,na.rm=T)
  
  estaciones=colnames(qanual)[-1]
  
  write.csv(pmonth,paste0("P_mensual.csv"),row.names=F) # saves table of WEAP results into directory
  write.csv(qmonth,paste0("Q_mensual.csv"),row.names=F) # saves table of WEAP results into directory
  write.csv(panual,paste0("P_anual.csv"),row.names=F) # saves table of WEAP results into directory
  write.csv(qanual,paste0("Q_anual.csv"),row.names=F) # saves table of WEAP results into directory
  
  ################## serie mensual y boxplot
  
  for (i in 1:ncol(DataQ)){
    pmonth1=pmonth
    pmonth1[,3:ncol(pmonth1)]=qmonth[,i+2]
    pmonth1[,3:ncol(pmonth1)]=pmonth1[,3:ncol(pmonth1)]/pmonth[,3:ncol(pmonth)]
    write.csv(pmonth1,paste0(colnames(DataQ)[i]," Q_P_mensual",".csv"),row.names=F) # saves table of WEAP results into directory
    
    pmonth1$Dates=DatesM
    ts=12
    data=reshape2::melt(pmonth1,id=c("Year","Month","Dates"))
    colnames(data)=c("Year","Month","Dates","EstacionP","Q_P")
    data$Mes=as.factor(data$Month)
    levels(data$Mes)=c('En','Fb','Mr','Ab','My','Jn','Jl','Ag','Sp','Oc','Nv','Dc')
    p<- ggplot(data, aes(x=Dates, y = Q_P, fill = EstacionP))+
      geom_line(colour= "blue", size = 0.8) +
      labs(title =paste0("Relaci?n porcentual mensual entre la estacion de caudal ",colnames(DataQ)[i]," y cada estacion de precipitacion"), y = "Relaci?n Q/P [%]") +  #caption = "Fuente: EPMAPS y FONAG", 
      #scale_x_continuous(breaks = seq(1,12,by=1),labels = c('En','Fb','Mr','Ab','My','Jn','Jl','Ag','Sp','Oc','Nv','Dc')) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
      facet_wrap(facets = vars(EstacionP), scales = "free_y")+
      theme_bw()
    p
    plotpath = paste0(colnames(DataQ)[i], " Q_P_mensual.jpg") #creates a pdf path to produce a graphic of the span of records in the Data
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    p<- ggplot(data, aes(x=Mes, y = Q_P))+ #, fill = Gauge
      geom_jitter(alpha=0.5,size=0.1)+
      geom_boxplot(outlier.shape = "") +
      stat_summary(fun=mean,col='blue',size=1,geom='line',aes(group=1))+
      labs(title =paste0("Relaci?n porcentual mensual entre la estacion de caudal ",colnames(DataQ)[i]," y cada estacion de precipitacion"), y = "Relaci?n Q/P [%]") +  #caption = "Fuente: EPMAPS y FONAG", 
      #scale_x_continuous(breaks = seq(1,12,by=1),labels = c('En','Fb','Mr','Ab','My','Jn','Jl','Ag','Sp','Oc','Nv','Dc')) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
      facet_wrap(facets = vars(EstacionP), scales = "free_y")+
      theme_bw()
    p
    plotpath = paste0(colnames(DataQ)[i], " Boxplot Q_P_mensual.jpg") #creates a pdf path to produce a graphic of the span of records in the Data
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
  }
  
  for (i in 1:ncol(DataQ)){
    panual1=panual
    panual1[,2:ncol(panual1)]=qanual[,i+1]
    panual1[,2:ncol(panual1)]=panual1[,2:ncol(panual1)]/panual[,2:ncol(panual1)]
    write.csv(panual1,paste0(colnames(DataQ)[i]," Q_P_anual",".csv"),row.names=F) # saves table of WEAP results into directory
    
    data=reshape2::melt(panual1,id=("Year"))
    colnames(data)=c("Year","EstacionP","Q_P")
    data$D=1
    
    p<- ggplot(data, aes(x=Year, y = Q_P))+
      #geom_point(colour= "blue") +  #, size = 0.8
      geom_line(colour= "blue", size = 0.8) +
      labs(title =paste0("Relaci?n porcentual anual entre la estacion de caudal ",colnames(DataQ)[i]," y cada estacion de precipitacion"), y = "Relaci?n Q/P [%]") +  #caption = "Fuente: EPMAPS y FONAG", 
      #scale_x_continuous(breaks = seq(1,12,by=1),labels = c('En','Fb','Mr','Ab','My','Jn','Jl','Ag','Sp','Oc','Nv','Dc')) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
      facet_wrap(facets = vars(EstacionP), scales = "free_y")+
      theme_bw()
    p
    plotpath = paste0(colnames(DataQ)[i], " Q_P_anual.jpg") #creates a pdf path to produce a graphic of the span of records in the Data
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
    p<- ggplot(data, aes(x=D, y = Q_P))+ #, fill = Gauge
      geom_jitter(alpha=0.5,size=0.1)+
      geom_boxplot(outlier.shape = "") +
      #stat_summary(fun=mean,col='blue',size=1,geom='line',aes(group=1))+
      labs(title =paste0("Boxplot Relaci?n porcentual anual entre la estacion de caudal ",colnames(DataQ)[i]," y cada estacion de precipitacion"), y = "Relaci?n Q/P [%]") +  #caption = "Fuente: EPMAPS y FONAG", 
      #scale_x_continuous(breaks = seq(1,12,by=1),labels = c('En','Fb','Mr','Ab','My','Jn','Jl','Ag','Sp','Oc','Nv','Dc')) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
      facet_wrap(facets = vars(EstacionP), scales = "free_y") +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      #theme_bw()
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    #p
    #plotpath = paste0(colnames(DataQ)[i], " Boxplot Q_P_anual_1.jpg") #creates a pdf path to produce a graphic of the span of records in the Data
    #ggsave(plotpath,width =40 , height = 22,units = "cm")
  
    p<- ggplot(data, aes(x=EstacionP, y = Q_P))+ #, fill = Gauge
      geom_jitter(alpha=0.5,size=0.1)+
      geom_boxplot(outlier.shape = "") +
      #stat_summary(fun=mean,col='blue',size=1,geom='line',aes(group=1))+
      labs(title =paste0("Boxplot Relaci?n porcentual entre la estacion de caudal ",colnames(DataQ)[i]," y cada estacion de precipitacion"), y = "Relaci?n Q/P [%]") +  #caption = "Fuente: EPMAPS y FONAG", 
      #scale_x_continuous(breaks = seq(1,12,by=1),labels = c('En','Fb','Mr','Ab','My','Jn','Jl','Ag','Sp','Oc','Nv','Dc')) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
      #facet_wrap(facets = vars(Gauge), scales = "free_y") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    #theme_bw()
    p
    plotpath = paste0(colnames(DataQ)[i], " Boxplot Q_P_anual.jpg") #creates a pdf path to produce a graphic of the span of records in the Data
    ggsave(plotpath,width =40 , height = 22,units = "cm")
    
  }
  ##########################################################################################################################
  
  setwd(dir_outg1)
  Carpeta_Outg="Serie multianual"
  dir.create(Carpeta_Outg,showWarnings=F)
  dir_outg = paste(c(dir_outg1,"\\",Carpeta_Outg),collapse="")
  setwd(dir_outg)
  #mensual multianual
  ##########################################################################################################################
  #here the aggregation from daily to monthly is done. For precipitation corresponds to a sum while for streamflow a average among values
  pmonth <- aggregate(dbp[,5:(ncol(dbp)-1)], by=list(Year=dbp$Year, Month=dbp$Month),sum,na.rm=F)
  #qmonth <- aggregate(dbq[,5:(ncol(dbq)-1)], by=list(Year=dbq$Year, Month=dbq$Month),mean,na.rm=T)
  qmonth <- aggregate(dbq[,5:(ncol(dbq)-1)], by=list(Year=dbq$Year, Month=dbq$Month),sum,na.rm=F)
  #here data is aggregated to obtain the mean monthly multiyear per variable
  qmonth<- aggregate(qmonth[,3:ncol(qmonth)], by=list(Mes=qmonth$Month),mean,na.rm=T)
  pmonth<- aggregate(pmonth[,3:ncol(pmonth)], by=list(Mes=pmonth$Month),mean,na.rm=T)
  
  #now database is exported
  write.csv(qmonth,paste0("Q_PmedioMensualMultianual.csv"),row.names=F) # saves table of WEAP results into directory
  write.csv(pmonth,paste0("P_PmedioMensualMultianual.csv"),row.names=F) # saves table of WEAP results into directory
  
  prc<- reshape2::melt(pmonth,"Mes")
  colnames(prc)=c("Mes","Estaciones.P", "p_mm")
  prc$EM=paste0(prc$Estaciones.P,prc$Mes)
  
  dsc<- reshape2::melt(qmonth,"Mes")
  colnames(dsc)=c("Mes","Estaciones.P", "q_mm")

  a <- ggplot(prc,aes(x=Mes)) +
    geom_col(aes(y=p_mm, col = "precipitaci?n"), fill = "#00AFBB", colour= "#00AFBB", alpha = 0.4) +
    scale_x_continuous(breaks = seq(1,12,by=1),labels = c('En','Fb','Mr','Ab','My','Jn','Jl','Ag','Sp','Oc','Nv','Dc')) +
    facet_wrap(facets = vars(Estaciones.P), scales = "free_y")+
    theme_bw()+
    labs(y= "Precipitacion [mm]", x = "Mes")
  #a 
  
  b <- ggplot(dsc,aes(x=Mes)) +
    geom_line(aes(y = q_mm), colour = "red",  size = .8) +
    facet_wrap(facets = vars(Estaciones.P), scales = "free_y")+
    theme_bw()+
    scale_x_continuous(breaks = seq(1,12,by=1),labels = c('En','Fb','Mr','Ab','My','Jn','Jl','Ag','Sp','Oc','Nv','Dc')) +
    labs(y= "Caudal [mm]")
  #b
  
  #1 way
  #grid.newpage()
  #grid.draw(rbind(ggplotGrob(a), ggplotGrob(b), size = "first"))
  
  #2 way
  grid.newpage()
  d<- grid.arrange(a, b, nrow=2, newpage = FALSE)
  #ggsave("d.png")
  #d
  plotpath = paste0("P y Q mm.jpg") #creates a pdf path to produce a graphic of the span of records in the Data
  ggsave(plotpath,width =40 , height = 22,units = "cm", d)
  
  #medios mensuales multianuales
  for (i in 1:ncol(DataQ)){
    qmonth1=pmonth
    qmonth1[,2:ncol(qmonth1)]=qmonth[,i+1]
    dsc<- reshape2::melt(qmonth1,"Mes")
    colnames(dsc)=c("Mes","Estaciones.P", "q_mm")
    dsc$EM=paste0(dsc$Estaciones.P,dsc$Mes)
    
    data<- merge(prc, dsc[,c("EM","q_mm")], "EM") # this method produces about 300 obs and 6 vars
    data$EM=NULL

  #grafico de relacion caudal precipitacion por estacion
  df<- data
  colnames(df)=c("mes","estacion","p_mm", "q_mm")
  
  df$ratio <- (df$q_mm/df$p_mm)
  p<- ggplot(data=df, aes(x=mes, y = ratio, fill = "estacion"))+
    geom_line(colour= "blue", size = 0.8) +
    labs(title =paste0("Relaci?n porcentual mensual multianual entre la estacion de caudal ",colnames(DataQ)[i]," y cada estacion de precipitacion"), y = "Relaci?n Q/P [%]") +  #caption = "Fuente: EPMAPS y FONAG", 
    scale_x_continuous(breaks = seq(1,12,by=1),labels = c('En','Fb','Mr','Ab','My','Jn','Jl','Ag','Sp','Oc','Nv','Dc')) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
    facet_wrap(facets = vars(estacion), scales = "free_y")+
    theme_bw()
  p
  plotpath = paste0(colnames(DataQ)[i], " ciclo anual q_p.jpg") #creates a pdf path to produce a graphic of the span of records in the Data
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  
  df1<- prc
  colnames(df1)=c("mes","variable","valor") #precipitacion
  
  df2<- dsc
  colnames(df2)=c("mes","variable","valor") #caudal
  
  plot1<- ggplot(df1, aes(mes))+
    geom_col(data=df1, aes(y=valor, color = "precipitacion"), fill = "#00AFBB", colour= "#00AFBB", alpha = 0.4)+
    geom_line(data=df2, aes(y=valor, col = "caudal"), colour = "red",  size = .8)+
    geom_point(data=df2, aes(y=valor), colour = "red")+
    labs(title =paste0("Ciclo anual estacion de caudal ",colnames(DataQ)[i]," y cada estacion de precipitacion"), y = "P y Q mm") +  #caption = "Fuente: EPMAPS y FONAG", 
    theme(panel.grid.minor = element_blank())+
    scale_x_continuous(breaks = seq(1,12,by=1),labels = c('En','Fb','Mr','Ab','My','Jn','Jl','Ag','Sp','Oc','Nv','Dc')) +
    facet_wrap(facets = vars(variable), scales = "free_y")+
    theme_bw()
  
  plot1
  plotpath = paste0(colnames(DataQ)[i], " P y Q mensuales multianules.jpg") #creates a pdf path to produce a graphic of the span of records in the Data
  ggsave(plotpath,width =40 , height = 22,units = "cm")
  
  }
  ##########################################################################################################################
}

difftime(Sys.time(),start)
#######################################################################################################

