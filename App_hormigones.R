#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(RODBC)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ODB)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyTime)
library(plotly)
library(DT)
library(V8)

jscode <- "shinyjs.refresh = function() { history.go(0); }"
# install.packages("ODB","DT") en caso de usar en otro equipo hay que instalar estos paquetes

CalcularMasas<-function(volh,humedad,m3){
  volh<-50
  m3<-c(781:793)
  humedad<-c(1:13)
  masas<-data.frame(m3,humedad)
  for(i in 1:13) {
    if(i==1){
      inicio<-((m3[i]*volh)/1000)
      a<-c(inicio)
    }
    if(i >1){
      calculo<-((m3[i]*volh)/1000)
      a<-c(a,calculo)
    }
  }
  masas$Masa_seca<-a
  mh1<-masas$Masa_seca[1]
  mh2<-masas$Masa_seca[2]
  mh3<-masas$Masa_seca[3]
  mh4<-(masas$Masa_seca[4]+(masas$Masa_seca[4]-masas$humedad[4]/100))
  mh5<-(masas$Masa_seca[5]+(masas$Masa_seca[5]-masas$humedad[5]/100))
  mh6<-(masas$Masa_seca[6]+(masas$Masa_seca[6]-masas$humedad[6]/100))
  mh7<-(masas$Masa_seca[7]+(masas$Masa_seca[7]-masas$humedad[7]/100))
  mh8<-(masas$Masa_seca[8]+(masas$Masa_seca[8]-masas$humedad[8]/100))
  mh9<-masas$Masa_seca[9]
  mh10<-masas$Masa_seca[10]
  mh11<-masas$Masa_seca[11]
  mh12<-masas$Masa_seca[12]
  mh13<-masas$Masa_seca[13]
  mh<-c(mh1,mh2,mh3,mh4,mh5,mh6,mh7,mh8,mh9,mh10,mh11,mh12,mh13)
  masas$masa_humeda<-mh
  return(masas)
}

CalcularGranulometria<-function(Tipo,IdMuestra,Pulgadas3,Pulgadas112,Pulgadas1,Pulgadas34,Pulgadas12,Pulgadas38,Pulgadas4,Pulgadas8,Pulgadas16,Pulgadas30,Pulgadas50,Pulgadas100,Fondo){
  
  
  Tipo_malla=c("3","1 1/2","1","3/4","1/2","3/8","4","8","16","30","50","100","Fondo")
  Masa=as.numeric(c(Pulgadas3,Pulgadas112,Pulgadas1,Pulgadas34,Pulgadas12,Pulgadas38,Pulgadas4,Pulgadas8,Pulgadas16,Pulgadas30,Pulgadas50,Pulgadas100,Fondo))
  df=data.frame(Tipo_malla,Masa)
  suma=sum(df$Masa)
  df$Porcentaje_ret<-(df$Masa/suma)*100
  for(i in 1:13){
    if(i==1){
      inicio<-100-df$Porcentaje_ret[i]
      a<-c(inicio)
    }
    if(i >1&&i<13){
      calculo<-(a[i-1]- df$Porcentaje_ret[i])
      a<-c(a,calculo)
    }
    if(i==13){
      a<-c(a,0)
    } 
  }
  df$porcentaje_pasa<-a
  if( "Grava" %in% Tipo ||"Gravilla" %in% Tipo||"Gravilla 13mm" %in% Tipo){
    Finura<-10-(df$porcentaje_pasa[1]+df$porcentaje_pasa[2]+df$porcentaje_pasa[4]+df$porcentaje_pasa[6]+df$porcentaje_pasa[7]+
                  df$porcentaje_pasa[8]+df$porcentaje_pasa[9]+df$porcentaje_pasa[10]+df$porcentaje_pasa[11]+df$porcentaje_pasa[12])/df$porcentaje_pasa[1]
  }else{
    Finura<-6-(porcentaje_pasa[7]+df$porcentaje_pasa[8]+df$porcentaje_pasa[9]+df$porcentaje_pasa[10]+df$porcentaje_pasa[11]+df$porcentaje_pasa[12])/porcentaje_pasa[7]
  }
  #agregar for para insercion de datos y finura
}


InsertarEstudio<-function(Nombre){
  today <- Sys.Date()
  format(today, format="%Y-%m-%d")
  conexion<-odb.open("Estudios Hormigones.odb")
  today<-paste("'",substr(today,1,10),"'")
  today<-gsub(" ", "", today, fixed = TRUE)
  orden<-"INSERT INTO \"Estudio\" (\"Nombre_estudio\",\"Fecha_estudio\") VALUES ('"
  orden<-paste(orden,Nombre,"',")
  orden<- paste(orden,today)
  orden<- paste(orden,")")
  odb.write(conexion,orden) 
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}

MostrarEstudioFechas<-function(fecha){
  conexion<-odb.open("Estudios Hormigones.odb")
  
  inicio<-paste("'",fecha[1],"'")
  inicio<-gsub(" ", "", inicio, fixed = TRUE)
  fin<-paste("'",fecha[2],"'")
  fin<-gsub(" ", "", fin, fixed = TRUE)
  orden<- paste("SELECT * FROM \"Estudio\" WHERE \"Fecha_estudio\" BETWEEN ",inicio,"AND",fin)
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
  
  
}
TodoEstudio<-function(Estudio){
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<- "SELECT * FROM \"Estudio\""
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
  
}
MostrarEstudio<-function(Estudio){
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<- paste("SELECT * FROM \"Estudio\" WHERE \"ID_Estudio\" =",Estudio)
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
  
}

InsertarArido<-function(IdEstudio,TipoMuestra,FechaMuestreo,FechaAnalisis,Planta){
  
  Muestreo<-paste("'",FechaMuestreo,"'")
  Muestreo<-gsub(" ", "", inicio, fixed = TRUE)
  Analisis<-paste("'",FechaAnalisis,"'")
  Analisis<-gsub(" ", "", inicio, fixed = TRUE)
  
  conexion<-odb.open("Estudios Hormigones.odb")
  
  orden<-"INSERT INTO \"Aridos\" (\"ID_Estudio\",\"Tipo_muestra\",\"Fecha_muestreo\",\"Fecha_analisis\",\"Planta_estudio\") VALUES ("
  orden<-paste(orden,IdEstudio,",'")
  orden<- paste(orden,TipoMuestra,"',")
  orden<- paste(orden,Muestreo,",",Analisis,",'")
  orden<- paste(orden,Planta,"'",")")
  odb.write(conexion,orden) 
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}
MostrarArido<-function(Estudio){
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<- paste("SELECT * FROM \"Aridos\" WHERE \"ID_Estudio\" =",Estudio)
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
  
}

TodoArido<-function(Estudio){
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<- "SELECT * FROM \"Aridos\""
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
  
}

MostrarAridoFechas<-function(fecha){
  conexion<-odb.open("Estudios Hormigones.odb")
  
  inicio<-paste("'",fecha[1],"'")
  inicio<-gsub(" ", "", inicio, fixed = TRUE)
  fin<-paste("'",fecha[2],"'")
  fin<-gsub(" ", "", fin, fixed = TRUE)
  orden<- paste("SELECT * FROM \"Aridos\" WHERE \"Fecha_muestreo\" BETWEEN ",inicio,"AND",fin)
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
}

InsertarGranulometria<-function(IdMuestra,TipoMalla,MasaRetenida,PorcentajeRetenido,PorcentajePasa){
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<-"INSERT INTO \"Granulometria\" (\"ID_Muestra\",\"Tipo_malla\",\"Masa_retenida\",\"Porcentaje_retenido\",\"Porcentaje_pasa\") VALUES ("
  orden<-paste(orden,IdMuestra,",'")
  orden<- paste(orden,TipoMalla,"',")
  orden<- paste(orden,MasaRetenida,",")
  orden<- paste(orden,PorcentajeRetenido,",")
  orden<- paste(orden,PorcentajePasa,")")
  odb.write(conexion,orden) 
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}
scaleFUN <- function(x) sprintf("%.2f", x)

GraficoGranulometria<-function(datos){
  malla<-c(76.120,37.500,25.000,19.000,12.500,9.500,4.750,2.000,0.850,0.600,0.300,0.150,0.075)
  porcentaje<-datos$Porcentaje_pasa
  df<-data.frame(malla,porcentaje)
  graf<-ggplot(df,aes(malla, porcentaje,colour = "Curva Granulometrica"))+ geom_line(size=1.2)+
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + xlab('Abertura de tamices [mm]') + ylab("Porcentaje que pasa")+
    scale_colour_manual(name  ="",
                        breaks=c("Curva Granulometrica"),
                        values = c("blue"))+ scale_x_continuous(labels=scaleFUN,breaks = pretty(df$malla, n =25))
  return(graf)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
}
MostrarGranulometria<-function(Estudio){
  
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<- paste("SELECT * FROM \"Aridos\" INNER JOIN \"Granulometria\" ON \"Aridos\".\"ID_Muestra\" 
                = \"Granulometria\".\"ID_Muestra\" WHERE \"ID_Estudio\" =",Estudio)
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
  return(muestra)
  
}

TodoGranulometria<-function(Estudio){
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<- "SELECT * FROM \"Aridos\" INNER JOIN \"Granulometria\" ON \"Aridos\".\"ID_Muestra\" 
  = \"Granulometria\".\"ID_Muestra\""
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
  
}

MostrarGranulometriaFechas<-function(fecha){
  conexion<-odb.open("Estudios Hormigones.odb")
  
  inicio<-paste("'",fecha[1],"'")
  inicio<-gsub(" ", "", inicio, fixed = TRUE)
  fin<-paste("'",fecha[2],"'")
  fin<-gsub(" ", "", fin, fixed = TRUE)
  orden<- paste("SELECT * FROM \"Aridos\" INNER JOIN \"Granulometria\" ON \"Aridos\".\"ID_Muestra\" 
                = \"Granulometria\".\"ID_Muestra\" WHERE \"Fecha_muestreo\" BETWEEN ",inicio,"AND",fin)
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
}
InsertarFinura<-function(IdMuestra,ModuloFinura){
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<-"INSERT INTO \"Finura\" (\"ID_Muestra\",\"Modulo_finura\") VALUES ("
  orden<-paste(orden,IdMuestra,",")
  orden<- paste(orden,ModuloFinura,")")
  odb.write(conexion,orden)
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}
InsertarFinos<-function(IdMuestra,PSS,PSL){
  Porcentaje=(PSS-PSL)/PSS*100
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<-"INSERT INTO \"Finos\" (\"ID_Muestra\",\"P_seco_sucio\",\"P_seco_lavado\",\"Porcentaje_finos\") VALUES ("
  orden<-paste(orden,IdMuestra,",")
  orden<-paste(orden,PSS,",")
  orden<-paste(orden,PSL,",")
  orden<- paste(orden,Porcentaje,")")
  odb.write(conexion,orden) 
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}
InsertarDensidadR<-function(IdMuestra,Pseco,PSSS,Psumergido,MasaAgua,MasaMaterial){
  
  Volumen=PSSS-Psumergido+MasaAgua-MasaMaterial
  Densidad=Pseco/Volumen
  Absorcion=(PSSS-Pseco)/Pseco*100
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<-"INSERT INTO \"Densidad_real\" (\"ID_Muestra\",\"Peso_seco\",\"Peso_SSS\",\"Peso_sumergido\",\"Masa_matraz_agua\",\"Masa_matraz_material\",\"Volumen\",\"Densidad_real_seca\",\"Porcentaje_absorcion\") VALUES ("                                   
  orden<-paste(orden,IdMuestra,",")
  orden<-paste(orden,Pseco,",")
  orden<-paste(orden,PSSS,",")
  orden<-paste(orden,Psumergido,",")
  orden<-paste(orden,MasaAgua,",")
  orden<-paste(orden,MasaMaterial,",")
  orden<-paste(orden,Volumen,",")
  orden<-paste(orden,Densidad,",")
  orden<- paste(orden,Absorcion,")")
  odb.write(conexion,orden) 
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}
InsertarDensidadAp<-function(IdMuestra,Medicion1,Medicion2,Medicion3,Volumen,Humedad){
  
  Densidad=((Medicion1+Medicion2+Medicion3)/3)/Volumen*1000
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<-"INSERT INTO \"Densidad_ap_suelta\" (\"ID_Muestra\",\"Medicion_1\",\"Medicion_2\",\"Medicion_3\",\"Volumen_medida\",\"Densidad_ap_humeda\",\"Porcentaje_humedad\") VALUES ("                                   
  orden<-paste(orden,IdMuestra,",")
  orden<-paste(orden,Medicion1,",")
  orden<-paste(orden,Medicion2,",")
  orden<-paste(orden,Medicion3,",")
  orden<-paste(orden,Volumen,",")
  orden<-paste(orden,Densidad,",")
  orden<- paste(orden,Humedad,")")
  odb.write(conexion,orden) 
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}
InsertarEqArena<-function(IdMuestra,Na1,Nt1,Na2,Nt2){
  
  Porcentaje=(Na1/Nt1+Na2/Nt2)*100/2 
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<-"INSERT INTO \"Eq_arena\" (\"ID_Muestra\",\"Na1\",\"Nt1\",\"Na2\",\"Nt2\",\"Porcentaje_eq_arena\") VALUES ("                                   
  orden<-paste(orden,IdMuestra,",")
  orden<-paste(orden,Na1,",")
  orden<-paste(orden,Nt1,",")
  orden<-paste(orden,Na2,",")
  orden<-paste(orden,Nt2,",")
  orden<- paste(orden,Porcentaje,")")
  odb.write(conexion,orden) 
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}
InsertarCubicidad<-function(IdMuestra,PMuestra,PChancado,PRodado,PLaja){
  
  Porcentaje=(PChancado+PLaja)/PMuestra*100    
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<-"INSERT INTO \"Cubicidad\" (\"ID_Muestra\",\"Peso_muestra\",\"Peso_chancado\",\"Peso_rodado\",\"Peso_laja\",\"Porcentaje_chancado\",\"Porcentaje_eq_arena\") VALUES ("                                   
  orden<-paste(orden,IdMuestra,",")
  orden<-paste(orden,PMuestra,",")
  orden<-paste(orden,PChancado,",")
  orden<-paste(orden,PRodado,",")
  orden<-paste(orden,PLaja,",")
  orden<- paste(orden,Porcentaje,")")
  odb.write(conexion,orden) 
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}
InsertarProveedor<-function(IdMuestra,Proveedor){
  
  
  conexion<-odb.open("Estudios Hormigones.odb")
  orden<-"INSERT INTO \"Proveedor\" (\"ID_Muestra\",\"Nombre_proveedor\") VALUES ("                                   
  orden<-paste(orden,IdMuestra,",'")
  orden<-paste(orden,Proveedor,"')")
  odb.write(conexion,orden) 
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title="Estudio Hormigones"),
                    dashboardSidebar(
                      menuItem(text="Menu",icon=icon("bars"),
                               menuSubItem(text="Mostrar Estudio",tabName="BuscarEstudio", icon=icon("search")),
                               menuSubItem(text="Mostrar Arido",tabName="BuscarArido",icon=icon("search")),
                               menuSubItem(text="Mostrar Granulometria",tabName="BuscarGranulometria", icon=icon("search")),
                               menuSubItem(text="Ingresar Estudio Hormigon",tabName="InsertarEstudio",icon=icon("cloud")))
                      
                      
                    ),
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName="BuscarEstudio",column(3,selectInput("DataEstudio", "Estudios", c("Mostrar Todo", "Mostrar Por ID","Mostrar Entre Fechas"))),
                                column(3,numericInput('Estudio',"Ingresar Codigo Estudio",min = 0,0)),
                                column(3,dateRangeInput('fechas','Ingresar Fechas:', format="yyyy-mm-dd",weekstart=1,language="es",separator="hasta",start = '2010-01-01',end = "2018-01-01")),
                                
                                box(DT::dataTableOutput("Estudio"),width = 12)),
                        tabItem(tabName="BuscarArido",
                                selectInput("DataArido", "Aridos", c("Mostrar Todo", "Mostrar Por ID De Estudio","Mostrar Entre Fechas")),
                                box(DT::dataTableOutput("Arido"),width = 12)),
                        
                        tabItem(tabName="BuscarGranulometria",column(3,selectInput("DataGranulometria", "Granulometrias", c("Mostrar Todo", "Mostrar Por ID De Estudio","Mostrar Entre Fechas"))),
                                column(3,numericInput('Estudio',"Ingresar Codigo Estudio",min = 0,0)),
                                box(DT::dataTableOutput("Granulometria"),width = 12),
                                box(plotOutput("GranulometriaGraf"),width = 12)),
                        tabItem(tabName="InsertarEstudio",
                                box(id ="Menu",width = 12,h2("Antecedentes"),fluidRow(
                                  column(3,textInput("NombreEstudio", "Nombre Del Estudio")),
                                  column(3,textInput("TipoHormigon", "Tipo de hormigon")),
                                  column(3,numericInput("VolumenHormigon", "Volumen Hormigon (lt)",value = 0,min = 0))
                                  ),
                                  
                                  fluidRow(column(3,dateInput("FechaConfeccion","Fecha confeccion",format="yyyy-mm-dd",weekstart=1,language="es",start="2000-01-01")),
                                           column(3,timeInput("HoraConfeccion", "Hora Confeccion",seconds = FALSE)),         
                                           column(3,checkboxGroupInput("Opciones", label = h3("Estudios A Realizar"), 
                                                                       choices = list("Dosificacion" = 1, "Ensayo de probetas" = 2, "Mantencios de conos" = 3, "Fraguado" = 4, "Granulometria" = 5),
                                                                       selected = 1))),
                                  h2("Dosificacion"),h3("Ingrese los proveedores y tipos correspondientes")
                                  ,fluidRow(column(2,textInput("ProvCemento","Cemento")),
                                            column(2,textInput("ProvFiller","Filler")),
                                            column(2,textInput("ProvAguatotal","Agua Total")),
                                            column(2,textInput("ProvArenaIndustrial","Arena Industrial")),
                                            column(2,textInput("ProvArenaCorrectora","Arena Correctora")),
                                            column(2,textInput("ProvGravilla13","Gravilla 13 mm"))),
                                  fluidRow(column(2,textInput("ProvGravilla20","Gravilla 20mm")),
                                           column(2,textInput("ProvGrava40","Grava 40mm")),
                                           column(2,textInput("ProvAditivo1","Aditivo 1")),
                                           column(2,textInput("ProvAditivo2","Aditivo 2")),
                                           column(2,textInput("ProvAditivo3","Aditivo 3")),
                                           column(2,textInput("ProvAditivo4","Aditivo 4")))
                                ),useShinyjs(),actionButton("IngresarEst", "INGRESAR",icon=icon("cloud"))
                        )
                      )))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  
  observeEvent(input$IngresarEst,{
    #if(contador==1) hacer ingreso de estudio
    
    if(input$IngresarEst ==1){
      removeUI(multiple = FALSE,
               selector = "div:has(> #Menu)"
      )
      
      
      if ( 1 %in% input$Opciones){
        insertUI(
          selector = "#IngresarEst",
          where = "beforeBegin",                                      
          ui = box(id ="Dosificacion",width = 12, h2("Dosificacion"),
        h3("Ingrese los volumenes correspondientes"),
        fluidRow(column(2,numericInput("VolCemento","Cemento",min = 0,value = 0)),
                 column(2,numericInput("VolFiller","Filler",min = 0,value = 0)),
                 column(2,numericInput("VolAguaTotal","Agua Total",min = 0,value = 0)),
                 column(2,numericInput("VolArenaIndustrial","Arena Industrial",min = 0,value = 0)),
                 column(2,numericInput("VolArenaCorrectora","Arena Correctora",min = 0,value = 0)),
                 column(2,numericInput("VolGravilla13","Gravilla 13mm",min = 0,value = 0))),
        fluidRow(column(2,numericInput("VolGravilla20","Gravilla 20mm",min = 0,value = 0)),
                 column(2,numericInput("VolGrava40","Grava 40mm",min = 0,value = 0)),
                 column(2,numericInput("VolAditivo1","Aditivo 1",min = 0,value = 0)),
                 column(2,numericInput("VolAditivo2","Aditivo 2",min = 0,value = 0)),
                 column(2,numericInput("VolAditivo3","Aditivo 3",min = 0,value = 0)),
                 column(2,numericInput("VolAditivo4","Aditivo 4",min = 0,value = 0))),
        h3("Ingrese las humedades"),
        fluidRow(column(2,numericInput("HumedadAI","Humedad Arena Industrial",min = 0,value = 0)),
                 column(2,numericInput("HumedadAC","Humedad Arena Correctora",min = 0,value = 0)),
                 column(2,numericInput("HumedadG13","Humedad Gravilla 13mm",min = 0,value = 0)),
                 column(2,numericInput("HumedadG20","Humedad Gravilla 20mm",min = 0,value = 0)),
                 column(2,numericInput("HumedadG40","Humedad Grava 40mm",min = 0,value = 0))),
        h3("Ingrese los datos correspondientes"),
        fluidRow(column(3,numericInput("MasaMuestra","Masa Muestra (gr)",min = 0,value = 0)),
                 column(3,numericInput("VolumenTacho","Volumen Tacho (cc)",min = 0,value = 0)),
                 column(3,numericInput("DifAgua","Diferencia Agua (gr)",min = 0,value = 0)),
                 column(3,numericInput("NumProbetas","Nurmero de Probetas",min = 0,value = 0))),
        fluidRow(column(3,numericInput("MedidaCono","Cono (cm)",min = 0,value = 0)),
                 column(3,numericInput("TempAmbiente","Temperatura Ambiente (°C)",min = 0,value = 0)),
                 column(3,numericInput("TempHormigon","Temperatura hormigon (°C)",min = 0,value = 0)),
                 column(3,textInput("TexturaHormigon","Textura hormigon"))),
        textAreaInput(inputId = "ObservacionDosificacion",label = "Observaciones")
        
        
        )
        )
      }
      if (2 %in% input$Opciones){
        insertUI(
          selector = "#IngresarEst",
          where = "beforeBegin",
          ui = box(id ="Probetas",width = 12,h2("Ensayos de Probetas"),h3("Ingrese datos tomados"),
                   fluidRow(column(2,h3("Dia 1")),
                            column(2,numericInput(inputId = 'MasaProbeta1',min = 0,value = 0,label = "Masa [g]")),
                   column(2,numericInput(inputId = 'CargaProbeta1',min = 0,value = 0,label = "Carga[kN]")),
                   column(6,textAreaInput("ObservacionProbeta1","Comentarios"))
                   ),
                   fluidRow(column(2,h3("Dia 1")),
                            column(2,numericInput(inputId = 'MasaProbeta2',min = 0,value = 0,label = "Masa [g]")),
                            column(2,numericInput(inputId = 'CargaProbeta2',min = 0,value = 0,label = "Carga[kN]")),
                            column(6,textAreaInput("ObservacionProbeta2","Comentarios"))
                   ),
                   fluidRow(column(2,h3("Dia 3")),
                            column(2,numericInput(inputId = 'MasaProbeta3',min = 0,value = 0,label = "Masa [g]")),
                            column(2,numericInput(inputId = 'CargaProbeta3',min = 0,value = 0,label = "Carga[kN]")),
                            column(6,textAreaInput("ObservacionProbeta3","Comentarios"))
                   ),
                   fluidRow(column(2,h3("Dia 3")),
                            column(2,numericInput(inputId = 'MasaProbeta4',min = 0,value = 0,label = "Masa [g]")),
                            column(2,numericInput(inputId = 'CargaProbeta4',min = 0,value = 0,label = "Carga[kN]")),
                            column(6,textAreaInput("ObservacionProbeta4","Comentarios"))
                   ),
                   fluidRow(column(2,h3("Dia 7")),
                            column(2,numericInput(inputId = 'MasaProbeta5',min = 0,value = 0,label = "Masa [g]")),
                            column(2,numericInput(inputId = 'CargaProbeta5',min = 0,value = 0,label = "Carga[kN]")),
                            column(6,textAreaInput("ObservacionProbeta5","Comentarios"))
                   ),
                   fluidRow(column(2,h3("Dia 7")),
                            column(2,numericInput(inputId = 'MasaProbeta6',min = 0,value = 0,label = "Masa [g]")),
                            column(2,numericInput(inputId = 'CargaProbeta6',min = 0,value = 0,label = "Carga[kN]")),
                            column(6,textAreaInput("ObservacionProbeta6","Comentarios"))
                   ),
                   fluidRow(column(2,h3("Dia 28")),
                            column(2,numericInput(inputId = 'MasaProbeta7',min = 0,value = 0,label = "Masa [g]")),
                            column(2,numericInput(inputId = 'CargaProbeta7',min = 0,value = 0,label = "Carga[kN]")),
                            column(6,textAreaInput("ObservacionProbeta7","Comentarios"))
                   ),
                   fluidRow(column(2,h3("Dia 28")),
                            column(2,numericInput(inputId = 'MasaProbeta8',min = 0,value = 0,label = "Masa [g]")),
                            column(2,numericInput(inputId = 'CargaProbeta8',min = 0,value = 0,label = "Carga[kN]")),
                            column(6,textAreaInput("ObservacionProbeta8","Comentarios"))
                   )
          ))}
      if (3 %in% input$Opciones){
        
              insertUI(
                selector = "#IngresarEst",
                where = "beforeBegin",
                ui = box(id ="Conos",width = 12,h2("Mantencion De Conos"),h3("Ingrese datos tomados"),
                         fluidRow(column(2,h3("Hora 0")),
                                  column(2,timeInput("HoraCono0", "Hora",seconds = FALSE)),
                                  column(2,numericInput(inputId = 'Cono1',min = 0,value = 0,label = "Cono")),
                                  column(2,numericInput(inputId = 'TAmbiente1',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                                  column(2,numericInput(inputId = 'THormigon1',min = 0,value = 0,label = "Temperatura Hormigon [°C]"))
                         ),
                         fluidRow(column(2,h3("Hora 0,5")),
                                  column(2,timeInput("HoraCono05", "Hora",seconds = FALSE)),
                                  column(2,numericInput(inputId = 'Cono2',min = 0,value = 0,label = "Cono")),
                                  column(2,numericInput(inputId = 'TAmbiente2',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                                  column(2,numericInput(inputId = 'THormigon2',min = 0,value = 0,label = "Temperatura Hormigon [°C]"))
                         ),
                         fluidRow(column(2,h3("Hora 1")),
                                  column(2,timeInput("HoraCono1", "Hora",seconds = FALSE)),
                                  column(2,numericInput(inputId = 'Cono3',min = 0,value = 0,label = "Cono")),
                                  column(2,numericInput(inputId = 'TAmbiente3',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                                  column(2,numericInput(inputId = 'THormigon3',min = 0,value = 0,label = "Temperatura Hormigon [°C]"))
                         ),
                         fluidRow(column(2,h3("Hora 1,5")),
                                  column(2,timeInput("HoraCono15", "Hora",seconds = FALSE)),
                                  column(2,numericInput(inputId = 'Cono4',min = 0,value = 0,label = "Cono")),
                                  column(2,numericInput(inputId = 'TAmbiente4',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                                  column(2,numericInput(inputId = 'THormigon4',min = 0,value = 0,label = "Temperatura Hormigon [°C]"))
                         ),
                         fluidRow(column(2,h3("Hora 2")),
                                  column(2,timeInput("HoraCono2", "Hora",seconds = FALSE)),
                                  column(2,numericInput(inputId = 'Cono5',min = 0,value = 0,label = "Cono")),
                                  column(2,numericInput(inputId = 'TAmbiente5',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                                  column(2,numericInput(inputId = 'THormigon5',min = 0,value = 0,label = "Temperatura Hormigon [°C]"))
                         ),
                         fluidRow(column(2,h3("Hora 2,5")),
                                  column(2,timeInput("HoraCono25", "Hora",seconds = FALSE)),
                                  column(2,numericInput(inputId = 'Cono6',min = 0,value = 0,label = "Cono")),
                                  column(2,numericInput(inputId = 'TAmbiente6',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                                  column(2,numericInput(inputId = 'THormigon6',min = 0,value = 0,label = "Temperatura Hormigon [°C]"))
                         ),
                         fluidRow(column(2,h3("Hora 3")),
                                  column(2,timeInput("HoraCono3", "Hora",seconds = FALSE)),
                                  column(2,numericInput(inputId = 'Cono7',min = 0,value = 0,label = "Cono")),
                                  column(2,numericInput(inputId = 'TAmbiente7',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                                  column(2,numericInput(inputId = 'THormigon7',min = 0,value = 0,label = "Temperatura Hormigon [°C]"))
                         ),
                         fluidRow(column(2,h3("Hora 3,5")),
                                  column(2,timeInput("HoraCono35", "Hora",seconds = FALSE)),
                                  column(2,numericInput(inputId = 'Cono8',min = 0,value = 0,label = "Cono")),
                                  column(2,numericInput(inputId = 'TAmbiente8',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                                  column(2,numericInput(inputId = 'THormigon8',min = 0,value = 0,label = "Temperatura Hormigon [°C]"))
                         ),
                fluidRow(column(2,h3("Hora 4")),
                         column(2,timeInput("HoraCono4", "Hora",seconds = FALSE)),
                                    column(2,numericInput(inputId = 'Cono9',min = 0,value = 0,label = "Cono")),
                                    column(2,numericInput(inputId = 'TAmbiente9',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                         column(2,numericInput(inputId = 'THormigon9',min = 0,value = 0,label = "Temperatura Hormigon [°C]"))
                         ),
                fluidRow(column(2,h3("Hora 4,5")),
                         column(2,timeInput("HoraCono45", "Hora",seconds = FALSE)),
                         column(2,numericInput(inputId = 'Cono10',min = 0,value = 0,label = "Cono")),
                         column(2,numericInput(inputId = 'TAmbiente10',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                         column(2,numericInput(inputId = 'THormigon10',min = 0,value = 0,label = "Temperatura Hormigon [°C]"))
                ),
                fluidRow(column(2,h3("Hora 5")),
                         column(2,timeInput("HoraCono5", "Hora",seconds = FALSE)),
                         column(2,numericInput(inputId = 'Cono11',min = 0,value = 0,label = "Cono")),
                         column(2,numericInput(inputId = 'TAmbiente11',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                         column(2,numericInput(inputId = 'THormigon11',min = 0,value = 0,label = "Temperatura Hormigon [°C]"))
                )))}
        
            
      
      if (4 %in% input$Opciones){
        insertUI(
          selector = "#IngresarEst",
          where = "beforeBegin",
          ui = box(id ="Fraguado",width = 12,h2("Fraguado"),h3("Ingrese datos tomados"),
                   fluidRow(column(2,h3("Hora 0")),
                            column(2,timeInput("HoraCarga1", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga1',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente1',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon1',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja1',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 0,5")),
                            column(2,timeInput("HoraCarga2", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga2',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente2',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon2',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja2',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 1")),
                            column(2,timeInput("HoraCarga3", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga3',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente3',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon3',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja3',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 1,5")),
                            column(2,timeInput("HoraCarga4", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga4',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente4',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon4',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja4',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 2")),
                            column(2,timeInput("HoraCarga5", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga5',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente5',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon5',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja5',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 2,5")),
                            column(2,timeInput("HoraCarga6", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga6',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente6',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon6',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja6',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 3")),
                            column(2,timeInput("HoraCarga7", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga7',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente7',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon7',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja7',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 3,5")),
                            column(2,timeInput("HoraCarga8", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga8',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente8',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon8',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja8',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 4")),
                            column(2,timeInput("HoraCarga9", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga9',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente9',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon9',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja9',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 4,5")),
                            column(2,timeInput("HoraCarga10", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga10',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente10',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon10',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja10',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 5")),
                            column(2,timeInput("HoraCarga11", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga11',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente11',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon11',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja11',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 5,5")),
                            column(2,timeInput("HoraCarga12", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga12',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente12',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon12',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja12',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 6")),
                            column(2,timeInput("HoraCarga13", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga13',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente13',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon13',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja13',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 6,5")),
                            column(2,timeInput("HoraCarga14", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga14',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente14',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon14',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja14',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 7")),
                            column(2,timeInput("HoraCarga15", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga15',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente15',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon15',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja15',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 7,5")),
                            column(2,timeInput("HoraCarga16", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga16',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente16',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon16',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja16',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 8")),
                            column(2,timeInput("HoraCarga17", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga17',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente17',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon17',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja17',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 8,5")),
                            column(2,timeInput("HoraCarga18", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga18',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente18',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon18',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja18',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 9")),
                            column(2,timeInput("HoraCarga19", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga19',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente19',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon19',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja19',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 9,5")),
                            column(2,timeInput("HoraCarga20", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga20',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente20',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon20',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja20',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 10")),
                            column(2,timeInput("HoraCarga21", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga21',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente21',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon21',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja21',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 10,5")),
                            column(2,timeInput("HoraCarga22", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga22',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente22',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon22',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja22',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 11")),
                            column(2,timeInput("HoraCarga23", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga23',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente23',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon23',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja23',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 11,5")),
                            column(2,timeInput("HoraCarga24", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga24',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente24',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon24',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja24',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 12")),
                            column(2,timeInput("HoraCarga25", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga25',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente25',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon25',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja25',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 12,5")),
                            column(2,timeInput("HoraCarga26", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga26',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente26',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon26',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja26',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 13")),
                            column(2,timeInput("HoraCarga27", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga27',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente27',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon27',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja27',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 13,5")),
                            column(2,timeInput("HoraCarga28", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga28',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente28',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon28',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja28',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 14")),
                            column(2,timeInput("HoraCarga29", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga29',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente29',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon28',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja29',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 14,5")),
                            column(2,timeInput("HoraCarga30", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga30',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente30',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon30',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja30',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 15")),
                            column(2,timeInput("HoraCarga31", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga31',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente31',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon31',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja31',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 15,5")),
                            column(2,timeInput("HoraCarga32", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga32',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente32',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon32',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja32',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 16")),
                            column(2,timeInput("HoraCarga33", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga33',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente33',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon33',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja33',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 16,5")),
                            column(2,timeInput("HoraCarga34", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga34',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente34',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon34',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja34',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 17")),
                            column(2,timeInput("HoraCarga35", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga35',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente35',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon35',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja35',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 17,5")),
                            column(2,timeInput("HoraCarga36", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga36',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente36',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon36',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja36',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 18")),
                            column(2,timeInput("HoraCarga37", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga37',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente37',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon37',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja37',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 18,5")),
                            column(2,timeInput("HoraCarga38", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga38',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente38',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon38',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja38',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 19")),
                            column(2,timeInput("HoraCarga39", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga39',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente39',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon39',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja39',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 19,5")),
                            column(2,timeInput("HoraCarga40", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga40',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente40',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon40',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja40',min = 0,value = 0,label = "D.Aguja[mm]"))
                   ),
                   fluidRow(column(2,h3("Hora 20")),
                            column(2,timeInput("HoraCarga41", "Hora",seconds = FALSE)),
                            column(2,numericInput(inputId = 'Carga41',min = 0,value = 0,label = "Carga[dN]")),
                            column(2,numericInput(inputId = 'TAmbiente41',min = 0,value = 0,label = "Temperatura Ambiente [°C]")),
                            column(2,numericInput(inputId = 'THormigon41',min = 0,value = 0,label = "Temperatura Hormigon [°C]")),
                            column(2,numericInput(inputId = 'Aguja41',min = 0,value = 0,label = "D.Aguja[mm]"))
                   )
                   
                   
          ))}
      
      if (5 %in% input$Opciones){
        if (input$ProvArenaIndustrial != ""){
        insertUI(
          selector = "#IngresarEst",
          where = "beforeBegin",
          ui = box(id ="Granulometria1",width = 12,h2("Arena Industrial"),h3("Ingrese la masa retenida")
                   ,column(1,numericInput(inputId = 'Pulgadas3AI',min = 0,value = 0,label = "3''",width = "100"))
                   ,column(1,numericInput(inputId = 'Pulgadas112AI',min = 0,value = 0,label = "1 1/2",width = "100"))
                   ,column(1,numericInput(inputId = 'Pulgadas1AI',min = 0,value = 0,label = "1''",width = "100"))
                   ,column(1,numericInput(inputId = 'Pulgadas34AI',min = 0,value = 0,label = "3/4",width = "100"))
                   ,column(1,numericInput(inputId = 'Pulgadas12AI',min = 0,value = 0,label = "1/2",width = "100"))
                   ,column(1,numericInput(inputId = 'Pulgadas38AI',min = 0,value = 0,label = "3/8",width = "100"))
                   ,column(1,numericInput(inputId = 'Pulgadas4AI',min = 0,value = 0,label = "4",width = "100"))
                   ,column(1,numericInput(inputId = 'Pulgadas8AI',min = 0,value = 0,label = "8",width = "100"))
                   ,column(1,numericInput(inputId = 'Pulgadas16AI',min = 0,value = 0,label = "16",width = "100"))
                   ,column(1,numericInput(inputId = 'Pulgadas30AI',min = 0,value = 0,label = "30",width = "100"))
                   ,column(1,numericInput(inputId = 'Pulgadas50AI',min = 0,value = 0,label = "50",width = "100"))
                   ,column(1,numericInput(inputId = 'Pulgadas100AI',min = 0,value = 0,label = "100",width = "100"))
                   ,column(1,numericInput(inputId = 'FondoAI',min = 0,value = 0,label = "Fondo",width = "100"))
                   
          ))}
        if (input$ProvArenaCorrectora != ""){
          insertUI(
            selector = "#IngresarEst",
            where = "beforeBegin",
            ui = box(id ="Granulometria2",width = 12,h2("Arena Correctora"),h3("Ingrese la masa retenida")
                     ,column(1,numericInput(inputId = 'Pulgadas3AC',min = 0,value = 0,label = "3''",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas112AC',min = 0,value = 0,label = "1 1/2",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas1AC',min = 0,value = 0,label = "1''",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas34AC',min = 0,value = 0,label = "3/4",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas12AC',min = 0,value = 0,label = "1/2",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas38AC',min = 0,value = 0,label = "3/8",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas4AC',min = 0,value = 0,label = "4",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas8AC',min = 0,value = 0,label = "8",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas16AC',min = 0,value = 0,label = "16",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas30AC',min = 0,value = 0,label = "30",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas50AC',min = 0,value = 0,label = "50",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas100AC',min = 0,value = 0,label = "100",width = "100"))
                     ,column(1,numericInput(inputId = 'FondoAC',min = 0,value = 0,label = "Fondo",width = "100"))
                     
            ))}
        if (input$ProvGravilla13 != ""){
          insertUI(
            selector = "#IngresarEst",
            where = "beforeBegin",
            ui = box(id ="Granulometria3",width = 12,h2("Gravilla 13mm"),h3("Ingrese la masa retenida")
                     ,column(1,numericInput(inputId = 'Pulgadas3G13',min = 0,value = 0,label = "3''",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas112G13',min = 0,value = 0,label = "1 1/2",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas1G13',min = 0,value = 0,label = "1''",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas34G13',min = 0,value = 0,label = "3/4",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas12G13',min = 0,value = 0,label = "1/2",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas38G13',min = 0,value = 0,label = "3/8",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas4G13',min = 0,value = 0,label = "4",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas8G13',min = 0,value = 0,label = "8",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas16G13',min = 0,value = 0,label = "16",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas30G13',min = 0,value = 0,label = "30",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas50G13',min = 0,value = 0,label = "50",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas100G13',min = 0,value = 0,label = "100",width = "100"))
                     ,column(1,numericInput(inputId = 'FondoG13',min = 0,value = 0,label = "Fondo",width = "100"))
                     
            ))}
        if (input$ProvGravilla20 != ""){
          insertUI(
            selector = "#IngresarEst",
            where = "beforeBegin",
            ui = box(id ="Granulometria4",width = 12,h2("Gravilla 20mm"),h3("Ingrese la masa retenida")
                     ,column(1,numericInput(inputId = 'Pulgadas3G20',min = 0,value = 0,label = "3''",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas112G20',min = 0,value = 0,label = "1 1/2",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas1G20',min = 0,value = 0,label = "1''",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas34G20',min = 0,value = 0,label = "3/4",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas12G20',min = 0,value = 0,label = "1/2",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas38G20',min = 0,value = 0,label = "3/8",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas4G20',min = 0,value = 0,label = "4",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas8G20',min = 0,value = 0,label = "8",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas16G20',min = 0,value = 0,label = "16",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas30G20',min = 0,value = 0,label = "30",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas50G20',min = 0,value = 0,label = "50",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas100G20',min = 0,value = 0,label = "100",width = "100"))
                     ,column(1,numericInput(inputId = 'FondoG20',min = 0,value = 0,label = "Fondo",width = "100"))
                     
            ))}
        if (input$ProvGrava40 != ""){
          insertUI(
            selector = "#IngresarEst",
            where = "beforeBegin",
            ui = box(id ="Granulometria5",width = 12,h2("Grava 40mm"),h3("Ingrese la masa retenida")
                     ,column(1,numericInput(inputId = 'Pulgadas3G40',min = 0,value = 0,label = "3''",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas112G40',min = 0,value = 0,label = "1 1/2",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas1G40',min = 0,value = 0,label = "1''",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas34G40',min = 0,value = 0,label = "3/4",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas12G40',min = 0,value = 0,label = "1/2",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas38G40',min = 0,value = 0,label = "3/8",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas4G40',min = 0,value = 0,label = "4",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas8G40',min = 0,value = 0,label = "8",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas16G40',min = 0,value = 0,label = "16",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas30G40',min = 0,value = 0,label = "30",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas50G40',min = 0,value = 0,label = "50",width = "100"))
                     ,column(1,numericInput(inputId = 'Pulgadas100G40',min = 0,value = 0,label = "100",width = "100"))
                     ,column(1,numericInput(inputId = 'FondoG40',min = 0,value = 0,label = "Fondo",width = "100"))
                     
            ))}
        
      }
      humedad<-c(0,0,0,input$HumedadAI,input$HumedadAC,input$HumedadG13,input$HumedadG20,input$HumedadG40,0,0,0,0,0)
      volh<-input$VolumenHormigon
      m3<-c(input$VolCemento,input$VolFiller,input$VolAguaTotal,input$VolArenaIndustrial,input$VolArenaCorrectora,input$VolGravilla13
            ,input$VolGravilla20,input$VolGrava40,input$VolAditivo1,input$VolAditivo2,input$VolAditivo3,input$VolAditivo4)
      
      #if(contador>1) ingreso de datos
    }
    if(input$IngresarEst ==2){
      removeUI(multiple = FALSE,
               selector = "div:has(> #Dosificacion)")
      removeUI(multiple = FALSE,
               selector = "div:has(> #Probetas)")
      removeUI(multiple = FALSE,
               selector = "div:has(> #Conos)")
      removeUI(multiple = FALSE,
               selector = "div:has(> #Fraguado)")
      removeUI(multiple = FALSE,
               selector = "div:has(> #Granulometria1)")
      removeUI(multiple = FALSE,
               selector = "div:has(> #Granulometria2)")
      removeUI(multiple = FALSE,
               selector = "div:has(> #Granulometria3)")
      removeUI(multiple = FALSE,
               selector = "div:has(> #Granulometria4)")
      removeUI(multiple = FALSE,
               selector = "div:has(> #Granulometria5)")
      insertUI(
        selector = "#IngresarEst",
        where = "beforeBegin",
        ui =h1("Datos Ingresados Correctamente")
        
      ) 
      insertUI(
        selector = "#IngresarEst",
        where = "afterEnd",
        ui =column(2, 
                   useShinyjs(),
                   extendShinyjs(text = jscode),
                   actionButton("refresh", "Nuevo ingreso",icon=icon("refresh")))
        
      )
      
      
    }
  })
  
  observeEvent(input$refresh, {
    js$refresh();
  })
  observe({
    shinyjs::hide("IngresarEst")
    
    if(input$IngresarEst <(2))
      shinyjs::show("IngresarEst")
  })
  
  
  datasetEstudio <- reactive({
    switch(input$DataEstudio,
           "Mostrar Todo" = TodoEstudio(input$Estudio),
           "Mostrar Por ID" = MostrarEstudio(input$Estudio),
           "Mostrar Entre Fechas" =MostrarEstudioFechas(as.character(input$fechas))
    )
  })
  datasetArido <- reactive({
    switch(input$DataArido,
           "Mostrar Todo" = TodoArido(input$Estudio),
           "Mostrar Por ID De Estudio" = MostrarArido(input$Estudio),
           "Mostrar Entre Fechas" =MostrarAridoFechas(as.character(input$fechas))
    )
  })
  datasetGranulometria <- reactive({
    switch(input$DataGranulometria,
           "Mostrar Todo" = TodoGranulometria(input$Estudio),
           "Mostrar Por ID De Estudio" = MostrarGranulometria(input$Estudio),
           "Mostrar Entre Fechas" =MostrarGranulometriaFechas(as.character(input$fechas))
    )
  })
  
  
  
  output$Estudio<-DT::renderDataTable({DT::datatable(datasetEstudio(),options = list(autoWidth = TRUE))})
  output$Arido<-DT::renderDataTable({DT::datatable(datasetArido(),options = list(autoWidth = TRUE))})
  output$Granulometria<- DT::renderDataTable({DT::datatable(datasetGranulometria(), 
                                                            options = list(autoWidth = TRUE,scrollX = TRUE,pageLength = 13))})
  
  output$GranulometriaGraf<-renderPlot(GraficoGranulometria(MostrarGranulometria(input$Estudio)))
})



# Run the application 
shinyApp(ui = ui, server = server)