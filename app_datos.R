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
library(plotly)
library(DT)
library(V8)

jscode <- "shinyjs.refresh = function() { history.go(0); }"
# install.packages("ODB","DT") en caso de usar en otro equipo hay que instalar estos paquetes

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
  conexion<-odb.open("Estudios.odb")
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
  conexion<-odb.open("Estudios.odb")
  
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
  
  conexion<-odb.open("Estudios.odb")
  orden<- "SELECT * FROM \"Estudio\""
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
  
}
MostrarEstudio<-function(Estudio){
  
  conexion<-odb.open("Estudios.odb")
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
  
  conexion<-odb.open("Estudios.odb")
  
  orden<-"INSERT INTO \"Aridos\" (\"ID_Estudio\",\"Tipo_muestra\",\"Fecha_muestreo\",\"Fecha_analisis\",\"Planta_estudio\") VALUES ("
  orden<-paste(orden,IdEstudio,",'")
  orden<- paste(orden,TipoMuestra,"',")
  orden<- paste(orden,Muestreo,",",Analisis,",'")
  orden<- paste(orden,Planta,"'",")")
  odb.write(conexion,orden) 
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}
MostrarArido<-function(Estudio){
  
  conexion<-odb.open("Estudios.odb")
  orden<- paste("SELECT * FROM \"Aridos\" WHERE \"ID_Estudio\" =",Estudio)
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
  
}

TodoArido<-function(Estudio){
  
  conexion<-odb.open("Estudios.odb")
  orden<- "SELECT * FROM \"Aridos\""
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
  
}

MostrarAridoFechas<-function(fecha){
  conexion<-odb.open("Estudios.odb")
  
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
  
  conexion<-odb.open("Estudios.odb")
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
  
  
  conexion<-odb.open("Estudios.odb")
  orden<- paste("SELECT * FROM \"Aridos\" INNER JOIN \"Granulometria\" ON \"Aridos\".\"ID_Muestra\" 
  = \"Granulometria\".\"ID_Muestra\" WHERE \"ID_Estudio\" =",Estudio)
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
  return(muestra)
  
}

TodoGranulometria<-function(Estudio){
  
  conexion<-odb.open("Estudios.odb")
  orden<- "SELECT * FROM \"Aridos\" INNER JOIN \"Granulometria\" ON \"Aridos\".\"ID_Muestra\" 
  = \"Granulometria\".\"ID_Muestra\""
  muestra<-odb.read(conexion,orden)
  #odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  return(muestra)
  
}

MostrarGranulometriaFechas<-function(fecha){
  conexion<-odb.open("Estudios.odb")
  
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
  
  conexion<-odb.open("Estudios.odb")
  orden<-"INSERT INTO \"Finura\" (\"ID_Muestra\",\"Modulo_finura\") VALUES ("
  orden<-paste(orden,IdMuestra,",")
  orden<- paste(orden,ModuloFinura,")")
  odb.write(conexion,orden)
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}
InsertarFinos<-function(IdMuestra,PSS,PSL){
  Porcentaje=(PSS-PSL)/PSS*100
  conexion<-odb.open("Estudios.odb")
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
  
  conexion<-odb.open("Estudios.odb")
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
  
  conexion<-odb.open("Estudios.odb")
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
  
  conexion<-odb.open("Estudios.odb")
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
  
  conexion<-odb.open("Estudios.odb")
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
  

  conexion<-odb.open("Estudios.odb")
  orden<-"INSERT INTO \"Proveedor\" (\"ID_Muestra\",\"Nombre_proveedor\") VALUES ("                                   
  orden<-paste(orden,IdMuestra,",'")
  orden<-paste(orden,Proveedor,"')")
  odb.write(conexion,orden) 
  #  odb.close(conexion,write = TRUE) USAR CUANDO EL SERVIDOR ESTE DISPONIBLE
  
}

ui <- dashboardPage(skin = "green",
  dashboardHeader(title="Estudio Aridos"),
  dashboardSidebar(
    menuItem(text="Menu",icon=icon("bars"),
             menuSubItem(text="Mostrar Estudio",tabName="BuscarEstudio", icon=icon("search")),
             menuSubItem(text="Mostrar Arido",tabName="BuscarArido",icon=icon("search")),
             menuSubItem(text="Mostrar Granulometria",tabName="BuscarGranulometria", icon=icon("search")),
             menuSubItem(text="Ingresar Estudio",tabName="InsertarEstudio",icon=icon("cloud")))
    
    
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
              column(3,numericInput('EstudioGr',"Ingresar Codigo Estudio",min = 0,0)),
              box(DT::dataTableOutput("Granulometria"),width = 12),
              box(plotOutput("GranulometriaGraf"),width = 12)),
      tabItem(tabName="InsertarEstudio",
              box(id ="Menu",width = 12,fluidRow(
                column(3,textInput("NombreEstudio", "Nombre Del Estudio", "Ingrese Nombre")),
                column(3,textInput("Proovedor", "Nombre Del Proveedor", "Ingrese Nombre")),
                column(3,selectInput("Planta", "Planta", c("Lonquen", "Divisa 1","Divisa 2"))),
                column(3,selectInput("Tipo", "Tipo de material", c("Arena Industrial", "Arena Correctora","Grava","Gravilla","Gravilla 13mm")))),
                
                fluidRow(column(6,numericInput('Cantidad',"Cantidad De Muestras Tomadas",width = "25%",min = 1,1)),
                         column(3,selectInput("PlantaProveedor", "Origen proveedor", c("Lonquen", "Divisa 1","Divisa 2"))),         
                column(3,checkboxGroupInput("Opciones", label = h3("Estudios A Realizar"), 
                                            choices = list("Granulometria" = 1, "Finos" = 2, "Densidad Real" = 3, "Densidad Aparente" = 4, "Equivalencia Arena" = 5, "Cubicidad" = 6),
                                            selected = 1)))
                ),useShinyjs(),actionButton("IngresarEst", "INGRESAR",icon=icon("cloud"))
    )
  )))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  
  observeEvent(input$IngresarEst,{
    #if(contador==1) hacer ingreso de estudio
    
    if(input$IngresarEst <=(input$Cantidad)){
    removeUI(multiple = FALSE,
             selector = "div:has(> #Menu)"
    )
      removeUI(multiple = FALSE,
               selector = "div:has(> #FechaEstudio)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #Granulometriabox)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #Finos)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #DensidadR)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #DensidadAp)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #EqArena)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #Cubicidad)"
      )
    insertUI(
      selector = "#IngresarEst",
      where = "beforeBegin",
      ui = box(id ="FechaEstudio",width = 12,h2("Fechas"),h3("Ingrese las fechas"),br(),br(),br(),br(),
               fluidRow(column(2,dateInput('FechaMuestra','Fecha de toma de muestra:', format="yyyy-mm-dd",weekstart=1,language="es",value = "2000-01-01")),
               column(2,dateInput('FechaAnalisis','Fecha del analisis realizado:', format="yyyy-mm-dd",weekstart=1,language="es",value = "2000-01-01")))
      ))
    if ( 1 %in% input$Opciones){
    insertUI(
      selector = "#IngresarEst",
      where = "beforeBegin",                                      
      ui = box(id ="Granulometriabox",width = 12,h2("Granulometria"),h3("Ingrese la masa retenida")
                                    ,column(1,numericInput(inputId = 'Pulgadas3',min = 0,value = 0,label = "3''",width = "100"))
                                    ,column(1,numericInput(inputId = 'Pulgadas112',min = 0,value = 0,label = "1 1/2",width = "100"))
                                    ,column(1,numericInput(inputId = 'Pulgadas1',min = 0,value = 0,label = "1''",width = "100"))
                                    ,column(1,numericInput(inputId = 'Pulgadas34',min = 0,value = 0,label = "3/4",width = "100"))
                                    ,column(1,numericInput(inputId = 'Pulgadas12',min = 0,value = 0,label = "1/2",width = "100"))
                                    ,column(1,numericInput(inputId = 'Pulgadas38',min = 0,value = 0,label = "3/8",width = "100"))
                                    ,column(1,numericInput(inputId = 'Pulgadas4',min = 0,value = 0,label = "4",width = "100"))
                                    ,column(1,numericInput(inputId = 'Pulgadas8',min = 0,value = 0,label = "8",width = "100"))
                                    ,column(1,numericInput(inputId = 'Pulgadas16',min = 0,value = 0,label = "16",width = "100"))
                                    ,column(1,numericInput(inputId = 'Pulgadas30',min = 0,value = 0,label = "30",width = "100"))
                                    ,column(1,numericInput(inputId = 'Pulgadas50',min = 0,value = 0,label = "50",width = "100"))
                                    ,column(1,numericInput(inputId = 'Pulgadas100',min = 0,value = 0,label = "100",width = "100"))
                                    ,column(1,numericInput(inputId = 'Fondo',min = 0,value = 0,label = "Fondo",width = "100"))
               ))
    }
    if (2 %in% input$Opciones){
      insertUI(
      selector = "#IngresarEst",
      where = "beforeBegin",
      ui = box(id ="Finos",width = 12,h2("Finos"),h3("Ingrese datos tomados"),
               column(2,numericInput(inputId = 'PSS',min = 0,value = 0,label = "Peso Seco Sucio",width = "150")),
               column(2,numericInput(inputId = 'PSL',min = 0,value = 0,label = "Peso Seco Lavado",width = "150"))
      ))}
    if (3 %in% input$Opciones){
      if(input$Tipo =="Grava"||input$Tipo =="Gravilla"||input$Tipo =="Gravilla 13mm"){
    insertUI(
      selector = "#IngresarEst",
      where = "beforeBegin",
      ui = box(id ="DensidadR",width = 12,h2("Densidad Real"),h3("Ingrese datos tomados"),
               column(2,numericInput(inputId = 'PSeco',min = 0,value = 0,label = "Peso Seco",width = "150")),
               column(2,numericInput(inputId = 'PSSS',min = 0,value = 0,label = "Peso SSS",width = "150")),
               column(2,numericInput(inputId = 'PSumergido',min = 0,value = 0,label = "Peso Sumergido",width = "150"))
               
      
      ))}else{
        insertUI(
          selector = "#IngresarEst",
          where = "beforeBegin",
          ui = box(id ="DensidadR",width = 12,h2("Densidad Real"),h3("Ingrese datos tomados"),
                   column(2,numericInput(inputId = 'PSeco',min = 0,value = 0,label = "Peso Seco",width = "150")),
                   column(2,numericInput(inputId = 'PSSS',min = 0,value = 0,label = "Peso SSS",width = "150")),
                   column(2,numericInput(inputId = 'MasaAgua',min = 0,value = 0,label = "Masa Matraz (Agua)",width = "150")),
                   column(2,numericInput(inputId = 'MasaMaterial',min = 0,value = 0,label = "Masa Matraz (Material)",width = "150"))
          ))
      }
      }
    if (4 %in% input$Opciones){
    insertUI(
      selector = "#IngresarEst",
      where = "beforeBegin",
      ui = box(id ="DensidadAp",width = 12,h2("Densidad Aparente"),h3("Ingrese datos tomados"),
               column(2,numericInput(inputId = 'Medicion1',min = 0,value = 0,label = "Medicion 1",width = "150")),
               column(2,numericInput(inputId = 'Medicion2',min = 0,value = 0,label = "Medicion 2",width = "150")),
               column(2,numericInput(inputId = 'Medicion3',min = 0,value = 0,label = "Medicion 3",width = "150")),
               column(2,numericInput(inputId = 'Volumen',min = 0,value = 0,label = "Volumen",width = "150")),
               column(2,numericInput(inputId = 'PorcentajeHumedad',min = 0,value = 0,label = "%Humedad",width = "150"))
               
      ))}
    
    if (5 %in% input$Opciones){
    insertUI(
      selector = "#IngresarEst",
      where = "beforeBegin",
      ui = box(id ="EqArena",width = 12,h2("Arena Equivalente"),h3("Ingrese datos tomados"),
               column(2,numericInput(inputId = 'Na1',min = 0,value = 0,label = "Na1",width = "150")),
               column(2,numericInput(inputId = 'Nt1',min = 0,value = 0,label = "Nt1",width = "150")),
               column(2,numericInput(inputId = 'Na2',min = 0,value = 0,label = "Na2",width = "150")),
               column(2,numericInput(inputId = 'Nt2',min = 0,value = 0,label = "Nt2",width = "150"))
               
      ))}
    if (6 %in% input$Opciones){
      insertUI(
        selector = "#IngresarEst",
        where = "beforeBegin",
        ui = box(id ="Cubicidad",width = 12,h2("Cubicidad"),h3("Ingrese datos tomados"),
                 column(2,numericInput(inputId = 'PesoMuestra',min = 0,value = 0,label = "Peso Muestra",width = "150")),
                 column(2,numericInput(inputId = 'PesoRodado',min = 0,value = 0,label = "Peso Rodado",width = "150")),
                 column(2,numericInput(inputId = 'PesoChancado',min = 0,value = 0,label = "Peso Chancado",width = "150")),
                 column(2,numericInput(inputId = 'PesoLaja',min = 0,value = 0,label = "Peso Laja",width = "150"))
                 
        ))}
    #if(contador>1) ingreso de datos
    }
    
    #Metodo para el final de los ingresos
    if(input$IngresarEst ==(input$Cantidad+1)){
      
      removeUI(multiple = FALSE,
               selector = "div:has(> #FechaEstudio)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #Granulometriabox)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #Finos)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #DensidadR)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #DensidadAp)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #EqArena)"
      )
      removeUI(multiple = FALSE,
               selector = "div:has(> #Cubicidad)"
      )
    
      
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
      
    )}
    
    })
  
  observeEvent(input$refresh, {
    js$refresh();
  })
  observe({
    shinyjs::hide("IngresarEst")
    
    if(input$IngresarEst <(input$Cantidad+1))
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
           "Mostrar Por ID De Estudio" = MostrarGranulometria(input$EstudioGr),
           "Mostrar Entre Fechas" =MostrarGranulometriaFechas(as.character(input$fechas))
    )
  })
  
  
  
  output$Estudio<-DT::renderDataTable({DT::datatable(datasetEstudio(),options = list(autoWidth = TRUE))})
  output$Arido<-DT::renderDataTable({DT::datatable(datasetArido(),options = list(autoWidth = TRUE))})
  output$Granulometria<- DT::renderDataTable({DT::datatable(datasetGranulometria(), 
                        options = list(autoWidth = TRUE,scrollX = TRUE,pageLength = 13))})
  
  output$GranulometriaGraf<-renderPlot(GraficoGranulometria(MostrarGranulometria(input$EstudioGr)))
})
  


# Run the application 
shinyApp(ui = ui, server = server)