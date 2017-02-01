library(XLConnect)
library(shiny)

Operaciones<-function(datos){
  NewDatos<-data.frame(datos$Sucursal,datos$Producto)
  colnames(NewDatos)<-c("Sucursal","Producto")
  
  if("CEMPOV" %in% colnames(datos) ){
    NewDatos$CEMPOV<-datos$CEMPOV
    
  }
  if("CEMACT" %in% colnames(datos) ){
    NewDatos$CEMACT<-datos$CEMACT  
    
  }
  if("CEMYUR" %in% colnames(datos) ){
    NewDatos$CEMYUR<-datos$CEMYUR  
    
  }
  if("CEMACOR" %in% colnames(datos) ){
    NewDatos$CEMACOR<-datos$CEMACOR  
    
  }
  if("FLACOR" %in% colnames(datos) ){
    NewDatos$FLACOR<-datos$FLACOR  
    
  }
  if("FILLER" %in% colnames(datos) ){
    NewDatos$FILLER<-datos$FILLER  
    
  }
  if("CEMTA1" %in% colnames(datos) ){
    NewDatos$CEMTA1<-datos$CEMTA1  
    
  }
  if("CEM2" %in% colnames(datos) ){
    NewDatos$CEM2<-datos$CEM2  
    
  }
  if("FILLERC" %in% colnames(datos) ){
    NewDatos$FILLERC<-datos$FILLERC  
    
  }
  if("ACTIVI" %in% colnames(datos) ){
    NewDatos$ACTIVI<-datos$ACTIVI  
    
  }
  NewDatos$SUMA_CEMENTANTE <-(
    (if("CEMPOV" %in% colnames(datos) ){NewDatos$CEMPOV}else {0})
    +
    (if("CEMACT" %in% colnames(datos) ){NewDatos$CEMACT}else {0})
    +
    (if("CEMYUR" %in% colnames(datos) ){NewDatos$CEMYUR}else {0})
    +
    (if("CEMACOR" %in% colnames(datos) ){NewDatos$CEMACOR}else {0})
    +
    (if("FLACOR" %in% colnames(datos) ){NewDatos$FLACOR}else {0})
    +
    (if("FILLER" %in% colnames(datos) ){NewDatos$FILLER}else {0})
    +
    (if("CEMTA1" %in% colnames(datos) ){NewDatos$CEMTA1}else {0})
    +
    (if("CEM2" %in% colnames(datos) ){NewDatos$CEM2}else {0})
    +
    (if("FILLERC" %in% colnames(datos) ){NewDatos$FILLERC}else {0})
    +
    (if("ACTIVI" %in% colnames(datos) ){NewDatos$ACTIVI}else {0})
  )
  
  return(NewDatos)
}

ui <- fluidPage(
  fileInput("uploadFile", "XLSX file"),
  dataTableOutput("summary"),
  downloadButton('downloadData', 'Download')
)

server <- function(input, output) ({
  
  dataset<-reactive({ 
    inFile <- input$uploadFile
    wb<-loadWorkbook(inFile$datapath)
    dat <- readWorksheet(wb, sheet = "Hoja1")
    #dat<-read.xlsx(inFile$datapath, 1)
    return(dat)
  })
  
  
  output$summary <- renderDataTable({dataset()})
  
  output$downloadData <- downloadHandler(
    
    filename = function() { "Suma Cementantes.xlsx" },
    
    content = function(file){
      fname <- paste(file,"xlsx",sep=".")
      wb <- loadWorkbook(fname, create = TRUE)
      createSheet(wb, name = "Hoja1")
      writeWorksheet(wb, Operaciones(dataset()), sheet = "Hoja1") 
      saveWorkbook(wb)
      file.rename(fname,file)
    }
  )
})
shinyApp(ui = ui, server = server)
