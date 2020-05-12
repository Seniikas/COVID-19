library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(DT)
library(shiny)
library(colourpicker)
library(plotly)
library(shinythemes)
library(rvest)
library(shinyBS)
library(data.table)

rm(list=ls())

url=read_html('https://www.worldometers.info/coronavirus/')

df=url%>%html_nodes('.main_table_countries_div')

df=url%>%html_nodes('.main_table_countries_div tbody')
df_t=df[[1]]
df_t_c=df[[1]] %>% html_nodes('tr td')%>% html_nodes('a')%>% 
  html_text()
#df_t_c=length(df_t_c)

#  Vectors

Country=c()
TotalCases=c()
NewCases=c()
TotalDeaths=c()
NewDeaths=c()
TotalRecovered=c()
ActiveCases=c()
Serious_Critical=c()
TotCases1Mpop=c()
TotDeaths1Mpop=c()
TotTests=c()
Tests1Mpop=c()


# Extract Nodes

for(i in 8:length(df_t_c)){
  Nodos=xml_child(df_t, i)
  Country=c(Country,xml_child(Nodos, 1)%>%html_text())
  TotalCases=c(TotalCases,xml_child(Nodos, 2)%>%html_text())
  NewCases=c(NewCases,xml_child(Nodos, 3)%>%html_text())
  TotalDeaths=c(TotalDeaths,xml_child(Nodos, 4)%>%html_text())
  NewDeaths=c(NewDeaths,xml_child(Nodos, 5)%>%html_text())
  TotalRecovered=c(TotalRecovered,xml_child(Nodos, 6)%>%html_text())
  ActiveCases=c(ActiveCases,xml_child(Nodos, 7)%>%html_text())
  Serious_Critical=c(Serious_Critical,xml_child(Nodos, 8)%>%html_text())
  TotCases1Mpop=c(TotCases1Mpop,xml_child(Nodos, 9)%>%html_text())
  TotDeaths1Mpop=c(TotDeaths1Mpop,xml_child(Nodos, 10)%>%html_text())
  TotTests=c(TotTests,xml_child(Nodos, 11)%>%html_text())  
  Tests1Mpop=c(Tests1Mpop,xml_child(Nodos, 12)%>%html_text())  
}

# Create dataframe

casos= data.frame(
  Country,
  TotalCases,
  NewCases,
  TotalDeaths,
  NewDeaths,
  TotalRecovered,
  ActiveCases,
  Serious_Critical,
  TotCases1Mpop,
  TotDeaths1Mpop, 
  TotTests,
  Tests1Mpop,
  stringsAsFactors = F
)

# Delete files

rm(df,df_t,Nodos,url,Country,
   TotalCases,
   NewCases,
   TotalDeaths,
   NewDeaths,
   TotalRecovered,
   ActiveCases,
   Serious_Critical,
   TotCases1Mpop,
   TotDeaths1Mpop,
   TotTests,
   Tests1Mpop,df_t_c,i)



colnames(casos)= c(
  "Pais",          "Casos_Totales"    ,   "Casos_Nuevos"    ,     "Total_Muertes"   ,   "Nuevas_Muertes"       ,
  "Total_Recuperados"  , "Casos_Activos"    ,  "Serious_Critical" ,"CasosTot1Mpop"  ,  "TotMuertes1Mpop"  ,
  "Tests_Totales"    ,     "Tests1Mpop"   
)
casos = casos %>% arrange(Pais)
ui=fluidPage (theme = shinytheme("simplex"),
              
  titlePanel(title=div(img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/SARS-CoV-2_without_background.png/1020px-SARS-CoV-2_without_background.png",
                           width = "75px", 
                           height = "75px"),
                       'Casos actualizados COVID-19')),

  tabsetPanel(
    tabPanel('Grafico',
             
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "paises",  
                             label = "Seleccionar los paises:",
                             choices =  unique(casos$Pais), 
                             selected = "Argentina",
                             multiple = TRUE),
                 checkboxInput('Normal',
                               'Normalizados por cantidad total de tests',
                               value=FALSE),

                 bsButton("BSGrafico", 
                          label = "Graficar", 
                          icon = icon("virus"), 
                          style = "success")
               ),
               mainPanel( 
                 plotlyOutput('GraficoCompara'),
                 h5("datos recolectados de la pagina: https://www.worldometers.info/coronavirus/")
  
                 )
             )
    ),
    
    tabPanel('Tabla',
             
             dataTableOutput("TablaCasos"),
             h5("datos recolectados de la pagina: https://www.worldometers.info/coronavirus/")
             
    ),
    
    br()
  )
)


server = function(input, output) { 
  
  
  v <- reactiveValues(doPlot = FALSE)
  
  #se hace lo mismo con todos los botones: cuando se apreta el boton, la variable (booleana) se vuelve verdadera
  #lo que habilita a que siga el resto del codigo como se vera mas adelante
  
  observeEvent(input$BSGrafico, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doPlot <-  input$BSGrafico
  })
  
  
  observeEvent(input$Grafico, {
    v$doPlot <- FALSE
  }) 
  
  observeEvent(input$Tabla, {
    v$doPlot <- FALSE
  })
  
  
  
  observeEvent(input$BSGrafico,{
    
    
    output$GraficoCompara <- renderPlotly({
      
      input$BSGrafico
      
      if(!v$doPlot) return()
      isolate({
          
          paisesG = casos %>% filter(Pais %in% input$paises)
          paises2 = transpose(paisesG)
          rownames(paises2) <- colnames(paisesG)
          colnames(paises2) <- paisesG$Pais
          paises2$Estadisticas = rownames(paises2)
          paises3 = paises2[2:length(paises2$Estadisticas),]
          for (i in 1:length(input$paises)){
            paises3[input$paises[i]] = as.numeric(gsub(",", "", gsub("\\.", "", paises3[,input$paises[i]])))
          }
          paises3 = replace(paises3,is.na(paises3),0)
          
          
        
        if(input$Normal){
      
          for (i in 1:length(input$paises)){
            paises3[input$paises[i]] = paises3[input$paises[i]] /paises3[paises3$Estadisticas=='Tests_Totales', input$paises[i]]
          }    
      
          fig <- plot_ly(paises3, y = paises3[,input$paises[1]], x = ~Estadisticas,type = 'bar', name = input$paises[1])
         
          if(length(input$paises) > 1) {  
            for (i in 2:length(input$paises)){
            fig <- fig %>% add_trace(y = paises3[,input$paises[i]], name = input$paises[i])
        }
          }
          fig <- fig %>% layout(yaxis = list(title = 'Normalizado'), barmode = 'group')
          

          
          
        }
        
        
        else{
          
          fig <- plot_ly(paises3, y = paises3[,input$paises[1]], x = ~Estadisticas,type = 'bar', name = input$paises[1])
          
          if(length(input$paises) > 1) {  
          for( i in 2:length(input$paises)){
            fig <- fig %>% add_trace(y = paises3[,input$paises[i]], name = input$paises[i])
          }
          
        }
          fig <- fig %>% layout(yaxis = list(title = 'Cantidad'), barmode = 'group')
          
          
          
        }
          
          fig
        
        
      })
    })
    
    
    
  })
  
  output$TablaCasos=renderDataTable({
    casos
    
  })
  
 
  
  }






shinyApp(ui = ui, server = server)