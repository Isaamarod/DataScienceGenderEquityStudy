## app.R ##
library(shinydashboard)
library(rworldmap)
library(readxl)
library(dplyr)

women <- read.csv("wbldata.csv")
datos<-read_excel("/home/isa/Escritorio/Enlace hacia dataScience/proyecto/DataScienceGenderEquityStudy/pruebasShinnyGender/wbl2019paneldata.xlsx",sheet="WBL_panel_long")
##Filtrado por anyo

datos_2009<-datos %>% filter(reportyr =="2009")
datos_2010<-datos %>% filter(reportyr =="2010")
datos_2011<-datos %>% filter(reportyr =="2011")
choices <- c("Año 2009"=datos_2009, "Año 2010"=datos_2010)


ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Gender Equality Study"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global women mean", tabName = "Global", icon = icon("venus")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Global",
              fluidRow(
                box(plotOutput("mPlot", click = "plot_click",height = 500)),
                verbatimTextOutput("info"),
                selectInput("select", label = h3("Select box"), 
                            choices = list("Global index"="global_index","Going Places"="GOING_PLACES","Starting a job"="STARTING_A_JOB",
                                           "Getting a paid"="GETTING_PAID","Getting Married"="GETTING_MARRIED",
                                           "Having children"="HAVING_CHILDREN","Running business"="RUNNING_BUSINESS","Managing assests"="MANAGING_ASSETS",
                                           "Getting a pension"="GETTING_A_PENSION"), 
                            selected = "global_index"),
               
                box(
                  title = "years",
                  footer = "2019 representa todos los valores desde 2009-2018",
                  sliderInput("slider2", "Number of years:", 2009,2019, 2019)
                )
              )
      ),

      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  #selected
  output$value <- renderPrint({ input$select })
  

  # Generate the Map 
  output$mPlot <- renderPlot({
    mydb <- reactive({
      
      if(input$slider2 == "2009")
      {
        test <- c("2009")
      }
      else if(input$slider2 == "2010")
      {
        test <- c("2010")
      }
      else if(input$slider2 == "2011")
      {
        test <- c("2011")
      }
      else if(input$slider2 == "2012")
      {
        test <- c("2012")
      }
      else if(input$slider2 == "2013")
      {
        test <- c("2013")
      }
      else if(input$slider2 == "2014")
      {
        test <- c("2014")
      }
      else if(input$slider2 == "2015")
      {
        test <- c("2015")
      }
      else if(input$slider2 == "2016")
      {
        test <- c("2016")
      }
      else if(input$slider2 == "2017")
      {
        test <- c("2017")
      }
      else if(input$slider2 == "2018")
      {
        test <- c("2018")
      }
      else if(input$slider2 == "2019")
        {
          test <- c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")
      
      }})
    
    datoss<-women %>% filter(year == mydb())
    sPDF <- joinCountryData2Map(datoss,joinCode='ISO3', nameJoinColumn='country_code') 
    mapParams <- mapPolys(sPDF, nameColumnToPlot=input$select, mapRegion='world',
                          missingCountryCol='dark grey', numCats=10, 
                          colourPalette=c('green4','green1','greenyellow','yellow','yellow2','orange','coral','red','red3','red4'),
                          addLegend=TRUE,
                          oceanCol='light blue')
    mtext("[Grey Color: No Data Available]",side=1,line=-1)
  })
  
  output$info <- renderText({
    paste0("Pais", input$plot_click$x, input$plot_click$y)
    
  })
  
  

  
}

shinyApp(ui, server)
