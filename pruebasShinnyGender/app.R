## app.R ##
library(shinydashboard)
library(rworldmap)
women <- read.csv("wbldata.csv")
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
                box(plotOutput("plot1", height = 250)),
                box(plotOutput("plot2", height = 250)),
                box(plotOutput("mPlot", click = "plot_click",height = 500)),
                verbatimTextOutput("info"),
                selectInput("select", label = h3("Select box"), 
                            choices = list("Choice 1" = women$HAVING_CHILDREN), 
                            selected = women$HAVING_CHILDREN),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                ),
                box(
                  title = "Controls",
                  sliderInput("slider2", "Number of observations:", 2009,2018, 2010)
                  #sliderInput("slider2", "Number of observations:", min(women$year),max(women$year), 2010)
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
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  output$plot2 <- renderPlot({
    womenS <- women$year[seq_len(input$slider2)]
    hist(women$GOING_PLACES)
  })
  # Join data frame 'comb_mdata' to the map
  sPDF <- joinCountryData2Map(women, joinCode='ISO3', nameJoinColumn='country_code')
  
  #selected
  output$value <- renderPrint({ input$select })
  
  # Generate the Map 
  output$mPlot <- renderPlot({
    mapParams <- mapPolys(sPDF, nameColumnToPlot=renderPrint(output$value), mapRegion='world',
                          missingCountryCol='dark grey', numCats=10, 
                          colourPalette=c('green4','green1','greenyellow','yellow','yellow2','orange','coral','red','red3','red4'),
                          #colourPalette=c('orchid','orchid1',' orchid2','magenta',' magenta1 ','magenta2 ','mediumorchid1',' mediumorchid2',' mediumorchid3',' mediumorchid4','hotpink4'),
                          addLegend=TRUE,
                          oceanCol='light blue')
    mtext("[Grey Color: No Data Available]",side=1,line=-1)
  })
  
  output$info <- renderText({
    paste0("Pais", input$plot_click$x, input$plot_click$y)
    
  })
  
  

  
}

shinyApp(ui, server)
