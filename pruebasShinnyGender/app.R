## app.R ##
library(shinydashboard)
library(rworldmap)
library(tidyverse)
library(ggthemes)
library("Matrix")
library(data.table)
library(fastDummies)

women <- read.csv("wbldata.csv")

ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Gender Equality Study"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global women mean", tabName = "Global", icon = icon("venus")),
      menuItem("Distributions", tabName = "Distributions", icon = icon("venus")),
      menuItem("Improvements and Worsening", tabName = "ImpandWors", icon = icon("venus")),
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
                            choices = list("Going Places"="GOING_PLACES","Starting a job"="STARTING_A_JOB",
                                           "Getting a paid"="GETTING_PAID","Getting Married"="GETTING_MARRIED",
                                           "Having children"="HAVING_CHILDREN","Running business"="RUNNING_BUSINESS","Managing assests"="MANAGING_ASSETS",
                                           "Getting a pension"="GETTING_A_PENSION"), 
                            selected = "HAVING_CHILDREN"),
                
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
      tabItem(tabName = "Distributions",
              
              
              fluidRow(
                box(title="Boxplot controls",
                    solidHeader = TRUE,
                    status = "warning",
                    sliderInput("sliderboxplot", "Select one year:", 2009,2018,2013))
              ),
              
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  title= "Global Index Distribution Per Region",
                  plotOutput("boxplot1", height = 500, click = "table_or_click"),
                  
                  box(
                    width = "100%",
                    solidHeader = TRUE,
                    status = "warning",
                    title="Table of selected points by click",
                    tableOutput('table_or_click'))
                  
                  ),
                
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  title= "Global Index Means",
                  plotOutput("gimeans", height = 500, click = "table_or_click2"),
                  
                  box(
                    width = "100%",
                    solidHeader = TRUE,
                    status = "warning",
                    title="Table of averages and points by click",
                    
                    box( title="Table of selected points", tableOutput('table_or_click2')),
                    
                    
                    box( title="Table of averages",tableOutput('table3'))
                    
                    
                    
                    )
                  
                  
                )
                
                
            ),
            fluidRow(
              box(
                status = "primary",
                solidHeader = TRUE,
                width = 1050,
                title="Same job equal remuneration - Global index, Violin Plot",
                plotOutput("violinPlot")
              )
            )
      ),
      
      
      #Distributions tab content
      tabItem(tabName = "widgets",
              h2("Hola")),
      
      tabItem(tabName = "ImpandWors",
              
              fluidRow(
                box(title="Years controls",
                    solidHeader = TRUE,
                    status = "warning",
                    sliderInput("sliderboxplot2", "Select one year:", 2009,2018,2013))
              ),
             
              fluidRow(
                
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 1050,
                  title="Improvement arrows",
                  plotOutput("arrows"),
                  
                  
                  box(
                    width = 600,
                    title="Region Selection",
                    solidHeader = TRUE,
                    status = "warning",
                    
                    box(radioButtons("radioB", "Select one region to inspect: ", c("East Asia & Pacific" = "East Asia & Pacific",
                                                                                   "Europe & Central Asia" = "Europe & Central Asia",
                                                                                   "High income: OECD" = "High income: OECD",
                                                                                   "Latin America & Caribbean" = "Latin America & Caribbean",
                                                                                   "Middle East & North Africa" = "Middle East & North Africa",
                                                                                   "South Asia" = "South Asia",
                                                                                   "Sub-Saharan Africa" = "Sub-Saharan Africa"))),
                    box(dataTableOutput("tabarrows"))
                    
                  )
                  
                )
                
              ),
              
              fluidRow(
                
                box(status = "primary",
                    solidHeader = TRUE,
                    width=1050,
                    title= "Countries that have worsened",
                    plotOutput("involutioning"),
                    
                    selectInput("selectworse", label = "Select a country", 
                                choices = list("Bahrain" = "Bahrain", "Slovenia" = "Slovenia", "Uzbekistan" = "Uzbekistan"), 
                                selected = "Slovenia"),
                    
                    dataTableOutput("tabworse")
              
                    )
  
              ),
              
              fluidRow(
                box(status = "primary",
                    solidHeader = TRUE,
                    width=1050,
                    title= "Countries that have improved the most",
                    plotOutput("evolutioning"),
                    
                    selectInput("selectbest", label = "Select a country", 
                                choices = list("Bolivia" = "BOL", "Congo, Dem. Rep." = "COD", "Guinea" = "GIN", "Maldives"="MDV", "Sao Tome and Principe"="STP"), 
                                selected = "COD"),
                    
                    dataTableOutput("tabimprov")
                    
                )
              )
              
              
              
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
  
  output$involutioning <- renderPlot({
    involutionaring_countries <- women %>% filter(country_code %in% c("BHR","UZB","SVN"))
    ggplot(involutionaring_countries, aes(x=year, y=global_index, group=country)) + geom_line(aes(color=country))+ geom_point(aes(color=country))
  })
  
  output$evolutioning <- renderPlot({
    highest_evolution_countries <-year_2018 %>% select(one_of("country_code","country","improvement")) %>% arrange(improvement,desc(country)) %>% top_n(5)
    datos_highest_evolution_countries <- datos %>% filter(country_code %in% highest_evolution_countries$country_code)
    
    ggplot(datos_highest_evolution_countries, aes(x=year, y=global_index, group=country)) + geom_line(aes(color=country))+ geom_point(aes(color=country))
  })
  
  output$boxplot1 <- renderPlot({
    ggplot(women%>%filter(year==input$sliderboxplot), aes(y=global_index, x=region)) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
  })
  
  output$table_or_click <- renderTable({
    if(!is.null(input$table_or_click)){
      nearPoints(women%>%filter(year==input$sliderboxplot)%>%select(one_of(c("country","region","income_group","global_index"))),input$table_or_click, addDist=FALSE)
    }else{
      head(women%>%filter(year==input$sliderboxplot)%>%select(one_of(c("country","region","income_group","global_index"))),n=5)
    }
  })
  
  output$tabworse<- renderDataTable({
    if(!is.null(input$selectworse)){
      involutionaring_countries <- women %>% filter(country %in% c(input$selectworse))
    }else{
      involutionaring_countries <- women %>% filter(country %in% c("Slovenia"))
    }
  }, options = list(
    pageLength=5,lengthMenu = c(5, 10, 15, 20), scrollX = TRUE, searching = FALSE
  ))
  
  
  output$tabimprov <- renderDataTable({
    if(!is.null(input$selectworse)){
      datos_highest_evolution_countries <- women %>% filter(country_code %in% c(input$selectbest))
    }else{
      datos_highest_evolution_countries <- women %>% filter(country_code %in% c("COD"))
    }
    
  },options = list(
    pageLength=5,lengthMenu = c(5, 10, 15, 20), scrollX = TRUE, searching = FALSE)
  )
  
  output$table_or_click2 <- renderTable({
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    datos_filtered <- datos_no_na%>%filter(year==input$sliderboxplot)
    means_df <- datos_filtered %>% group_by(region) %>% dplyr::summarize(Mean = mean(global_index))
    
    if(!is.null(input$table_or_click2)){
      nearPoints(datos_filtered%>%select(one_of(c("country","region","income_group","global_index"))),input$table_or_click2, addDist=FALSE)
    }else{
      means_df
    }
    
  })
  
  output$table3 <- renderTable({
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    datos_filtered <- datos_no_na%>%filter(year==input$sliderboxplot)
    means_df <- datos_filtered %>% group_by(region) %>% dplyr::summarize(Mean = mean(global_index))
    means_df
  })
  
  output$violinPlot <- renderPlot({
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    
    ggplot(datos_no_na %>% filter(year==input$sliderboxplot),aes(x=region, y=global_index, fill=equal_work_remuneration_law)) + geom_violin(trim=FALSE) + theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
  })

  
  output$arrows <- renderPlot({
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    
    year_2009 <- datos_no_na %>% filter(year %in% c(2009))
    year_2018 <- datos_no_na %>% filter(year %in% c(input$sliderboxplot2))
    
    year_2018[!(year_2018$country %in% year_2009$country),]
    
    year_2018 <- year_2018 %>% filter(!(country_code=="SSD"))
    year_2018$improvement<- year_2018$global_index - year_2009$global_index
    year_2018$global_index_2009 <- year_2009$global_index
    
    for_improvement_plot <-tibble :: tibble(country = year_2018$country,global_index_2009=year_2018$global_index_2009, global_index_2018 = year_2018$global_index, region=year_2018$region)
    
    
    df <- for_improvement_plot %>% filter(region==input$radioB) %>% select(-one_of(c("region"))) %>% mutate(direction = ifelse(global_index_2018 - global_index_2009 >0, "up", "down")) %>% melt(id=c("country","direction"))
    
    
    ggplot(df, aes(x=country,y=value, color=variable, group=country)) + geom_point(size=4) + geom_path(aes(color=direction), arrow = arrow()) + theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) 
    
    
  })
  
  output$tabarrows <- renderDataTable({
    
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    
    year_2009 <- datos_no_na %>% filter(year %in% c(2009))
    year_2018 <- datos_no_na %>% filter(year %in% c(input$sliderboxplot2))
    
    year_2018[!(year_2018$country %in% year_2009$country),]
    
    year_2018 <- year_2018 %>% filter(!(country_code=="SSD"))
    year_2018$improvement<- year_2018$global_index - year_2009$global_index
    year_2018$global_index_2009 <- year_2009$global_index
    
    for_improvement_plot <-tibble :: tibble(country = year_2018$country,global_index_selectedyear=year_2018$global_index_2009, global_index_2018 = year_2018$global_index, region=year_2018$region)
    
    
    df <- for_improvement_plot %>% filter(region==input$radioB) %>% select(-one_of(c("region"))) %>% mutate(direction = ifelse(global_index_2018 - global_index_selectedyear >0, "up", "down")) %>% melt(id=c("country","direction"))
    
    df%>%select(one_of(c("country","variable","value")))
    
    
  },
  options = 
    list(pagingType = "simple",pageLength = 10)
  )
  
  output$gimeans <- renderPlot({
    
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    datos_filtered <- datos_no_na%>%filter(year==input$sliderboxplot)
    
    
    ggplot(datos_filtered, aes(x=region, y=global_index)) + geom_jitter(width = 0.05, size = 2., color="#00BFC4") + stat_summary(geom = "point", fun.y = mean, colour = "#F8766D", size = 5)  + theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) + geom_hline(yintercept = mean(datos_filtered$global_index), color="#7CAE00", size=2)
  })
  
  # Join data frame 'comb_mdata' to the map
  sPDF <- joinCountryData2Map(women, joinCode='ISO3', nameJoinColumn='country_code')
  
  #selected
  output$value <- renderPrint({ input$select })
  
  # Generate the Map 
  output$mPlot <- renderPlot({
    mapParams <- mapPolys(sPDF, nameColumnToPlot=input$select, mapRegion='world',
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
