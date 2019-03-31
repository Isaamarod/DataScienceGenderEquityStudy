## app.R ##
library(shinydashboard)
library(rworldmap)
library(tidyverse)
library(ggthemes)
library("Matrix")
library(data.table)
library(fastDummies)
library(plyr)
library(dplyr)

women <- read.csv("wbldata.csv")

mi_moda<-function(var){
  frec.var<-table(var)
  valor<-which(frec.var==max(frec.var)) # Elementos con el valor maximo
  names(valor)
}


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Gender Equality Study"),
                    ## Sidebar content
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Exploratory analysis", tabName = "explana", icon = icon("venus")),
                        menuItem("Global women mean", tabName = "Global", icon = icon("venus")),
                        menuItem("Distributions", tabName = "Distributions", icon = icon("venus")),
                        menuItem("Improvements and Worsening", tabName = "ImpandWors", icon = icon("venus")),
                        menuItem("Laws", tabName = "Laws", icon = icon("venus")),
                        menuItem("Widgets", tabName = "widgets", icon = icon("th"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        
                        tabItem(tabName = "explana",
                                fluidRow(
                                  box(
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    title= "Exploratory analysis for the whole dataset",
                                    
                                    valueBoxOutput("dfshape", width=100),
                                    
                                    
                                    box(
                                      title = "Countries by Region", width = NULL, background = "maroon",
                                      tableOutput("tablecountriesbyregion")
                                    ),
                                    
                                    box(
                                      title = "Countries by Incomes", width = NULL, background = "maroon",
                                      tableOutput("tablecountriesbyincomes")
                                    ),
                                    
                                    infoBoxOutput("GIrange", width=100),
                                    
                                    box(
                                      title = "Interval Global Index ", width = NULL, background = "light-blue",
                                      tableOutput("GIintervals")
                                    ),
                                    
                                    box(title="Fashion", width = NULL, background = "light-blue",
                                        tableOutput("tablemodas")
                                    ),
                                    
                                    box(title="Means", width = NULL, background = "light-blue",
                                        tableOutput("tablemeans")),
                                    
                                    box(title="Median", width = NULL, background = "light-blue",
                                        tableOutput("tablemedians")),
                                    
                                    box(
                                      title = "Statistics by topic", width = NULL, background = "maroon",
                                      tableOutput("tablegoingplaces"),
                                      selectInput("selectcolumn", label = "Select one topic", 
                                                  choices = list("Going places" = "GOING_PLACES", "Starting a job"="STARTING_A_JOB", "Getting paid"="GETTING_PAID", "Getting married"="GETTING_MARRIED",
                                                                 "Having children"="HAVING_CHILDREN","Running business"="RUNNING_BUSINESS","Managing assets"="MANAGING_ASSETS", "Getting a pension"="GETTING_A_PENSION", "Global Index"="global_index"), 
                                                  selected = "Going places")
                                    ),
                                    
                                    box(
                                      infoBoxOutput("topicsRange", width=100),
                                      selectInput("selectcolumn2", label = "Select one topic", 
                                                  choices = list("Going places" = "GOING_PLACES", "Starting a job"="STARTING_A_JOB", "Getting paid"="GETTING_PAID", "Getting married"="GETTING_MARRIED",
                                                                 "Having children"="HAVING_CHILDREN","Running business"="RUNNING_BUSINESS","Managing assets"="MANAGING_ASSETS", "Getting a pension"="GETTING_A_PENSION", "Global Index"="global_index"), 
                                                  selected = "Going places")
                                    ),
                                    box(
                                      infoBoxOutput("stdtopics", width=100),
                                      selectInput("selectcolumn3", label = "Select one topic", 
                                                  choices = list("Going places" = "GOING_PLACES", "Starting a job"="STARTING_A_JOB", "Getting paid"="GETTING_PAID", "Getting married"="GETTING_MARRIED",
                                                                 "Having children"="HAVING_CHILDREN","Running business"="RUNNING_BUSINESS","Managing assets"="MANAGING_ASSETS", "Getting a pension"="GETTING_A_PENSION", "Global Index"="global_index"), 
                                                  selected = "Going places")
                                    )
                                    
                                  ),
                                  box(
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    title = "Covariance and Correlation",
                                    
                                    box(
                                      title = "Covariance", width = NULL, background = "light-blue",
                                      div(style = 'overflow-x: scroll', tableOutput("covariancetable"))
                                      
                                    ),
                                    
                                    box(
                                      title = "Correlation", width = NULL, background = "light-blue",
                                      div(style = 'overflow-x: scroll', tableOutput("corrtable"))
                                      
                                    )
                                    
                                    
                                  ),
                                  
                                  box(
                                    status = "warning",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    title = "Histogram and density",
                                    plotOutput("histanddens")
                                  )
                                  
                                )
                                
                        ),
                        
                        
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
                                ),
                                fluidRow(
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width=1050,
                                  title= "Countries that have improved the most",
                                  box(dataTableOutput("laws"))
                                )
                                
                                
                                
                        ),
                        
                        tabItem(tabName = "Laws",
                                
                                fluidRow(
                                  box(title="Years controls",
                                      solidHeader = TRUE,
                                      status = "warning",
                                      sliderInput("sliderboxplotlaw", "Select one year:", 2009,2018,2013))
                                ),
                                
                                fluidRow(
                                  
                                  box(
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 1050,
                                    title="Improvement arrows",
                                    plotOutput("arrowsLaw"),
                                    
                                    
                                    box(
                                      width = 600,
                                      title="Laws Selection",
                                      solidHeader = TRUE,
                                      status = "warning",
                                      
                                      box(selectInput("selectLaw", label = "Select a law", 
                                                      choices = list("Going Places" = "GOING PLACES",
                                                                     "Starting a job" = "STARTING A JOB",
                                                                     "Getting married" = "GETTING MARRIED",
                                                                     "Having children" = "HAVING_CHILDREN",
                                                                     "Running Business" = "RUNNING BUSINESS",
                                                                     "Managing assets" = "MANAGING ASSETS",
                                                                     "Getting a pension" = "GETTING A PENSION"), 
                                                      selected = "Getting married"),
                                          
                                          box(dataTableOutput("laws"))
                                          
                                      )),
                                    box(
                                      width = 600,
                                      title="Region Selection",
                                      solidHeader = TRUE,
                                      status = "warning",
                                      
                                      box(radioButtons("radioRegion", "Select one region to inspect: ", c("East Asia & Pacific" = "East Asia & Pacific",
                                                                                                          "Europe & Central Asia" = "Europe & Central Asia",
                                                                                                          "High income: OECD" = "High income: OECD",
                                                                                                          "Latin America & Caribbean" = "Latin America & Caribbean",
                                                                                                          "Middle East & North Africa" = "Middle East & North Africa",
                                                                                                          "South Asia" = "South Asia",
                                                                                                          "Sub-Saharan Africa" = "Sub-Saharan Africa"))),
                                      box(dataTableOutput("tabarrows"))
                                      
                                    )
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
  
  output$dfshape <- renderInfoBox({
    valueBox(
      "1870 x 49", "Observations x Features", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$GIrange <- renderInfoBox({
    
    infoBox(
      "Global Index Range (min,max)","23.13 - 100",icon=icon("list"),color="yellow"
    )
  })
  
  
  output$histanddens<-renderPlot({
    
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    datos_continuous_features <- datos_no_na %>% select_if(is.numeric)
    drop.cols <- c("year")
    datos_continuous_features <- datos_continuous_features %>% select(-drop.cols) 
    
    
    ggplot(gather(datos_continuous_features), aes(value)) + geom_histogram(aes(y=stat(density))) + geom_density(col="red") + facet_wrap(~key, scales = 'free_x')
  })
  
  output$stdtopics<-renderInfoBox({
    
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    
    columna <- input$selectcolumn3
    
    if(columna == "GOING_PLACES"){
      return(infoBox(
        "Standard Deviation",sd(datos_no_na$GOING_PLACES),icon=icon("list"),color="yellow"
      ))
    }else if(columna == "STARTING_A_JOB"){
      return(infoBox(
        "Standard Deviation",sd(datos_no_na$STARTING_A_JOB),icon=icon("list"),color="yellow"
      ))
    }else if(columna == "GETTING_PAID"){
      return(infoBox(
        "Standard Deviation",sd(datos_no_na$GETTING_PAID),icon=icon("list"),color="yellow"
      ))
    }else if(columna == "GETTING_MARRIED"){
      return(infoBox(
        "Standard Deviation",sd(datos_no_na$GETTING_MARRIED),icon=icon("list"),color="yellow"
      ))
      
    }else if(columna == "HAVING_CHILDREN"){
      return(infoBox(
        "Standard Deviation",sd(datos_no_na$HAVING_CHILDREN),icon=icon("list"),color="yellow"
      ))
      
    }else if(columna=="RUNNING_BUSINESS"){
      return(infoBox(
        "Standard Deviation",sd(datos_no_na$RUNNING_BUSINESS),icon=icon("list"),color="yellow"
      ))
      
    }else if(columna == "MANAGING_ASSETS"){
      return(infoBox(
        "Standard Deviation",sd(datos_no_na$MANAGING_ASSETS),icon=icon("list"),color="yellow"
      ))
      
    }else if(columna == "GETTING_A_PENSION"){
      return(infoBox(
        "Standard Deviation",sd(datos_no_na$GETTING_A_PENSION),icon=icon("list"),color="yellow"
      ))
      
    }else if(columna == "global_index"){
      return(infoBox(
        "Standard Deviation",sd(datos_no_na$global_index),icon=icon("list"),color="yellow"
      ))
      
    }
    
  })
  
  output$topicsRange<-renderInfoBox({
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    
    columna <- input$selectcolumn2
    
    if(columna == "GOING_PLACES"){
      return(infoBox(
        "Range",max(datos_no_na$GOING_PLACES)-min(datos_no_na$GOING_PLACES),icon=icon("list"),color="yellow"
      ))
    }else if(columna == "STARTING_A_JOB"){
      return(infoBox(
        "Range",max(datos_no_na$STARTING_A_JOB)-min(datos_no_na$STARTING_A_JOB),icon=icon("list"),color="yellow"
      ))
    }else if(columna == "GETTING_PAID"){
      return(infoBox(
        "Range",max(datos_no_na$GETTING_PAID)-min(datos_no_na$GETTING_PAID),icon=icon("list"),color="yellow"
      ))
    }else if(columna == "GETTING_MARRIED"){
      return(infoBox(
        "Range",max(datos_no_na$GETTING_MARRIED)-min(datos_no_na$GETTING_MARRIED),icon=icon("list"),color="yellow"
      ))
      
    }else if(columna == "HAVING_CHILDREN"){
      return(infoBox(
        "Range",max(datos_no_na$HAVING_CHILDREN)-min(datos_no_na$HAVING_CHILDREN),icon=icon("list"),color="yellow"
      ))
      
    }else if(columna=="RUNNING_BUSINESS"){
      return(infoBox(
        "Range",max(datos_no_na$RUNNING_BUSINESS)-min(datos_no_na$RUNNING_BUSINESS),icon=icon("list"),color="yellow"
      ))
      
    }else if(columna == "MANAGING_ASSETS"){
      return(infoBox(
        "Range",max(datos_no_na$MANAGING_ASSETS)-min(datos_no_na$MANAGING_ASSETS),icon=icon("list"),color="yellow"
      ))
      
    }else if(columna == "GETTING_A_PENSION"){
      return(infoBox(
        "Range",max(datos_no_na$GETTING_A_PENSION)-min(datos_no_na$GETTING_A_PENSION),icon=icon("list"),color="yellow"
      ))
      
    }else if(columna == "global_index"){
      return(infoBox(
        "Range",max(datos_no_na$global_index)-min(datos_no_na$global_index),icon=icon("list"),color="yellow"
      ))
      
    }
    
    
  })
  
  output$tablecountriesbyregion <- renderTable(table(women$region))
  
  output$tablecountriesbyincomes <- renderTable(table(women$income_group))
  
  output$covariancetable <- renderTable({
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    datos_continuous_features <- datos_no_na %>% select_if(is.numeric)
    drop.cols <- c("year")
    datos_continuous_features <- datos_continuous_features %>% select(-drop.cols) 
    
    return(cov(datos_continuous_features))
  })
  
  output$corrtable<-renderTable({
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    datos_continuous_features <- datos_no_na %>% select_if(is.numeric)
    drop.cols <- c("year")
    datos_continuous_features <- datos_continuous_features %>% select(-drop.cols) 
    
    return(cor(datos_continuous_features))
  })
  
  
  output$tablegoingplaces <- renderTable({
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    
    columna <- input$selectcolumn
    
    if(columna == "GOING_PLACES"){
      return(ddply(datos_no_na, "region", summarise, min=min(GOING_PLACES), firstQu.=quantile(GOING_PLACES,0.25),mean = mean(GOING_PLACES), median=median(GOING_PLACES), thirdQu.=quantile(GOING_PLACES,0.75), max=max(GOING_PLACES)))
    }else if(columna == "STARTING_A_JOB"){
      return(ddply(datos_no_na, "region", summarise, min=min(STARTING_A_JOB), firstQu.=quantile(STARTING_A_JOB,0.25),mean = mean(STARTING_A_JOB), median=median(STARTING_A_JOB), thirdQu.=quantile(STARTING_A_JOB,0.75), max=max(STARTING_A_JOB)))
    }else if(columna == "GETTING_PAID"){
      return(ddply(datos_no_na, "region", summarise, min=min(GETTING_PAID), firstQu.=quantile(GETTING_PAID,0.25),mean = mean(GETTING_PAID), median=median(GETTING_PAID), thirdQu.=quantile(GETTING_PAID,0.75), max=max(GETTING_PAID)))
    }else if(columna == "GETTING_MARRIED"){
      return(ddply(datos_no_na, "region", summarise, min=min(GETTING_MARRIED), firstQu.=quantile(GETTING_MARRIED,0.25),mean = mean(GETTING_MARRIED), median=median(GETTING_MARRIED), thirdQu.=quantile(GETTING_MARRIED,0.75), max=max(GETTING_MARRIED)))
    }else if(columna == "HAVING_CHILDREN"){
      return(ddply(datos_no_na, "region", summarise, min=min(HAVING_CHILDREN), firstQu.=quantile(HAVING_CHILDREN,0.25),mean = mean(HAVING_CHILDREN), median=median(HAVING_CHILDREN), thirdQu.=quantile(HAVING_CHILDREN,0.75), max=max(HAVING_CHILDREN)))
    }else if(columna=="RUNNING_BUSINESS"){
      return(ddply(datos_no_na, "region", summarise, min=min(RUNNING_BUSINESS), firstQu.=quantile(RUNNING_BUSINESS,0.25),mean = mean(RUNNING_BUSINESS), median=median(RUNNING_BUSINESS), thirdQu.=quantile(RUNNING_BUSINESS,0.75), max=max(RUNNING_BUSINESS)))
    }else if(columna == "MANAGING_ASSETS"){
      return(ddply(datos_no_na, "region", summarise, min=min(MANAGING_ASSETS), firstQu.=quantile(MANAGING_ASSETS,0.25),mean = mean(MANAGING_ASSETS), median=median(MANAGING_ASSETS), thirdQu.=quantile(STARTING_A_JOB,0.75), max=max(MANAGING_ASSETS)))
    }else if(columna == "GETTING_A_PENSION"){
      return(ddply(datos_no_na, "region", summarise, min=min(GETTING_A_PENSION), firstQu.=quantile(GETTING_A_PENSION,0.25),mean = mean(GETTING_A_PENSION), median=median(GETTING_A_PENSION), thirdQu.=quantile(GETTING_A_PENSION,0.75), max=max(GETTING_A_PENSION)))
    }else if(columna == "global_index"){
      return(ddply(datos_no_na, "region", summarise, min=min(global_index), firstQu.=quantile(global_index,0.25),mean = mean(global_index), median=median(global_index), thirdQu.=quantile(global_index,0.75), max=max(global_index)))
    }
    
  })
  
  
  output$GIintervals <- renderTable({
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    intervalos <- cut(datos_no_na$global_index,breaks=seq(23.13, 100,length=nclass.Sturges(datos_no_na$global_index)),include.lowest=TRUE)
    return (table(intervalos))
    
  })
  
  output$tablemodas <- renderTable({
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    intervalos=cut(datos_no_na$global_index,breaks=seq(23.13, 100,length=nclass.Sturges(datos_no_na$global_index)),include.lowest=TRUE)
    topics = c("income_group","GOING PLACES","STARTING A JOB","GETTING PAID","GETTING MARRIED","HAVING CHILDREN", "RUNNING BUSINESS", "MANAGING ASSETS", "GETTING A PENSION", "global_index")
    modas = c(mi_moda(datos_no_na$income_group),mi_moda(datos_no_na$GOING_PLACES),mi_moda(datos_no_na$STARTING_A_JOB),mi_moda(datos_no_na$GETTING_PAID),mi_moda(datos_no_na$GETTING_MARRIED),mi_moda(datos_no_na$HAVING_CHILDREN), mi_moda(datos_no_na$RUNNING_BUSINESS), mi_moda(datos_no_na$MANAGING_ASSETS),mi_moda(datos_no_na$GETTING_A_PENSION), mi_moda(intervalos))
    
    df_modas <- tibble("variable"=topics,"moda"=modas)
    
    return(df_modas)
  })
  
  output$tablemeans <- renderTable({
    
    topics = c("GOING PLACES","STARTING A JOB","GETTING PAID","GETTING MARRIED","HAVING CHILDREN", "RUNNING BUSINESS", "MANAGING ASSETS", "GETTING A PENSION", "global_index")
    medias = c(92.77108,
               73.42704,
               63.01874,
               80.5087,
               48.50067,
               81.96118,
               85.95716,
               74.68206,
               73.8148)
    
    df_medias <- tibble("variable"=topics,"means"=medias)
    
    return(df_medias)
  })
  
  
  output$tablemedians <- renderTable({
    
    topics = c("GOING PLACES","STARTING A JOB","GETTING PAID","GETTING MARRIED","HAVING CHILDREN", "RUNNING BUSINESS", "MANAGING ASSETS", "GETTING A PENSION", "global_index")
    medias = c(100,
               75,
               75,
               80,
               40,
               75,
               100,
               75,
               75)
    
    df_medias <- tibble("variable"=topics,"medians"=medias)
    
    return(df_medias)
  })
  
  output$involutioning <- renderPlot({
    involutionaring_countries <- women %>% filter(country_code %in% c("BHR","UZB","SVN"))
    ggplot(involutionaring_countries, aes(x=year, y=global_index, group=country)) + geom_line(aes(color=country))+ geom_point(aes(color=country))
  })
  
  output$evolutioning <- renderPlot({
    
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    
    year_2018 <- datos_no_na %>% filter(year %in% c(2018))
    year_2009 <- datos_no_na %>% filter(year %in% c(2009))
    year_2018 <- year_2018 %>% filter(!(country_code=="SSD"))
    
    year_2018$improvement<- year_2018$global_index - year_2009$global_index
    year_2018$global_index_2009 <- year_2009$global_index
    
    
    highest_evolution_countries <-year_2018 %>% select(one_of("country_code","country","improvement")) %>% arrange(improvement,desc(country)) %>% top_n(5)
    datos_highest_evolution_countries <- datos_no_na %>% filter(country_code %in% highest_evolution_countries$country_code)
    
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
  
  #Laws
  output$laws <- renderDataTable({
    
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    
    year_2009 <- datos_no_na %>% filter(year %in% c(2009))
    year_2018 <- datos_no_na %>% filter(year %in% c(input$sliderboxplotlaw))
    
    year_2018[!(year_2018$country %in% year_2009$country),]
    
    #year_2018 <- year_2018 %>% filter(!(country_code=="SSD"))
    year_2018$improvement<- year_2018$HAVING_CHILDREN - year_2009$HAVING_CHILDREN
    year_2018$global_index_2009 <- year_2009$HAVING_CHILDREN
    
    for_improvement_plot <-tibble :: tibble(country = year_2018$country,global_index_selectedyear=year_2018$global_index_2009, global_index_2018 = year_2018$HAVING_CHILDREN, region=year_2018$region)
    
    
    df <- for_improvement_plot %>% filter(region==input$radioRegion) %>% select(-one_of(c("region"))) %>% mutate(direction = ifelse(global_index_2018 - global_index_selectedyear >0, "up", "down")) %>% melt(id=c("country","direction"))
    
    df%>%select(one_of(c("country","variable","value")))
    
    
  },
  options = 
    list(pagingType = "simple",pageLength = 10)
  )
  
  output$arrowsLaw <- renderPlot({
    datos_na <- women[is.na(women$global_index),]
    datos_no_na <- setdiff(women, datos_na)
    
    year_2009 <- datos_no_na %>% filter(year %in% c(2009))
    year_2018 <- datos_no_na %>% filter(year %in% c(input$sliderboxplotlaw))
    
    year_2018[!(year_2018$country %in% year_2009$country),]
    
    #year_2018 <- year_2018 %>% filter(!(country_code=="SSD"))
    year_2018$improvement<- year_2018$HAVING_CHILDREN - year_2009$HAVING_CHILDREN
    year_2018$global_index_2009 <- year_2009$c(input$selectLaw)
    
    for_improvement_plot <-tibble :: tibble(country = year_2018$country,global_index_2009=year_2018$global_index_2009, global_index_2018 = year_2018$HAVING_CHILDREN, region=year_2018$region)
    
    
    df <- for_improvement_plot %>% filter(region==input$radioRegion) %>% select(-one_of(c("region"))) %>% mutate(direction = ifelse(global_index_2018 - global_index_2009 >0, "up", "down")) %>% melt(id=c("country","direction"))
    
    
    ggplot(df, aes(x=country,y=value, color=variable, group=country)) + geom_point(size=4) + geom_path(aes(color=direction), arrow = arrow()) + theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) 
    
    
  })
  
  
}

shinyApp(ui, server)