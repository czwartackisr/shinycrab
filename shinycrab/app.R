library(shiny)
library(dplyr)
library(ggplot2)
library(DataCombine)
library(PerformanceAnalytics)
crab <- read.csv("data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
    titlePanel("Crab Bivariate Modeling (Czwartacki)"),
    sidebarLayout(
        sidebarPanel(
          
            selectInput("SpeciesCode", label = h3("Species Code Selector"),
                      choices = list("D003", "D004", "D005", "D130", "F001"), 
                      selected = 3, multiple = TRUE),
          
            selectInput("outcome", label = h3("Dependent Variable (Abundance)"),
                        choices = list("Harbor Trawl Juvenile CPUE" = "B90_JuvCPUE",
                                       "Harbor Trawl Subadult CPUE" = "B90_SubadultCPUE",
                                       "Harbor Trawl Adult CPUE" = "B90_AdultCPUE", 
                                       "Harbor Trawl CPUE" = "B90_CPUE",
                                       "Creek Trawl Juvenile CPUE" = "T38_JuvCPUE",
                                       "Creek Trawl Subadult CPUE" = "T38_SubadultCPUE",
                                       "Creek Trawl Adult CPUE" = "T38_AdultCPUE",
                                       "Creek Trawl CPUE" = "T38_CPUE",
                                       "SCECAP Creek Juvenile CPUE" = "E98_JuvCPUE",
                                       "SCECAP Creek Subadult CPUE" = "E98_SubadultCPUE",
                                       "SCECAP Creek Adult CPUE" = "E98_AdultCPUE",
                                       "SCECAP Creek CPUE" = "E98_CPUE",
                                       "SCECAP Open Water Juvenile CPUE" = "E99_JuvCPUE",
                                       "SCECAP Open Water Subadult CPUE" = "E99_SubadultCPUE",
                                       "SCECAP Open Water Adult CPUE" = "E99_AdultCPUE",
                                       "SCECAP Open Water CPUE" = "E99_CPUE",
                                       "4-hr potting Survey CPUE" = "P88_CPUE",
                                       "24-hr potting Survey CPUE" = "S16_CPUE",
                                       "Trammel Survey CPUE" = "T06_CPUE",
                                       "Ashley River Landings" = "AshleyLandings",
                                       "Cooper River Landings" = "CooperLandings",
                                       "Wando River Landings" = "WandoLandings",
                                       "Charleston Harbor Landings" = "ChsHarborLandings",
                                       "Charleston Harbor (all systems) Sum Landings" = "SumLandings",
                                       "Charleston Harbor (all systems) Mean Annual Landings" = "MeanLandings",
                                       "Ashley River Landings CPUE" = "AshleyLandingsCPUE", 
                                       "Cooper River Landings CPUE" = "CooperLandingsCPUE", 
                                       "Wando River Landings CPUE" = "WandoLandingsCPUE", 
                                       "Charleston Harbor Landings CPUE" = "ChsHarborLandingsCPUE", 
                                       "Charleston Harbor (all systems) Mean Annual CPUE" = "MeanLandingsCPUE"), selected = 1),
            
            
            selectInput("dataset", h3("Independent Variable (Group)"), choices = c("Abundance",
                                                                          "Salinity",
                                                                          "Temperature",
                                                                          "Precipitation",
                                                                          "Climate")),        
            HTML('</br>'),
            uiOutput('iv')
            
            
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Distribution", # Plots of distributions
                                 fluidRow(
                                     column(6, plotOutput("distribution1")),
                                     column(6, plotOutput("distribution2")))),
                        tabPanel("Scatterplot", 
                                 fluidRow(
                                   column(12, h4("Lag o (No Lag)"), plotOutput("scatterplot0")),
                                   column(12, h4("Lag 1 (Dep Var +1 yr)"), plotOutput("scatterplot1")),
                                   column(12, h4("Lag 2 (Dep Var +1 yr, Ind Var -1 yr)"), plotOutput("scatterplot2")))),
                        tabPanel("Regression Summary",
                                 fluidRow(
                                   column(12, h4("Lag 0 (No Lag)"), verbatimTextOutput("summary0")),
                                   column(12, h4("Lag 1 (Dep Var +1 yr)"), verbatimTextOutput("summary1")),
                                   column(12, h4("Lag 2 (Dep Var +1 yr, Ind Var -1 yr)"), verbatimTextOutput("summary2")))), # Regression output
                        tabPanel("Abundance Correlations", 
                                 fluidRow(
                                   column(6, h4("Current Variable Kendall's Correlation"), verbatimTextOutput("currentCorr")),
                                   column(12, h4("Harbor Trawl - Creek Trawl Abundance Correlations"), plotOutput("corrAbun1")),
                                   column(12, h4("Creek Trawl - Chas Harbor Landings"), plotOutput("corrAbun2")),
                                   column(12, h4("Harbor Trawl - Chas Harbor Landings"), plotOutput("corrAbun3")))),
                        tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                        
            )
        )
    ))



# SERVER
server <- function(input, output) {
    
    # list of data sets from--rich.shinyyapps.io/regression/--Still need to tie into DBs for switch
    datasetInput <- reactive({
      switch(input$dataset,
           "Abundance" = select(crab, 28:31, 33:36, 38:41, 44:47, 50, 53, 56, 57:68),
           "Salinity" = select(crab, 11:26, 32, 37, 42, 48, 51, 54),
           "Temperature" = select(crab, 9, 10, 27, 43, 49, 52, 55),    
           "Precipitation" = select(crab, 2:8),
           "Climate" = select(crab, 70:93))
    })
    
    # independent variable
    output$iv = renderUI({
      selectInput('iv', h5('Choose an Ind Var from Group'), choices = names(datasetInput()))
    })
    
    # regression formula
    regFormula <- reactive({
      as.formula(paste(input$outcome, '~', input$iv))
    })
    
    # bivariate model
    reg.model <- reactive({
      lm(regFormula(), data = datasetInput())
    })
    
    
    
    ## GRAPHICS
    
    
    
    # Regression output
    output$summary0 <- renderPrint({
      fit1 <- lm(crab[,input$outcome] ~ crab[,input$iv])
      names(fit1$coefficients) <- c("Intercept", input$var2)
      summary(fit1)
    })
    
    # Regression w/ lead output
    output$summary1 <- renderPrint({
      fit2 <- lm(lead(crab[,input$outcome]) ~ crab[,input$iv])
      names(fit2$coefficients) <- c("Intercept", input$var2)
      summary(fit2)
    })
    
    # Regression w/ lead and lag output
    output$summary2 <- renderPrint({
      fit3 <- lm(lead(crab[,input$outcome]) ~ lag(crab[,input$iv]))
      names(fit3$coefficients) <- c("Intercept", input$var2)
      summary(fit3)
    })
    
    
    
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(crab, options = list(lengthChange = FALSE))
    })
    
    
    
    
    # Scatterplot output
    
    output$scatterplot0 <- renderPlot({
        plot(crab[,input$iv], crab[,input$outcome], main="Lag 0",
             xlab=input$iv, ylab=input$outcome, pch=19)
        abline(lm(crab[,input$outcome] ~ crab[,input$iv]), col="red")
        
    }, height=400)
    
    # Scatterplot output w/ lead
    output$scatterplot1 <- renderPlot({
      plot(lead(crab[,input$iv]), crab[,input$outcome], main="Lag 1",
           xlab=input$iv, ylab=input$outcome, pch=19)
      abline(lm(lead(crab[,input$outcome]) ~ crab[,input$iv]), col="red")
    }, height=400)
    
    # Scatterplot output w/ lead and lag
    output$scatterplot2 <- renderPlot({
      plot(lead(crab[,input$iv]), lag(crab[,input$outcome]), main="Lag 2",
           xlab=input$iv, ylab=input$outcome, pch=19)
      abline(lm(crab[,input$outcome] ~ crab[,input$iv]), col="red")
    }, height=400)
    
    
    
    # Histogram output var 1
    output$distribution1 <- renderPlot({
        hist(crab[,input$outcome], main="", xlab=input$outcome)
        }, height=300, width=300)
    
    # Histogram output var 2
    output$distribution2 <- renderPlot({
        hist(crab[,input$iv], main="", xlab=input$iv)
        }, height=300, width=300)
   
    
    
    
    # correlation matrix B90 T38
    output$corrAbun1 <- renderPlot({
        juvcorr2 <- select(crab, 28:31, 44:47)
        chart.Correlation(juvcorr2, histogram = FALSE, pch=19) 
        })
    
    # correlation matrix B90 Landings
    output$corrAbun2 <- renderPlot({
      juvcorr3 <- select(crab, c(44:47, 57:62))
      chart.Correlation(juvcorr3, histogram = FALSE, pch=19) 
    })
    
    # correlation matrix T38 Landings
    output$corrAbun3 <- renderPlot({
      juvcorr4 <- select(crab, 28:31, 57:62)
      chart.Correlation(juvcorr4, histogram = FALSE, pch=19) 
    })
    
    
}

shinyApp(ui = ui, server = server)