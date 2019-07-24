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
          
            selectInput("crabdataset", h3("Dependent Variable (Group)"), choices = c("Abundance")),        
            HTML('</br>'),
            uiOutput('outcome'),
            
            
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
                                   column(6, h6("Method = Kendall's"), verbatimTextOutput("currentCorr")),
                                   column(12, h4("Harbor Trawl - Creek Trawl Abundance Correlations"), plotOutput("corrAbun1")),
                                   column(12, h4("Creek Trawl - Chas Harbor Landings"), plotOutput("corrAbun2")),
                                   column(12, h4("Harbor Trawl - Chas Harbor Landings"), plotOutput("corrAbun3")))),
                        tabPanel("Residuals",                   
                                 plotOutput("residuals_hist"),
                                 plotOutput("residuals_scatter"),
                                 plotOutput("residuals_qqline")),
                        tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                        
            )
        )
    ))



# SERVER
server <- function(input, output) {
    
    # Infependent variable Input
    datasetInput <- reactive({
      switch(input$dataset,
           "Abundance" = select(crab, 28:31, 33:36, 38:41, 44:47, 50, 53, 56, 57:68),
           "Salinity" = select(crab, 11:26, 32, 37, 42, 48, 51, 54),
           "Temperature" = select(crab, 9, 10, 27, 43, 49, 52, 55),    
           "Precipitation" = select(crab, 2:8),
           "Climate" = select(crab, 70:93))
    })
    
    # independent variable Output
    output$iv = renderUI({
      selectInput('iv', h5('Choose an Ind Var from Group'), choices = names(datasetInput()))
    })
    
    
    # dependent variable Input
    outcomeInput <- reactive({
      switch(input$crabdataset,
           "Abundance" = select(crab, 28:31, 33:36, 38:41, 44:47, 50, 53, 56, 57:68),
           "Abundance Lag 1" = lead("Abundance"))
    })
     
      
    # Dependent variable Output
    output$outcome = renderUI({
      selectInput('outcome', h5('Choose an Ind Var from Group'), choices = names(outcomeInput()))
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
      fit0 <- lm(crab[,input$outcome] ~ crab[,input$iv])
      names(fit0$coefficients) <- c("Intercept", input$var2)
      summary(fit0)
    })
    
    # Regression w/ lead output
    output$summary1 <- renderPrint({
      fit1 <- lm(lead(crab[,input$outcome]) ~ crab[,input$iv])
      names(fit1$coefficients) <- c("Intercept", input$var2)
      summary(fit1)
    })
    
    # Regression w/ lead and lag output
    output$summary2 <- renderPrint({
      fit2 <- lm(lead(crab[,input$outcome]) ~ lag(crab[,input$iv]))
      names(fit2$coefficients) <- c("Intercept", input$var2)
      summary(fit2)
    })

    
    
    
    
    # Scatterplot output
    
    output$scatterplot0 <- renderPlot({
        plot(crab[,input$iv], crab[,input$outcome], main="Lag 0",
             xlab=input$iv, ylab=input$outcome, pch=19)
        abline(lm(crab[,input$outcome] ~ crab[,input$iv]), col="red")
        
    }, height=350, width=350)
    
    # Scatterplot output w/ lead
    output$scatterplot1 <- renderPlot({
      plot(lead(crab[,input$iv]), crab[,input$outcome], main="Lag 1",
           xlab=input$iv, ylab=input$outcome, pch=19)
      abline(lm(lead(crab[,input$outcome]) ~ crab[,input$iv]), col="red")
    }, height=350, width=350)
    
    # Scatterplot output w/ lead and lag
    output$scatterplot2 <- renderPlot({
      plot(lead(crab[,input$iv]), lag(crab[,input$outcome]), main="Lag 2",
           xlab=input$iv, ylab=input$outcome, pch=19)
      abline(lm(crab[,input$outcome] ~ crab[,input$iv]), col="red")
    }, height=350, width=350)
   
    
    
    
    
    # residuals
    output$residuals_hist <- renderPlot({
      hist(reg.model()$residuals, main = paste(input$outcome, '~', input$iv), xlab = 'Residuals') 
    })
    
    output$residuals_scatter <- renderPlot({
      plot(reg.model()$residuals ~ crabdatasetInput()[,input$iv], na.rm = TRUE, xlab = input$iv, ylab = 'Residuals')
      abline(h = 0, lty = 3) 
    })
    
    output$residuals_qqline <- renderPlot({
      qqnorm(reg.model()$residuals)
      qqline(reg.model()$residuals) 
    })    
    
    
    
    
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
        chart.Correlation(juvcorr2, histogram = FALSE, pch=19, method = "kendall") 
        })
    
    # correlation matrix B90 Landings
    output$corrAbun2 <- renderPlot({
      juvcorr3 <- select(crab, c(44:47, 57:62))
      chart.Correlation(juvcorr3, histogram = FALSE, pch=19, method = "kendall") 
    })
    
    # correlation matrix T38 Landings
    output$corrAbun3 <- renderPlot({
      juvcorr4 <- select(crab, 28:31, 57:62)
      chart.Correlation(juvcorr4, histogram = FALSE, pch=19, method = "kendall") 
    })
    
    
    
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(crab, options = list(lengthChange = FALSE))
    })    
    
}

shinyApp(ui = ui, server = server)