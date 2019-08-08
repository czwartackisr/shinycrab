library(shiny)
library(dplyr)
library(ggplot2)
library(DataCombine)
library(PerformanceAnalytics)
library(knitr)
library(rmarkdown)
crab <- read.csv("data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
    titlePanel("Crab Bivariate Modeling (Czwartacki)"),
    sidebarLayout(
        sidebarPanel(
          
            selectInput("SpeciesCode", label = h3("Species Code Selector"),
                      choices = list("D003", "D004", "D005", "D130", "F001"), 
                      selected = 3, multiple = TRUE),
          
            selectInput("dataset2", h3("Dependent Variable (Group)"), choices = c("Abundance")),        
            HTML('</br>'),
            uiOutput('dv'),
            
            selectInput("dataset", h3("Independent Variable (Group)"), choices = c("Abundance",
                                                                                   "Salinity",
                                                                                   "Temperature",
                                                                                   "Precipitation",
                                                                                   "Climate")),        
            HTML('</br>'),
            uiOutput('iv'),
            
            radioButtons('format', h5('Document format'), c('PDF', 'HTML', 'Word'), inline = TRUE),
            downloadButton('downloadReport')
            
            
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Distribution", # Plots of distributions
                                 fluidRow(
                                   column(6, plotOutput("distribution1")),
                                   column(6, plotOutput("dvTime"))),
                                 fluidRow(
                                   column(6, plotOutput("distribution2")),
                                   column(6, plotOutput("ivTime")))),
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
                        tabPanel("Residuals",                   
                                 column(12, h4("Lag o (No Lag)"), plotOutput("residuals_hist0")),
                                 column(12, h4("Lag o (No Lag)"), plotOutput("residuals_scatter0")),
                                 column(12, h4("Lag o (No Lag)"), plotOutput("residuals_qqline0")),
                                 column(12, h4("Lag 1 (Dep Var +1 yr)"), plotOutput("residuals_hist1")),
                                 column(12, h4("Lag 1 (Dep Var +1 yr)"), plotOutput("residuals_scatter1")),
                                 column(12, h4("Lag 1 (Dep Var +1 yr)"), plotOutput("residuals_qqline1")),
                                 column(12, h4("Lag 2 (Dep Var +1 yr, Ind Var -1 yr)"), plotOutput("residuals_hist2")),
                                 column(12, h4("Lag 2 (Dep Var +1 yr, Ind Var -1 yr)"), plotOutput("residuals_scatter2")),
                                 column(12, h4("Lag 2 (Dep Var +1 yr, Ind Var -1 yr)"), plotOutput("residuals_qqline2"))),
                        tabPanel("Select Correlations", 
                                 fluidRow(
                                   #column(6, h6("Method = Kendall's"), plotOutput("currentCorr")),
                                   column(12, h4("Harbor Trawl - Creek Trawl Abundance Correlations"), plotOutput("corrAbun1")),
                                   column(12, h4("Creek Trawl - Chas Harbor Landings"), plotOutput("corrAbun2")),
                                   column(12, h4("Harbor Trawl - Chas Harbor Landings"), plotOutput("corrAbun3")),
                                   column(12, h4("Salinity Correlations"), plotOutput("corrAbun4")))),
                        tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                        
            )
        )
    ))



# SERVER
server <- function(input, output) {
    
    # Dependent Variable Input
    dataset2Input <- reactive({
      switch(input$dataset2,
             "Abundance" = select(crab, 28:33, 35:38, 40:43, 46:51, 54, 57, 60, 61:72))
    })
    
    # Infependent variable Input
    datasetInput <- reactive({
      switch(input$dataset,
           "Abundance" = select(crab, 28:33, 35:38, 40:43, 46:51, 54, 57, 60, 61:72),
           "Salinity" = select(crab, 11:26, 34, 39, 44, 52, 55, 58),
           "Temperature" = select(crab, 9, 10, 27, 45, 53, 56, 59),    
           "Precipitation" = select(crab, 2:8),
           "Climate" = select(crab, 73:98))
    })
    
    # Dependent variable Output
    output$dv = renderUI({
      selectInput('dv', h5('Choose an Dep Var from Group'), choices = names(dataset2Input()))
    })
    
    
    # independent variable Output
    output$iv = renderUI({
      selectInput('iv', h5('Choose an Ind Var from Group'), choices = names(datasetInput()))
    })
     
      
    
     
    # regression formula
    regFormula <- reactive({
      as.formula(paste(input$dv, '~', input$iv))
    })
    
    # bivariate model 
    reg.model0 <- reactive({
      lm(crab[,input$dv] ~ crab[,input$iv])
    })
    
    # bivariate model 
    reg.model1 <- reactive({
      lm(lead(crab[,input$dv]) ~ crab[,input$iv])
    })
    
    # bivariate model 
    reg.model2 <- reactive({
      lm(lead(crab[,input$dv]) ~ lag(crab[,input$iv]))
    })
    
    
    
    
    
    ## GRAPHICS
 
    
    
    
    
    # residuals
    output$residuals_hist0 <- renderPlot({
      hist(reg.model0()$residuals, main = paste(input$dv, '~', input$iv), xlab = 'Residuals') 
    })
    
    output$residuals_scatter0 <- renderPlot({
      res <- qplot(fitted(reg.model0()), residuals(reg.model0()))
      res + geom_hline(yintercept = 0, col="red")
    })
    
    output$residuals_qqline0 <- renderPlot({
      qqnorm(reg.model0()$residuals)
      qqline(reg.model0()$residuals) 
    })   
    
    # residuals lead
    output$residuals_hist1 <- renderPlot({
      hist(reg.model1()$residuals, main = paste(input$dv, '~', input$iv), xlab = 'Residuals') 
    })
    
    output$residuals_scatter1 <- renderPlot({
      res <- qplot(fitted(reg.model1()), residuals(reg.model1()))
      res + geom_hline(yintercept = 0, col="red")
    })
    
    output$residuals_qqline1 <- renderPlot({
      qqnorm(reg.model1()$residuals)
      qqline(reg.model1()$residuals) 
    })   
    
    # residuals lead/lag
    output$residuals_hist2 <- renderPlot({
      hist(reg.model2()$residuals, main = paste(input$dv, '~', input$iv), xlab = 'Residuals') 
    })
    
    output$residuals_scatter2 <- renderPlot({
      res <- qplot(fitted(reg.model2()), residuals(reg.model2()))
      res + geom_hline(yintercept = 0, col="red")
    })
    
    output$residuals_qqline2 <- renderPlot({
      qqnorm(reg.model2()$residuals)
      qqline(reg.model2()$residuals) 
    })   
    
    
    
    
    
    # Regression output
    output$summary0 <- renderPrint({
      fit0 <- lm(crab[,input$dv] ~ crab[,input$iv])
      names(fit0$coefficients) <- c("Intercept", input$var2)
      summary(fit0)
    })
    
    # Regression w/ lead output
    output$summary1 <- renderPrint({
      fit1 <- lm(lead(crab[,input$dv]) ~ crab[,input$iv])
      names(fit1$coefficients) <- c("Intercept", input$var2)
      summary(fit1)
    })
    
    # Regression w/ lead and lag output
    output$summary2 <- renderPrint({
      fit2 <- lm(lead(crab[,input$dv]) ~ lag(crab[,input$iv]))
      names(fit2$coefficients) <- c("Intercept", input$var2)
      summary(fit2)
    })

    
    
    
    
    
    # correlation matrix B90 T38
    output$corrAbun1 <- renderPlot({
        juvcorr1 <- select(crab, 28:33, 46:51)
        chart.Correlation(juvcorr1, histogram = FALSE, pch=19, method = "kendall") 
    })
    
    # correlation matrix B90 Landings
    output$corrAbun2 <- renderPlot({
      juvcorr2 <- select(crab, c(46:51, 61:72))
      chart.Correlation(juvcorr2, histogram = FALSE, pch=19, method = "kendall") 
    })
    
    # correlation matrix T38 Landings
    output$corrAbun3 <- renderPlot({
      juvcorr3 <- select(crab, 28:33, 61:72)
      chart.Correlation(juvcorr3, histogram = FALSE, pch=19, method = "kendall") 
    })
    
    # correlation matrix Salinity
    output$corrAbun4 <- renderPlot({
      juvcorr4 <- select(crab, 11:26, 44, 55)
      chart.Correlation(juvcorr4, histogram = FALSE, pch=19, method = "kendall") 
    }, height = 700)
    
    
    
    
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(crab, options = list(lengthChange = FALSE))
    }) 

    
    
    
    
    # Scatterplot output
    
    output$scatterplot0 <- renderPlot({
        plot(crab[,input$iv], crab[,input$dv], main="Lag 0",
             xlab=input$iv, ylab=input$dv, pch=19)
        abline(lm(crab[,input$dv] ~ crab[,input$iv]), col="red")
        
    }, height=400, width = 500)
    
    # Scatterplot output w/ lead
    output$scatterplot1 <- renderPlot({
      plot(lead(crab[,input$iv]), crab[,input$dv], main="Lag 1",
           xlab=input$iv, ylab=input$dv, pch=19)
      abline(lm(lead(crab[,input$dv]) ~ crab[,input$iv]), col="red")
    }, height=400, width = 500)
    
    # Scatterplot output w/ lead and lag
    output$scatterplot2 <- renderPlot({
      plot(lead(crab[,input$iv]), lag(crab[,input$dv]), main="Lag 2",
           xlab=input$iv, ylab=input$dv, pch=19)
      abline(lm(crab[,input$dv] ~ crab[,input$iv]), col="red")
    }, height=400, width = 500)

    
    
    
    
    # Histogram output var 1
    output$distribution1 <- renderPlot({
        hist(crab[,input$dv], main="Dependent Variable Distribution", xlab=input$dv)
        })
    
    # Histogram output var 2
    output$distribution2 <- renderPlot({
        hist(crab[,input$iv], main="Independent Variable Distribution", xlab=input$iv)
        })
    
   # Time series output iv
    output$ivTime <- renderPlot({
      plot(crab$Year,crab[,input$iv], main = "Independent Variable Time Series",
           type = "l", lty = "twodash",
           xlab = "Year", ylab = "Indep Var Metric (Abundance, PSU, Degrees Celsius, etc.)") 
      })
    
    # Time series output dv
    output$dvTime <- renderPlot({
      plot(crab$Year,crab[,input$dv], main = "Dependent Variable Time Series",
           type = "l", lty = "twodash",
           xlab = "Year", ylab = "Abundance Metric")
      })
    
    
    
    
    
    # download report
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      
      content = function(file) {
        src <- normalizePath('report.Rmd')
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd')
        
        library(rmarkdown)
        out <- render('report.Rmd', switch(
          input$format,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      })
    
}

shinyApp(ui = ui, server = server)