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
      
      selectInput("outcome", label = h3("Abundance (Dependent Variable)"),
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
                                 "SCECAP Open Water CPUE" = "E99_JuvCPUE",
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
      
      #selectInput("outcomelead", label = h4("Dependent Variable Lead (+ years)"), 0:1, multiple = FALSE),
      
      
      selectInput("indepvar", label = h3("Abiotic (Independent Variable)"),
                  choices = list("Winter Precip (City of Chas)" = "ChasCity_WinterPrecip",
                                 "Spring Precip (City of Chas)" = "ChasCity_SpringPrecip",
                                 "Summer Precip (City of Chas)" = "ChasCity_SummerPrecip",
                                 "Fall Precip (City of Chas)" = "ChasCity_FallPrecip",
                                 "Annual Precip (City of Chas)" = "ChasCity_AnnualPrecip",
                                 "Mean Annual Precip (City of Charleston" = "ChasCity_MeanAnnualMonthlyPrecip",
                                 "Customs House Temperature (Mean Annual Water Year)" = "CustomsHouse_MeanAnnualTemp_WaterYr",
                                 "Customs House Temperature (Mean Annual Calendar Year)" = "CustomsHouse_MeanAnnualTemp_CalendarYr",
                                 "Cooper CSI Raw (Mean Annual)" = "MeanCooperCSIRaw",
                                 "Ashley CSI Raw (Mean Annual)" = "MeanAshleyCSIRaw",
                                 "Wando CSI Raw (Mean Annual)" = "MeanAnnualWandoCSI",
                                 "Ashley April CSI (12 mo)" = "AshleyCSI_April_12mo",
                                 "Ashley April CSI (24 mo)" = "AshleyCSI_April24mo",
                                 "Ashley CSI Raw (Mean Annual Water yr)" = "AshleyCSI_WaterYear",
                                 "Ashley CSI (Winter)" = "AshleyCSI_Winter",
                                 "Cooper April CSI (12 mo)" = "CooperCSI_April_12mo",
                                 "Cooper April CSI (24 mo)" = "CooperCSI_April24mo",
                                 "Cooper CSI Raw (Mean Annual Water yr)" = "CooperCSI_WaterYear",
                                 "Cooper CSI (Winter)" = "CooperCSI_Winter",
                                 "Wando April CSI (12 mo)" = "WandoCSI_April_12mo",
                                 "Wando April CSI (24 mo)" = "WandoCSI_April24mo",
                                 "Wando CSI Raw (Mean Annual Water yr)" = "WandoCSI_WaterYear",
                                 "Wando CSI (Winter)" = "WandoCSI_Winter",
                                 "B90 Salinity (Mean Annual)" = "B90_SpotSal",
                                 "B90 Temperature (Mean Annual)" =  "B90_SpotTemp",
                                 "SCECAP Creeks Salinity (Mean Annual)" = "E98_SpotSal",
                                 "SCECAP Open Water Salinity (Mean Annual)" = "E99_SpotSal",
                                 "Creek Trawl Salinity (Mean Annual)" = "T38_SpotSal",
                                 "Creek Trawl Temperature (Mean Annual)" = "T38_SpotTemp",
                                 "Ashley Pot Survey Salinity (Mean Annual)" = "P88_SpotSal",
                                 "Ashley Pot Survey Temperature (Mean Annual)" = "P88_SpotTemp",
                                 "Trammel Survey Salinity (Mean Annual)" = "T06_SpotSal",
                                 "Trammel Survey Temperature (Mean Annual)" = "T06_SpotTemp",
                                 "Ashley 24hr Pot Survey Salinity (Mean Annual)" = "S16_SpotSal",
                                 "Ashley 24hr Pot Survey Temperature (Mean Annual)" = "S16_SpotTemp",
                                 "ENSO (Dec-Jan)" = "ENSO_DJ",
                                 "ENSO (Jan-Feb)" = "ENSO_JF",
                                 "ENSO (Feb-Mar)" = "ENSO_FM",
                                 "ENSO (Mar-Apr)" = "ENSO_MA",
                                 "ENSO (Apr-May)" = "ENSO_AM",
                                 "ENSO (May-Jun)" = "ENSO_MJ",
                                 "ENSO (Jun-Jul)" = "ENSO_JJ",
                                 "ENSO (Jul-Aug)" = "ENSO_JA",
                                 "ENSO (Aug-Sep)" = "ENSO_AS",
                                 "ENSO (Sep-Oct)" = "ENSO_SO",
                                 "ENSO (Oct-Nov)" = "ENSO_ON",
                                 "ENSO (Nov-Dec)" = "ENSO_ND",
                                 "ONI (Dec-Feb)" = "ONI_DJF",
                                 "ONI (Dec-Feb)" = "ONI_JFM",
                                 "ONI (Feb-Apr)" = "ONI_FMA",
                                 "ONI (Mar-May)" = "ONI_MAM",
                                 "ONI (Apr-Jun)" = "ONI_AMJ",
                                 "ONI (May-Jul)" = "ONI_MJJ",
                                 "ONI (Jun-Aug)" = "ONI_JJA",
                                 "ONI (Jul-Sep)" = "ONI_JAS",
                                 "ONI (Aug-Oct)" = "ONI_ASO",
                                 "ONI (Sep-Nov)" = "ONI_SON",
                                 "ONI (Oct-Dec)" = "ONI_OND",
                                 "ONI (Nov-Jan)" = "ONI_NDJ"), selected = 1)
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
           "Abundance" = select(crab, 28:31, 33:36, 38:41, 44:47, 50, 53, 56:68),
           "Precipitation" = select(crab, 2:8),
           "Temperature" = select(crab, 9, 10, 27, 43, 49, 52, 55),    
           "Salinity" = select(crab, 11:26, 32, 37, 42, 48, 51, 54),
           "Climate" = select(crab, 70:93))
  })
  
  # dependent variable
  output$dv = renderUI({
    selectInput('dv', h5('Dependent Variable'), choices = names(datasetInput()))
  })
  
  # independent variable
  output$iv = renderUI({
    selectInput('iv', h5('Independent Variable'), choices = names(datasetInput()))
  })
  
  # Regression output
  output$summary0 <- renderPrint({
    fit <- lm(crab[,input$outcome] ~ crab[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  # Regression w/ lead output
  output$summary1 <- renderPrint({
    fit1 <- lm(lead(crab[,input$outcome]) ~ crab[,input$indepvar])
    names(fit1$coefficients) <- c("Intercept", input$var3)
    summary(fit1)
  })
  
  # Regression w/ lead and lag output
  output$summary2 <- renderPrint({
    fit2 <- lm(lead(crab[,input$outcome]) ~ lag(crab[,input$indepvar]))
    names(fit2$coefficients) <- c("Intercept", input$var4)
    summary(fit2)
  })
  
  
  
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(crab, options = list(lengthChange = FALSE))
  })
  
  
  
  
  # Scatterplot output
  
  output$scatterplot0 <- renderPlot({
    plot(crab[,input$indepvar], crab[,input$outcome], main="Lag 0",
         xlab=input$indepvar, ylab=input$outcome, pch=19)
    abline(lm(crab[,input$outcome] ~ crab[,input$indepvar]), col="red")
    
  }, height=400)
  
  # Scatterplot output w/ lead
  output$scatterplot1 <- renderPlot({
    plot(lead(crab[,input$indepvar]), crab[,input$outcome], main="Lag 1",
         xlab=input$indepvar, ylab=input$outcome, pch=19)
    abline(lm(lead(crab[,input$outcome]) ~ crab[,input$indepvar]), col="red")
  }, height=400)
  
  # Scatterplot output w/ lead and lag
  output$scatterplot2 <- renderPlot({
    plot(lead(crab[,input$indepvar]), lag(crab[,input$outcome]), main="Lag 2",
         xlab=input$indepvar, ylab=input$outcome, pch=19)
    abline(lm(crab[,input$outcome] ~ crab[,input$indepvar]), col="red")
  }, height=400)
  
  
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(crab[,input$outcome], main="", xlab=input$outcome)
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    hist(crab[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
  
  
  
  
  # correlation matrix B90 T38
  output$corrAbun <- renderPlot({
    B90crab <- select(crab, 28:31)
    B90crab_lead <- lead(B90crab)
    B90corr <- cbind(B90crab, B90crab_lead)
    chart.Correlation(B90corr, histogram = FALSE, pch=19)
  })
  
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