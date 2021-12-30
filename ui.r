library(shiny) 
library(DT)
library(shinyWidgets) 

ui <- fluidPage(
  titlePanel("Build a Linear Model"),
  sidebarPanel(
    
    fileInput(
      inputId = "filedata",
      label = "Upload data. csv",
      multiple = FALSE,
      accept = c(".csv"),
      buttonLabel = "Choosing ...",
      placeholder = "No files selected yet"
    ),
    uiOutput("xvariable"),
    uiOutput("yvariable")
  ), #sidebarpanel
  
  mainPanel( #DTOutput("tb1"), 
    fluidRow(column(6, verbatimTextOutput('lmSummary')) , column(6, plotOutput('diagnosticPlot')))
  )
) #fluidpage


server <- function(input, output) {
  
  data <- reactive({
    req(input$filedata)
    inData <- input$filedata
    if (is.null(inData)){ return(NULL) }
    mydata <- read.csv(inData$datapath, header = TRUE, sep=",")
  })
  output$tb1 <- renderDT(data())

   output$xvariable <- renderUI({
     req(data())
     xa<-colnames(data())
     pickerInput(inputId = 'xvar',
                 label = 'Select x-axis variable',
                 choices = c(xa[1:length(xa)]), selected=xa[2],
                 options = list(`style` = "btn-info"),
                 multiple = TRUE)
  
   })
   output$yvariable <- renderUI({
     req(data())
     ya<-colnames(data())
     pickerInput(inputId = 'yvar',
                 label = 'Select y-axis variable',
                 choices = c(ya[1:length(ya)]), selected=ya[1],
                 options = list(`style` = "btn-info"),
                 multiple = FALSE)
  
   })
  
  lmModel <- reactive({
    req(data(),input$xvar,input$yvar)
    x <- as.numeric(data()[[as.name(input$xvar)]])
    y <- as.numeric(data()[[as.name(input$yvar)]])
    current_formula <- paste0(input$yvar, " ~ ", paste0(input$xvar, collapse = " + "))
    current_formula <- as.formula(current_formula)
    model <- lm(current_formula, data = data(), na.action=na.exclude)
    return(model)
  })
  
  
  
  
  
  
  
  output$lmSummary <- renderPrint({
    req(lmModel())
    summary(lmModel())
  })
  
  output$diagnosticPlot <- renderPlot({
    req(lmModel())
    par(mfrow = c(2,2))
    plot(lmModel())
  })
}




shinyApp(ui = ui, server = server)
