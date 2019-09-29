library(shiny)
library(shinythemes)
library(shinydashboard)

ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "Interactive Health Mangement in Big Data Area", # Title we want
      tabPanel("Wearable Device Recordings",
               sidebarPanel(
                 fileInput("file", "File input:"),
                 # textInput("txt", "Text input:", "general"),
                 sliderInput(inputId = "age", "Age (years)", 1, 120, 25),
                 # numericInput(inputId = "age",
                 #              "Age (years old)", value = 25),
                 sliderInput(inputId = "alb",
                              "Albumin (g/dL)", 1, 120, 25),
                 sliderInput(inputId = "creat",
                              "Creatinine (umol/L)", 1, 120, 25),
                 sliderInput(inputId = "glu",
                              "Fasting glucose (mmol/L)", 1, 120, 25),
                 sliderInput(inputId = "crp",
                              "C-Reactive Protein (mg/dL)", 1, 120, 25),
                 sliderInput(inputId = "lymph",
                              "Lymphocyte percentage (%)", 1, 120, 25),
                 sliderInput(inputId = "mcv",
                              "Mean cell volume (fL)", 1, 120, 25),
                 sliderInput(inputId = "rdw",
                              "Red blood cell width (%)", 1, 120, 25),
                 sliderInput(inputId = "wbc (U/L)",
                              "White blood cell counts (1000 cells / ul)", 1, 120, 25),
                 sliderInput(inputId = "alkp",
                              "Alkaline phosphatase (U/L)", 1, 120, 25)
                 # selectInput("selectage", label = h3("Age"), 
                 #             choices = list("<= 70" = 0, "71 - 75" = 4, "76 - 80" = 6, "81 - 90" = 9), 
                 #             selected = 0),
                 # tags$h5("Deafult actionButton:"),
                 # actionButton("action", "Search"),
                 # tags$h5("actionButton with CSS class:"),
                 # actionButton("action2", "Action button", class = "btn-primary")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Physical Activity",
                            h3("Your Overall Health"),
                            p(textOutput("phenotypicage")),
                            h3("Your Personal Health Chip"),
                            imageOutput("chip1"),
                            tableOutput("table"),
                            h4("Verbatim text output"),
                            h1("Header 1"),
                            h2("Header 2"),
                            h3("Header 3"),
                            h4("Header 4"),
                            h5("Header 5")
                   ),
                   tabPanel("Blood Pressure", "This panel is intentionally left blank"),
                   tabPanel("Heart Rate", "This panel is intentionally left blank")
                 )
               )
      ),
      tabPanel("Lab Measurements", "This panel is intentionally left blank"),
      tabPanel("Medications", "This panel is intentionally left blank")
    )
  )
  
  
#######################
# DEFINE SERVER LOGIC #
#######################

  server = function(input, output, server) {
    
    #  phenocal <- reactive({
    #   
    #   phenoage <- 141.5 + 
    #     log(-0.00553*log(1-(1- 
    #                           exp(-1.51714*(-19.907-0.0336*input$alb +
    #                                           0.0954*input$creat + 
    #                                           0.1953*input$glu +
    #                                           0.0954*log(input$crp) -
    #                                           0.012*input$lymph +
    #                                           0.0268*input$mcv +
    #                                           0.3306*input$rdw +
    #                                           0.00188*input$alkp +
    #                                           0.0554*input$wbc +
    #                                           0.0804*input$age)/
    #                                 0.0076927))))/0.09165
    #   
    #   chroage = input$age
    #   
    #   print(paste0("Your chronological age is ",chroage,". Your phenotypic age is ", phenoage,"."))
    #   #paste(input$txt, input$slider, format(input$date), sep = ", ")
    # })
    
     output$phenotypicage <- renderText({
       
       phenotypeage <- 141.5 + 
         log(-0.00553*log(1-(1-exp(-1.51714*(-19.907-0.0336*input$alb +
                                               0.0954*input$creat + 
                                               0.1953*input$glu +
                                               0.0954*log(input$crp) -
                                               0.012*input$lymph +
                                               0.0268*input$mcv +
                                               0.3306*input$rdw +
                                               0.00188*input$alkp +
                                               0.0554*input$wbc +
                                               0.0804*input$age)/
                                     0.0076927))))/0.09165
       
       paste0("Your chronological age is ", input$age,". Your phenotypic age is ", phenotypeage,".")
     
       })
     
    # output$txtout <- renderPrint({
    #   phenocal()
    # })
     
     # Pre-rendered image: 'chip1'
     output$chip1 <- renderImage({
       
       filename <- normalizePath(file.path('www/',
                                           paste0('chip1.png')))
       list(src = filename,
            width = 400,
            height = 400)
       
     }, deleteFile = FALSE)  

    output$table <- renderTable({
      head(cars, 4)
    })
  }



shinyApp(ui, server)