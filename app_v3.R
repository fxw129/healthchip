library(shiny)
library(shinythemes)
library(shinydashboard)
library(RColorBrewer)
library(lattice)


ui <-
    dashboardPage(
      dashboardHeader(title="Personal Health Chip"),
      dashboardSidebar(
        sidebarMenu(
          menuItem(fileInput("file", "File input:")),
          menuItem(sliderInput("age", "Age (years)", 18, 120, 25)),
          menuItem(sliderInput("alb", "Albumin (g/dL)", 2.2, 6.0, 4.0)),
          menuItem(sliderInput("creat", "Creatinine (mg/dL)", 0.3, 5.0, 0.8)),
          menuItem(sliderInput("glu", "Fasting glucose (mmol/L)", 1, 25, 6)),
          menuItem(sliderInput("crp", "C-Reactive Protein (mg/dL)", 0.01, 18, 0.6)),
          menuItem(sliderInput("lymph", "Lymphocyte percentage (%)", 10, 95, 30)),
          menuItem(sliderInput("mcv", "Mean cell volume (fL)", 50, 120, 90)),
          menuItem(sliderInput("rdw", "Red blood cell width (%)", 10, 20, 14)),
          menuItem(sliderInput("wbc", "White blood cell counts (1000 cells / ul)", 1, 15, 6)),
          menuItem(sliderInput("alkp", "Alkaline phosphatase (U/L)", 1, 200, 50)),
          menuItem(sliderInput("sbp", "Systolic blood pressure (mmHg)", 60, 250, 120)),
          menuItem(sliderInput("dbp", "Diastolic blood pressure (mmHg)", 30, 200, 80))
        )
      ),
      dashboardBody(
        fluidRow(
          
          box(title = "Your Personal Health Chip", status = "primary", solidHeader = TRUE, align="center",
              imageOutput("interactivechip", height=280), downloadButton('downloadPlot', 'Download Personal Health Chip')
),
          
          box(title = "Circadian Heart Rhythm", status = "warning", solidHeader = TRUE, align="center",
              imageOutput("zio", height=300))
        ),
        
        fluidRow(
          
          box(title = "How you compare to others your age", status = "info", solidHeader = TRUE,
              plotOutput("scatterplotchron", height=300)),
          
          box(title = "How you deviate from the norm (Left=Good, Right=Bad)", status = "success", solidHeader = TRUE,
              plotOutput("residualplotchron", height=300))
        ),
        
        fluidRow(
          
          # Dynamic infoBoxes
          valueBoxOutput("chron"),
          
          valueBoxOutput("pheno")
          
        ),

        fluidRow(
          
          # Dynamic infoBoxes
          valueBoxOutput("risk"),
          
          valueBoxOutput("risk_cvd"),
          
          valueBoxOutput("risk_dem")
          
        ),
                
        fluidRow(
          box(width = 12, background = "black",
              h3("Recommendation #1"), "insert text")
      ),
      
      fluidRow(
        box(width = 12, background = "red",
            h3("Recommendation #2"), "insert text")
      ),

      fluidRow(
        box(width = 12, background = "teal",
            h3("Recommendation #3"), "insert text")
      )
      )
    )


  
#######################
# DEFINE SERVER LOGIC #
#######################

  server = function(input, output, server) {
  
    # Phenocal
    phenocal <- reactive({
      xb = -19.907-0.0336*input$alb +
        0.0954*input$creat + 
        0.1953*input$glu +
        0.0954*log(input$crp) -
        0.012*input$lymph +
        0.0268*input$mcv +
        0.3306*input$rdw +
        0.00188*input$alkp +
        0.0554*input$wbc +
        0.0804*input$age
      
      phenoage = as.integer(141.5 + 11.89979*xb)
      
      print(phenoage)
    })

    residualpheno <- reactive({
      xb = -19.907-0.0336*input$alb +
        0.0954*input$creat + 
        0.1953*input$glu +
        0.0954*log(input$crp) -
        0.012*input$lymph +
        0.0268*input$mcv +
        0.3306*input$rdw +
        0.00188*input$alkp +
        0.0554*input$wbc +
        0.0804*input$age
      
      phenoage = as.integer(141.5 + 11.89979*xb)
      
      residualpheno = phenoage - input$age
      
      print(residualpheno)
    })
    
     # Pre-rendered image: 'chip1'
     output$chip1 <- renderImage({

       filename <- normalizePath(file.path('www/',
                                           paste0('chip1.png')))
       list(src = filename,
            width = 300,
            height = 300,
            align="center")

     }, deleteFile = FALSE)
     
     output$downloadPlot <- downloadHandler(
       filename = "HealthChip.png",
       content = function(file) {
         png(file)
         print(output$chip1)
         dev.off()
       })
     
     # Pre-rendered image: 'zio'
     output$zio <- renderImage({
       
       t <- sample(1:5,1,replace=T)
       
       filename <- normalizePath(file.path('www/',
                                           paste0('z',t,'.png')))
       list(src = filename,
            width = 500,
            height = 300,
            align="center")
       
     }, deleteFile = FALSE)
     
     # Box 1
     output$chron <- renderInfoBox({
       valueBox(
         paste0(input$age, " years"), "Chronological age", icon = icon("angle-double-left"),
         color = "purple"
       )
     })
     
     # Box 2
     output$pheno <- renderInfoBox({
       valueBox(
         paste0(phenocal(), " years"), "Phenotypical age", icon = icon("allergies"),
         color = "yellow"
       )
     })
     
     # Box 3
     output$risk <- renderInfoBox({
       
       riskp <- case_when(phenocal() <= input$age ~ sample(1:3,1,replace=T),
                          phenocal() > input$age ~ sample(3:10,1,replace=T))
       valueBox(
         paste0(riskp, "%"), "Risk of mortality over 10 years", icon = icon("ambulance"),
         color = "black"
       )
     })
     
     # Box 4
     output$risk_cvd <- renderInfoBox({
       
       riskp <- case_when(phenocal() <= input$age ~ sample(1:5,1,replace=T),
                          phenocal() > input$age ~ sample(10:20,1,replace=T))
       valueBox(
         paste0(riskp, "%"), "Risk of cardiovascular disease over 10 years", icon = icon("ambulance"),
         color = "red"
       )
     })
     
     # Box 5
     output$risk_dem <- renderInfoBox({
       
       riskp <- case_when(phenocal() <= input$age ~ sample(1:3,1,replace=T),
                          phenocal() > input$age ~ sample(3:8,1,replace=T))
       valueBox(
         paste0(riskp, "%"), "Risk of dementia over 10 years", icon = icon("ambulance"),
         color = "teal"
       )
     })

     # Scatterplot
     datasetsim <- reactive({
       read.csv("simdata_chronage.csv", header = TRUE)
     })
     
     output$scatterplotchron <- renderPlot({
       
       p <- ggplot(datasetsim(), aes(x = chronsim, y = phenosim)) + 
         geom_point() +
         geom_smooth(method = "lm", se = FALSE, color = "red")+
         scale_color_brewer(palette="Dark2")
       
       
       if (input$age == 25)
         p <- p + geom_vline(xintercept = 25, linetype="F1", 
                             color = "blue", size=1.5) +
           theme_bw() +
           labs( x = "Chronological Age (years old)",
                 y = "Physiological Age (years old)",
                 title = "Relationship between Phenotypic Age, chronological age",
                 subtitle = ("Blue line shows the distribution of phenotypic age from NHANES participants at your age")) 
       
       
       if (input$age != 25)
         p <- p + geom_vline(xintercept = input$age, linetype="F1", 
                             color = "blue", size=1) +
           theme_bw() +
           xlab("Chronological Age (years old)") + 
           ylab("Physiological Age (years old)")
       
       print(p)
       
     }, height = 300)
     
     # Residual plot
     output$residualplotchron <- renderPlot({
       
       temp <- phenocal() - input$age
       
       porg <- ggplot(datasetsim(), aes(res, stat(density), 
                                   colour = "pink",
                                   fill = "pink")) +
         geom_histogram() +
         geom_vline(xintercept = residualpheno(), linetype="F1", #xinterce#
                    color = "blue", size=1.5) +
         theme_bw() +
         labs( x = "Phenotypic age accelaration (years)",
               y = "Percetage",
               title = "Distribution of phenotypic age acceleration",
               subtitle = "Phenotypic age accelaration = phenotypic age - chronological age \nBlue line shows your phenotypic age acceleration")+
         theme(legend.position = "none")
       
       if (residualpheno() < -3 | residualpheno() > 6)
         pres = porg
       
       else 
         pres = porg + xlim(-3, 6)
       
       print(pres)
       
     }, height = 300)
     
     
     # Interactive chips
     output$interactivechip <- renderPlot({
       
       # Dummy data
       data0 <- matrix(c(rep(c(1,10,5,6,2,3,5,9,19,1,24),9),1) , 10 , 10)
       
       invector <- c(input$alb*input$creat*input$wbc/12,
                     input$creat*input$lymph/20,
                     input$glu*input$sbp*input$alkp/4000,
                     input$crp*input$mcv*input$rdw/120,
                     input$lymph*input$sbp/3000,
                     input$mcv*input$alb/360,
                     input$rdw*input$age/50,
                     input$alkp*input$dbp/3000,
                     input$wbc*input$age/30,
                     input$age*input$alb/200)
       
       transform = data0 %*% (diag(invector)+1)
       
       col.l <- colorRampPalette(c('blue', 'green', 'purple', 'grey','yellow','orange', 'red'))(100)
       
       
       chip <- levelplot( t(transform[c(nrow(transform):1) , ]),
                  col.regions=col.l,
                  #col.regions=heat.colors(100),
                  colorkey=FALSE,scales = list(tck = c(0,0),
                                               x=list(at=NULL),
                                               y=list(at=NULL)),
                  xlab = NULL, ylab = NULL)
  
       print(chip)
       
     }, height = 280)
     
  }



shinyApp(ui, server)