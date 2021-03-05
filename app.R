library(shiny)

ui <- fluidPage(

    titlePanel("TP IRIS {GOUMEIDA AHMED SEYFEDDINE - MLDS}"),
    
    
    
    sidebarLayout(
        
        sidebarPanel(
          
         
          fileInput("file1", "Choose input data"),
          uiOutput("category1"),          
          uiOutput("category2"),

          
          fluidRow(
                column(12, 
                       htmlOutput(outputId = "tp")  
                )
            ),
  
  
            
        ),
        mainPanel(
            fluidRow(
                column(12, 
                   mainPanel(
                       tabsetPanel(
                           id = 'dataset',
                           tabPanel("Bank Marketing", 
                                    DT::dataTableOutput("mytable3")
                                    ),
                           tabPanel("Summary", 
                                    verbatimTextOutput("summary")
                                    
                           ),
                           tabPanel("BoxPlot",
                                    plotOutput(outputId = "boxplot"),
                                    plotOutput(outputId = "boxplotSpecies")
                                    ),
                           tabPanel("Pie", 
                                    fluidRow(
                                      column(12, 
                                             plotOutput(outputId = "Pie")
                                      )
                                    )
                                    
                           ),
                           tabPanel("Hostogram", 
                                    fluidRow(
                                            column(12, 
                                               plotOutput(outputId = "HistogramPW"),
                                               sliderInput("bins",
                                                           "Number of bins:",
                                                           min = 1,
                                                           max = 50,
                                                           value = 5)
                                               )
                                    )
                                    
                           ),
                           tabPanel("Nuage", 
                                    fluidRow(
                                      column(12, 
                                             plotOutput(outputId = "Nuage")
                                             
                                      )
                                    )
                           ),
                           tabPanel("Coorelation", 
                                    
                                    plotOutput(outputId = "Coorelation")
                                    
                           ),
                           tabPanel("Customer attrition", 
                                    fluidRow(
                                      column(12, 
                                             plotOutput(outputId = "Customer_attrition")
                                      )
                                    )
                                    
                           ),
                           tabPanel("Variables distribution in customer attrition", 
                                    plotOutput(outputId = "barplotProfils")
                                    

                           ),
                           tabPanel("Variable ciblé", 
                                    plotOutput(outputId = "barplotBi")
                                    
                           ),
                           tabPanel("About", 
                                    
                                    htmlOutput(outputId = "About")
                           )
                           
                       )
                   )
                  )
            )
        
    ))
)

#------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  
    options(shiny.maxRequestSize=30*1024^2)
    output$tp<- renderUI({
      HTML(
        paste("<h2>TP1 et 2  IRIS</h2>"),
        paste("<h3>Objectif: </h3>"),
        paste("<h4> Creer une shinyapp permettant l’exploration univarie d’un jeu de donnees comportant des variables quantitatives et qualitatives.Votre analyse devra etremise en ligne via votre compte shiny sur shinyapps.io.</h4>" ),
        paste("<ul>
                <li> La liste des jeux de donn´ees possibles est disponible sur mon site google, dans la sectionTeaching ! Datasets mini-projet.</li>
                <li> Vos analyses doivent reprendre les diff´erents graphiques et coefficients pr´esent´es dans les slides de cours (Univariate analysis).</li>
                <li> Il est n´ecessaire de cr´eer un compte sur shinyapps.io pour charger et lancer votre application finale.</li>
                <li> Il est n´ecessaire de cr´eer un compte sur shinyapps.io pour charger et lancer votre application finale.</li>
              </ul>"
              ),
        paste("<h3>---------------------------------------------</h3>"),
        
        paste("<ol>
                <li> Choisir un jeu de donn´ees (parmi ceux propos´es ou un jeu de donn´ees de votre choix).</li>
                <li> Faire l’analyse univari´ee des variables.</li>
                <li> Restituer l’´etude dans une shinyapp qui sera charg´ee dans votre compte shiny</li>
                <li> Pr´evoir un espace d’aide o`u vous donnerez la probl´ematique du jeu de donn´ees et un bref descriptif des donn´ees.</li>
              </ol>"
        )
        )
    })

    myData <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
  
      data <- read.csv(inFile$datapath, header = TRUE,sep = ";")
    })
  
  
    output$category1 <- renderUI({
      selectizeInput('cat1', 'Choose one variable', choices = c("All",sort(as.character(unique(names(myData()))))),selected = "age")
    })
    output$category2 <- renderUI({
      selectizeInput('cat2', 'Choose the seconde variable', choices = c("All",sort(as.character(unique(names(myData()))))),selected = "age")
    })
    
    df_subset <- eventReactive(input$cat1,{
      if(input$cat1=="age") {df_subset <- data}
      else{df_subset <- data[input$cat1,]}
      
    })

    observeEvent(input$cat1, {
      print(paste0("You have chosen: ", input$cat1))
      
    })
    # customize the length drop-down menu; display 5 rows per page by default
    output$mytable3 <- DT::renderDataTable({
        DT::datatable(myData())
      
    })
    
    #BOXPLOT
    output$boxplot <- renderPlot({
        boxplot(myData()[input$cat1],
                at = c(1),
                names = c("Petal.W"),
                col = c("orange"),
                main = input$cat1,
                xlab = input$cat1,las = 1,
                border = "brown",
                notch = TRUE,
                horizontal = TRUE
        )
    })
    #HISTOGRAM
    
    output$HistogramPW <- renderPlot({
      
        pw = myData()[[input$cat1]]

        bins <- seq(min(pw), max(pw), length.out = input$bins + 1)
        hist(pw,
             breaks = bins,
             col = "orange",
             main = input$cat1,
             xlab = input$cat1,
             ylab = "Number ")

    })
    
    output$summary <- renderPrint({
      dataset <- myData()
      summary(dataset)
    })
    
    #-------------------------Pie--------------------------------------
    output$Pie <- renderPlot({
      pie(table(myData()[input$cat1]), labels = names(table(myData()[input$cat1])), 
          main = input$cat1, col=c())    
      })
  
    #-------------------------Customer attrition--------------------------------------
    output$Customer_attrition <- renderPlot({
      pie(table(myData()$y), labels = names(table(myData()$y)), 
          main = "churn", col=c())    
    })
    
    #----------------------PAIRS--------------------------------------
    
    #-----------------------NUAGE---------------------------------------
    library(ggplot2)
    
    output$Nuage <- renderPlot({
      # Basic scatter plot
      p <- ggplot(myData(), aes(x=myData()[[input$cat1]], y=myData()[[input$cat2]])) + geom_point()
      p + labs(x = input$cat1,y = input$cat2)

    })
    
    
    #-----------------------LR--------------------------------------------
    library(DataExplorer)
    library(corrplot)
    output$Coorelation <- renderPlot({
            plot_correlation(myData())
      
    })
    #---------------------- Barplot---------------------------------------
   
    # Bidimensionnel
    output$barplotBi <- renderPlot({
      # Diagramme en barres entre les variables 'Level' et 'Sex'
      p <- ggplot(myData(), aes(x =myData()[[input$cat1]], fill = y)) + geom_bar()
      p + labs(x = input$cat1)
    })
    
    output$barplotProfils <- renderPlot({
      # Diagramme de profils entre les variables 'Level' et 'Sex'
      p <- ggplot(myData(), aes(x = myData()[[input$cat1]], fill = y)) + geom_bar(position = "fill")
      p + labs(x = input$cat1)
      
    })
  
    #----------------------ABOUT------------------------------------------
    output$About <- renderUI({
        
        HTML(
            paste("<h1>About</h1>"),
            paste("<h1>Edgar Anderson's Iris Data</h1>"),
            paste("<h2>Description</h2>"),
            paste("<p>This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.</p>"),
            paste("<h2>Usage</h2>"),
            paste("<p>iris , iris3</p>"),
            paste("<h2>Format</h2>"),
            paste("<p>iris is a data frame with 150 cases (rows) and 5 variables (columns) named Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species.
iris3 gives the same data arranged as a 3-dimensional array of size 50 by 4 by 3, as represented by S-PLUS. The first dimension gives the case number within the species subsample, the second the measurements with names Sepal L., Sepal W., Petal L., and Petal W., and the third the species.</p>"),
            paste("<h2>Source</h2>"),
            paste("<p>Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7, Part II, 179–188.

The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5.</p>"),
            
            paste("<h2>References</h2>"),
            paste("<p>Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole. (has iris3 as iris.)</p>"),
            
            paste("<h2>See Also</h2>"),
            paste("<p>matplot some examples of which use iris.</p>"),
            
            paste("<h2>Examples</h2>")
            
            
            )
      
        
        })
    
}

shinyApp(ui, server)