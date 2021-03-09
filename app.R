rm(list = ls())
library(shiny)
library (reticulate )
library(dplyr)
library(class)
library(caret)
library(shiny)
library(Metrics)
library(scales)
library(RColorBrewer)
library(grid)
library(DataExplorer)
library(corrplot)
ui <- fluidPage(

    titlePanel("TP IRIS {GOUMEIDA AHMED SEYFEDDINE - MLDS}"),
    
    
    
    sidebarLayout(
        
        sidebarPanel(
          
         
          fileInput("file1", "Choose input data"),
          uiOutput("category1"),          
          uiOutput("category2"),
          sliderInput("k",
                      "number of neighbors (K of KNN)",
                      min = 1,
                      max = 20,
                      value = 5),

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
                           tabPanel("Variable ciblé(age)", 
                                    
                                    fluidRow(
                                      column(12,
                                             plotOutput(outputId = "barplotBi")
                                             ),
                                    fluidRow(      
                                      column(12,
                                             plotOutput("ScatterPlot")
                                             )
                                    )
                           )),
                           tabPanel("KNN", 
                                    dataTableOutput('confusionMatrix'),
                                    verbatimTextOutput("value")
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
      #print(paste0("You have chosen: ", myData()[1:20]))
      
    })
    observeEvent(input$k, {
      print(paste0("k = : ", input$k))
      #print(paste0("You have chosen: ", myData()[1:20]))
      
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

    output$Coorelation <- renderPlot({
            plot_correlation(myData())
      
    })
    #---------------------- Barplot---------------------------------------
   
    # Bidimensionnel
    output$barplotBi <- renderPlot({
      # Diagramme en barres entre les variables 'Level' et 'Sex'
      p <- ggplot(myData(), aes(x =myData()$age, fill = y)) + geom_bar()
      p + labs(x = "age")
    })
    
    output$ScatterPlot <- renderPlot({
      ggplot(myData(), aes_string(x = myData()$age, 
                            y = myData()[[input$cat1]], 
                            color = "factor(y)")) + 
        geom_point(size = 2, position = position_jitter(w = 0.1, h = 0.1)) + 
        labs(x = "Age",
             y = input$cat1) +
        fte_theme() + 
        scale_color_manual(name = "Churn",values=c("#7A99AC", "#E4002B")) 
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
#----------------------KNN--------------------------------------------
  
    

      
      output$value <- renderText({ paste("Classification Error = ",ce(test.Y,knn.pred)) })
      output$confusionMatrix <- renderDataTable({
        
        data= myData()
        print(paste0("y 9bel myData ", data$y))
        
        #data <- as.data.frame(apply(data, 2, as.numeric))
        as.numeric(data[,21])->data[,21]
        
        data <- na.omit(data)
        
        data$y[data$y > 0] <- 1
        data$y <- factor( data$y, levels = c(0,1), labels = c("negative", "positive"))
        print(paste0("y   =  ", data$y))
        
        # standardize all point except the response variable
        standardized.X <- scale(data[,-21])
        set.seed(55)

        # create training and test sets
        training.index <- caret::createDataPartition(data$y, p = .8,list = F)
        train.X <- standardized.X[training.index,]
        test.X  <- standardized.X[-training.index,]
        train.Y <- data$y[training.index]
        test.Y <- data$y[-training.index]
        
        set.seed(1)
        knn.pred <- knn(data.frame(train.X[,]),
                        data.frame(test.X[,]),
                        train.Y, k = input$k)
        
        
        
        # modify this to show title - confusion matrix
        # /false posit  ive/positive false negative/negative
        true.positive    <- sum(knn.pred == "positive" & test.Y == "positive")
        false.positive   <- sum(knn.pred == "negative" & test.Y == "positive")
        true.negative    <- sum(knn.pred == "negative" & test.Y == "negative")
        false.negative   <- sum(knn.pred == "positive" & test.Y == "negative")
        row.names <- c("Prediction - FALSE", "Prediction - TRUE" )
        col.names <- c("Reference - FALSE", "Reference - TRUE")
        cbind(Outcome = row.names, as.data.frame(matrix( 
          c(true.negative, false.negative, false.positive, true.positive) ,
          nrow = 2, ncol = 2, dimnames = list(row.names, col.names))))
      }, options = table.settings
      )
      
      table.settings <- list(searching = F, pageLength = 5, bLengthChange = F,
                             bPaginate = F, bInfo = F )
    
    
}

#----------------------------------THEME-------------------------------------------
# define theme for ggplots ####
fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]
  text.size <- 14
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.50)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=text.size,color=color.axis.title)) +
    theme(legend.title = element_text(size=text.size,color=color.axis.title)) +
    theme(legend.position = "bottom") +
    theme(legend.direction = "vertical") +
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=text.size, vjust=1.25)) +
    theme(axis.text.x=element_text(size=text.size,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=text.size,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=text.size,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=text.size,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
shinyApp(ui, server)