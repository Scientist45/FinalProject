# Define server logic for random distribution app ----
library(readr)
dataset <- read_csv("movie_metadataupdated.csv")


server <- function(input, output) {
  library(tidyverse)
  library(factoextra)
  library(devtools)
  library(DT)
  library(caret)
  library(shiny)
  library(rpart)
  library(rpart.plot)
  library(rattle)
  library(RColorBrewer)
  
  #######################################################
  
  # Information Tab
  ##Need to have InformationTab.Rmd file for this to work
  # function to render .Rmd files to html
  inclRmd <- function(path, r_env = parent.frame()) {
    encoding <- getOption("shiny.site.encoding", default = "UTF-8")
    paste(
      readLines(path, warn = FALSE, encoding = encoding),
      collapse = '\n'
    ) %>%
      knitr::knit2html(
        text = .,
        fragment.only = TRUE,
        envir = r_env,
        options = "",
        stylesheet = "",
        encoding = encoding
      ) %>%
      gsub("&lt;!--/html_preserve--&gt;","",.) %>%  ## knitr adds this
      gsub("&lt;!--html_preserve--&gt;","",.) %>%   ## knitr adds this
      HTML
  }
  
  #Formatted text for information tab
  output$page1 <- renderUI({
    inclRmd("InformationTab.Rmd")
  })
  
  
  ######################################################
  
  #Process Data
  ProcessData <- reactive({
    dataset <- read_csv("movie_metadataupdated.csv")
    dataset <- dataset[complete.cases(dataset), ]
    dataset <- dataset %>% subset(genres == "Action" | genres == "Adventure" | genres == "Animation" | genres == "Comedy" | genres == "Drama" | genres == "Fantasy" | genres == "Horror")
  })
  
  #####################################
  
  #PCA Graph with genre as the color
  output$PCAplot <- renderPlot({
    
    #get data
    dataForPlot <- ProcessData()
    dataForPlot2 <- dataForPlot %>% subset(country == "USA" | country == "UK" | country == "Japan" | country == "New Zealand"| country == "Australia")
    Data1 <- dataForPlot2 %>% select(duration, gross, cast_total_facebook_likes, budget, title_year, imdb_score)
    
    #Get PCA values
    pcadata <- prcomp(Data1, center = TRUE, scale=TRUE)
    
    if(input$pcaVariable == "genres"){
      #Make Plot
      fviz_pca_biplot(pcadata, geom.ind = "point", pointshape = 21,
                      pointsize = 2,
                      fill.ind = dataForPlot2$genres,
                      col.ind = "black",
                      palette = "jco",
                      addEllipses = TRUE,
                      label = "var",
                      col.var = "black",
                      repel = TRUE,
                      legend.title = "Genre") +
        ggtitle("2D PCA-plot from the Highest Grossing Films Dataset") +
        theme(plot.title = element_text(hjust = 0.5))
    }else{
      #Make Plot
      fviz_pca_biplot(pcadata, geom.ind = "point", pointshape = 21,
                      pointsize = 2,
                      fill.ind = dataForPlot2$country,
                      col.ind = "black",
                      palette = "jco",
                      addEllipses = TRUE,
                      label = "var",
                      col.var = "black",
                      repel = TRUE,
                      legend.title = "Genre") +
        ggtitle("2D PCA-plot from the Highest Grossing Films Dataset") +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  #############################################
  
  # Generate an HTML table view of the data ----
  output$table <- DT::renderDataTable({
    dataset2 <- ProcessData()
    Data2 <- dataset2 %>% arrange(desc(gross))
    Data2 <- Data2[1:500,]
    Data3 <- Data2 %>% select("director_name", "gross", "genres", "movie_title", "budget", "title_year", "imdb_score")
    Data3
  })
  
  tabledata <- reactive({
  dataset2 <- ProcessData()
  Data2 <- dataset2 %>% arrange(desc(gross))
  Data2 <- Data2[1:500,]
  Data3 <- Data2 %>% select("director_name", "gross", "genres", "movie_title", "budget", "title_year", "imdb_score")
  Data3
})
  ########################################
  
  #download table
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("downloadData", "csv", sep=".")
    },
    content= function(file){
      write.csv(tabledata(), file)
    }
  )
  
  #####################################
  
  #Histogram of Rating and Genres
  #Data for his plot
  getData <- reactive({
    HisData <- ProcessData()
    Data1 <- HisData %>% select(duration, gross, cast_total_facebook_likes, budget, title_year, imdb_score)
    datagenres <- HisData %>% filter(genres == input$genres)
    datagenres
  })
  
  ################################################
  
  # Histogram plotting and setup for download button
  
  hisPlotSetup <- reactive(
    ggplot(data = getData(), aes(x = gross)) +
      geom_histogram(bins = 20) + ggtitle("Histogram of Gross Income by Selected Genre") + xlab("Gross Profit") + ylab("Count")
  )
  
  output$hisPlot <- renderPlot(hisPlotSetup())
  ######################################
  
  #His plot info
  output$hisInfo <- renderText({
    #Get data
    hisInfo <- getData()
    #paste
    paste("The mean gross profit for", input$genres, "is", round(mean(hisInfo$gross)))
  })
  
  output$math <- renderUI({
    withMathJax(helpText('Mean Equation: $$Mean = \\frac{\\sum_{}X}n \\cdot$$'))
  })
  
  output$math2 <- renderUI({
    withMathJax(sprintf("Where \\(X\\) is the sum of gross values and \\(n\\) is the number of items aka count."))
  })
  
  ########################################
  # Histogram Plot download button code
  output$downloadPlot <- downloadHandler(
    filename = "hisPlot.png" ,
    content = function(file){
      png(file)
      plot(hisPlotSetup())
      dev.off()
    }
  )
  
  #################################################
  
  #Download Button Histogram data table
  output$downloadHisData <- downloadHandler(
    filename = function(){
      paste("downloadData", "csv", sep=".")
    },
    content= function(file){
      write.csv(getData(), file)
    }
  )
  
  #################################################
  
  #Make table for histogram data
  output$HisTableData <- DT::renderDataTable({
    HisTableData1 <- getData()
    HisTableData1
  })
  
  ##################################################
  
  #Plotting Linear regression model
  output$LMPlot <- renderPlot({
    
    modelingdataset <- ProcessData()
    
    #Data Pre-Processing
    ModData <- modelingdataset %>% select("gross", "cast_total_facebook_likes","budget", "imdb_score")
    
    #Plot the linear regression Model with variable selected on drop down menu
    if(input$variable == "imdb_score"){
      #plot imbd_score and gross
      plot(ModData$imdb_score, ModData$gross, xlab = "IMDB Score", ylab = "Gross Profit", main = "IMDB Score vs Gross Profit")
      #fit linear model
      lmline1 <- lm(gross ~ imdb_score, ModData)
      #graph fit line
      abline(lmline1)
    }
    else{
      #plot cast total facebook lines and gross
      plot(ModData$cast_total_facebook_likes, ModData$gross, xlab = "Cast Total Facebook Likes", ylab = "Gross Profit", main = "Cast Total Facebook Likes vs Gross Profit")
      #fit linear model
      lmline2 <- lm(gross ~ cast_total_facebook_likes, ModData)
      #graph fit line
      abline(lmline2)
    }
    
  })
  
  #################################################
  #Click on the plot and get output
  output$info <- renderText({
    paste0("Click points on the graph to find the ", input$variable, " and Gross Profit values for individual points", "\n", input$variable, " is equal to ", input$plot_click$x, "\n", "Gross Profit is equal to ", input$plot_click$y)
  })
  
  
  #################################################
  #Predicting Linear regression model
  output$LMText <- renderText({
    
    modelingdataset <- ProcessData()
    
    #Data Pre-Processing
    ModData <- modelingdataset %>% select("gross", "cast_total_facebook_likes","budget", "imdb_score")
    
    #Predict the linear regression Model with variable selected on drop down menu
    if(input$variable == "imdb_score"){
      #fit linear model
      lmline1 <- lm(gross ~ imdb_score, ModData)
      #graph fit line
      pred1 <- predict(lmline1, data.frame(imdb_score = input$predictvalue))
      paste(c("Predicted Gross Profit for imdb score =", pred1))
    }
    else{
      #fit linear model
      lmline2 <- lm(gross ~ cast_total_facebook_likes, ModData)
      #graph fit line
      pred2 <- predict(lmline2, data.frame(cast_total_facebook_likes = input$predictvalue))
      paste(c("Predicted Gross Profit for the Casts total facebook likes =", pred2))
    }
    
    
  })
  
  #################################################
  
  #Modeling Tree Model
  output$Tree <- renderPlot({
    
    modelingdataset <- ProcessData()
    
    #Data Pre-Processing
    ModData <- modelingdataset %>% select("gross", "cast_total_facebook_likes","budget", "imdb_score")
    
    #split into training and test
    Train <- sample(1:nrow(ModData), size = nrow(ModData)*0.8)
    Test <- dplyr::setdiff(1:nrow(ModData), Train)
    ModTrain <- ModData[Train, ]
    ModTest <- ModData[Test, ]#split into training and test
    Train <- sample(1:nrow(ModData), size = nrow(ModData)*0.8)
    Test <- dplyr::setdiff(1:nrow(ModData), Train)
    ModTrain <- ModData[Train, ]
    ModTest <- ModData[Test, ]
    
    
    # Tree Model
    if(input$TreePick & input$fancy){
      class.tree <- rpart(gross ~ ., data = ModTrain)
      fancyRpartPlot(class.tree)
    } else if (input$TreePick){
      class.tree <- rpart(gross ~ ., data = ModTrain)
      prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 4, varlen = 0)
    } else {
      class.tree <- rpart(gross ~ cast_total_facebook_likes + imdb_score, data = ModTrain)
      prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 4, varlen = 0)
    }
    
  })
  ##############################################
  
  
  
}