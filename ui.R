library(shiny)
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Movie Metadata"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h3("Graph and Numeric Options"),
      #Menu with PCA fill variable
      selectizeInput("pcaVariable", "Select fill variable for PCA Model", choices = list("genres", "country")),
      #Menu with genre options
      selectizeInput("genres", "Select genres for Numeric and Graphical Summaries", choices = list("Action", "Adventure", "Animation", "Comedy", "Drama", "Fantasy", "Horror")),
      #Menu with variable options
      selectizeInput("variable", "Select Variable for Linear Regression Model", choices = list("imdb_score", "cast total facebook likes")),
      numericInput("predictvalue", "Choose a value and a prediction will be made for the linear regression model", value = 9),
      checkboxInput("TreePick", "Include Budget on Tree Model?"),
      #if treepick is checked then a new box pops up.
      conditionalPanel(
        checkboxInput("fancy", "Fancy Plot"),
        condition = "input.TreePick == true")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Information", uiOutput("page1")),
                  tabPanel("PCA Plot", plotOutput("PCAplot")),
                  tabPanel("Table", downloadButton("downloadData", "Download"), div(style='height:600px; overflow:scroll',DT::dataTableOutput("table"))),
                  tabPanel("Data Summary", 
                           fluidRow(
                             column(8, (downloadButton("downloadPlot", "Download Plot"))),
                             column(8, (downloadButton("downloadHisData", "Download Data"))),
                             column(8, plotOutput("hisPlot")),
                             column(8, textOutput('hisInfo')),
                             column(8, withMathJax(), uiOutput('math')),
                             column(8, withMathJax(), uiOutput('math2')),
                             column(8, br()),
                             column(8, h2("Table of Movie Data Selected by Genre")),
                             column(8, DT::dataTableOutput("HisTableData"))
                           )),
                  tabPanel("Modeling", 
                           fluidRow(
                             column(8, plotOutput("LMPlot", click = "plot_click")),
                             column(8, verbatimTextOutput("info")),
                             column(8, textOutput("LMText")),
                             column(8, plotOutput("Tree"))
                           ))
      )
    )
  )
)