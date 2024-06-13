# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# This app is used to evaluate multiple forecast models using Taylor diagrams
# Published by Yi Xu https://cdnsciencepub.com/doi/pdf/10.1139/cjfas-2023-0139
# Contact yi.xu@dfw.wa.gov for questions, last update 2024.6.13.
# Data is an old fraser sockeye forecast retro analysis, for demonstration purpose only

library(shiny)

library(plotrix)

raw <- read.csv("retro_demo.csv") # replace this with your own file

stock <- unique(raw$popID)
age <- unique(raw$age)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title and subtitle
  fluidRow(
    column(12, align = "center",
           h1("Taylor Diagram"),
           h4("A Salmon Forecast Model Evaluation Tool")
    )
  ),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel( width = 3,
      
      # Input: Selectize input for selecting a stock
      selectizeInput(
        inputId = "selectedStock",     # Input ID
        label = "Select stock",        # Input label
        choices = stock                # Dropdown choices
      ), 
      # Input: Selectize input for selecting an age
      selectizeInput(
        inputId = "selectedAge",     # Input ID
        label = "Select age",        # Input label
        choices = age                # Dropdown choices
      ),
      br(),
      div("Developed by Yi Xu"),
      div("yi.xu@dfw.wa.gov"),
      div("Last update 2024.6.13."),
      div("Method in Xu et al. 2024 CJFAS")
    ),
    
    # Main panel for displaying outputs
    mainPanel( width = 9,
      
      # Output: Plot of the Taylor diagram
      plotOutput(outputId = "Taylor", height = "480px")
    )
  )
)

# Define server logic required to draw the Taylor diagram
server <- function(input, output) {
    
    output$Taylor <- renderPlot({
      # Select the proper index for stock and age
      idx <- which(raw$popID == input$selectedStock & raw$age == input$selectedAge)
      
      models <-unique(raw$model[idx])
      yr <- unique(raw$retyr[idx])
      obs <- unique(raw$obs[idx])
      
      
      par(pty="s",xpd = TRUE,  omi = c(0,0,0,0), bg = NA)
      # plot observation first
      taylor.diagram(obs,obs,col = "black", pch = 19,normalize = TRUE,main = "",
                     mar = c(2,0,2,7),xpd=NA, cex.main = 2, pcex = 1.2)
      # initialize the shape and color of legend (including observation and model)
      mypch_list <- 19
      mycolor_list <- "black"
      
      iric <- ipow<- ilak <- inaive <- 0 # for different group of models, use the same color but different shape
      
      for (i in 1:length(models)){ # loop over models
        
        forecast <- as.numeric(raw$forecast[raw$model==models[i]&raw$popID == input$selectedStock & raw$age == input$selectedAge])
        obs <- as.numeric(raw$obs[raw$model==models[i]&raw$popID == input$selectedStock & raw$age == input$selectedAge])

        # define color and shape for each model
        if (grepl("Ricker",models[i])) {
          mycolor = "red3"
          mypch = iric
          iric = iric + 1
        } else if (grepl("Power",models[i])) {
          mycolor = "blue"
          mypch = ipow
          ipow = ipow + 1
        } else if (grepl("Larkin",models[i])) {
          mycolor = "gold"
          mypch = ilak+15
          ilak = ilak + 1
        } else  if (grepl("sibling",models[i])) {
          mycolor = "forestgreen"
          mypch = 19
        } else {
          mycolor = "purple"
          mypch = inaive
          inaive =inaive + 1
          if(inaive > 25) inaive = inaive -25 # if they are not the Fraser sockeye models, then error check if they are more than 25 models (25 max for pch, need to change this accordingly when model names are finalized)
        }
        
        taylor.diagram(obs,forecast, # plot Taylor diagram for the model 
                       pch = mypch,col = mycolor,pcex = 1.2,
                       add = TRUE, normalize = TRUE)
        mypch_list <- c(mypch_list,mypch)
        mycolor_list <- c(mycolor_list,mycolor)
      }
      
      legend("right",inset = c(-0.7,0), legend = c("observation",models),
             col = mycolor_list, pch = mypch_list,cex = 1.1,
             bty = "n",xpd = NA, ncol = 2) # plot legend
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
