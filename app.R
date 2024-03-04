#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
ui <- fluidPage(
  titlePanel("Predict how many wanted brexit to happen"),
  sidebarLayout(
    sidebarPanel(
      p("Before Brexit, pollsters conducted surveys to find out how many people
      wanted Britain to exit Europeon Union. The slider provides for selecting
      the the sample size for the surveys. You can move the slider to select the
      sample size of the survey and a prediction is provided as to how many
      people (as a fraction of 1) wanted Brexit."),
      sliderInput("sliderSamplesize", "Sample size people",
                  min = 450, max = 4850, value = 500, step = 100),
      p("The pollsters who carried out more than ten polls have been used to 
      highlight, while the plot contains points from all the pollsters. You
      can select the pollster whose sample points will be highlighted by
      selecting the checkbox corresponding to the pollster."),
      checkboxInput("showPollsterA", "Highlight ICM",
                    value = TRUE),
      checkboxInput("showPollsterB", "Highlight YouGov",
                    value = TRUE),
      checkboxInput("showPollsterC", "Highlight ORB",
                    value = TRUE)
    ),
    mainPanel(
      plotOutput("plot1"),
      h3("Wanted brexit to happen"),
      textOutput("PredLeave")
    )
  )
)

library(dslabs)
library(dplyr)
library(ggplot2)
data("brexit_polls")

# function for Server logic
server <- function(input, output){
  modelL <- lm(leave ~ samplesize, data = brexit_polls)
  
  moreThan10Polls <- brexit_polls %>%
    count(pollster) %>%
    arrange(desc(n)) %>%
    filter(n > 10) %>% .$pollster
  
  moreThan10Polls_data <- brexit_polls %>%
    filter(pollster %in% moreThan10Polls) %>%
    select(pollster, samplesize, leave)
  
  icm <- moreThan10Polls_data$pollster=="ICM"
  yougov <- moreThan10Polls_data$pollster=="YouGov"
  orb <- moreThan10Polls_data$pollster=="ORB"
  i_y <- icm | yougov
  i_o <- icm | orb
  y_o <- yougov | orb
  iyo <- icm | yougov | orb
  
  aplot <- brexit_polls %>%
    ggplot(aes(samplesize, leave)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", col = "darkblue", se = FALSE)
  
  addPlot <- function(ind){
    aplot + geom_point(aes(colour = pollster), size = 3,
                       data = moreThan10Polls_data[ind,])
  }
  
  modelLpred <- reactive({
    sampleSizeInput <- input$sliderSamplesize
    predict(modelL, newdata = data.frame(samplesize = sampleSizeInput))
  })
  
  output$plot1 <- renderPlot({
    sampleSizeInput <- input$sliderSamplesize
    addPoint <- geom_point(aes(sampleSizeInput, round(modelLpred(), 4)),
                           col = "darkblue", size = 4)
    
    if(input$showPollsterA & !input$showPollsterB & !input$showPollsterC){
      addPlot(icm) + addPoint
    } else if(input$showPollsterB & !input$showPollsterA & !input$showPollsterC){
      addPlot(yougov) + addPoint
    } else if(input$showPollsterC & !input$showPollsterA & !input$showPollsterB){
      addPlot(orb) + addPoint 
    } else if(input$showPollsterA & input$showPollsterB & !input$showPollsterC){
        addPlot(i_y) + addPoint
    } else if(input$showPollsterA & !input$showPollsterB & input$showPollsterC){
        addPlot(i_o) + addPoint
    } else if(!input$showPollsterA & input$showPollsterB & input$showPollsterC){
        addPlot(y_o) + addPoint
    } else if(input$showPollsterA & input$showPollsterB & input$showPollsterC){
      addPlot(iyo) + addPoint
    } else{
      aplot + addPoint
    }
  })
  
  output$PredLeave <- renderText({
    round(modelLpred(),4)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)