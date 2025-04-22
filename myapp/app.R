library(shiny)
library(datasets)
library(ggplot2)
library(dplyr)

# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
# mpgData <- read.csv("/Users/gabistegmann/Library/CloudStorage/GoogleDrive-gabistegmann@gmail.com/My Drive/Personal/R Learning/Shiny Apps/shiny_app_04 - dummy data play/dummy_data.csv")

id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
disease = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
artp = rnorm(length(id))
sp_rate = rnorm(length(id))

mpgData = cbind.data.frame(id, disease, artp, sp_rate)

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Boxplots for SpeechFeatures"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable:",
                  c("Speaking Rate" = "sp_rate",
                    "Artic. Prec." = "artp")),
      
      selectInput("participant", "Participant:",
                  c("Part. 1" = 1,
                    "Part. 2" = 2,
                    "Part. 3" = 3,
                    "Part. 4" = 4,
                    "Part. 5" = 5)),
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("sp_artp"),
      
      plotOutput("box_1"),
      
      plotOutput("box_2")
      
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste(input$variable, "~ as.factor(disease)")
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$sp_artp <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
  output$box_1 <- renderPlot({
    ggplot(mpgData, aes(x = sp_rate, y = artp)) + geom_point()
  })
  
  
  
  output$box_2 <- renderPlot({
    
    data_participants = mpgData %>%
      filter(id %in% input$participant)
    
    ggplot(data_participants, aes(x = sp_rate, y = artp)) + geom_point()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
