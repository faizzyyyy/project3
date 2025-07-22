library(shiny)
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)

# Load dataset
data <- read.csv("car.csv")

# Preprocessing
set.seed(123)
data$Fuel_Type <- as.factor(data$Fuel_Type)
data$Selling_type <- as.factor(data$Selling_type)
data$Transmission <- as.factor(data$Transmission)
data$Owner <- as.factor(data$Owner)
data$Car_Name <- NULL

# Split data
trainIndex <- createDataPartition(data$Selling_Price, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train Random Forest model
model <- randomForest(Selling_Price ~ ., data = trainData)
predictions <- predict(model, newdata = testData)

# UI
ui <- fluidPage(
  titlePanel("Car Price Prediction App"),
  tabsetPanel(
    tabPanel("Data Preview", 
             fluidPage(style = "background-color:#f0f0f0;",
                       h3("Car Dataset Preview"),
                       DT::dataTableOutput("dataPreview")
             )
    ),
    tabPanel("Exploratory Data Analysis",
             fluidPage(style = "background-color:#fdf6e3;",
                       plotOutput("distPlot"),
                       plotOutput("fuelBoxPlot"),
                       plotOutput("transmissionPlot"),
                       plotOutput("correlationPlot")
             )
    ),
    tabPanel("Prediction Model",
             fluidPage(style = "background-color:#f6f8fa;",
                       fluidRow(
                         column(4,
                                numericInput("year", "Year of Manufacture", value = 2015, min = 2000, max = 2025),
                                numericInput("present_price", "Present Price (in Lakhs)", value = 5),
                                numericInput("kms_driven", "Kilometers Driven", value = 30000)
                         ),
                         column(4,
                                selectInput("fuel_type", "Fuel Type", choices = levels(data$Fuel_Type)),
                                selectInput("seller_type", "Seller Type", choices = levels(data$Selling_type)),
                                selectInput("transmission", "Transmission", choices = levels(data$Transmission))
                         ),
                         column(4,
                                selectInput("owner", "Number of Owners", choices = levels(data$Owner)),
                                br(),
                                actionButton("predictBtn", "Predict Price")
                         )
                       ),
                       h3("Predicted Car Selling Price:"),
                       verbatimTextOutput("predictionOutput")
             )
    ),
    tabPanel("Model Evaluation",
             fluidPage(style = "background-color:#e0f7fa;",
                       h3("Model Evaluation"),
                       verbatimTextOutput("modelSummary"),
                       plotOutput("residualPlot")
             )
    )
  )
)

# Server
server <- function(input, output) {
  output$dataPreview <- DT::renderDataTable({
    DT::datatable(data)
  })
  
  output$distPlot <- renderPlot({
    ggplot(data, aes(x = Selling_Price)) + 
      geom_histogram(fill = "steelblue", bins = 30) + 
      theme_minimal()
  })
  
  output$fuelBoxPlot <- renderPlot({
    ggplot(data, aes(x = Fuel_Type, y = Selling_Price, fill = Fuel_Type)) +
      geom_boxplot() + theme_minimal()
  })
  
  output$transmissionPlot <- renderPlot({
    ggplot(data, aes(x = Transmission, fill = Transmission)) +
      geom_bar() + theme_minimal()
  })
  
  output$correlationPlot <- renderPlot({
    corr_data <- data %>% 
      select_if(is.numeric) %>% 
      cor()
    corrplot::corrplot(corr_data, method = "color", addCoef.col = "black")
  })
  
  observeEvent(input$predictBtn, {
    new_data <- data.frame(
      Year = input$year,
      Present_Price = input$present_price,
      Driven_kms = input$kms_driven,
      Fuel_Type = factor(input$fuel_type, levels = levels(data$Fuel_Type)),
      Selling_type = factor(input$seller_type, levels = levels(data$Selling_type)),
      Transmission = factor(input$transmission, levels = levels(data$Transmission)),
      Owner = factor(input$owner, levels = levels(data$Owner))
    )
    pred <- predict(model, newdata = new_data)
    output$predictionOutput <- renderText({ paste("Rs.", round(pred, 2), "Lakhs") })
  })
  
  output$modelSummary <- renderPrint({
    postResample(pred = predictions, obs = testData$Selling_Price)
  })
  
  output$residualPlot <- renderPlot({
    residuals <- testData$Selling_Price - predictions
    plot(predictions, residuals, 
         xlab = "Predicted", ylab = "Residuals", 
         main = "Residuals vs Predicted", col = "blue", pch = 20)
    abline(h = 0, col = "red")
  })
}

shinyApp(ui = ui, server = server)
