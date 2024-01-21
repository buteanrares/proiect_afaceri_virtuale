library(shiny)
library(arules)

data <- read.csv("grocery_data.csv")

ui <- fluidPage(
  titlePanel("Online Groceries Analysis App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      checkboxInput("header", "Header", TRUE),
      selectInput("item_col", "Item Column", names(data), selected = names(data)[3]),
      actionButton("analyze_btn", "Analyze")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Association Rules", tableOutput("rules_table")),
        tabPanel("Most Purchased Items", plotOutput("pie_chart")),
        tabPanel("Seasonal Analysis", plotOutput("seasonal_plot"))
      )
    )
  )
)

server <- function(input, output) {
  
  data_input <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = input$header)
  })
  
  apriori_results <- eventReactive(input$analyze_btn, {
    data <- data_input()
    transactions <- as(split(data[, input$item_col], data$Member_number), "transactions")
    
    # Apriori algorithm
    rules <- apriori(transactions, parameter = list(support = 0.005, confidence = 0.5))
    return(rules)
  })
  
  # Output top 100 most confident associations in a table
  output$rules_table <- renderTable({
    rules <- apriori_results()
    
    top_associations <- head(rules[order(rules@quality$confidence, decreasing = TRUE)], 100)
    
    as.data.frame(inspect(top_associations))
  })
  
  # Output pie chart of the most purchased items
  output$pie_chart <- renderPlot({
    data <- data_input()
    item_counts <- table(data[, input$item_col])
    top_items <- head(sort(item_counts, decreasing = TRUE), 10)
    
    pie(top_items, labels = names(top_items), main = "Top 10 Most Purchased Items", col = rainbow(length(top_items)))
  })
  
  # Reactive function for Seasonal Analysis
  seasonal_analysis <- eventReactive(input$analyze_btn, {
    data <- data_input()
    data$Date <- as.Date(data$Date, format="%d-%m-%Y")
    
    data$Month <- format(data$Date, "%m")
    data$Year <- format(data$Date, "%Y")
    
    monthly_counts <- table(data$Month)
    
    plot(monthly_counts, type = "b", col = "blue", xlab = "Month", ylab = "Number of Purchases",
         main = "Seasonal Trend Analysis")
  })
  
  # Output plot for Seasonal Analysis
  output$seasonal_plot <- renderPlot({
    seasonal_plot <- seasonal_analysis()
    seasonal_plot
  })
  
}

shinyApp(ui = ui, server = server)
