library(shiny)
ui <- fluidPage(
  titlePanel("📊 Return Distribution Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("ticker", "Enter Ticker (Yahoo Finance symbol):", value = "AAPL"),
      dateRangeInput("daterange", "Select Date Range:",
                     start = "2023-01-01", end = Sys.Date()),
      actionButton("go", "Analyze")
    ),
    
    mainPanel(
      h4("Summary Statistics"),
      tableOutput("stats"),
      
      h4("Histogram of Returns"),
      plotOutput("histPlot"),
      
      h4("Time Series of Log Returns"),
      plotOutput("tsPlot")
    )
  )
)

server <- function(input, output) {
  returns_data <- eventReactive(input$go, {
    tq_get(input$ticker,
           from = input$daterange[1],
           to   = input$daterange[2]) %>%
      mutate(log_return = log(adjusted / lag(adjusted))) %>%
      drop_na()
  })
  
  output$stats <- renderTable({
    data <- returns_data()
    tibble(
      Mean = mean(data$log_return),
      Volatility = sd(data$log_return),
      Skewness = skewness(data$log_return),
      Kurtosis = kurtosis(data$log_return)
    )
  })
  
  output$histPlot <- renderPlot({
    data <- returns_data()
    ggplot(data, aes(x = log_return)) +
      geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.7) +
      labs(title = paste("Histogram of", input$ticker, "Returns"),
           x = "Log Return", y = "Frequency") +
      theme_minimal()
  })
  
  output$tsPlot <- renderPlot({
    data <- returns_data()
    ggplot(data, aes(x = date, y = log_return)) +
      geom_line(color = "darkorange") +
      labs(title = paste(input$ticker, "Daily Log Returns"),
           x = "Date", y = "Log Return") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)