library(shiny)
library(arules)
library(dplyr)
library(readxl)
library(cluster)

# Function to clean data
clean_data <- function(data) {
  sum(is.na(data)) #no NA data found
  
  payment_counts <- table(data$paymentType)
  boxplot( x = data$count,
           main = "Objects count",
           xlab = "Count") #outliers found in count and fixed by the following code
  
  summary(data) #to get q3 , q1
  
  iqr <- 6 - 2 # Calculate the interquartile range (IQR), q3-q1
  
  # Define the lower and upper bounds to identify outliers
  lower_bound <- 2 - 1.5 * iqr 
  upper_bound <- 6 + 1.5 * iqr
  
  # Remove outliers from the count variable
  data_clean <- data %>%
    filter(count >= lower_bound & count <= upper_bound)
  
  sum(duplicated(data_clean)) # 2 duplicates found
  
  data_clean <- distinct(data_clean) #removing the duplicated
  
  return(data_clean)
}

# UI Definition
ui <- fluidPage(
  titlePanel("Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Number of Clusters Input
      numericInput("num_clusters", "Number of Clusters (2-4):", value = 2, min = 2, max = 4),
      # File Input for CSV
      fileInput("file", "Choose CSV File", accept = ".csv"),
      # Apriori Support Input
      numericInput("support_input", "Apriori Support (0.001 - 1):", value = 0.1, min = 0.001, max = 1),
      # Apriori Confidence Input
      numericInput("confidence_input", "Apriori Confidence (0.001 - 1):", value = 0.5, min = 0.001, max = 1),
      # Analyze Data Button
      actionButton("analyze_button", "Analyze Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 fluidRow(
                   column(6, plotOutput("barplot_city")),
                   column(6, plotOutput("pie_chart_payment"))
                 ),
                 fluidRow(
                   column(6, plotOutput("scatterplot_age")),
                   column(6, plotOutput("boxplot_outliers"))
                 )
        ),
        tabPanel("K-Means Clustering", tableOutput("clusters_table")),
        tabPanel("Apriori Rules", verbatimTextOutput("apriori_rules_text")) # Change here
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$barplot_city <- renderPlot({
    req(input$file)  # Ensure file is uploaded
    data <- data()
    cleaned <- clean_data(data)
    
    DataByCity <- cleaned %>%
      group_by(city) %>%
      summarise(TotalSpending = sum(total)) %>%
      arrange(desc(TotalSpending))
    
    barplot(
      height = DataByCity$TotalSpending,
      names.arg = DataByCity$city,
      col = c("red"),
      main = "Compare total spending By City",
      las = 2
    )
  })
  
  output$pie_chart_payment <- renderPlot({
    req(input$file)  # Ensure file is uploaded
    data <- data()
    cleaned <- clean_data(data)
    
    payment_counts <- table(cleaned$paymentType)
    x = table(payment_counts)
    percentage = paste0(round(100*x/sum(x)),"%")
    pie(x, labels = percentage, main = "Compare payment", col=c("green","yellow"))
    legend("bottomright", legend = c("Cash", "Credit"), fill = c("green", "yellow"))
  })
  
  output$scatterplot_age <- renderPlot({
    req(input$file)  # Ensure file is uploaded
    data <- data()
    cleaned <- clean_data(data)
    
    DataByAge <- cleaned %>%
      group_by(age) %>%
      summarise(TotalSpending = sum(total)) %>%
      arrange((age))
    
    plot(
      x = DataByAge$age,
      y = DataByAge$TotalSpending,
      col = "blue",
      xlab = "Age",
      ylab = "Total Spending",
      main = "Scatter plot of Total Spending By Age"
    )
  })
  
  output$boxplot_outliers <- renderPlot({
    req(input$file)  # Ensure file is uploaded
    data <- data()
    cleaned <- clean_data(data)
    
    boxplot(cleaned$count, main = "Boxplot of Total Spending (Without Outliers)", outline = FALSE)
  })
  
  output$clusters_table <- renderTable({
    req(input$file)  # Ensure file is uploaded
    data <- data()
    NumberOfCluster <- input$num_clusters
    if (NumberOfCluster >= 2 && NumberOfCluster <= 4) {
      kmeansofData <- kmeans(data[, c("age", "total")], centers = NumberOfCluster)
      DataFrame <- data.frame(Customer = data$customer, Age = data$age, Total = data$total, "computed cluster" = kmeansofData$cluster)
      DataFrame
    } else {
      data.frame("Message" = "Please enter a number of clusters between 2 and 4.")
    }
  })
  
  output$apriori_rules_text <- renderPrint({
    req(input$file)  # Ensure file is uploaded
    data <- data()
    support <- input$support_input
    confidence <- input$confidence_input
    
    transactions_itemsv1 <- paste(data$items, sep="\n")
    items_file <- write(transactions_itemsv1, file="transactions_data.txt")
    tdata <- read.transactions("transactions_data.txt", sep=",")
    
    apriori_rules <- apriori(tdata, parameter = list(supp = support, conf = confidence))
    inspect(head(sort(apriori_rules, by = "confidence"), n=30000))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)





