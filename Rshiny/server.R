library(shiny)
library(ggplot2)
library(dplyr)

filtered_combined_data <- read.csv("filtered_combined_data.csv")

# Define the server
server <- function(input, output, session) {
  
  # Reactive dataset
  data <- reactive({
    filtered_combined_data
  })
  
  # Update dropdown options based on the dataset
  observe({
    updateSelectInput(session, "x_attr", 
                      choices = names(data()), 
                      selected = names(data())[1])
  })
  
  # Render the plot
  output$plot <- renderPlot({
    req(input$x_attr)
    
    dataset <- data()
    
    # Ensure quality column is a factor
    dataset$quality <- as.factor(dataset$quality)
    
    # If the chosen attribute is numeric and the plot type is not scatterplot, bin it
    if (is.numeric(dataset[[input$x_attr]]) && input$plot_type != "Scatterplot") {
      dataset[[input$x_attr]] <- cut(dataset[[input$x_attr]], breaks = input$bins, include.lowest = TRUE)
    }
    
    # Generate the selected plot type
    if (input$plot_type == "Scatterplot") {
      p <- ggplot(dataset, aes_string(x = input$x_attr, y = "quality")) +
        geom_point(alpha = 0.5) +
        labs(title = paste("Quality vs", input$x_attr), 
             x = input$x_attr, 
             y = "Quality") +
        theme_minimal()
      
      # Add facet wrapping by quality if the checkbox is selected
      if (input$facet_by_quality) {
        p <- p + facet_wrap(~quality)
      }
      
      p
      
    } else if (input$plot_type == "Bar Chart (Count)") {
      p <- ggplot(dataset, aes_string(x = input$x_attr, fill = "quality")) +
        geom_bar(position = "dodge") +
        labs(title = paste("Count of Quality by", input$x_attr), 
             x = input$x_attr, 
             y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Add totals on bars if checkbox is selected
      if (input$show_totals) {
        p <- p + geom_text(stat = "count", aes(label = ..count..), 
                           position = position_dodge(width = 0.9), vjust = -0.5)
      }
      
      p
      
    } else if (input$plot_type == "Percentage Bar Chart") {
      percentage_data <- dataset %>%
        group_by_at(vars(input$x_attr, "quality")) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by_at(vars(input$x_attr)) %>%
        mutate(percentage = (count / sum(count)) * 100)
      
      p <- ggplot(percentage_data, aes_string(x = input$x_attr, y = "percentage", fill = "quality")) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Percentage of Quality by", input$x_attr), 
             x = input$x_attr, 
             y = "Percentage (%)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Add totals on bars if checkbox is selected
      if (input$show_totals) {
        p <- p + geom_text(aes(label = round(percentage, 1)), 
                           position = position_dodge(width = 0.9), vjust = -0.5)
      }
      
      p
    }
  })
}
